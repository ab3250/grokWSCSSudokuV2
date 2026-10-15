(import scheme 
        (chicken base) (chicken io) (chicken tcp)
        (chicken bitwise) (chicken blob) (chicken string)
        (srfi 1) (srfi 13) (srfi 18)
        base64 simple-sha1)

(define magic "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")

;; ---------- User hooks (override these) ----------
(define *clients* (make-parameter '()))



;; ---------- Helpers ----------
(define (websocket-accept key)
  (let* ((hex (string->sha1sum (string-append key magic)))
         (raw (let loop ((i 0) (acc '()))
                (if (>= i 40)
                    (list->string (reverse acc))
                    (loop (+ i 2)
                          (cons (integer->char
                                 (string->number (substring hex i (+ i 2)) 16))
                                acc))))))
    (string-trim (base64-encode raw))))

(define (read-headers in)
  (let loop ((lines '()))
    (let ((line (read-line in)))
      (cond ((or (eof-object? line) (string=? line "")) (reverse lines))
            (else (loop (cons line lines)))))))

(define (find-header headers prefix)
  (let ((found (find (lambda (l) (string-prefix? prefix l)) headers)))
    (and found (string-trim (substring found (string-length prefix))))))

;; Frame builder (FIN + opcode + length + payload)
(define (make-frame opcode payload)
  (let* ((len (string-length payload))
         (h1 (integer->char (bitwise-ior #x80 opcode)))
         (h2 (cond ((< len 126) (integer->char len))
                   ((< len 65536) (integer->char 126))
                   (else (integer->char 127)))))
    (string-append
     (string h1 h2)
     (cond ((< len 126) "")
           ((< len 65536)
            (string (integer->char (arithmetic-shift len -8))
                    (integer->char (bitwise-and len #xff))))
           (else
            ;; 64-bit length (big-endian)
            (let loop ((i 7) (acc ""))
              (if (< i 0) acc
                  (loop (- i 1)
                        (string-append acc
                          (string (integer->char
                                   (bitwise-and (arithmetic-shift len (* i -8)) #xff)))))))))
     payload)))

;; Unmask in place-style (returns new string)
(define (unmask payload mask)
  (let loop ((i 0) (acc '()))
    (if (>= i (string-length payload))
        (list->string (reverse acc))
        (loop (+ i 1)
              (cons (integer->char
                     (bitwise-xor (char->integer (string-ref payload i))
                                  (list-ref mask (modulo i 4))))
                    acc)))))

;; Basic UTF-8 check (rejects overlong / surrogates / invalid)
(define (valid-utf8? s)
  (let loop ((i 0))
    (if (>= i (string-length s)) #t
        (let ((b (char->integer (string-ref s i))))
          (cond ((< b #x80) (loop (+ i 1)))
                ((and (>= b #xc2) (< b #xe0) (< (+ i 1) (string-length s))
                      (= (bitwise-and (char->integer (string-ref s (+ i 1))) #xc0) #x80))
                 (loop (+ i 2)))
                ((and (>= b #xe0) (< b #xf0) (< (+ i 2) (string-length s))
                      (= (bitwise-and (char->integer (string-ref s (+ i 1))) #xc0) #x80)
                      (= (bitwise-and (char->integer (string-ref s (+ i 2))) #xc0) #x80))
                 (loop (+ i 3)))
                ((and (>= b #xf0) (< b #xf5) (< (+ i 3) (string-length s))
                      (= (bitwise-and (char->integer (string-ref s (+ i 1))) #xc0) #x80)
                      (= (bitwise-and (char->integer (string-ref s (+ i 2))) #xc0) #x80)
                      (= (bitwise-and (char->integer (string-ref s (+ i 3))) #xc0) #x80))
                 (loop (+ i 4)))
                (else #f))))))

;; ---------- Frame reader ----------
(define (read-frame in)
  (let ((b1 (read-byte in))
        (b2 (read-byte in)))
    (if (or (eof-object? b1) (eof-object? b2))
        (values #f #f #f #f)
        (let* ((fin (not (zero? (bitwise-and b1 #x80))))
               (rsv (bitwise-and b1 #x70))
               (opcode (bitwise-and b1 #x0f))
               (masked (not (zero? (bitwise-and b2 #x80))))
               (len7 (bitwise-and b2 #x7f))
               (payload-len
                (cond ((= len7 126)
                       (bitwise-ior (arithmetic-shift (read-byte in) 8)
                                    (read-byte in)))
                      ((= len7 127)
                       ;; simplified 64-bit (enough for Autobahn)
                       (let loop ((i 0) (n 0))
                         (if (>= i 8) n
                             (loop (+ i 1)
                                   (+ (arithmetic-shift n 8) (read-byte in))))))
                      (else len7)))
               (mask (and masked
                          (list (read-byte in) (read-byte in)
                                (read-byte in) (read-byte in))))
               (raw (read-string payload-len in)))
          (if (or (eof-object? raw) (not (= (string-length raw) payload-len)))
              (values #f #f #f #f)
              (let ((payload (if mask (unmask raw mask) raw)))
                (values fin opcode rsv payload)))))))

;; ---------- Send helpers (for your Scheme code) ----------
(define (send-text out text)
  (display (make-frame 1 text) out)
  (flush-output out))

(define (send-binary out data)
  (display (make-frame 2 data) out)
  (flush-output out))

(define (send-close out code reason)
  (let ((payload (string-append
                  (string (integer->char (arithmetic-shift code -8))
                          (integer->char (bitwise-and code #xff)))
                  reason)))
    (display (make-frame 8 payload) out)
    (flush-output out)))

(define (broadcast text)
  (for-each (lambda (c)
              (handle-exceptions exn #f
                (send-text c text)))
            (*clients*)))

;; ---------- Connection handler ----------
(define (handle-websocket in out)
  (let ((frag-opcode #f)
        (frag-buf ""))
    (let loop ()
      (let-values (((fin opcode rsv payload) (read-frame in)))
        (cond
          ((not opcode)
           (on-close out 1006 "abnormal"))
          ((not (zero? rsv))
           (send-close out 1002 "RSV")
           (on-close out 1002 "RSV"))
          ;; control frames
          ((= opcode 8)                 ; close
           (let* ((code (if (>= (string-length payload) 2)
                            (bitwise-ior (arithmetic-shift (char->integer (string-ref payload 0)) 8)
                                         (char->integer (string-ref payload 1)))
                            1005))
                  (reason (if (> (string-length payload) 2)
                              (substring payload 2)
                              "")))
             (send-close out code reason)
             (on-close out code reason)))
          ((= opcode 9)                 ; ping
           (display (make-frame #xA payload) out)
           (flush-output out)
           (loop))
          ((= opcode #xA)               ; pong
           (loop))
          ;; data frames
          ((or (= opcode 1) (= opcode 2) (= opcode 0))
           (cond
             ((= opcode 0)              ; continuation
              (unless frag-opcode
                (send-close out 1002 "unexpected continuation")
                (on-close out 1002 "unexpected continuation")
                (loop))
              (set! frag-buf (string-append frag-buf payload))
              (when fin
                (if (= frag-opcode 1)
                    (if (valid-utf8? frag-buf)
                        (on-text out frag-buf)
                        (begin (send-close out 1007 "invalid utf-8")
                               (on-close out 1007 "invalid utf-8")))
                    (on-binary out frag-buf))
                (set! frag-opcode #f)
                (set! frag-buf "")))
             (else                      ; text or binary start
              (when frag-opcode
                (send-close out 1002 "new message while fragmenting")
                (on-close out 1002 "new message while fragmenting"))
              (if fin
                  (if (= opcode 1)
                      (if (valid-utf8? payload)
                          (on-text out payload)
                          (begin (send-close out 1007 "invalid utf-8")
                                 (on-close out 1007 "invalid utf-8")))
                      (on-binary out payload))
                  (begin
                    (set! frag-opcode opcode)
                    (set! frag-buf payload)))))
           (loop))
          (else
           (send-close out 1002 "bad opcode")
           (on-close out 1002 "bad opcode")))))))

;; ---------- Server ----------

(define (start-server port)
  (let ((listener (tcp-listen port)))
    (print "WebSocket server on port " port)
    (let accept-loop ()
      (let-values (((in out) (tcp-accept listener)))
        (thread-start!
         (lambda ()
           (let ((headers (read-headers in)))
             (let ((key (find-header headers "Sec-WebSocket-Key:"))
                   (upgrade (find-header headers "Upgrade:")))
               (if (and key upgrade (string-contains upgrade "websocket"))
                   (begin
                     (display
                      (string-append
                       "HTTP/1.1 101 Switching Protocols\r\n"
                       "Upgrade: websocket\r\n"
                       "Connection: Upgrade\r\n"
                       "Sec-WebSocket-Accept: "
                       (websocket-accept key)
                       "\r\n\r\n")
                      out)
                     (flush-output out)
                     (*clients* (cons out (*clients*)))
                     (on-connect out)
                     (handle-exceptions exn
                       (on-error out exn)
                       (handle-websocket in out))
                     (*clients* (delete out (*clients*) eq?)))
                   (print "not websocket"))))
           (close-input-port in)
           (close-output-port out)))
        (accept-loop)))))
