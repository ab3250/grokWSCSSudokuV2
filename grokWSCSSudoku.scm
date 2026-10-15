 (import scheme (chicken base) (chicken io) (chicken tcp)
   (chicken bitwise) (chicken blob) (chicken string)
   (srfi 1) (srfi 13) (srfi 18) base64 simple-sha1 (srfi 180)(vector-lib)(chicken port))

(load "lib/ws.scm")
(load "lib/sudoku.scm")



(define grid1 "530070000600195000098000060800060003400803001700020006060000280000419005000080079")
(define grid2 "032010000801000003000006400200005000060100078000200000500907060010000000008000930")
(define grid3 "103070002000000040090005001020100503007000200405002060200800030050000000800020709")

(define (grid-string grid)
  (with-output-to-string
    (lambda ()
      (json-write
       (vector
        '((type . "grid"))
        `((num . ,(identity grid))))))))

(display (grid-string (string-copy grid1)))
;(define (grid-string grid)    
; (json->string  (list->vector `((("type" . "grid")) (("num" . ,(return-grid grid)))))))




;; ---------- Your Scheme code goes here ----------
;; Example: override the hooks
;;
;; ---------- User hooks (override these) ----------



(define (on-text client text)

    ;;
    ;(send-text client (grid-string (string-copy grid1)))
;    (dodis  "530070000600195000098000060800060003400803001700020006060000280000419005000080079")
    ;;
   ;(print "got: " text)
   (send-text client text)
   ;(broadcast (string-append "echo: " text))
   )

(define (on-connect client)
  ;; client is a port (or a record if you prefer)
  (print "client connected")
  #t)

(define (on-binary client data)
  ;; data is a string of raw bytes
  (send-binary client data))

(define (on-close client code reason)
  (print "client closed " code " " reason))

(define (on-error client exn)
  (print "error: " exn))

;; ---------- Send helpers (for your Scheme code) ----------
; (define (broadcast text)
;   (for-each (lambda (c)
;               (handle-exceptions exn #f
;                 (send-text c text)))
;             (*clients*)))

; (define (send-text out text)
;   (display (make-frame 1 text) out)
;   (flush-output out))

; (define (send-binary out data)
;   (display (make-frame 2 data) out)
;   (flush-output out))

; (define (send-close out code reason)
;   (let ((payload (string-append
;                   (string (integer->char (arithmetic-shift code -8))
;                           (integer->char (bitwise-and code #xff)))
;                   reason)))
;     (display (make-frame 8 payload) out)
;     (flush-output out)))



(start-server 8081)

