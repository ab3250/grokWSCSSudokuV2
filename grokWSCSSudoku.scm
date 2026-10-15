 (import scheme (chicken base) (chicken io) (chicken tcp)
   (chicken bitwise) (chicken blob) (chicken string)
   (srfi 1) (srfi 13) (srfi 18) base64 simple-sha1)

(load "lib/ws.scm")




(define grid1 "530070000600195000098000060800060003400803001700020006060000280000419005000080079")
(define grid2 "032010000801000003000006400200005000060100078000200000500907060010000000008000930")
(define grid3 "103070002000000040090005001020100503007000200405002060200800030050000000800020709")


;; ---------- sudoku -------------

;; ---------- grid helpers ----------

(define (row-col->cell r c) (+ (* r 9) c))
(define (cell->row cell) (quotient cell 9))
(define (cell->col cell) (remainder cell 9))

(define (box-of cell)
  (let ((r (cell->row cell))
        (c (cell->col cell)))
    (+ (* (quotient r 3) 3) (quotient c 3))))

;; peers[cell] = all other cells in the same row, column or box
(define peers
  (let ((tbl (make-vector 81)))
    (do ((cell 0 (+ cell 1)))
        ((= cell 81) tbl)
      (let* ((r (cell->row cell))
             (c (cell->col cell))
             (b (box-of cell))
             (acc '()))
        (do ((i 0 (+ i 1))) ((= i 9))
          (let ((rr (row-col->cell r i))
                (cc (row-col->cell i c)))
            (unless (= rr cell) (set! acc (cons rr acc)))
            (unless (= cc cell) (set! acc (cons cc acc)))))
        (let ((br (* (quotient r 3) 3))
              (bc (* (quotient c 3) 3)))
          (do ((i 0 (+ i 1))) ((= i 3))
            (do ((j 0 (+ j 1))) ((= j 3))
              (let ((bc* (row-col->cell (+ br i) (+ bc j))))
                (unless (= bc* cell)
                  (set! acc (cons bc* acc)))))))
        (vector-set! tbl cell (delete-duplicates acc))))))

(define (peers-of cell) (vector-ref peers cell))

;; ---------- core solver ----------

(define (digit->char n) (integer->char (+ (char->integer #\0) n)))
(define (char->digit ch) (- (char->integer ch) (char->integer #\0)))

(define (possible? grid cell num)
  (every (lambda (p)
           (not (= (char->digit (string-ref grid p)) num)))
         (peers-of cell)))

(define (solve grid)
  (let loop ((cell 0))
    (cond
      ((= cell 81)
       grid)                              ; solved — return the grid
      ((not (char=? (string-ref grid cell) #\0))
       (loop (+ cell 1)))                 ; skip filled cells
      (else
       (let try ((n 1))
         (cond
           ((> n 9) #f)                   ; no digit works here
           ((possible? grid cell n)
            (string-set! grid cell (digit->char n))
            (or (solve grid)               ; recurse; stop if it succeeded
                (begin
                  (string-set! grid cell #\0)
                  (try (+ n 1)))))         ; otherwise backtrack and try next
           (else
            (try (+ n 1)))))))))                              ; no solution

;; ---------- driver ----------

(define (print-grid grid)
  (do ((r 0 (+ r 1))) ((= r 9))
    (do ((c 0 (+ c 1))) ((= c 9))
      (display (string-ref grid (row-col->cell r c))))
    (newline)))

(define (dodis puzzle)
  (let ((grid (string-copy puzzle)))
    (if (solve grid)
        (print-grid grid)
        (display "no solution\n"))
    ;(exit)
    ))



;; ---------- Your Scheme code goes here ----------
;; Example: override the hooks
;;
;; ---------- User hooks (override these) ----------



(define (on-text client text)
    ;;
    (dodis  "530070000600195000098000060800060003400803001700020006060000280000419005000080079")
    ;;
   (print "got: " text)
   (broadcast (string-append "echo: " text)))

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
(define (broadcast text)
  (for-each (lambda (c)
              (handle-exceptions exn #f
                (send-text c text)))
            (*clients*)))

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



(start-server 8081)

