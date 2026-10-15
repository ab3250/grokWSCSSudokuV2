(import (chicken base)
        (scheme)
        (srfi 1)
        (srfi 69)
        (srfi 196)
        (chicken process-context))


;; ---------- driver ----------

(define (print-grid grid)
  (do ((r 0 (+ r 1))) ((= r 9))
    (do ((c 0 (+ c 1))) ((= c 9))
      (display (string-ref grid (row-col->cell r c))))
    (newline)))

(define (main puzzle)
  (let ((grid (string-copy puzzle)))
    (if (solve grid)
        (print-grid grid)
        (display "no solution\n"))
    (exit)))

;; sample
(main "530070000600195000098000060800060003400803001700020006060000280000419005000080079")
