(import
  (chicken base)
  (scheme)
  (srfi 196)
  (srfi 130)
  (srfi 69))

(load "lib/cells.scm")
(load "lib/175.scm")


;;;TODO - make cells file independent

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define grid1 "530070000600195000098000060800060003400803001700020006060000280000419005000080079")
(define grid2 "032010000801000003000006400200005000060100078000200000500907060010000000008000930")
(define grid3 "103070002000000040090005001020100503007000200405002060200800030050000000800020709")
        
(define-syntax let/ec 
  (syntax-rules ()
    ((_ return body ...)
     (call-with-current-continuation
      (lambda (return)
        body ...)))))

(define (solve grid)
  (let/ec return    
    (range-map (lambda (row) (range-map (lambda(col)  
        (if (=(ascii-digit-value (string-ref grid (row-col->cell row col)) 10)  0)        
           (let num-loop ((num 1))             
             (if (not (= 10 num))
               (begin            
                 (if (possible? row col num grid)
                   (begin
                     (string-set! grid  (row-col->cell row col) (ascii-nth-digit num))
                     (solve grid)                    
                     (when (no-zeros-left? grid)(return 0))                     
                     (string-set! grid  (row-col->cell row col) (ascii-nth-digit 0))))
                 (num-loop (+ 1 num)))                                              
                 (return 0)))))
    (numeric-range 0  9)))(numeric-range 0  9))
  (print-grid grid)))

(define (print-find-all-possibles-table)
    (range-map (lambda (cell)     
      (display (list cell (hash-table-ref find-all-possibles-table cell)))(newline)

    )(numeric-range 0 81))

)

(define (main grid)
  (init-cells-affected-hash-table)
  (init-find-all-possibles-table grid)
  (solve (string-copy grid))
  ;(range->list (split grid))
  (print-grid grid)
  (print-find-all-possibles-table)
)


(main grid2)
