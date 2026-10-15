#lang r7rs

(import
    (scheme base)
    (scheme write)
    (srfi 175)
    (srfi 196)
    (srfi 158)
)
 ;(srfi 175)
 ;   (scheme red)
 ;   (chibi time)
 ;   (sudokuWs)
 ;   (chibi io) 
 ;   (delay)
 ;   (scheme vector)    
 ;   (ab websocket websocket)
   
;(include "175.sld")
;(include "196.sld")
;(include "srfi-158.sld")
; (include "srfi-180-add.scm")
(define (get-box-cells row col)
  (let ((box (get-box-number row col))(acc (list-accumulator)))
  (range-map (lambda(x)
    (range-map (lambda(y)
      (acc
       (+ x y  (* box 3) (if (or (= box 0)(= box 1)(= box 2)) 0  (if (or (= box 3)(= box 4)(= box 5)) 18  (if (or (= box 6)(= box 7)(= box 8)) 36))))

       )
      )
    (numeric-range 0 3)))
  (numeric-range 0 26 9))
(acc (eof-object))))

(define (test box)
  (let ((acc (list-accumulator)))
  (range-map (lambda(x)
    (range-map (lambda(y)
      (acc (+ x y  (* box 3) (if (or (= box 0)(= box 1)(= box 2)) 0  (if (or (= box 3)(= box 4)(= box 5)) 18  (if (or (= box 6)(= box 7)(= box 8)) 36)))))
       )        
    (numeric-range 0 3)))
  (numeric-range 0 26 9))
 (acc (eof-object))))

;(display (test 0))(newline)
;(display (test 1))(newline)
;(display (test 2))(newline)
;(display (test 3))(newline)
;(display (test 4))(newline)
;(display (test 5))(newline)
;(display (test 6))(newline)
;(display (test 7))(newline)
;(display (test 8))(newline)



(define grid1 "530070000600195000098000060800060003400803001700020006060000280000419005000080079")
(define grid2 "032010000801000003000006400200005000060100078000200000500907060010000000008000930")
(define grid3 "103070002000000040090005001020100503007000200405002060200800030050000000800020709")

  (define for (lambda (row end func)
      (let loop ((index row))
        (if (> index end) #t
          (begin
            (func index)
            (loop (+ index 1)))))))

(define-syntax nested-loop
  (syntax-rules ()
    ((_ l1 l1-row l1-end l2 l2-row l2-end body ...)
         (for l1-row l1-end (lambda(l1)
		    (for l2-row l2-end (lambda(l2)
			       (begin
				body ... ))))))))

(define (check cell-content-list num grid) ;free to place number t/f  
  (let loop ((cell 0))  
    (if (= (ascii-digit-value (list-ref cell-content-list cell) 10) num)
      #f
      (if (not (= cell 8)) (loop (+ cell 1)) #t))))     

(define (no-zeros-left? grid) 
    (let ((length (string-length grid)))
      (let loop ((count 0)) 
        (if (= (ascii-digit-value (string-ref grid count) 10) 0)
          #f
        (if (not (= count (- length 1)))
          (loop (+ count 1))
          #t)))))

(define (get-row-cell-content row grid)
  (map (lambda(x)(string-ref grid x))(get-row-cells row)))

(define (get-col-cell-content col grid)
  (map (lambda(x)(string-ref grid x))(get-col-cells col)))

(define (get-box-cell-content row col grid)
  (display (list?(get-box-cells row col)))
  (map (lambda(x)(string-ref grid x))(get-box-cells row col)))

; (define (get-cell-content cell)
;   (string-ref grid cell))


(define (get-row-cells row )      
   (map (lambda (x) (+ (* row 9) x)) '(0 1 2 3 4 5 6 7 8)))

(define (get-col-cells col)
  (map (lambda (x) (+ x col)) '(0 9 18 27 36 45 54 63 72)))

;(define (get-box-cells row col)
;        (let ((box (get-box-number row col)))
;          (cond
;            ((= box 0) '(0 1 2 9 10 11 18 19 20))
;            ((= box 1) '(3 4 5 12 13 14 21 22 23))
;            ((= box 2) '(6 7 8 15 16 17 24 25 26))
;            ((= box 3) '(27 28 29 36 37 38 45 46 47))
;            ((= box 4) '(30 31 32 39 40 41 48 49 50))
;            ((= box 5) '(33 34 35 42 43 44 51 52 53))
;           ((= box 6) '(54 55 56 63 64 65 72 73 74))
;            ((= box 7) '(57 58 59 66 67 68 75 76 77))
;            ((= box 8) '(60 61 62 69 70 71 78 79 80)))))

(define (get-box-number row col) 
      (cond 
        ((or (= row 0) (= row 1) (= row 2))
          (cond 
            ((or (= col 0) (= col 1) (= col 2)) 0)
            ((or (= col 3) (= col 4) (= col 5)) 1)
            ((or (= col 6) (= col 7) (= col 8)) 2)))
        ((or(= row 3) (= row 4) (= row 5))
          (cond
            ((or (= col 0) (= col 1) (= col 2)) 3)
            ((or (= col 3) (= col 4) (= col 5)) 4)
            ((or (= col 6) (= col 7) (= col 8)) 5)))      
        ((or (= row 6) (= row 7) (= row 8))
          (cond 
            ((or (= col 0) (= col 1) (= col 2)) 6)
            ((or (= col 3) (= col 4) (= col 5)) 7)
            ((or (= col 6) (= col 7) (= col 8)) 8)))))

(define (print-grid grid)
    (newline)
    (for 0 8 (lambda(row)
      (for 0 8 (lambda(col)
        (display (string-ref grid (row-col->cell row col)))))
        (newline))))

(define (row-col->cell row col)
    (+ (* row 9) col))

(define (cell->row-col cell)
    (cons 'a 'b))

(define (possible? row col num grid)
  (and 
      (check (get-row-cell-content row grid) num grid)
      (check (get-col-cell-content col grid) num grid)
      (check (get-box-cell-content row col grid) num grid)))

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
                      ;(print-grid grid)                     
                     (solve grid)                    
                     (when (no-zeros-left? grid)(return))                     
                     (string-set! grid  (row-col->cell row col) (ascii-nth-digit 0))))
                 (num-loop (+ 1 num)))                              
                 (return)))))
    (numeric-range 0 9)))(numeric-range 0 9))   
  (print-grid grid)))
              




(solve (string-copy grid2))
