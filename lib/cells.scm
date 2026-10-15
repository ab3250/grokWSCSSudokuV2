(import
  (chicken base)
  (scheme)
  (srfi 196)
  (srfi 158)
  (srfi 130)
  (srfi 69))

(load "lib/175.scm")

(define for (lambda (row end func)
  (let loop ((index row))
    (if (> index end) #t
      (begin
        (func index)
        (loop (+ index 1)))))))

(define (eof-object) #!eof)

(define (between-inclusive x min max)
    (and (>= x min)(<= x max)))

(define (print-grid grid)
    (newline)
    (for 0 8 (lambda(row)
      (for 0 8 (lambda(col)
        (display (string-ref grid (row-col->cell row col)))))
        (newline))))

(define (no-zeros-left? grid)
    (= 0 (string-count grid (lambda (x) (char=? #\0 x)))))

(define (row-col->cell row col)
    (+ (* row 9) col))

(define (cell->row-col cell)
     (cons (quotient cell 9)  (remainder cell 9)))

(define (get-box-offset box)
    (cond
        ((between-inclusive box 0 2) 0)
        ((between-inclusive box 3 5) 18)
        ((between-inclusive box 6 8) 36)))

(define (get-box-cells row col)
    (let ((box (get-box-number row col))(acc (list-accumulator)))
        (range-map (lambda(x)
            (range-map (lambda(y)
                (acc (+ x y  (* box 3) (get-box-offset box))))
            (numeric-range 0 3)))
        (numeric-range 0 26 9))
    (acc (eof-object))))

(define (get-cell-list-content list grid)
  (map (lambda(x)(string-ref grid x))list))

(define (get-row-cells row )      
   (map (lambda (x) (+ (* row 9) x)) (range->list(numeric-range 0  9))))

(define (get-col-cells col)
  (map (lambda (x) (+ x col)) '(0 9 18 27 36 45 54 63 72)))

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

(define (check cell-content-list num grid) ;free to place number t/f  
  (let loop ((cell# 0))  
    (if (= (ascii-digit-value (list-ref cell-content-list cell#) 10) num)
      #f
      (if (= cell# 26) #t (loop (+ cell# 1))))))     


(define cells-affected-hash-table (make-hash-table equal?)) ; Create a hash table using equal? for key comparison

(define find-all-possibles-table (make-hash-table equal?))

(define (init-cells-affected-hash-table)
  (range-map (lambda (cell) 
    (let* ((row-col-pair (cell->row-col cell)) (row (car row-col-pair)) (col (cdr row-col-pair) ))
      (hash-table-set! cells-affected-hash-table cell (append (get-row-cells row) (get-col-cells col) (get-box-cells row col))))
  )(numeric-range 0 81)))

(define (possible? row col num grid)   
        (check (get-cell-list-content  (hash-table-ref cells-affected-hash-table (row-col->cell row col)) grid) num grid))

(define (init-find-all-possibles-table grid)  
    (range-map (lambda (cells)
         (hash-table-set! find-all-possibles-table cells (range->list (range-filter  (lambda (nums);;;;;     
          (check (get-cell-list-content  (hash-table-ref cells-affected-hash-table cells) grid) nums grid)
;;;;;
        )(numeric-range 1 10))) ))         
        
         (numeric-range 0 81)))


