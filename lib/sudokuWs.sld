(define-library (sudokuWs)
  (export       
    grid1
    grid2
    grid3
    possible?
    row-col->cell
    no-zeros-left?
    print-grid
    for
    let/ec 
  ;  lock-buttons
  ;  unlock-buttons
    lock
    grid-string
    nested-loop
    ;;;;
    get-row-cells
    get-col-cells
    get-row-cell-content
    get-col-cell-content
    get-box-cells
    identity
    cell->row-col 
    get-box-cells
    unless
    displayln    
    lock
    for
    nested-loop
    let/ec
    gblFd 
  
  
  )
    
 (import
  (chibi time)
  (scheme base)
  (scheme red)  
  (chibi string)  
  (schemepunk json)
  ;(chibi json)
  (chibi string)
  (srfi 175))

  ;library for sudokuWs

(begin  
   (define grid1 "530070000600195000098000060800060003400803001700020006060000280000419005000080079")
   (define grid2 "032010000801000003000006400200005000060100078000200000500907060010000000008000930")
   
   (define grid3 "103070002000000040090005001020100503007000200405002060200800030050000000800020709")
;(define grid3 "000000000000000000000000000000000000000000000000000000000000000000000000000000000")
 ;(define grid3 "111111111111111111111111111111111111111111111111111111111111111111111111111111111")

 ;global variables
(define gblFd -1)
;;;;;
;functions


(define (delay sec)
    (define start (current-seconds))
    (let timeloop ()    
        (if ( < (- (current-seconds) start) sec) (timeloop))))

; (define-syntax unless
;   (syntax-rules ()
;     ((unless test . body)
;      (when (not test) . body))))

(define (displayln x)
  (display x)
  (newline))

(define (identity x) x)

(define (grid-string grid)    
  (json->string  (list->vector `((("type" . "grid")) (("num" . ,(identity grid)))))))

(define-syntax lock
  (syntax-rules ()
    ((_ body ...)
      (begin (lock-buttons)
        body ...
        (unlock-buttons)))))

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

(define-syntax let/ec 
  (syntax-rules ()
    ((_ return body ...)
     (call-with-current-continuation
      (lambda (return)
        body ...)))))

; (define call-with-input-file2 
;     (lambda (filename proc)
;   	  (let ((p (open-input-file filename)))
;        (let ((str (proc p)))  
;         (close-input-port p)
;         str))))

; (define call-with-output-file2
;     (lambda(filename proc str)
;       (let ((p (open-output-file filename)))
;         (proc str p)    
;         (close-output-port p))))





;;;;;;;;;;;;;
    
));eolib

  




