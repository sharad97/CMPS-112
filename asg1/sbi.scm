#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;
;; CREATED BY: SHARAD SHRESTHA
;;
;; NAME
;;    sbi.scm - a silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

;; Define *stderr*
;; standard error
(define *stderr* (current-error-port))

;; Define *stdin*
;; Standard input
(define *stdin* (current-input-port))

;; Run-file
(define *run-file*
    (let-values
        (((dirpath basepath root?)
          (split-path (find-system-path 'run-file))))
        (path->string basepath))) 
        
;; Die        
;; Displays error then exits 
 (define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1))  

;; Usage-exit    
;; Prints then die   
(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename")))

;; Readlist-from-inputfile
;; Reads input-file line by line into a list (program)   
(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)program))))

;; Writes program line by line                  
(define (write-program-by-line filename program)
;   (printf "==================================================~n")
;   (printf "~a: ~s~n" *run-file* filename)
;   (printf "==================================================~n")
;   (printf "(~n")
    (map (lambda (row)(hash-labels row)) program)
    (executes program))

;; program line counter    
(define linum 0)

;; input counter
(define num 0)

;; Symbol table is global hash table
;; Symbol table stores variable as key and its value
(define *sym-table* (make-hash))


;; Function table is global hash table
;; Function table stores function as key and its value
(define *fun-table* (make-hash))


;; Label table is global hash table
;; Label table stores label as key and its value
(define *lab-table* (make-hash))


;; Inserts pair (key and value) into symbol table
(define (symbol-put! key value)
        (hash-set! *sym-table* key value))

;; gets symbol key         
(define (symbol-get key)
        (hash-ref *sym-table* key))
        
;; Inserts pair (key and value) into function table
(define (function-put! key value)
        (hash-set! *fun-table* key value))
        
;; gets function key         
(define (function-get key)
        (hash-ref *fun-table* key))

;; Inserts pair (key and value) into label table        
(define (label-put! key value)
        (hash-set! *lab-table* key value))

;; gets label key        
(define (label-get key)
        (hash-ref *lab-table* key))
        
;; checks if symbol has key    
(define (symbol-has-key key)    
        (hash-has-key? *sym-table* key))              
        
;; checks if symbol table has key or not
(define (exes arg)
    (if (symbol-has-key (car arg))
        (exes_cond arg)
        (die (list "symbol table error!\n"))))        
 
;; if symbol table has key then executes following command 
(define (exes_cond arg)
    (cond 
        ((procedure? (gets arg))
         (apply (gets arg) 
         (map (lambda(value1)(evaluate value1))(cdr arg))))
        ((vector? (gets arg))
         (vector-ref (gets arg)(car(cdr arg))))
        ((number? (gets arg))
         (+ 0.0 (gets arg)))))

;; checks whether cdr of line is null or not null (recursive function)
(define (check x)
    (if (not (null?(cdr x)))
        (check (cdr x))
        (when (pair? (car x))
            ((function-get (car (car x)))(cdr (car x))))))           
        
;; inserts symbol pair (car of key and value)   
(define (puts arg x)
        (symbol-put! (car arg) x))

;; gets symbol car of key       
(define (gets arg)
        (symbol-get (car arg))) 
        
 ;; gets label car of key       
(define (goto2 arg)
    (hash-ref *lab-table* (car arg)))    

;; prints       
(define (print_it arg)
    (when (not(null? arg))
          (map (lambda (value1) (display (evaluate  value1))) arg)
          (newline)))    

;; creates an array
(define (dim arg)
    (when (not(null? arg))
          (set! arg (car arg))
          (define vals (evaluate (car(cdr arg))))
          (define vect (make-vector vals (car arg)))
          (let((lists vect))
          (puts arg (+ vals 1)))))

;; creates varaibles
(define (let_it arg)
    (when (not(null? arg))
          (define vals (car(cdr arg)))
          (puts arg (evaluate  vals))))
;; inputs         
(define (inputs arg)
    (let ((input (read *stdin*)))
         (cond
            ((number? input)
             (map (lambda (value1)(symbol-put! value1 input)) arg)
             (set! num (+ 1 num)))
            ((eof-object? input) (set! num (- 1)))
            (else (die))))
    (symbol-put! 'inputcount num))
              
;; if condition      
(define (ifs arg)
    (when (not(null? arg))
          (when (evaluate (car arg))
                (gotos (cdr arg)))))

;; jumps
(define (gotos arg )
    (when (not(null? arg))
          (set! linum (goto2 arg))))

;; initialize and insert into symbol table
(for-each
    (lambda (pair)
            (hash-set! *sym-table* (car pair) (car(cdr pair))))
    `(
        (+       ,+)
        (-       ,-)
        (*       ,*)
        (/       ,/)
        (^       ,expt)
        (sqrt    ,sqrt)
        (exp     ,exp)
        (log     ,log)
        (sin     ,sin) 
        (cos     ,cos)
        (tan     ,tan)
        (acos    ,acos)
        (asin    ,asin)
        (atan    ,atan)
        (abs     ,abs)
        (ceil    ,ceiling)
        (floor   ,floor)
        (round   ,round)
        (=       ,=)
        (<       ,<)
        (>       ,>)
        (>=      ,>=)
        (<=      ,<=) 
        (pi      3.141592653589793238462643383279502884197169399)
        (e       2.718281828459045235360287471352662497757247093)
        (sqrt_2  1.414213562373095048801688724209698078569671875)
        (log10_2 0.301029995663981195213738894724493026768189881)
        (div     ,(lambda (x y) (floor (/ x y))))
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (mod     ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (<>      ,(lambda (x y) (not (= x y))))
        (log2    ,(lambda (x) (/ (log x) (log 2.0))))
        (trunc   ,truncate)))

(for-each
  (lambda (pair)
          (hash-set! *fun-table*(car pair) (car(cdr pair))))
    `(      
        (print ,print_it)
        (dim   ,dim)
        (let   ,let_it)
        (input ,inputs)
        (if    ,ifs)
        (goto  ,gotos)))     

;; scans label 
;; stores key and value in hash table
(define (hash-labels row)
    (when (not (null? (cdr row)))
          (when (and (not (pair? (car(cdr row)))) 
                     (not(null? (car(cdr row)))))
                (label-put!(car(cdr row))(- (car row) 1 )))))

;; executes program (recursive function)
(define (executes program)
    (when (< linum (length program))
        (let((row (list-ref program linum)))
             (set! linum (+ linum 1))
             (check row)
             (executes program ))))
             
;; evaluates arguments
(define (evaluate arg)
    (cond
        ((string? arg)arg)
        ((number? arg)(if (= 0 arg) '0.0 arg))
        ((and(symbol? arg) (hash-ref *sym-table* arg #f)))
        ((list? arg)(exes arg))))

;; main
(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (write-program-by-line sbprogfile program))))

(main (vector->list (current-command-line-arguments)))
