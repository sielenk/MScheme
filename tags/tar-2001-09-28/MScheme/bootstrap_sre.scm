; -*- scheme -*-
; $Id$

(begin
  ;procedure+: reduce <procedure> <initial> <list> 
  ;    Combines all the elements of list using the binary
  ;    operation procedure. For example, using + one can 
  ;    add up all the elements: 
  ;
  ;    (reduce + 0 list-of-numbers)
  ;
  ;    The argument initial is used only if list is empty;
  ;    in this case initial is the result of the call to reduce.
  ;    If list has a single argument, it is returned. Otherwise,
  ;    the arguments are reduced in a left-associative fashion.
  ;    For example: 
  ;
  ;    (reduce + 0 '(1 2 3 4))                 =>  10
  ;    (reduce + 0 '(1 2))                     =>  3
  ;    (reduce + 0 '(1))                       =>  1
  ;    (reduce + 0 '())                        =>  0
  ;    (reduce + 0 '(foo))                     =>  foo
  ;    (reduce list '() '(1 2 3 4))            =>  (((1 2) 3) 4)

  (define (reduce func initial args)
    (define (helper head tail)
      (if (null? tail)
        head
        (helper (func head (car tail)) (cdr tail))))

    (if (null? args)
      initial
      (helper (car args) (cdr args))))


  ;procedure+: reduce-right procedure initial list 
  ;    Like reduce except that it is right-associative. 
  ;
  ;    (reduce-right list '() '(1 2 3 4))      =>  (1 (2 (3 4)))

  (define (reduce-right func initial args)
    (define (helper head tail)
      (if (null? tail)
        head
        (func head (helper (car tail) (cdr tail)))))

    (if (null? args)
      initial
      (helper (car args) (cdr args))))


  ;procedure+: fold-right procedure initial list 
  ;    Combines all of the elements of list using the binary
  ;    operation procedure. Unlike reduce and reduce-right,
  ;    initial is always used: 
  ;
  ;    (fold-right + 0 '(1 2 3 4))             =>  10
  ;    (fold-right + 0 '(foo))                 error--> Illegal datum
  ;    (fold-right list '() '(1 2 3 4))        =>  (1 (2 (3 (4 ()))))
  ;
  ;    Fold-right has interesting properties because it
  ;    establishes a homomorphism between (cons, ()) and
  ;    (procedure, initial). It can be thought of as replacing
  ;    the pairs in the spine of the list with procedure and replacing
  ;    the () at the end with initial. Many of the classical
  ;    list-processing procedures can be expressed in terms of
  ;    fold-right, at least for the simple versions that take 
  ;    a fixed number of arguments: 
  ;
  ;    (define (copy-list list)
  ;      (fold-right cons '() list))
  ;
  ;    (define (append list1 list2)
  ;      (fold-right cons list2 list1))
  ;
  ;    (define (map p list) 
  ;      (fold-right (lambda (x r) (cons (p x) r)) '() list))
  ;
  ;    (define (reverse items)
  ;      (fold-right (lambda (x r) (append r (list x))) '() items))

  (define (fold-right func initial args)
    (define (helper head tail)
      (func
        head
        (if (null? tail)
          initial
          (helper
            (car tail)
            (cdr tail)))))

    (if (null? args)
      initial
      (helper
        (car args)
        (cdr args))))


  ;procedure+: fold-left procedure initial list 
  ;    Combines all the elements of list using the binary operation
  ;    procedure. Elements are combined starting with initial
  ;    and then the elements of list from left to right. Whereas
  ;    fold-right is recursive in nature, capturing the essence
  ;    of cdr-ing down a list and then computing a result, fold-left
  ;    is iterative in nature, combining the elements as the list
  ;    is traversed.
  ;
  ;    (fold-left list '() '(1 2 3 4))         =>  ((((() 1) 2) 3) 4)
  ;
  ;    (define (length list)
  ;      (fold-left (lambda (sum element) (+ sum 1)) 0 list))
  ;
  ;    (define (reverse items)
  ;      (fold-left (lambda (x y) (cons y x)) () items))

  (define (fold-left func initial args)
    (define (helper head tail)
      (if (null? tail)
        head
        (helper (func head (car tail)) (cdr tail)))
    (helper initial args)))



  (define (primitve-map f l)
    (fold-right
      (lambda (x r)
        (cons
          (f x)
          r))
      '()
      l))

  (define (transpose lists)
    (let loop ((rest lists))
      (if (null? (car rest))
        '()
        (cons
          (primitve-map car rest)
          (loop (primitve-map cdr rest))))))

  (define (map func . lists)
    (if (null? (cdr lists))
      (primitve-map
        func
        (car lists))
      (primitve-map
        (lambda (list) (apply func list))
        (transpose lists))))

  (define (for-each func . lists)
    (reverse (apply map func (map reverse lists))))


  (define (force object)
    (object))


  (define (call-with-input-file filename proc)
    (let* ((port   (open-input-file filename))
           (result (proc port)))
      (close-input-port port)
      result))
                                       
  (define (call-with-output-file filename proc)
    (let* ((port   (open-output-file filename))
           (result (proc port)))
      (close-output-port port)
      result))

  (define current-input-port  'dummy)
  (define current-output-port 'dummy)

  (define with-input-from-file 'dummy)
  (define with-output-to-file  'dummy)

  (let ((basic-read        read)
        (basic-read-char   read-char)
        (basic-peek-char   peek-char)
        (basic-char-ready? char-ready?)
        (basic-write       write)
        (basic-display     display)
        (basic-write-char  write-char)
        (cip (open-input-file  #f))
        (cop (open-output-file #f)))

    (set! current-input-port  (lambda () cip))
    (set! current-output-port (lambda () cop))

    (define (wrap-cip func)
      (lambda args
        (if (null? args)
          (func cip)
          (apply func args))))

    (define (wrap-cop func)
      (lambda (obj . args)
        (if (null? args)
          (func obj cop)
          (apply func obj args))))

    (set! read        (wrap-cip basic-read       ))
    (set! read-char   (wrap-cip basic-read-char  ))
    (set! peek-char   (wrap-cip basic-peek-char  ))
    (set! char-ready? (wrap-cip basic-char-ready?))
    (set! write       (wrap-cop basic-write      ))
    (set! display     (wrap-cop basic-display    ))
    (set! write-char  (wrap-cop basic-write-char ))

    (set! with-input-from-file
      (lambda (filename thunk)
        (call-with-input-file
          filename
          (lambda (new-cip)
            (let ((old-cip cip))
              (dynamic-wind
                (lambda () (set! cip new-cip))
                thunk
                (lambda () (set! cip old-cip))))))))

    (set! with-output-to-file
      (lambda (filename thunk)
        (call-with-output-file
          filename
          (lambda (new-cop)
            (let ((old-cop cop))
              (dynamic-wind
                (lambda () (set! cop new-cop))
                thunk
                (lambda () (set! cop old-cop)))))))))

  (define (newline . args) (apply display #\newline args))

  (define (make-load env)
    (lambda (filename)
      (let ((port (open-input-file filename)))
        (let eval-expr ((expr (read port)) (result '()))
          (if (eof-object? expr)
            result
            (eval-expr (read port) (eval expr env)))))))

  (define load (make-load (current-environment)))

  (list 'begin
    (list 'define 'map                   map)
    (list 'define 'for-each              for-each)
    (list 'define 'force                 force)
    (list 'define 'call-with-input-file  call-with-input-file)
    (list 'define 'call-with-output-file call-with-output-file)
    (list 'define 'current-input-port    current-input-port)
    (list 'define 'current-output-port   current-output-port)
    (list 'define 'with-input-from-file  with-input-from-file)
    (list 'define 'with-output-to-file   with-output-to-file)
    (list 'define 'read                  read)
    (list 'define 'read-char             read-char)
    (list 'define 'peek-char             peek-char)
    (list 'define 'char-ready?           char-ready?)
    (list 'define 'write                 write)
    (list 'define 'display               display)
    (list 'define 'write-char            write-char)
    (list 'define 'newline               newline)
    (list 'define 'load
      (list make-load
        (list current-environment)))))
