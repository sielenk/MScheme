; -*- scheme -*-

; Initialisation code for the scheme machine.
; Copyright (C) 2001  Marvin H. Sielenkemper
;
; This file is part of MScheme.
;
; MScheme is free software; you can redistribute it and/or modify 
; it under the terms of the GNU General Public License as published by 
; the Free Software Foundation; either version 2 of the License, 
; or (at your option) any later version. 
;
; MScheme is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details. 
;
; You should have received a copy of the GNU General Public License
; along with MScheme; see the file COPYING. If not, write to 
; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
; Boston, MA  02111-1307, USA. */

; $Id$

(begin
  (define update-null-environment
    (let ()
      (define (and-func def-env use-env . args)
        (if (null? args)
          #t
          (let ((head (car args))
                (tail (cdr args)))
            (if (null? tail)
              head
              (list 'if head (cons 'and tail) #f)))))

      (define (or-func def-env use-env . args)
        (if (null? args)
          #f
          (let ((head (car args))
                (tail (cdr args)))
            (if (null? tail)
              head
              (let ((id (unique-id)))
                (list 'let (list (list id head))
                      (list 'if id id (cons 'or tail))))))))

      (define (make-promise proc)
        (let ((result-ready? #f)
              (result #f))
          (lambda ()
            (if result-ready?
              result
              (let ((x (proc)))
                (if result-ready?
                  result
                  (begin
                    (set! result-ready? #t)
                    (set! result x)
                    result)))))))

      (define (delay-func def-env use-env expression)
        (list make-promise (list 'lambda '() expression)))

      (define (wrapper func)
        (lambda (def-env use-env . args)
          (cons
           use-env
           (apply func def-env use-env args))))

      (lambda (env)
        (eval
         (list 'begin
           (list 'define-syntax 'and        (wrapper and-func))
           (list 'define-syntax 'or         (wrapper or-func))
           (list 'define-syntax 'delay      (wrapper delay-func)))
         env)
        env)))

  (update-null-environment (current-environment))


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
    (if (null? args)
        initial
        (let helper ([head (car args)] 
                     [tail (cdr args)])
          (if (null? tail)
              head
              (helper 
                (func head
                      (car tail))
                (cdr tail))))))


  ;procedure+: reduce-right procedure initial list 
  ;    Like reduce except that it is right-associative. 
  ;
  ;    (reduce-right list '() '(1 2 3 4))      =>  (1 (2 (3 4)))

  (define (reduce-right func initial args)
    (if (null? args)
        initial
        (let helper ([head (car args)]
                     [tail (cdr args)])
          (if (null? tail)
              head
              (func head 
                    (helper (car tail)
                            (cdr tail)))))))



  ;procedure+: fold-right procedure initial list 
  ;    Combines all of the elements of list using the binary
  ;    operation procedure. Unlike reduce and reduce-right,
  ;    initial is always used: 
  ;
  ;    (fold-right + 0 '(1 2 3 4))             =>  10
  ;    (fold-right + 0 '(foo))                 error --> Illegal datum
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
    (if (null? args)
      initial
      (let helper ([head (car args)]
                   [tail (cdr args)])
        (func head
              (if (null? tail)
                  initial
                  (helper (car tail)
                          (cdr tail)))))))



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
    (let helper ([head initial]
                 [tail args   ])
      (if (null? tail)
          head
          (helper
            (func head
                  (car tail))
            (cdr tail)))))


  (define update-scheme-report-environment
    (let ()
      (define (primitve-map f l)
        (fold-right
          (lambda (x r)
            (cons (f x) r))
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

      (let ([basic-read        read]
            [basic-read-char   read-char]
            [basic-peek-char   peek-char]
            [basic-char-ready? char-ready?]
            [basic-write       write]
            [basic-display     display]
            [basic-write-char  write-char]

            [basic-null-environment          null-environment]
            [basic-scheme-report-environment scheme-report-environment]

            [cip initial-input-port]
            [cop initial-output-port])

        (define current-input-port  (lambda () cip))
        (define current-output-port (lambda () cop))

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

        (define read        (wrap-cip basic-read       ))
        (define read-char   (wrap-cip basic-read-char  ))
        (define peek-char   (wrap-cip basic-peek-char  ))
        (define char-ready? (wrap-cip basic-char-ready?))
        (define write       (wrap-cop basic-write      ))
        (define display     (wrap-cop basic-display    ))
        (define write-char  (wrap-cop basic-write-char ))

        (define with-input-from-file
          (lambda (filename thunk)
            (call-with-input-file
              filename
              (lambda (new-cip)
                (let ([old-cip cip])
                  (dynamic-wind
                    (lambda () (set! cip new-cip))
                    thunk
                    (lambda () (set! cip old-cip))))))))

        (define with-output-to-file
          (lambda (filename thunk)
            (call-with-output-file
              filename
              (lambda (new-cop)
                (let ([old-cop cop])
                  (dynamic-wind
                    (lambda () (set! cop new-cop))
                    thunk
                    (lambda () (set! cop old-cop))))))))

        (define (newline . args) (apply display #\newline args))

        (define (make-load env)
          (lambda (filename)
            (let ([port (open-input-file filename)])
              (let eval-expr ([expr (read port)] [result '()])
                (if (eof-object? expr)
                  result
                  (eval-expr (read port) (eval expr env)))))))

        (define (null-environment version)
          (update-null-environment
            (basic-null-environment version)))

        (define (scheme-report-environment version)
          (update-scheme-report-environment
            (update-null-environment
              (basic-scheme-report-environment version))))

        (define (make-definition sym)
          (list 'define sym (eval sym (current-environment))))

        (define definition-list
          (cons 'begin
            (map make-definition 
              '(map for-each force 
                call-with-input-file call-with-output-file 
                current-input-port current-output-port 
                with-input-from-file with-output-to-file 
                read read-char peek-char char-ready? 
                write display write-char newline
                null-environment
                scheme-report-environment))))

        (lambda (env)
          (eval definition-list env)
          (eval (list 'define 'load (make-load env)) env)
          env))))

  (update-scheme-report-environment (current-environment)))
