; -*- scheme -*-

; Initialisation code for the syntax forms in the null environment.
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
  (define (quasiquote-func def-env use-env arg)
    (define (tag-uq? x)
      (if (pair? x)
          (eqv? (car x) 'unquote)
          #f))

    (define (tag-uqs? x)
      (if (pair? x)
          (eqv? (car x) 'unquote-splicing)
          #f))

    (define (tag-qq? x)
      (if (pair? x)
          (eqv? (car x) 'quasiquote)
          #f))

   (define (tag-data x)
     (car (cdr x)))

   (define (error x) '())

   (define qq-expand-list 'dummy)

   (define (qq-expand x)
     (if (tag-uq? x)
         (tag-data x)
         (if (tag-uqs? x)
             (error (list "unexpected unquote-splicing" x))
             (if (tag-qq? x)
                 (qq-expand (qq-expand (tag-data)))
                 (if (pair? x)
                     (list append
                           (qq-expand-list (car x))
                           (qq-expand      (cdr x)))
                     (list 'quote x))))))

   (define (qq-expand-list x)
     (if (tag-uq? x)
         (list list (tag-data x))
         (if (tag-uqs? x)
             (tag-data x)
             (if (tag-qq? x)
                 (qq-expand-list (qq-expand (tag-data x)))
                 (if (pair? x)
                     (list list
                           (list append
                                 (qq-expand-list (car x))
                                 (qq-expand      (cdr x))))
                     (list 'quote (list x)))))))

   (qq-expand arg))

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

  (list 'begin
    (list 'define-syntax 'quasiquote (wrapper quasiquote-func))
    (list 'define-syntax 'and        (wrapper and-func))
    (list 'define-syntax 'or         (wrapper or-func))
    (list 'define-syntax 'delay      (wrapper delay-func))))
