; -*- scheme -*-

; Scheme implementation of the read-eval-print loop.
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

; $Id: rep.scm,v 3.9 2002/12/20 12:33:25 sielenk Exp $

(let* ((user-env    (scheme-report-environment 5))
       (user-eval   (lambda (expr)
                      (eval expr user-env)))
       (user-define (lambda (sym val)
                      (user-eval (list 'define sym val))))
       (user-load   (user-eval 'load)))

  (define error->cause        car)
  (define error->message      cadr)
  (define error->continuation caddr)
  (define error->retryable    cadddr)

  (define (display-nl . args)
    (for-each display args)
    (newline))

  (define (print-error error)
    (let ((cause   (error->cause        error))
          (message (error->message      error))
          (stack   (error->continuation error)))
      (display-nl "error     : " message)
      (display    "caused by : ")
      (write cause)
      (newline)
      (display-nl "--- begin of stack ---")
      (display-nl stack)
      (display-nl "--- end of stack ---")))

  (define (try-with-error-handler try-thunk error-handler)
    (let* ((old-handler (reset-error-handler
                          (lambda (error)
                            (print-error   error)
                            (error-handler error))))
		   (result (try-thunk)))
      (reset-error-handler old-handler)
      result))

  (define (create-label)
    (call-with-current-continuation
      (lambda (k)
        (define (label) (k label))
        label)))

  (define (retry-on-error thunk)
    (define retry (create-label))
    (try-with-error-handler
      thunk
      (lambda (error) (retry))))

  (define (REP-read depth prompt quit-thunk)
    (let* ((query
            (lambda ()
              (if prompt
                (begin
                  (if (> depth 0)
                    (begin
                      (display #\[)
                      (display depth)
                      (display #\])))
                  (prompt)))
              (read)))
           (input (retry-on-error query)))
      (if (eof-object? input)
        (begin
          (display-nl "<Ctrl-D>")
          (quit-thunk))
        input)))

  (define (REP-eval error-REP expr)
    (call-with-current-continuation
      (lambda (return)
        (try-with-error-handler
          (lambda () (user-eval expr))
          (lambda (error)
            (if (error->retryable error)
              (begin
                (display-nl "press Ctrl-D to return to outer REP")
                ((error->continuation error) (error-REP)))
              (return '<error>)))))))

  (define (REP-print expr)
    (write expr)
    (newline)
    expr)

  (define (REP prompt)
    (let internal-REP ((level 0))
      (call-with-current-continuation
        (lambda (quit)
          (let loop ((result #f))
            (loop
              (REP-print
                (REP-eval
                  (lambda () (internal-REP (+ level 1)))
                  (REP-read
                    level
                    prompt
                    (lambda () (quit result)))))))))))

  (display-nl "Welcome to MScheme")
  (display-nl "press Ctrl-D to quit")
  (REP (lambda () (display "--> ")))
  (display-nl "bye ... "))
