; -*- scheme -*-
; $Id$

(define user-env
  (scheme-report-environment 5))
        
(define (user-eval expr)
  (eval expr user-env))

(define (user-define sym val)
  (user-eval (list 'define sym val)))

(define user-load (user-eval 'load))

(define (REP-read depth prompt quit-thunk)
  (if prompt
    (begin
      (if (> depth 0)
        (begin
          (display #\[)
          (display depth)
          (display #\])))
      (prompt)))
  (let ((input (read)))
    (if (eof-object? input)
      (quit-thunk)
      input)))

(define REP-eval user-eval)

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
                (REP-read
                  level
                  prompt
                  (lambda ()
                    (quit result)))))))))))

(display "Welcome to MScheme")
(newline)
(display "press Ctrl-D to quit")
(newline)
(define result (REP (lambda () (display "--> "))))
(newline)
(display "bye ...")
(newline)
result
