; -*- scheme -*-
; $Id$

(let* ((user-env    (scheme-report-environment 5))
       (user-eval   (lambda (expr)
                      (eval expr user-env)))
       (user-define (lambda (sym val)
                      (user-eval (list 'define sym val))))
       (user-load   (user-eval 'load)))

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

  (dynamic-wind
    (lambda ()
      (display "Welcome to MScheme")
      (newline)
      (display "press Ctrl-D to quit")
      (newline))
    (lambda ()
      (REP (lambda () (display "--> "))))
    (lambda ()
      (newline)
      (display "bye ...")
      (newline))))
