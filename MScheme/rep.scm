; -*- scheme -*-
; $Id$

(let* ([user-env    (scheme-report-environment 5)]
       [user-eval   (lambda (expr)
                      (eval expr user-env))]
       [user-define (lambda (sym val)
                      (user-eval (list 'define sym val)))]
       [user-load   (user-eval 'load)])

  (define (cadr   x) (car (cdr           x  )))
  (define (caddr  x) (car (cdr (cdr      x ))))
  (define (cadddr x) (car (cdr (cdr (cdr x)))))

  (define error->cause        car)
  (define error->message      cadr)
  (define error->continuation caddr)
  (define error->retryable    cadddr)

  (define (display-nl . args)
    (for-each display args)
    (newline))

  (define (print-error error)
    (let ([cause   (error->cause        error)]
          [message (error->message      error)]
          [stack   (error->continuation error)])
      (display-nl "error     : " message)
      (display    "caused by : ")
      (write cause)
      (newline)
      (display-nl "--- begin of stack ---")
      (display-nl stack)
      (display-nl "--- end of stack ---")))

  (define (on-error f)
    (lambda ()
      (let ([error (last-error)])
        (if error
          (begin
            (print-error error)
            (f error))))))

  (define (try-with-error-handler try-thunk error-handler)
    (dynamic-wind
      (lambda () 'nop)
      try-thunk
      (on-error error-handler)))

  (define (come-back)
    (call-with-current-continuation
      (lambda (k)
        (define (retry) (k retry))
        retry)))

  (define (retry-on-error thunk)
    (define retry 'dummy)
    (try-with-error-handler
      (lambda ()
        (set! retry (come-back))
        (thunk))
      (lambda (error)
        (retry))))

  (define (REP-read depth prompt quit-thunk)
    (let* ([query
            (lambda ()
              (if prompt
                (begin
                  (if (> depth 0)
                    (begin
                      (display #\[)
                      (display depth)
                      (display #\])))
                  (prompt)))
              (read))]
           [input (retry-on-error query)])
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
    (let internal-rep ([level 0])
      (call-with-current-continuation
        (lambda (quit)
          (let loop ([result #f])
            (loop
              (REP-print
                (REP-eval
                  (lambda () (internal-rep (+ level 1)))
                  (REP-read
                    level
                    prompt
                    (lambda () (quit result)))))))))))

  (display-nl "Welcome to MScheme")
  (display-nl "press Ctrl-D to quit")
  (REP (lambda () (display "--> ")))
  (display-nl "bye ... "))
