(let ((basic-sre  scheme-report-environment)
      (fix-io     (read (open-input-file "fix-io.scm")))
      (fix-syntax (read (open-input-file "fix-syntax.scm"))))
  (set! scheme-report-environment
    (eval 'scheme-report-environment
      (let fixed-sre ((n 5))
        (define env (basic-sre n))
        (eval fix-io     env)
        (eval fix-syntax env)
        (eval
          (list 'define 'load
            (lambda (filename)
              (let ((port (open-input-file filename)))
                (let eval-expr ((expr (read port)) (result '()))
                  (if (eof-object? expr)
                    result
                    (eval-expr (read port) (eval expr env)))))))
          env)
        (eval (list 'set! 'scheme-report-environment fixed-sre) env)
        env))))
