(let ((basic-sre scheme-report-environment))
  (set! scheme-report-environment
    (eval 'scheme-report-environment
      (let fixed-sre ((n 5))
        (define env (basic-sre n))
        (eval (read (open-input-file "fix-io.scm")) env)
        (eval (read (open-input-file "fix-syntax.scm")) env)
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
