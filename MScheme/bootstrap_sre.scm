(begin
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
