(begin
  (define call-with-input-file  'dummy)
  (define call-with-output-file 'dummy)

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

    (set! call-with-input-file
      (lambda (filename proc)
        (let* ((port   (open-input-file filename))
               (result (proc port)))
          (close-input-port port)
          result)))

    (set! call-with-output-file
      (lambda (filename proc)
        (let* ((port   (open-output-file filename))
               (result (proc port)))
          (close-output-port port)
          result)))

    (set! with-input-from-file
      (lambda (filename thunk)
        (call-with-input-file
          filename
          (lambda (new-cip)
            (let ((old-cip cip))
              (set! cip new-cip)
              (thunk)
              (set! cip old-cip))))))

    (set! with-output-to-file
      (lambda (filename thunk)
        (call-with-output-file
          filename
          (lambda (new-cop)
            (let ((old-cop cop))
              (set! cop new-cop)
              (thunk)
              (set! cop old-cop)))))))

  (define (newline . args) (apply display #\newline args)))
