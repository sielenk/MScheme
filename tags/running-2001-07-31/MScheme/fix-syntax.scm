(begin
  (define-syntax quasiquote
    (lambda (def-env use-env arg)
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

     (qq-expand arg)))

  (define-syntax and
    (lambda (def-env use-env . args)
      (if (null? args)
        #t
        (let ((head (car args))
              (tail (cdr args)))
          (if (null? tail)
            head
            `(if ,head (and ,@tail) #f))))))

  (define-syntax or
    (lambda (def-env use-env . args)
      (if (null? args)
        #f
        (let ((head (car args))
              (tail (cdr args)))
          (if (null? tail)
            head
            (let ((id (unique-id)))
              `(let ((,id ,head)) (if ,id ,id (or ,@tail))))))))))
