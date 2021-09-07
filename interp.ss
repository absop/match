(load "match.ss")

(define (interpret exp)

  (define (syntax-error exp)
    (errorf 'interpret "syntax error at: ~a" exp))

  (define (runtime-error format exp)
    (errorf 'interpret format exp))

  (define (extend-env xs vs env)
    (append (map cons xs vs) env))

  (define (lookup-var x env)
    (cond [(assq x env) => cdr]
          [(runtime-error "undefined variable ~a" x)]))

  (define (general-apply proc vals)
    (match proc
      [,(procedure? internal) (apply internal vals)]
      [(closure ,exp ,env)
       (match exp
         [(lambda (,(symbol? args) ...) ,body)
          (let ([narg (length args)] [nval (length vals)])
            (cond
              [(> narg nval)
               (runtime-error "too few arguments of ~a" exp)]
              [(< narg nval)
               (runtime-error "too many arguments of ~a" exp)]
              [else (interp body (extend-env args vals env))]))]
         [(lambda ,(symbol? args) ,body)
          (interp body (cons `(,args . ,vals) env))]
         [_ (syntax-error exp)])]
      [_ (runtime-error "~a not a procedure" proc)]))

  (define (interp exp env)
    (match exp
           [,(boolean? x) x]
           [,(number? x) x]
           [,(symbol? x) (lookup-var x env)]
           [() '()]
           [(lambda ,args ,body) `(closure ,exp ,env)]

           [(if ,test ,then ,else)
            (if (interp test env)
                (interp then env)
                (interp else env))]

           [(let ([,(symbol? vars) ,exps] ...) ,body)
            (interp `((lambda ,vars ,body) ,@exps) env)]

           [(let* ([,(symbol? vars) ,exps] ...) ,body)
            (interp (fold-right
                      (lambda (x e body) `((lambda (,x) ,body) ,e))
                      body vars exps)
                    env)]

           [(and ,es ...) (andmap (lambda (e) (interp e env)) es)]
           [(or ,es ...) (ormap (lambda (e) (interp e env)) es)]

           [(,proc ,args ...)
            (let ([proc (interp proc env)]
                  [args (map (lambda (x) (interp x env)) args)])
              (general-apply proc args))]

           [_ (syntax-error exp)]))

  (interp exp `([+      . ,+]
                [-      . ,-]
                [*      . ,*]
                [/      . ,/]
                [=      . ,=]
                [equal? . ,equal?]
                [list   . ,list]
                [cons   . ,cons]
                [car    . ,car]
                [cdr    . ,cdr]
                [null?  . ,null?]
                [apply  . ,general-apply])))


(printf "~a\n"
  (interpret '(let ([x 1] [y 2] [z 3])
                (+ (* x x)
                   (* y y)
                   (* z z)))))
