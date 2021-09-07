(load "match.ss")

(let ([$apply apply])
  (define-record-type closure
    (fields (mutable name) args body env)
    (protocol
      (lambda (new)
        (lambda (exp name args body env)
          (or (and (not (null? body))
                   (match args
                     [,(symbol?) (new name args body env)]
                     [(,(symbol? args) ... . ,rest)
                      (guard (or (symbol? rest) (null? rest)))
                      (new name `(,@args . ,rest) body env)]
                     [_ #f]))
              (errorf 'make-closure "invalid syntax ~a" exp))))))

  (define (push-frame vars vals env)
    (cons (map cons vars vals) env))

  (define (lookup-bind var env)
    (ormap (lambda (frame) (assq var frame)) env))

  (define (lookup var env)
    (cond
      [(lookup-bind var env) => cdr]
      [(errorf 'lookup "undefined variable ~a" var)]))

  (define (assign! var val env)
    (set-car! env (cons (cons var val) (car env))))

  (define (set! var val env)
    (cond
      [(lookup-bind var env) => (lambda (bind) (set-cdr! bind val))]
      [else (assign! var val *the-global-env*)]))

  (define (define! exp id body env)
    (let-values ([(var val) (parse-define id body env)])
      (if (assq var (car env))
          (errorf 'define! "redefine variable ~a" var)
          (assign! var val env))))

  (define (parse-define id body env)
    (match (list id body)
      [(,(symbol?) (,exp))
       (let ([v (eval exp env)])
         (match v
           [(closure #f) (closure-name-set! v id)]
           [_ #f])
         (values id v))]
      [((,id . ,args) _)
       (let ([v (make-closure exp id args body env)])
         (values id v))]
      [_ (errorf 'parse-define "invalid syntax ~a" exp)]))

  (define (evals body env)
    (fold-left
      (lambda (r e) (eval e env))
      (eval (car body) env)
      (cdr body)))

  (define (eval/new-frame vars vals body env)
    ; (printf "eval/new-frame: vars: ~a, vals: ~a, body: ~a\n" vars vals body)
    (evals body (push-frame vars vals env)))

  (define (eval-let exp binds body env)
    (or (and (not (null? body))
             (match binds
               [((,(symbol? vars) ,exps) ...)
                (let ([vals (map (lambda (e) (eval e env)) exps)])
                  (eval/new-frame vars vals body env))]
               [_ #f]))
        (errorf 'eval-let "syntax error ~a" exp)))

  (define (parse-args demand supply)
    (let f ([formals demand]
            [actuals supply]
            [vars '()]
            [vals '()])
      (cond
        [(and (pair? formals) (pair? actuals))
         (f (cdr formals)
            (cdr actuals)
            (cons (car formals) vars)
            (cons (car actuals) vals))]
        [(and (null? formals) (null? actuals))
         (values (reverse vars) (reverse vals))]
        [(symbol? formals)
         (values (reverse (cons formals vars))
                 (reverse (cons actuals vals)))]
        [(null? actuals)
         (errorf 'apply "too few arguments ~a ~a" demand supply)]
        [(pair? actuals)
         (errorf 'apply "too many arguments ~a ~a" demand supply)]
        )))

  (define (apply proc vals)
    (match proc
      [,(procedure?) ($apply proc vals)]
      [(closure ,name ,args ,body ,env)
       (let-values ([(args vals) (parse-args args vals)])
         (if name
             (eval/new-frame `(,@args ,name) `(,@vals ,proc) body env)
             (eval/new-frame args vals body env)))]
      [else (errorf 'apply "~a is not a procedure" proc)]))

  ;; 实现 lexical eval 的方式之一是:
  ;; 应用每个函数时，都隐式地传入表示环境的参数。
  ;; eval 如果没有显示传入的`env`参数，则使用上层作用域隐式传入的`env`参数
  (define (eval exp env)
    (match exp
      [,(symbol?) (lookup exp env)]
      [,(number?) exp]
      [,(string?) exp]
      [,(boolean?) exp]
      [(quote ,exp) exp]
      [(lambda ,args . ,body) (make-closure exp #f args body env)]
      [(if ,test ,then ,else)
       (if (eval test env) (eval then env) (eval else env))]
      [(let ,binds . ,body) (eval-let exp binds body env)]
      [(define ,var . ,body) (define! exp var body env)]
      [(set! ,(symbol? var) ,exp) (set! var (eval exp env) env)]
      [(begin ,exp . ,exps) (evals (cons exp exps) env)]
      [(,proc . ,exps)
       (apply (eval proc env) (map (lambda (exp) (eval exp env)) exps))]
      [_ (errorf 'eval "invalid syntax ~a" exp)]))

  (define *the-global-env*
    `(()
      ([apply    .  ,apply]
       [car      .  ,car]
       [cdr      .  ,cdr]
       [caar     .  ,caar]
       [cdar     .  ,cdar]
       [cadr     .  ,cadr]
       [cddr     .  ,cddr]
       [caddr    .  ,caddr]
       [cdddr    .  ,cdddr]
       [eq?      .  ,eq?]
       [equal?   .  ,equal?]
       [eqv?     .  ,eqv?]
       [eval     .  ,eval]
       [cons     .  ,cons]
       [list     .  ,list]
       [null?    .  ,null?]
       [list?    .  ,list?]
       [zero?    .  ,zero?]
       [=        .  ,=]
       [<        .  ,<]
       [>        .  ,>]
       [+        .  ,+]
       [-        .  ,-]
       [*        .  ,*]
       [/        .  ,/]
       [^        .  ,expt]
       [%        .  ,remainder]
       [mod      .  ,mod]
       [div      .  ,div]
       [sin      .  ,sin]
       [cos      .  ,cos]
       [tan      .  ,tan]
       [sqrt     .  ,sqrt]
       [display  .  ,display]
       [printf   .  ,printf])))

  ; (define (parse exp)
  ;   [(let ,id ([,(symbol? vars) ,exps] ...) . ,body) 'todo]
  ;   [(letrec ([,(symbol? vars) ,exps] ...) . ,body) 'todo]
  ;   [(let* ([,(symbol? vars) ,exps] ...) . ,body) 'todo]
  ;   [(or . ,exps) (or->if exps)]
  ;   [(and . ,exps) (and->if exps)])

  (define (read-input)
    (display "> ")
    (read))

  (define (print v)
    (match v
      [_ (guard (eq? v (void))) #t]
      [(closure ,name)
       (if (symbol? name)
           (printf "#<procedure ~a>\n" name)
           (printf "#<procedure>\n"))]
      [_ (printf "~a\n" v)]))

  (define-syntax (try x)
    (syntax-case x (catch)
      [(try body (catch (e) handle-error ...))
       (with-syntax ([print-exception (datum->syntax #'try 'print-exception)]
                     [exception->string (datum->syntax #'try 'exception->string)])
         (syntax (call/cc
                   (lambda (k)
                     (with-exception-handler
                       (lambda (e)
                         (define (exception->string e)
                           (apply format
                             (cons (condition-message e)
                                   (condition-irritants e))))
                         (define (print-exception e)
                           (display "Exception: ")
                           (display (exception->string e))
                           (newline))
                         handle-error
                         ...
                         (k e))
                       (lambda () body))))))]))

  (let repl ()
    (let ([input (read-input)])
      (try (let ([value (eval input *the-global-env*)])
             (print value))
           (catch (e) (print-exception e)))
      (repl)))
)
