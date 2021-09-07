
(define-syntax (match x)
  (define (ellipsis? x)
    (and (identifier? x) (free-identifier=? x #'(... ...))))

  (define (wildcard? x)
    (and (identifier? x) (free-identifier=? x #'_)))

  (define (gensymmap xs)
    (map (lambda (x) (datum->syntax #'* (gensym (format "~a" x)))) xs))

  (define (parse-pattern v p binds k)
    (with-syntax ([v v])
      (syntax-case p (unquote)
        [p (wildcard? #'p) (k binds)]
        [() #`(if (null? v) #,(k binds) #f)]
        [,x (identifier? #'x) (k (cons #'(x v) binds))]
        [,(sat?) #`(if (sat? v) #,(k binds) #f)]
        [,(sat? x) (identifier? #'x)
         #`(if (sat? v) #,(k (cons #'(x v) binds)) #f)]

        ; [(pcar ?ell) (ellipsis? #'?ell) (parse-pattern #'v #'pcar binds k)]
        [(pcar ?ell . rest) (ellipsis? #'?ell)
         (with-syntax ([(vcar vcdr) (gensymmap '("vcar" "vcdr"))]
                       [(pair next) (gensymmap '("pair" "next"))])
           (let ([vars '()] [lists '()]
                 [bind (lambda (var val) #`(#,var (reverse #,val)))])
             (with-syntax
               ([code (parse-pattern #'vcar #'pcar '()
                        (lambda (binds)
                          (set! vars (map car binds))
                          (set! lists (gensymmap (syntax->datum vars)))
                          (with-syntax ([((val list) ...)
                                         (map list (map cadr binds) lists)])
                            #'(next (cdr pair) (cons val list) ...))))])
               (let ([binds (append (map bind vars lists) binds)])
                 (with-syntax ([(list ...) lists])
                   #`(let next ([pair v] [list '()] ...)
                       (or (and (pair? pair)
                                (let ([vcar (car pair)])
                                  code))
                           #,(parse-pattern #'pair #'rest binds k))))))))]

        [(pcar . pcdr)
         (with-syntax ([(vcar vcdr) (gensymmap '("vcar" "vcdr"))])
           #`(if (pair? v)
                 (let ([vcar (car v)] [vcdr (cdr v)])
                   #,(parse-pattern #'vcar #'pcar binds
                       (lambda (binds)
                         (parse-pattern #'vcdr #'pcdr binds k))))
                 #f))]

        [p (identifier? #'p) #`(if (eq? v 'p) #,(k binds) #f)]
        [p #`(if (equal? v 'p) #,(k binds) #f)])))

  (define (convert-clause c)
    (syntax-case c (guard)
      [(p (guard g ...) e es ...) #'(p (guard g ...) (begin e es ...))]
      [(p e es ...) #'(p (guard #t) (begin e es ...))]
      [_ (errorf 'match "invalid clause ~s" (syntax->datum c))]))

  (define (eliminate g k ok no)
    (define (false? x) (eq? x #f))
    (define (true? x) (and (not (eq? x #f)) (atom? x)))
    (with-syntax ([(guard . g) g] [k k] [ok ok] [no no])
      (cond
        [(andmap true? (syntax->datum #'g)) #'(k ok)]
        [(ormap false? (syntax->datum #'g)) #'no]
        [else #`(if (and #,@#'g) (k ok) no)])))

  (define (parse-clause k v c)
    (with-syntax ([(p g ok) (convert-clause c)])
      (parse-pattern #'v #'p '()
        (lambda (binds)
          #`(let #,(reverse binds)
              #,(eliminate #'g k #'ok #f))))))

  (syntax-case x ()
    [(_ e c0 c1 ...)
     #`(let ([v e])
         (call/cc
           (lambda (k)
             #,@(map (lambda (c) (parse-clause #'k #'v c))
                     #'(c0 c1 ...))
             (errorf 'match "failed to match ~s" v))))]))
