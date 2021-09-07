;; Pattern matching for list
;; p ::= _
;;     | ()
;;     | ,x
;;     | ,(sat? x)
;;     | ,(sat?)
;;     | (tag ,x y ...)    -- record-type
;;     | (pcar ... . rest)
;;     | (pcar . pcdr)
;;     | symbol
;;     | datum

;; This version supports macth for top-level defined record types


(define-syntax (match x)
  (define (ellipsis? x)
    (and (identifier? x) (free-identifier=? x #'(... ...))))

  (define (wildcard? x)
    (and (identifier? x) (free-identifier=? x #'_)))

  (define (record-type id)
    (and (identifier? id)
         (let ([id (syntax->datum id)])
           (call/cc
             (lambda (k)
               (with-exception-handler
                 (lambda (e) (k #f))
                 (lambda ()
                   (eval `(record-type-descriptor ,id)))))))))

  (define (gensymmap fmt xs)
    (map (lambda (x) (datum->syntax #'match (gensym (format fmt x)))) xs))

  (define (formatsym fmt xs)
    (string->symbol (apply format fmt xs)))

  (define (id=? x) (lambda (y) (free-identifier=? x y)))

  (define (symbolic=? x)
    (let ([x (syntax->datum x)])
      (lambda (y) (eq? x (syntax->datum y)))))

  (define (bind-once x v binds k)
    (if (assp (id=? x) binds)
        #`(if (equal? #,v #,x)
              #,(k binds)
              #f)
        #`(let ([#,x #,v])
            #,(k (cons `(,x ,v) binds)))))

  (define (split-new-binds xs ls binds)
    (let f ([xs xs] [ls ls] [nbs '()] [eqs '()])
      (if (null? xs)
          (values (reverse nbs) (reverse eqs))
          (let ([x (car xs)] [xs (cdr xs)]
                [l (car ls)] [ls (cdr ls)])
            (if (assp (id=? x) binds)
                (f xs ls nbs (cons #`(equal? #,l #,x) eqs))
                (f xs ls (cons `(,x ,l) nbs) eqs))))))

  (define (parse-record p v binds k)
    (with-syntax ([(rtn fds ...) p] [v v])
      (let* ([pat (syntax->datum p)]
             [rtd (record-type #'rtn)]
             [vfn (record-type-field-names rtd)])

        (define fd-getter
          (let ([rtn (syntax->datum #'rtn)])
            (lambda (fn)
              (let ([getter (formatsym "~a-~a" (list rtn fn))])
                (if (top-level-bound? getter)
                    (datum->syntax #'match getter)
                    (errorf 'match "~a is not a field of the record type ~a"
                      fn rtn))))))

        (define (check-cons fn fns)
          (if (memq fn fns)
              (errorf 'match "duplicated fields in the record pattern ~a" pat)
              (cons fn fns)))

        (define (parse-fields i fds fns binds)
          (syntax-case fds ()
            [() (k binds)]
            [(fd . fds) (< i (vector-length vfn))
             (syntax-case #'fd (unquote)
               [,fn (let* ([fnsym (syntax->datum #'fn)]
                           [fns (check-cons fnsym fns)])
                      (with-syntax ([getter (fd-getter fnsym)])
                        (bind-once #'fn #'(getter v) binds
                          (lambda (binds)
                            (parse-fields (+ i 1) #'fds fns binds)))))]
               [val (let* ([fnsym (vector-ref vfn i)]
                           [fns (check-cons fnsym fns)])
                      (with-syntax ([getter (fd-getter fnsym)])
                        (if (wildcard? #'val)
                            (parse-fields (+ i 1) #'fds fns binds)
                            #`(if (equal? (getter v) val)
                                  #,(parse-fields (+ i 1) #'fds fns binds)
                                  #f))))])]
            [_ (errorf 'match "too many fields in the record pattern ~a" pat)]))

        #`(if ((record-predicate (record-type-descriptor rtn)) v)
              #,(parse-fields 0 #'(fds ...) '() binds)
              #f))))

  (define (parse-ellipsis p v binds k)
    (with-syntax ([(pcar rest) p] [v v]
                  [(vcar vcdr) (gensymmap "~a" '("vcar" "vcdr"))]
                  [(pair next) (gensymmap "~a" '("pair" "next"))])
      (let ([xs '()] [ls '()])
        (with-syntax
          ([code (parse-pattern #'pcar #'vcar '()
                   (lambda (binds)
                     (set! xs (map car binds))
                     (set! ls (gensymmap "~a*" (syntax->datum xs)))
                     (with-syntax ([((x l) ...) (map list xs ls)])
                       #'(next (cdr pair) (cons x l) ...))))])
          (let-values ([(nbs eqs) (split-new-binds xs ls binds)])
            (let* ([binds (append nbs binds)]
                   [ctn #`(let #,nbs #,(parse-pattern #'rest #'pair binds k))])
              (with-syntax ([(l ...) ls])
                #`(let next ([pair v] [l '()] ...)
                    (or (and (pair? pair)
                             (let ([vcar (car pair)])
                               code))
                        (let ([l (reverse l)] ...)
                          #,(if (null? eqs)
                                ctn
                                #`(if (and #,@eqs)
                                      #,ctn
                                      #f))))))))))))

  (define (parse-pattern p v binds k)
    (with-syntax ([v v])
      (syntax-case p (unquote)
        [p (wildcard? #'p) (k binds)]
        [() #`(if (null? v) #,(k binds) #f)]
        [,x (identifier? #'x) (bind-once #'x #'v binds k)]
        [,(sat?) #`(if (sat? v) #,(k binds) #f)]
        [,(sat? x) (identifier? #'x)
         #`(if (sat? v) #,(bind-once #'x #'v binds k) #f)]

        [(rtn fds ...) (record-type #'rtn)
         (parse-record #'(rtn fds ...) #'v binds k)]

        [(pcar ?ell . rest) (ellipsis? #'?ell)
         (parse-ellipsis #'(pcar rest) #'v binds k)]

        [(pcar . pcdr)
         (with-syntax ([(vcar vcdr) (gensymmap "~a" '("vcar" "vcdr"))])
           #`(if (pair? v)
                 (let ([vcar (car v)] [vcdr (cdr v)])
                   #,(parse-pattern #'pcar #'vcar binds
                       (lambda (binds)
                         (parse-pattern #'pcdr #'vcdr binds k))))
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
        [(andmap true? (syntax->datum #'g))
         #'(call-with-values (lambda () ok) k)]
        [(ormap false? (syntax->datum #'g)) #'no]
        [else #`(if (and #,@#'g)
                    (call-with-values (lambda () ok) k)
                    no)])))

  (define (parse-clause k v c)
    (with-syntax ([(p g ok) (convert-clause c)])
      (parse-pattern #'p #'v '()
        (lambda (binds)
          (eliminate #'g k #'ok #f)))))

  (define (parse-clauses k v cs)
    (syntax-case cs ()
      [(c . cs)
       (syntax-case (parse-clause k v #'c) (call-with-values)
         [(call-with-values p c) #'((call-with-values p c))]
         [#f (parse-clauses k v #'cs)]
         [c0 (cons #'c0 (parse-clauses k v #'cs))])]
      [() #`((errorf 'match "failed to match ~s" #,v))]))

  (syntax-case x ()
    [(_ e c0 c1 ...)
     #`(let ([v e])
         (call/cc
           (lambda (k)
             #,@(parse-clauses #'k #'v #'(c0 c1 ...)))))]))


;; Uncomment this code to run tests
#;
(let ()

(match '(let-values
          ([(x y z) (values 1 2 3)] [(a b c) (values 'a 'b 'c)])
          (list a b c x y z))
  [(let-values ([(,(symbol? vars) ...) ,exps] ...) ,body ...)
   (printf "~a\n" `(let-values ,vars ,exps ,body))])
;; (let-values ((x y z) (a b c)) ((values 1 2 3) (values 'a 'b 'c))
;;   ((list a b c x y z)))


(match '(x y z 1 2 3 . end)
  [(,(symbol? syms) ... ,rest ... . end)
   (printf "syms: ~a\n" syms)
   (printf "rest: ~a\n" rest)])
;; syms: (x y z)
;; rest: (1 2 3)


(match '(1 1 (1))
  [(,x ,x (,x))
   (printf "x=~a\n" x)])
;; x=1


(match '(1 2 3 (1 2 3))
  [(,x ... (,x ...))
   (printf "x=~a\n" x)])
;; x=(1 2 3)


(match '(1 2 3 1 2 3 (1 2 3))
  [(,x ... ,x ... (,x ...))
   (printf "x=~a\n" x)])
;; x=(1 2 3)


(match '(1 2 3 1 2 3 (1 2 3))
  [(,x ... ,y ... (,z ...))
   (printf "x=~a, y=~a, z=~a\n" x y z)])
;; x=(1 2 3 1 2 3), y=(), z=(1 2 3)
)

;; Uncomment this code to output the target code
#;
(parameterize ([print-gensym 'pretty/suffix])
  (pretty-print
    (expand
      '(match '(1 2 3 1 2 3 (1 2 3))
         [(,x ... ,x ... (,x ...))
          (printf "x=~a\n" x)]
         [_ (guard #f) #f]
         [_ (guard #t) #t]
         ))))
;; The output is shown below

#;
(let ([v.0 '(1 2 3 1 2 3 (1 2 3))])
  (call/cc
    (lambda (k.1)
      ((letrec ([next.2 (lambda (pair.3 x*.4)
                          (let ([t.5 (if (pair? pair.3)
                                         (let ([vcar.6 (car pair.3)])
                                           (let ([x.7 vcar.6])
                                             (next.2 (cdr pair.3) (cons x.7 x*.4))))
                                         #f)])
                            (if t.5
                                t.5
                                (let ([x*.8 (reverse x*.4)])
                                  (let ([x.9 x*.8])
                                    ((letrec ([next.10 (lambda (pair.11 x*.12)
                                                         (let ([t.13 (if (pair? pair.11)
                                                                         (let ([vcar.14 (car pair.11)])
                                                                           (let ([x.15 vcar.14])
                                                                             (next.10 (cdr pair.11) (cons x.15
                                                                                             x*.12))))
                                                                         #f)])
                                                           (if t.13
                                                               t.13
                                                               (let ([x*.16 (reverse x*.12)])
                                                                 (if (equal? x*.16 x.9)
                                                                     (if (pair? pair.11)
                                                                         (let ([vcar.17 (car pair.11)] [vcdr.18 (cdr pair.11)])
                                                                           ((letrec ([next.19 (lambda (pair.20 x*.21)
                                                                                                (let ([t.22 (if (pair? pair.20)
                                                                                                                (let ([vcar.23 (car pair.20)])
                                                                                                                  (let ([x.24 vcar.23])
                                                                                                                    (next.19 (cdr pair.20) (cons x.24
                                                                                                                                    x*.21))))
                                                                                                                #f)])
                                                                                                  (if t.22
                                                                                                      t.22
                                                                                                      (let ([x*.25 (reverse x*.21)])
                                                                                                        (if (equal? x*.25 x.9)
                                                                                                            (if (null? pair.20)
                                                                                                                (if (null? vcdr.18)
                                                                                                                    (call-with-values
                                                                                                                      (lambda ()
                                                                                                                        (printf "x=~a\n" x.9))
                                                                                                                      k.1)
                                                                                                                    #f)
                                                                                                                #f)
                                                                                                            #f)))))])
                                                                              next.19) vcar.17 '()))
                                                                         #f)
                                                                     #f)))))])
                                       next.10) pair.3 '()))))))])
         next.2) v.0 '())
      (call-with-values (lambda () #t) k.1))))
