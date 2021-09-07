(#2%load "match.ss")

(let ([$apply.0 #2%apply])
  (letrec* ([rtd.1 (#2%$make-record-type-descriptor
                     #!base-rtd
                     'closure
                     #f
                     #f
                     #f
                     #f
                     '#((mutable name)
                        (immutable args)
                        (immutable body)
                        (immutable env))
                     'define-record-type)]
            [rcd.2 (#2%$make-record-constructor-descriptor
                     rtd.1
                     #f
                     (lambda (new.3)
                       (lambda (exp.4 name.5 args.6 body.7 env.8)
                         (let ([t.9 (if (#2%not (#2%null? body.7))
                                        (let ([v.10 args.6])
                                          (#2%call/cc
                                            (lambda (k.11)
                                              (if (#2%symbol? v.10)
                                                  (#2%call-with-values
                                                    (lambda ()
                                                      (new.3 name.5 args.6 body.7 env.8))
                                                    k.11)
                                                  #f)
                                              ((letrec ([next.12 (lambda (pair.13 args*.14)
                                                                   (let ([t.15 (if (#2%pair? pair.13)
                                                                                   (let ([vcar.16 (#2%car pair.13)])
                                                                                     (if (#2%symbol? vcar.16)
                                                                                         (let ([args.17 vcar.16])
                                                                                           (next.12 (#2%cdr pair.13)
                                                                                                    (#2%cons args.17 args*.14)))
                                                                                         #f))
                                                                                   #f)])
                                                                     (if t.15
                                                                         t.15
                                                                         (let ([args*.18 (#2%reverse args*.14)])
                                                                           (let ([args.19 args*.18])
                                                                             (let ([rest.20 pair.13])
                                                                               (if (let ([t.21 (#2%symbol? rest.20)])
                                                                                     (if t.21
                                                                                         t.21
                                                                                         (#2%null? rest.20)))
                                                                                   (#2%call-with-values
                                                                                     (lambda ()
                                                                                       (new.3 name.5
                                                                                         (#2%append args.19 rest.20)
                                                                                         body.7
                                                                                         env.8))
                                                                                     k.11)
                                                                                   #f)))))))])
                                                 next.12) v.10 '())
                                              (#2%call-with-values (lambda ()
                                                                     #f) k.11))))
                                        #f)])
                           (if t.9
                               t.9
                               (#2%errorf
                                 'make-closure
                                 "invalid syntax ~a"
                                 exp.4)))))
                     'define-record-type)]
            [make-closure.22 (#2%r6rs:record-constructor rcd.2)]
            [closure?.23 (#2%record-predicate rtd.1)]
            [closure-name.24 (#2%record-accessor rtd.1 0)]
            [closure-args.25 (#2%record-accessor rtd.1 1)]
            [closure-body.26 (#2%record-accessor rtd.1 2)]
            [closure-env.27 (#2%record-accessor rtd.1 3)]
            [closure-name-set!.28 (#2%record-mutator rtd.1 0)]
            [push-frame.29
              (lambda (vars.30 vals.31 env.32)
                (#2%cons (#2%map #2%cons vars.30 vals.31) env.32))]
            [lookup-bind.33
              (lambda (var.34 env.35)
                (#2%ormap (lambda (frame.36) (#2%assq var.34 frame.36))
                          env.35))]
            [lookup.37
              (lambda (var.38 env.39)
                (let ([t.40 (lookup-bind.33 var.38 env.39)])
                  (if t.40
                      ((let ([p.41 #2%cdr]) p.41) t.40)
                      (let ([t.42 (#2%errorf 'lookup "undefined variable ~a" var.38)])
                        (if t.42 t.42 (#2%void))))))]
            [assign!.43
              (lambda (var.44 val.45 env.46)
                (#2%set-car! env.46
                  (#2%cons (#2%cons var.44 val.45) (#2%car env.46))))]
            [set!.47 (lambda (var.48 val.49 env.50)
                       (let ([t.51 (lookup-bind.33 var.48 env.50)])
                         (if t.51
                             ((let ([p.52 (lambda (bind.53)
                                            (#2%set-cdr! bind.53 val.49))])
                                p.52) t.51)
                             (assign!.43 var.48 val.49 *the-global-env*.54))))]
            [define!.55
              (lambda (exp.56 id.57 body.58 env.59)
                (#2%call-with-values
                  (lambda ()
                    (parse-define.60 id.57 body.58 env.59))
                  (case-lambda
                    [(var.61 val.62)
                     (let ([var.63 var.61] [val.64 val.62])
                       (if (#2%assq var.63 (#2%car env.59))
                           (#2%errorf 'define! "redefine variable ~a" var.63)
                           (assign!.43 var.63 val.64 env.59)))]
                    [args.65 (#2%assertion-violation
                               'let-values
                               "incorrect number of values from rhs (parse-define id body env)")])))]
            [parse-define.60
              (lambda (id.66 body.67 env.68)
                (let ([v.69 (#2%list id.66 body.67)])
                  (#2%call/cc
                    (lambda (k.70)
                      (if (#2%pair? v.69)
                          (let ([vcar.71 (#2%car v.69)] [vcdr.72 (#2%cdr v.69)])
                            (if (#2%symbol? vcar.71)
                                (if (#2%pair? vcdr.72)
                                    (let ([vcar.73 (#2%car vcdr.72)]
                                          [vcdr.74 (#2%cdr vcdr.72)])
                                      (if (#2%pair? vcar.73)
                                          (let ([vcar.75 (#2%car vcar.73)]
                                                [vcdr.76 (#2%cdr vcar.73)])
                                            (let ([exp.77 vcar.75])
                                              (if (#2%null? vcdr.76)
                                                  (if (#2%null? vcdr.74)
                                                      (#2%call-with-values
                                                        (lambda ()
                                                          (let ([v.78 (eval.79 exp.77 env.68)])
                                                            (let ([v.80 v.78])
                                                              (#2%call/cc
                                                                (lambda (k.81)
                                                                  (let ([vfn.82 (#2%record-type-field-names rtd.1)])
                                                                    (let ([len.83 (#2%vector-length vfn.82)])
                                                                      (letrec* ([fn->fi.84
                                                                                  (lambda (fn.85)
                                                                                    (let ([t.86 ((letrec ([index.87 (lambda (i.88)
                                                                                                                      (if (#2%< i.88 len.83)
                                                                                                                          (let ([t.89 (if (#2%eq? fn.85 (#2%vector-ref vfn.82 i.88))
                                                                                                                                          i.88
                                                                                                                                          #f)])
                                                                                                                            (if t.89
                                                                                                                                t.89
                                                                                                                                (index.87 (#2%+ i.88 1))))
                                                                                                                          #f))])
                                                                                                   index.87) 0)])
                                                                                      (if t.86
                                                                                          t.86
                                                                                          (#2%errorf
                                                                                            'match
                                                                                            "~a is not a field of the record type ~a"
                                                                                            fn.85
                                                                                            'closure))))])
                                                                               (if ((#2%record-predicate rtd.1) v.80)
                                                                                   (if (#2%< len.83 1)
                                                                                       (#2%errorf
                                                                                         'match
                                                                                         "too many fields in the record pattern ~a"
                                                                                         '(closure #f))
                                                                                       (let ([fis.90 (#2%list 0)])
                                                                                         (check-unique fis.90 '(closure #f))
                                                                                         (if (#2%equal? ((#2%record-accessor rtd.1 0) v.80) #f)
                                                                                             (#2%call-with-values
                                                                                               (lambda ()
                                                                                                 (closure-name-set!.28 v.78 id.66))
                                                                                               k.81)
                                                                                             #f)))
                                                                                   #f))))
                                                                  (#2%call-with-values (lambda ()
                                                                                         #f) k.81))))
                                                            (#2%values id.66 v.78)))
                                                        k.70)
                                                      #f)
                                                  #f)))
                                          #f))
                                    #f)
                                #f))
                          #f)
                      (if (#2%pair? v.69)
                          (let ([vcar.91 (#2%car v.69)] [vcdr.92 (#2%cdr v.69)])
                            (if (#2%pair? vcar.91)
                                (let ([vcar.93 (#2%car vcar.91)]
                                      [vcdr.94 (#2%cdr vcar.91)])
                                  (let ([id.95 vcar.93])
                                    (let ([args.96 vcdr.94])
                                      (if (#2%pair? vcdr.92)
                                          (let ([vcar.97 (#2%car vcdr.92)]
                                                [vcdr.98 (#2%cdr vcdr.92)])
                                            (if (#2%null? vcdr.98)
                                                (#2%call-with-values
                                                  (lambda ()
                                                    (let ([v.99 (make-closure.22
                                                                  #2%exp
                                                                  id.95
                                                                  args.96
                                                                  body.67
                                                                  env.68)])
                                                      (#2%values id.95 v.99)))
                                                  k.70)
                                                #f))
                                          #f))))
                                #f))
                          #f)
                      (#2%call-with-values
                        (lambda ()
                          (#2%errorf
                            'parse-define
                            "invalid syntax ~a"
                            #2%exp))
                        k.70)))))]
            [evals.100
              (lambda (body.101 env.102)
                (#2%fold-left
                  (lambda (r.103 e.104)
                    (eval.79 e.104 env.102))
                  (eval.79 (#2%car body.101) env.102)
                  (#2%cdr body.101)))]
            [eval/new-frame.105
              (lambda (vars.106 vals.107 body.108 env.109)
                (evals.100 body.108
                  (push-frame.29 vars.106 vals.107 env.109)))]
            [eval-let.110
              (lambda (exp.111 binds.112 body.113 env.114)
                (let ([t.115 (if (#2%not (#2%null? body.113))
                                 (let ([v.116 binds.112])
                                   (#2%call/cc
                                     (lambda (k.117)
                                       ((letrec ([next.118 (lambda (pair.119 exps*.120 vars*.121)
                                                             (let ([t.122 (if (#2%pair? pair.119)
                                                                              (let ([vcar.123 (#2%car pair.119)])
                                                                                (if (#2%pair? vcar.123)
                                                                                    (let ([vcar.124 (#2%car vcar.123)]
                                                                                          [vcdr.125 (#2%cdr vcar.123)])
                                                                                      (if (#2%symbol? vcar.124)
                                                                                          (let ([vars.126 vcar.124])
                                                                                            (if (#2%pair? vcdr.125)
                                                                                                (let ([vcar.127 (#2%car vcdr.125)]
                                                                                                      [vcdr.128 (#2%cdr vcdr.125)])
                                                                                                  (let ([exps.129 vcar.127])
                                                                                                    (if (#2%null? vcdr.128)
                                                                                                        (next.118 (#2%cdr pair.119)
                                                                                                          (#2%cons exps.129 exps*.120)
                                                                                                          (#2%cons vars.126 vars*.121))
                                                                                                        #f)))
                                                                                                #f))
                                                                                          #f))
                                                                                    #f))
                                                                              #f)])
                                                               (if t.122
                                                                   t.122
                                                                   (let ([exps*.130 (#2%reverse exps*.120)]
                                                                         [vars*.131 (#2%reverse vars*.121)])
                                                                     (let ([exps.132 exps*.130] [vars.133 vars*.131])
                                                                       (if (#2%null? pair.119)
                                                                           (#2%call-with-values
                                                                             (lambda ()
                                                                               (let ([vals.134 (#2%map (lambda (e.135)
                                                                                                         (eval.79 e.135 env.114))
                                                                                                       exps.132)])
                                                                                 (eval/new-frame.105
                                                                                   vars.133
                                                                                   vals.134
                                                                                   body.113
                                                                                   env.114)))
                                                                             k.117)
                                                                           #f))))))])
                                          next.118) v.116 '() '())
                                       (#2%call-with-values (lambda () #f) k.117))))
                                 #f)])
                  (if t.115
                      t.115
                      (#2%errorf 'eval-let "syntax error ~a" exp.111))))]
            [parse-args.136
              (lambda (demand.137 supply.138)
                ((letrec ([f.139 (lambda (formals.140 actuals.141 vars.142 vals.143)
                                   (if (if (#2%pair? formals.140)
                                           (#2%pair? actuals.141)
                                           #f)
                                       (f.139 (#2%cdr formals.140)
                                         (#2%cdr actuals.141)
                                         (#2%cons (#2%car formals.140) vars.142)
                                         (#2%cons (#2%car actuals.141) vals.143))
                                       (if (if (#2%null? formals.140)
                                               (#2%null? actuals.141)
                                               #f)
                                           (#2%values (#2%reverse vars.142)
                                             (#2%reverse vals.143))
                                           (if (#2%symbol? formals.140)
                                               (#2%values
                                                 (#2%reverse (#2%cons formals.140 vars.142))
                                                 (#2%reverse (#2%cons actuals.141 vals.143)))
                                               (if (#2%null? actuals.141)
                                                   (#2%errorf
                                                     'apply
                                                     "too few arguments ~a ~a"
                                                     demand.137
                                                     supply.138)
                                                   (if (#2%pair? actuals.141)
                                                       (#2%errorf
                                                         'apply
                                                         "too many arguments ~a ~a"
                                                         demand.137
                                                         supply.138)
                                                       (#2%void)))))))])
                   f.139)
                 demand.137
                 supply.138
                 '()
                 '()))]
            [apply.144
              (lambda (proc.145 vals.146)
                (let ([v.147 proc.145])
                  (#2%call/cc
                    (lambda (k.148)
                      (if (#2%procedure? v.147)
                          (#2%call-with-values
                            (lambda ()
                              ($apply.0 proc.145 vals.146))
                            k.148)
                          #f)
                      (let ([vfn.149 (#2%record-type-field-names rtd.1)])
                        (let ([len.150 (#2%vector-length vfn.149)])
                          (letrec* ([fn->fi.151
                                      (lambda (fn.152)
                                        (let ([t.153 ((letrec ([index.154
                                                                 (lambda (i.155)
                                                                   (if (#2%< i.155 len.150)
                                                                       (let ([t.156 (if (#2%eq? fn.152 (#2%vector-ref vfn.149 i.155))
                                                                                        i.155
                                                                                        #f)])
                                                                         (if t.156
                                                                             t.156
                                                                             (index.154 (#2%+ i.155 1))))
                                                                       #f))])
                                                        index.154) 0)])
                                          (if t.153
                                              t.153
                                              (#2%errorf
                                                'match
                                                "~a is not a field of the record type ~a"
                                                fn.152
                                                'closure))))])
                            (if ((#2%record-predicate rtd.1) v.147)
                                (if (#2%< len.150 4)
                                    (#2%errorf
                                      'match
                                      "too many fields in the record pattern ~a"
                                      '(closure ,name ,args ,body ,env))
                                    (let ([fis.157 (#2%list (fn->fi.151 'name)
                                                            (fn->fi.151 'args)
                                                            (fn->fi.151 'body)
                                                            (fn->fi.151 'env))])
                                      (check-unique fis.157
                                        '(closure ,name ,args ,body ,env))
                                      (let ([vfi.158 (#2%list->vector fis.157)])
                                        (let ([name.159 ((#2%record-accessor
                                                           rtd.1
                                                           (#2%vector-ref vfi.158 0)) v.147)])
                                          (let ([args.160 ((#2%record-accessor
                                                             rtd.1
                                                             (#2%vector-ref vfi.158 1)) v.147)])
                                            (let ([body.161 ((#2%record-accessor
                                                               rtd.1
                                                               (#2%vector-ref vfi.158 2)) v.147)])
                                              (let ([env.162 ((#2%record-accessor
                                                                rtd.1
                                                                (#2%vector-ref vfi.158 3)) v.147)])
                                                (#2%call-with-values
                                                  (lambda ()
                                                    (#2%call-with-values
                                                      (lambda ()
                                                        (parse-args.136 args.160 vals.146))
                                                      (case-lambda
                                                        [(args.163 vals.164)
                                                         (let ([args.165 args.163] [vals.166 vals.164])
                                                           (if name.159
                                                               (eval/new-frame.105
                                                                 (#2%append args.165 (#2%list name.159))
                                                                 (#2%append vals.166 (#2%list proc.145))
                                                                 body.161
                                                                 env.162)
                                                               (eval/new-frame.105
                                                                 args.165
                                                                 vals.166
                                                                 body.161
                                                                 env.162)))]
                                                        [args.167 (#2%assertion-violation
                                                                    'let-values
                                                                    "incorrect number of values from rhs (parse-args args vals)")])))
                                                  k.148))))))))
                                #f))))
                      (if (#2%eq? v.147 'else)
                          (#2%call-with-values
                            (lambda ()
                              (#2%errorf
                                'apply
                                "~a is not a procedure"
                                proc.145))
                            k.148)
                          #f)
                      (#2%errorf 'match "failed to match ~s" v.147)))))]
            [eval.79 (lambda (exp.168 env.169)
                       (let ([v.170 exp.168])
                         (#2%call/cc
                           (lambda (k.171)
                             (if (#2%symbol? v.170)
                                 (#2%call-with-values
                                   (lambda ()
                                     (lookup.37 exp.168 env.169))
                                   k.171)
                                 #f)
                             (if (#2%number? v.170)
                                 (#2%call-with-values (lambda () exp.168) k.171)
                                 #f)
                             (if (#2%string? v.170)
                                 (#2%call-with-values (lambda () exp.168) k.171)
                                 #f)
                             (if (#2%boolean? v.170)
                                 (#2%call-with-values (lambda () exp.168) k.171)
                                 #f)
                             (if (#2%pair? v.170)
                                 (let ([vcar.172 (#2%car v.170)]
                                       [vcdr.173 (#2%cdr v.170)])
                                   (if (#2%eq? vcar.172 'quote)
                                       (if (#2%pair? vcdr.173)
                                           (let ([vcar.174 (#2%car vcdr.173)]
                                                 [vcdr.175 (#2%cdr vcdr.173)])
                                             (let ([exp.176 vcar.174])
                                               (if (#2%null? vcdr.175)
                                                   (#2%call-with-values (lambda ()
                                                                          exp.176) k.171)
                                                   #f)))
                                           #f)
                                       #f))
                                 #f)
                             (if (#2%pair? v.170)
                                 (let ([vcar.177 (#2%car v.170)]
                                       [vcdr.178 (#2%cdr v.170)])
                                   (if (#2%eq? vcar.177 'lambda)
                                       (if (#2%pair? vcdr.178)
                                           (let ([vcar.179 (#2%car vcdr.178)]
                                                 [vcdr.180 (#2%cdr vcdr.178)])
                                             (let ([args.181 vcar.179])
                                               (let ([body.182 vcdr.180])
                                                 (#2%call-with-values
                                                   (lambda ()
                                                     (make-closure.22
                                                       exp.168
                                                       #f
                                                       args.181
                                                       body.182
                                                       env.169))
                                                   k.171))))
                                           #f)
                                       #f))
                                 #f)
                             (if (#2%pair? v.170)
                                 (let ([vcar.183 (#2%car v.170)]
                                       [vcdr.184 (#2%cdr v.170)])
                                   (if (#2%eq? vcar.183 'if)
                                       (if (#2%pair? vcdr.184)
                                           (let ([vcar.185 (#2%car vcdr.184)]
                                                 [vcdr.186 (#2%cdr vcdr.184)])
                                             (let ([test.187 vcar.185])
                                               (if (#2%pair? vcdr.186)
                                                   (let ([vcar.188 (#2%car vcdr.186)]
                                                         [vcdr.189 (#2%cdr vcdr.186)])
                                                     (let ([then.190 vcar.188])
                                                       (if (#2%pair? vcdr.189)
                                                           (let ([vcar.191 (#2%car vcdr.189)]
                                                                 [vcdr.192 (#2%cdr vcdr.189)])
                                                             (let ([else.193 vcar.191])
                                                               (if (#2%null? vcdr.192)
                                                                   (#2%call-with-values
                                                                     (lambda ()
                                                                       (if (eval.79 test.187 env.169)
                                                                           (eval.79 then.190 env.169)
                                                                           (eval.79 else.193 env.169)))
                                                                     k.171)
                                                                   #f)))
                                                           #f)))
                                                   #f)))
                                           #f)
                                       #f))
                                 #f)
                             (if (#2%pair? v.170)
                                 (let ([vcar.194 (#2%car v.170)]
                                       [vcdr.195 (#2%cdr v.170)])
                                   (if (#2%eq? vcar.194 'let)
                                       (if (#2%pair? vcdr.195)
                                           (let ([vcar.196 (#2%car vcdr.195)]
                                                 [vcdr.197 (#2%cdr vcdr.195)])
                                             (let ([binds.198 vcar.196])
                                               (let ([body.199 vcdr.197])
                                                 (#2%call-with-values
                                                   (lambda ()
                                                     (eval-let.110 exp.168 binds.198 body.199 env.169))
                                                   k.171))))
                                           #f)
                                       #f))
                                 #f)
                             (if (#2%pair? v.170)
                                 (let ([vcar.200 (#2%car v.170)]
                                       [vcdr.201 (#2%cdr v.170)])
                                   (if (#2%eq? vcar.200 'define)
                                       (if (#2%pair? vcdr.201)
                                           (let ([vcar.202 (#2%car vcdr.201)]
                                                 [vcdr.203 (#2%cdr vcdr.201)])
                                             (let ([var.204 vcar.202])
                                               (let ([body.205 vcdr.203])
                                                 (#2%call-with-values
                                                   (lambda ()
                                                     (define!.55 exp.168 var.204 body.205 env.169))
                                                   k.171))))
                                           #f)
                                       #f))
                                 #f)
                             (if (#2%pair? v.170)
                                 (let ([vcar.206 (#2%car v.170)]
                                       [vcdr.207 (#2%cdr v.170)])
                                   (if (#2%eq? vcar.206 'set!)
                                       (if (#2%pair? vcdr.207)
                                           (let ([vcar.208 (#2%car vcdr.207)]
                                                 [vcdr.209 (#2%cdr vcdr.207)])
                                             (if (#2%symbol? vcar.208)
                                                 (let ([var.210 vcar.208])
                                                   (if (#2%pair? vcdr.209)
                                                       (let ([vcar.211 (#2%car vcdr.209)]
                                                             [vcdr.212 (#2%cdr vcdr.209)])
                                                         (let ([exp.213 vcar.211])
                                                           (if (#2%null? vcdr.212)
                                                               (#2%call-with-values
                                                                 (lambda ()
                                                                   (set!.47 var.210
                                                                     (eval.79 exp.213 env.169)
                                                                     env.169))
                                                                 k.171)
                                                               #f)))
                                                       #f))
                                                 #f))
                                           #f)
                                       #f))
                                 #f)
                             (if (#2%pair? v.170)
                                 (let ([vcar.214 (#2%car v.170)]
                                       [vcdr.215 (#2%cdr v.170)])
                                   (if (#2%eq? vcar.214 'begin)
                                       (if (#2%pair? vcdr.215)
                                           (let ([vcar.216 (#2%car vcdr.215)]
                                                 [vcdr.217 (#2%cdr vcdr.215)])
                                             (let ([exp.218 vcar.216])
                                               (let ([exps.219 vcdr.217])
                                                 (#2%call-with-values
                                                   (lambda ()
                                                     (evals.100 (#2%cons exp.218 exps.219) env.169))
                                                   k.171))))
                                           #f)
                                       #f))
                                 #f)
                             (if (#2%pair? v.170)
                                 (let ([vcar.220 (#2%car v.170)]
                                       [vcdr.221 (#2%cdr v.170)])
                                   (let ([proc.222 vcar.220])
                                     (let ([exps.223 vcdr.221])
                                       (#2%call-with-values
                                         (lambda ()
                                           (apply.144
                                             (eval.79 proc.222 env.169)
                                             (#2%map (lambda (exp.224)
                                                       (eval.79 exp.224 env.169))
                                                     exps.223)))
                                         k.171))))
                                 #f)
                             (#2%call-with-values
                               (lambda ()
                                 (#2%errorf 'eval "invalid syntax ~a" exp.168))
                               k.171)))))]
            [*the-global-env*.54
              (#2%list '()
                (#2%list (#2%cons 'apply apply.144)
                         (#2%cons 'car #2%car)
                         (#2%cons 'cdr #2%cdr)
                         (#2%cons 'caar #2%caar)
                         (#2%cons 'cdar #2%cdar)
                         (#2%cons 'cadr #2%cadr)
                         (#2%cons 'cddr #2%cddr)
                         (#2%cons 'caddr #2%caddr)
                         (#2%cons 'cdddr #2%cdddr)
                         (#2%cons 'eq? #2%eq?)
                         (#2%cons 'equal? #2%equal?)
                         (#2%cons 'eqv? #2%eqv?)
                         (#2%cons 'eval eval.79)
                         (#2%cons 'cons #2%cons)
                         (#2%cons 'list #2%list)
                         (#2%cons 'null? #2%null?)
                         (#2%cons 'list? #2%list?)
                         (#2%cons 'zero? #2%zero?)
                         (#2%cons '= #2%=)
                         (#2%cons '< #2%<)
                         (#2%cons '> #2%>)
                         (#2%cons '+ #2%+)
                         (#2%cons '- #2%-)
                         (#2%cons '* #2%*)
                         (#2%cons '/ #2%/)
                         (#2%cons '^ #2%expt)
                         (#2%cons '% #2%remainder)
                         (#2%cons 'mod #2%mod)
                         (#2%cons 'div #2%div)
                         (#2%cons 'sin #2%sin)
                         (#2%cons 'cos #2%cos)
                         (#2%cons 'tan #2%tan)
                         (#2%cons 'sqrt #2%sqrt)
                         (#2%cons 'display #2%display)
                         (#2%cons 'printf #2%printf)))]
            [read-input.225 (lambda () (#2%display "> ") (#2%read))]
            [print.226
              (lambda (v.227)
                (let ([v.228 v.227])
                  (#2%call/cc
                    (lambda (k.229)
                      (if (#2%eq? v.227 (#2%void))
                          (#2%call-with-values (lambda () #t) k.229)
                          #f)
                      (let ([vfn.230 (#2%record-type-field-names rtd.1)])
                        (let ([len.231 (#2%vector-length vfn.230)])
                          (letrec* ([fn->fi.232
                                      (lambda (fn.233)
                                        (let ([t.234 ((letrec ([index.235
                                                                 (lambda (i.236)
                                                                   (if (#2%< i.236 len.231)
                                                                       (let ([t.237 (if (#2%eq? fn.233 (#2%vector-ref vfn.230 i.236))
                                                                                        i.236
                                                                                        #f)])
                                                                         (if t.237
                                                                             t.237
                                                                             (index.235 (#2%+ i.236 1))))
                                                                       #f))])
                                                        index.235) 0)])
                                          (if t.234
                                              t.234
                                              (#2%errorf
                                                'match
                                                "~a is not a field of the record type ~a"
                                                fn.233
                                                'closure))))])
                                   (if ((#2%record-predicate rtd.1) v.228)
                                       (if (#2%< len.231 1)
                                           (#2%errorf
                                             'match
                                             "too many fields in the record pattern ~a"
                                             '(closure ,name))
                                           (let ([fis.238 (#2%list (fn->fi.232 'name))])
                                             (check-unique fis.238 '(closure ,name))
                                             (let ([vfi.239 (#2%list->vector fis.238)])
                                               (let ([name.240 ((#2%record-accessor
                                                                  rtd.1
                                                                  (#2%vector-ref vfi.239 0)) v.228)])
                                                 (#2%call-with-values
                                                   (lambda ()
                                                     (if (#2%symbol? name.240)
                                                         (#2%printf "#<procedure ~a>\n" name.240)
                                                         (#2%printf "#<procedure>\n")))
                                                   k.229)))))
                                       #f))))
                      (#2%call-with-values
                        (lambda () (#2%printf "~a\n" v.227))
                        k.229)))))])
           ((letrec ([repl.241 (lambda ()
                                 (let ([input.242 (read-input.225)])
                                   (#2%call/cc
                                     (lambda (k.243)
                                       (#2%with-exception-handler
                                         (lambda (e.244)
                                           (letrec* ([exception->string.245
                                                       (lambda (e.246)
                                                         (apply.144
                                                           #2%format
                                                           (#2%cons (#2%condition-message e.246)
                                                                    (#2%condition-irritants e.246))))]
                                                     [print-exception.247
                                                       (lambda (e.248)
                                                         (#2%display "Exception: ")
                                                         (#2%display (exception->string.245 e.248))
                                                         (#2%newline))])
                                                    (print-exception.247 e.244)
                                                    (k.243 e.244)))
                                         (lambda ()
                                           (let ([value.249
                                                   (eval.79 input.242 *the-global-env*.54)])
                                             (print.226 value.249))))))
                                   (repl.241)))])
              repl.241))))
