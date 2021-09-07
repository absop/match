(define pprint-expand
  (case-lambda
    [(exp) (pprint-expand exp (standard-output-port))]
    [(exp op)
     (parameterize ([print-gensym 'pretty/suffix])
       (pretty-print (expand exp) op))]))


(define (expand-file in out)
  (unless (string? in)
          (errorf 'expand-file "~s is not a string" in))
  (unless (string? out)
          (errorf 'expand-file "~s is not a string" out))
  (call-with-port
    (open-file-input-port in
      (file-options)
      (buffer-mode block)
      (native-transcoder))
    (lambda (ip)
      (call-with-port
        (open-file-output-port out
          (file-options no-fail)
          (buffer-mode block)
          (native-transcoder))
        (lambda (op)
          (let loop ()
            (let ([exp (read ip)])
              (unless (eof-object? exp)
                      (pprint-expand exp op)
                      (newline op)
                      (loop)))))))))

(load "match.ss")

(expand-file "interp-with-match.ss" "interp-with-match.expanded.ss")
