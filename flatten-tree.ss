(load "match.ss")


(define (flatten tree)
  (match tree
    [() '()]
    [(,hd . ,tl) `(,@(flatten hd) ,@(flatten tl))]
    [,x (list x)]
  ))


(printf "~a\n" (flatten '(((1 2 (3 4))) (5) ((6 7) 8 9))))
