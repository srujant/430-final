(define h (make-hash (list (cons 5 6) (cons 9 10))))
(define y (hash-ref h 9))
(hash-set! h 9 11)
(+ y (hash-ref h 9))