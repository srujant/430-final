#lang racket
(require "utils.rkt")
(define (correct? e env) 
    (define (var? x) (symbol? x))
    (define ((fun? env) e)
      (match e
        [`(lambda ,x ,e) #t] [(? prim?) #t] [else
      (if (hash-has-key? env e) (let ([fun (hash-ref env e)]) (match fun [`(lambda ,x ,e) #t] [(? prim?) #t] [else (raise "Not a function being applied to.")] )) (raise "Using variable that is not initialized."))]))
    (define ((rec/with env) e)
      (correct? e env))
    (define (no-duplicates? lst)
      (= (set-count (list->set lst)) (length lst)))
    (define (ext env keys vals)
      (foldl (lambda (key value currhash) (hash-set currhash key value)) env keys vals))
    (define (improper-args? args)
      (if (var? args)
          #t
          (and (cons? args)
               (var? (car args))
               (improper-args? (cdr args)))))
    (define (cond-clause? cls) 
      (match cls
        [`(,(? (rec/with env))) #t]
        [`(,(? (rec/with env)) ,(? (rec/with env))) #t]
        [else #f]))
    (define (case-clause? cls) 
      (match cls
        [`((,(? datum?) ...) ,(? (rec/with env))) #t]
        [else #f]))
    (match e
      [`(letrec* ([,(? var? xs) ,es] ...) ,e0)
       (and (no-duplicates? xs)
            (andmap (rec/with (ext env xs es))
                    (cons e0 es)))
      ]
      [`(letrec ([,(? var? xs) ,es] ...) ,e0)
       (and (no-duplicates? xs)
            (andmap (rec/with (ext env xs))
                    (cons e0 es)))]
         
      [`(let* () ,e0)
       ((rec/with env) e0)]
      [`(let* ([,x ,e0]) ,e1)
       ((rec/with env) `(let ([,x ,e0]) ,e1))]
      [`(let* ([,x ,e0] . ,rest) ,e1)
       ((rec/with env) `(let ([,x ,e0]) (let* ,rest ,e1)))]
      [`(let ([,(? symbol? xs) ,(? (rec/with env) es)] ...) ,e0)
       (and (no-duplicates? xs)
            ((rec/with (ext env xs es)) e0))]
      [`(let ,(? var? lp)  ([,xs ,es] ...) ,e0)
       ((rec/with env) `(letrec ([,lp (lambda ,xs ,e0)]) (,lp . ,es)))]
         
      [`(lambda (,(? var? xs) ...) ,e0)
       (and (no-duplicates? xs)
            ((rec/with (ext env xs (map void xs))) e0))]
      [`(lambda ,(? var? x) ,e0)
       ((rec/with (ext env (list x))) e0)]
      [`(lambda ,(? improper-args? args) ,e0)
       (and (no-duplicates? (flatten args))
            ((rec/with (ext env (flatten args))) e0))]
      [`(delay ,(? (rec/with env))) #t]
      [`(force ,(? (rec/with env))) #t]
      [`(guard (,(? symbol? x) ,clauses ...) ,(? (rec/with env)))
       (correct? `(cond . ,clauses) (ext env (list x)))]
      [`(raise ,(? (rec/with env))) #t]
      [`(dynamic-wind ,(? (rec/with env)) ,(? (rec/with env)) ,(? (rec/with env))) #t]
      [`(cond ,(? cond-clause?) ...) #t]
      [`(cond ,(? cond-clause?) ... (else ,(? (rec/with env)))) #t]
      [`(case ,(? (rec/with env)) ,(? case-clause?) ...) #t]
      [`(case ,(? (rec/with env)) ,(? case-clause?) ... (else ,(? (rec/with env)))) #t]
      [`(and ,(? (rec/with env)) ...) #t]
      [`(or ,(? (rec/with env)) ...) #t]
      [`(when ,(? (rec/with env)) ,(? (rec/with env))) #t]
      [`(unless ,(? (rec/with env)) ,(? (rec/with env))) #t]
      [`(if ,(? (rec/with env)) ,(? (rec/with env)) ,(? (rec/with env))) #t]
      [`(set! ,(? symbol?) ,(? (rec/with env))) #t]
      [`(begin ,(? (rec/with env)) ,(? (rec/with env)) ...) #t]
      [`(call/cc ,(? (rec/with env))) #t]
      [`(let/cc ,(? symbol? x) ,eb) ((rec/with (ext env (list x))) eb)]

      [(? var? x) (if (hash-has-key? env x) #t (if (prim? x) #t (raise "Using variable that is not initialized.")))]
      [`(quote ,(? datum?)) #t]

      [`(,(? prim?) ,(? (rec/with env)) ...) #t]
      [`(apply ,(? (rec/with env)) ,(? (rec/with env))) #t]
      [`(,fun ,es ...) (if ((fun? env) fun) (match fun
        [`(lambda ,x ,e) (if (= (length x) (length es)) #t (if (< (length x) (length es)) (raise "Too many arguments given") (if (> (length x) (length es)) (raise "Too few arguments given") void)))] [(? prim?) #t] [else
      (if (hash-has-key? env fun) (let ([funs (hash-ref env fun)]) (match funs [`(lambda ,x ,e) (if (= (length x) (length es)) #t (if (< (length x) (length es)) (raise "Too many arguments given") (if (> (length x) (length es)) (raise "Too few arguments given") void)))] [(? prim?) #t] [else (raise "Not a function being applied to.")] )) (raise "Using variable that is not initialized."))]) (raise "Not a function being applied to."))]
      [else #f]
      ))