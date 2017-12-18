#lang racket

(provide top-level)
(require "utils.rkt")

(define (top-level e)

  (define (T-qq qq [depth 0])
    (match qq
      [(list 'unquote exp)
       (if (= depth 0)
           (T exp)
           (list 'list ''unquote (T-qq exp (sub1 depth))))]
      [`(quasiquote ,qq1)
       `(list 'quasiquote ,(T-qq qq1 (add1 depth)))]
      [`(,qq1 . ,qrest)
       `(cons ,(T-qq qq1 depth) ,(T-qq qrest depth))]
      [else `(quote ,qq)]))
  (define (T-pattern-guard k pat)
    (match pat ; generate a boolean expression for "does k match pat"
      [``,qq-pat
       (T-pattern-guard k (T-qq qq-pat))]
      [`(cons ,p0 ,p1)
       (define k0 (gensym 'pk))
       (define k1 (gensym 'pk))
       `(and (cons? ,k)
             (let ([,k0 (car ,k)]
                   [,k1 (cdr ,k)])
               (and ,(T-pattern-guard k0 p0)
                    ,(T-pattern-guard k1 p1))))]
      [`(? ,pred ,p0)
       `(and (,(T pred) ,k) ,(T-pattern-guard k p0))]
      [(? symbol? x)
       ''#t]
      [`(quote ,(? datum? d))
       `(equal? ',d ,k)]
      [(? datum? d)
       `(equal? ',d ,k)]))
  (define (T-pattern-bindings k pat thunk)
    (match pat ; generate bindings for k, which matches pat, around (thunk)
      [``,qq-pat
       (T-pattern-bindings k (T-qq qq-pat) thunk)]
      [`(cons ,p0 ,p1)
       (define k0 (gensym 'pk))
       (define k1 (gensym 'pk))
       `(let ([,k0 (car ,k)]
              [,k1 (cdr ,k)])
          ,(T-pattern-bindings k0 p0 (lambda () (T-pattern-bindings k1 p1 thunk))))]
      [`(? ,pred ,p0)
       (T-pattern-bindings k p0 thunk)]
      [(? symbol? x)
       `(let ([,x ,k])
          ,(thunk))]
      [`(quote ,(? datum? d))
       (thunk)]
      [(? datum? d)
       (thunk)]))
  (define (extract-defs beg)
    (define not-define?
      (match-lambda [`(define . _) #f] [else #t]))
    (match beg
      [`(begin (define ,(? symbol? x) ,body) ,es ...)
       (match-define (cons defs e+) (extract-defs `(begin . ,es)))
       (cons (cons `[,x ,(T body)] defs) e+)]
      [`(begin
          (define (,(? symbol? f) ,(? symbol? xs) ... ,(list def-xs def-es) ...) ,bodies ...)
          ,es ...)
       (match-define (cons defs e+) (extract-defs `(begin . ,es)))
       (cons (cons `[,f ,(T `(lambda (,@xs ,@(map list def-xs def-es)) . ,bodies))] defs) e+)]
      [`(begin
          (define (,(? symbol? f) ,(? symbol? xs) ... . ,(? symbol? args)) ,bodies ...)
          ,es ...)
       (match-define (cons defs e+) (extract-defs `(begin . ,es)))
       (cons (cons `[,f ,(T `(lambda (,@xs . ,args) . ,bodies))] defs) e+)]
      [`(begin (begin ,e0s ...) ,e1s ...)
       (extract-defs `(begin ,@e0s ,@e1s))]
      [`(begin)
       (cons '() '(begin))]
      [`(begin ,(? not-define? nde) ,es ...)
       (let ([nde+ (T nde)])
         (if (null? es)
             (cons '() nde+)
             (match-let ([(cons defs e+) (extract-defs `(begin . ,es))])
               (cons (cons `(,(gensym 'tmp) ,nde+) defs) e+))))]))
  (define (T-lambda-with-default-params xs def-xs def-es body+)
    (define args (gensym 'args))
    (define bindings
      (let lp ([dxs def-xs] [des+ (map T def-es)]
                            [args args] [bindings '()])
        (if (null? dxs)
            bindings
            (let* ([pair (gensym 'pair)]
                   [new-args (gensym 'args)]
                   [new-bindings (list
                                  `(,pair (if (null? ,args)
                                              (cons ,(car des+) '())
                                              ,args))
                                  `(,(car dxs) (car ,pair))
                                  `(,new-args (cdr ,pair)))])
              (lp (cdr dxs) (cdr des+) new-args (append bindings new-bindings))))))
    (if (null? def-xs)
        `(lambda ,xs ,body+)
        `(lambda (,@xs . ,args) (let* ,bindings ,body+))))
  (define (T e)
    (match e
      [`(begin ,es ...)
       (match-define (cons defs e+) (extract-defs e))
       `(letrec* ,defs ,e+)]
      [`(let ,loop ([,xs ,rhss] ...) ,es ...) 
       `(let ,loop ,(map list xs (map T rhss)) ,(T `(begin . ,es)))]
      [`(cond [,guards ,ess ...] ...)
       `(cond ,@(map (lambda (guard es) (if (null? es) `(,(T guard)) `[,(T guard) ,(T `(begin . ,es))])) guards ess))]
      [`(case ,key [,guards ,ess ...] ...)
       `(case ,(T key) ,@(map (lambda (guard es) `[,guard ,(T `(begin . ,es))]) guards ess))]
      [`(guard (,x [,guards ,ess ...] ...) ,bodies ...)
       `(guard (,x ,@(map (lambda (guard es) (if (null? es) `(,(T guard)) `(,(T guard) ,(T `(begin . ,es))))) guards ess))
               ,(T `(begin . ,bodies)))]
      [`(lambda ,param ,es ...)
       (match param
         [(? symbol? x)
          `(lambda ,x ,(T `(begin . ,es)))]
         [`(,(? symbol? xs) ... . ,(? symbol? args))
          `(lambda (,@xs . ,args) ,(T `(begin . ,es)))]
         [`(,(? symbol? xs) ... ,(list def-xs def-es) ...)
          (T-lambda-with-default-params xs def-xs def-es (T `(begin . ,es)))]
         [else (error "unexpected arg list")])]
      [`(match ,(? symbol? k))
       `(raise '"Match failed")]
      [`(match ,(? symbol? k) [,pat ,es ...] . ,rest)
       `(if ,(T (T-pattern-guard k pat))
            ,(T (T-pattern-bindings k pat (lambda () `(begin . ,es))))
            ,(T `(match ,k . ,rest)))]
      [`(match ,key . ,rest)
       (define k (gensym 'match-key))
       `(let ([,k ,(T key)]) ,(T `(match ,k . ,rest)))]
      ; Handle various forms with implicit begins at once
      [`(,(and tag (or 'letrec 'letrec* 'let 'let* 'when 'unless)) ,stuff ,es ...)
       ;(define top-leveled (match stuff [ `([,xs ,exps] ...) (define bindings (map (lambda (h1 h2) `[,h1 ,(T h2)]) xs exps)) `(,tag ,bindings ,(T `(begin . ,es)))]))
       `(,tag ,(T stuff) ,(T `(begin . ,es)))
       ]
      [``,quasi (T-qq quasi)]
      [`',d `',d]
      [(? symbol? x) x]
      [`(,es ...) (map T es)]
      [(? datum? d) `',d]))

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
            (andmap (rec/with (ext env xs es))
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
       ((rec/with (ext env (list x) (map void (list x)))) e0)]
      [`(lambda ,(? improper-args? args) ,e0)
       (and (no-duplicates? (flatten args))
            ((rec/with (ext env (flatten args) (map void (flatten args)))) e0))]
      [`(delay ,(? (rec/with env))) #t]
      [`(force ,(? (rec/with env))) #t]
      [`(guard (,(? symbol? x) ,clauses ...) ,(? (rec/with env)))
       (correct? `(cond . ,clauses) (ext env (list x) (map void (list x))))]
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
  
  (define final (T e))
  (if (correct? final (hash)) final void))
