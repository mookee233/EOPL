#lang eopl

;; ex 2.4
(define empty-stack '())
(define push
  (lambda (s x) (cons s x)))
(define pop
  (lambda (s) (cdr s)))
(define top
  (lambda (s) (car s)))
(define empty-stack?
  (lambda (s) (eq? s '())))

;; example
(define empty-env
  (lambda () (list 'empty-env)))
(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))
(define apply-env
  (lambda (env search-var)
    (cond [(empty-env? env)
           (report-no-binding-found search-var)]
          [(eq? (car env) 'extend-env)
           (let ([next-var (cadr env)]
                 [next-val (caddr env)]
                 [next-env (cadddr env)])
             (if (eq? search-var next-var)
                 next-val
                 (apply-env next-env search-var)))]
          [else (report-invalid-arg env)])))
(define empty-env?
  (lambda (env)
    (eq? (car env) 'empty-env)))
(define report-no-binding-found
  (lambda (var)
    (eopl:error 'apply-env "No binding for: ~s" var)))
(define report-invalid-arg
  (lambda (env)
    (eopl:error 'apply-env "Bad environmrnt: ~s" env)))
