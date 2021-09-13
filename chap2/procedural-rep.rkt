#lang eopl

;; ex 2.12
(define empty-stack
  (lambda (sym)
    (lambda ()
      (eopl:error 'top "Empty stack"))))
(define push
  (lambda (s x)
    (lambda (sym)
      (cond [(eq? sym 'pop)
             (lambda () s)]
            [(eq? sym 'top)
             (lambda () x)]))))
(define pop
  (lambda (s) ((s 'pop))))
(define top
  (lambda (s) ((s 'top))))

;; ex 2.13, 2.14
(define empty-env
  (lambda ()
    (lambda (sym)
      (cond [(eq? sym 'empty-env?) #t]
            [(eq? sym 'apply-env)
             (lambda (search-var)
               (report-no-binding-found search-var))]
            [(eq? sym 'has-binding?)
             (lambda (search-var) #f)]))))
(define extend-env
  (lambda (saved-var saved-val saved-env)
    (lambda (sym)
      (cond [(eq? sym 'empty-env?) #f]
            [(eq? sym 'apply-env)
             (lambda (search-var)
               (if (eq? search-var saved-var)
                   saved-val
                   (apply-env saved-env search-var)))]
            [(eq? sym 'has-binding?)
             (lambda (search-var)
               (if (eq? search-var saved-var)
                   #t
                   (has-binding? saved-env search-var)))]))))
(define apply-env
  (lambda (env search-var)
    ((env 'apply-env) search-var)))
(define empty-env?
  (lambda (env)
    (env 'empty-env?)))
(define has-binding?
  (lambda (env search-var)
    ((env 'has-binding?) search-var)))
(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for: ~s" search-var)))
