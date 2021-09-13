#lang eopl

(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))
(define occurs-free?
  (lambda (search-var exp)
    (cases lc-exp exp
           (var-exp (var) (eq? var search-var))
           (lambda-exp (bound-var body)
                       (and (not (eq? bound-var search-var))
                            (occurs-free? search-var body)))
           (app-exp (rator rand)
                    (or (occurs-free? search-var rator)
                        (occurs-free? search-var rand))))))

;; S-list ::= ({S-exp}*)
;; S-exp ::= Symbol | S-list
(define-datatype s-list s-list?
  (empty-s-list)
  (non-empty-s-list
   (first s-exp?)
   (rest s-list?)))
(define-datatype s-exp s-exp?
  (symbol-s-exp (sym symbol?))
  (s-list-s-exp (slst s-list?)))

;; ex 2.21
;; Env-exp ::= (empty-env)
;;         ::= (extend-env Identifier Scheme-value Env-exp)
(define-datatype env-exp env-exp?
  (empty-env)
  (extend-env
   (saved-var symbol?)
   (saved-val (lambda (x) #t))
   (saved-env env-exp?)))
(define has-binding?
  (lambda (search-var env)
    (cases env-exp env
           (empty-env () #f)
           (extend-env (saved-var saved-val saved-env)
                       (or (eq? saved-var search-var)
                           (has-binding? search-var saved-env))))))

;; ex 2.22
;; Stack ::= (empty-stack)
;;       ::= (non-empty-stack Scheme-value Stack)
(define-datatype stack stack?
  (empty-stack)
  (non-empty-stack
   (saved-val (lambda (x) #t))
   (saved-stack stack?)))
(define push
  (lambda (val stk)
    (non-empty-stack val stk)))
(define pop
  (lambda (stk)
    (cases stack stk
           (empty-stack () (eopl:error 'pop "empty stack"))
           (non-empty-stack (saved-val saved-stack)
                            saved-stack))))
(define top
  (lambda (stk)
    (cases stack stk
           (empty-stack () (eopl:error 'top "empty stack"))
           (non-empty-stack (saved-val saved-stack)
                            saved-val))))
(define empty-stack?
  (lambda (stk)
    (cases stack stk
           (empty-stack () #t)
           (non-empty-stack (saved-val saved-stack) #f))))

;; ex 2.23
(define identifier?
  (lambda (x) (and (not (eq? x 'lambda)) (symbol? x))))

;; ex 2.24
(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))
(define bintree-to-list
  (lambda (bt)
    (cases bintree bt
           (leaf-node (num) (list 'leaf-node num))
           (interior-node (key left right)
                          (list 'interior-node
                                key
                                (bintree-to-list left)
                                (bintree-to-list right))))))

;; ex 2.25
(define max-interior
  (lambda (bt)
    (cases bintree bt
           (leaf-node (num) (eopl:error 'max-interior "empty bintree"))
           (interior-node
            (key left right)
            (let ([ret (sum-interior bt)])
              (car ret))))))
(define sum-interior
  (lambda (bt)
    (cases bintree bt
           (leaf-node (num) (eopl:error))
           (interior-node
            (key left right)
            (let ([l (leaf-node? left)]
                  [r (leaf-node? right)])
              (cond [(and l r)
                     (list key (+ (get-num left) (get-num right)))]
                    [(and l (not r))
                     (let ([r-ret (sum-interior right)])
                       (let ([s (+ (get-num left) (cadr r-ret))])
                         (if (> (cadr r-ret) s)
                             r-ret
                             (list key s))))]
                    [(and (not l) r)
                     (let ([l-ret (sum-interior left)])
                       (let ([s (+ (get-num right) (cadr l-ret))])
                         (if (> (cadr l-ret) s)
                             l-ret
                             (list key s))))]
                    [(and (not l) (not r))
                     (let ([l-ret (sum-interior left)]
                           [r-ret (sum-interior right)])
                       (let ([m (if (> (cadr l-ret) (cadr r-ret))
                                    l-ret r-ret)]
                             [s (+ (cadr l-ret) (cadr r-ret))])
                         (if (> (cadr m) s)
                             m (list key s))))]))))))
(define leaf-node?
  (lambda (bt)
    (cases bintree bt
           (leaf-node (num) #t)
           (interior-node (key left right) #f))))
(define get-num
  (lambda (bt)
    (cases bintree bt
           (leaf-node (num) num)
           (interior-node (key left right)
                          (eopl:error 'get-num "not leaf node")))))
(define bt1
  (interior-node 'foo (leaf-node 2) (leaf-node 3)))
(define bt2
  (interior-node 'bar (leaf-node -1) bt1))
(define bt3
  (interior-node 'baz bt2 (leaf-node 1)))

;; ex 2.26
;; Red-blue-tree ::= Red-blue-subtree
;; Red-blue-subtree ::= (red-node Red-blue-subtree Red-blue-subtree)
;;                  ::= (blue-node {Red-blue-subtree}*)
;;                  ::= (leaf-node Int)
(define-datatype red-blue-tree red-blue-tree?
  (rbt (tree red-blue-subtree?)))
(define-datatype red-blue-subtree red-blue-subtree?
  (red-node
   (left red-blue-subtree?)
   (right red-blue-subtree?))
  (blue-node
   (node blue-nodes?))
  (rb-leaf-node
   (num integer?)))
(define-datatype blue-nodes blue-nodes?
  (empty-blue-nodes)
  (non-empty-blue-nodes
   (first red-blue-subtree?)
   (rest blue-nodes?)))

(define parse-expression
  (lambda (datum)
    (cond [(symbol? datum) (var-exp datum)]
          [(pair? datum)
           (if (eq? (car datum) 'lambda)
               (if (valid-lambda-exp? datum)
                   (lambda-exp (cadr datum)
                               (parse-expression (caddr datum)))
                   (eopl:error 'parse-expression
                               "invalid lambda-exp: ~s" datum))
               (if (valid-app-exp? exp)
                   (app-exp (parse-expression (car datum))
                            (parse-expression (cadr datum)))
                   (eopl:error 'parse-expression
                               "invalid app-exp: ~s" datum)))]
          [else (report-invalid-concrete-syntax datum)])))
(define unparse-lc-exp
  (lambda (exp)
    (cases lc-exp exp
           (var-exp (var) var)
           (lambda-exp (bound-var body)
                       (list 'lambda (list bound-var)
                             (unparse-lc-exp body)))
           (app-exp (rator rand)
                    (list (unparse-lc-exp rator)
                          (unparse-lc-exp rand))))))
(define report-invalid-concrete-syntax
  (lambda (datum)
    (eopl:error 'invalid-concrete-syntax ":~s" datum)))

;; ex 2.27
(define lc1 (list (list 'lambda 'a (list 'a 'b)) 'c))

;; ex 2.28
(define unparse-lc-exp2
  (lambda (exp)
    (cases lc-exp exp
           (var-exp (var) (symbol->string var))
           (lambda-exp (bound-var body)
                       (string-append
                        "proc "
                        (symbol->string bound-var)
                        " => "
                        (unparse-lc-exp2 body)))
           (app-exp (rator rand)
                    (string-append
                     (unparse-lc-exp2 rator)
                     " ("
                     (unparse-lc-exp2 rand)
                     ")")))))
(define lc-exp0
  (app-exp (var-exp 'x) (var-exp 'y)))
(define lc-exp1 (lambda-exp
                 'x (lambda-exp
                     'y (app-exp (lambda-exp
                                  'x (app-exp
                                      (var-exp 'x) (var-exp 'y)))
                                 (var-exp 'x)))))

;; ex 2.30
(define valid-lambda-exp?
  (lambda (exp)
    (eq? (length exp) 3)))
(define valid-app-exp?
  (lambda (exp)
    (eq? (length exp) 2)))
