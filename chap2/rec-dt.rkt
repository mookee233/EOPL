#lang eopl

;; ex 2.15
;; constructors
(define var-exp
  (lambda (var)
    (list 'var-exp var)))
(define lambda-exp
  (lambda (var body)
    (list 'lambda-exp var body)))
(define app-exp
  (lambda (rator rand)
    (list 'app-exp rator rand)))

;; predicates
(define var-exp?
  (lambda (l-exp)
    (eq? (car l-exp) 'var-exp)))
(define lambda-exp?
  (lambda (l-exp)
    (eq? (car l-exp) 'lambda-exp)))
(define app-exp?
  (lambda (l-exp)
    (eq? (car l-exp) 'app-exp)))

;; extractors
(define var-exp->var
  (lambda (l-exp) (cadr l-exp)))
(define lambda-exp->bound-var
  (lambda (l-exp) (cadr l-exp)))
(define lambda-exp->body
  (lambda (l-exp) (caddr l-exp)))
(define app-exp->rator
  (lambda (l-exp) (cadr l-exp)))
(define app-exp->rand
  (lambda (l-exp) (caddr l-exp)))

(define occurs-free?
  (lambda (search-var exp)
    (cond [(var-exp? exp)
           (eq? search-var (var-exp->var exp))]
          [(lambda-exp? exp)
           (and (not (eq? search-var (lambda-exp->bound-var exp)))
                (occurs-free? search-var (lambda-exp->body exp)))]
          [(app-exp? exp)
           (or (occurs-free? search-var (app-exp->rator exp))
               (occurs-free? search-var (app-exp->rand exp)))])))

(define success 0)
(define check-expect
  (lambda (expr expected)
    (if (equal? expr expected)
        (set! success (+ success 1))
        (eopl:error 'check-expect "expected ~s given ~s"
                    expected expr))))

(define exp0 (var-exp 'x))
(check-expect (occurs-free? 'x exp0) #t)
(check-expect (occurs-free? 'x (lambda-exp 'x (var-exp 'x))) #f)
(check-expect (occurs-free? 'y (lambda-exp 'x (var-exp 'y))) #t)
(define exp1
  (app-exp
   (lambda-exp 'x (var-exp 'x))
   (var-exp 'z)))
(define exp2
  (app-exp (var-exp 'x) (var-exp 'y)))
(check-expect (occurs-free? 'x exp1) #f)
(check-expect (occurs-free? 'z exp1) #t)

;; NodeInSequence ::= (Int Listof(Int) Listof(Int))
(define number->sequence
  (lambda (x) (list x '() '())))
(define current-element
  (lambda (seq) (car seq)))
(define move-to-left
  (lambda (seq)
    (let ([left (cadr seq)]
          [right (caddr seq)]
          [elem (car seq)])
      (list (car left) (cdr left) (cons elem right)))))
(define move-to-right
  (lambda (seq)
    (let ([left (cadr seq)]
          [right (caddr seq)]
          [elem (car seq)])
      (list (car right) (cons elem left) (cdr right)))))
(define insert-to-left
  (lambda (new-elem seq)
    (let ([left (cadr seq)]
          [right (caddr seq)]
          [elem (car seq)])
      (list elem (cons new-elem left) right))))
(define insert-to-right
  (lambda (new-elem seq)
    (let ([left (cadr seq)]
          [right (caddr seq)]
          [elem (car seq)])
      (list elem left (cons new-elem right)))))
(define at-left-end?
  (lambda (seq)
    (eq? (cadr seq) '())))
(define at-right-end?
  (lambda (seq)
    (eq? (caddr seq) '())))

(define seq0
  (insert-to-right
   11 (insert-to-left
       13 (insert-to-left
           10 (insert-to-right
               5 (number->sequence 7))))))
(check-expect (move-to-left seq0)
              (list 13 (list 10) (list 7 11 5)))

;; Bintree ::= () | (Int Bintree Bintree)
(define number->bintree
  (lambda (x) (list x '() '())))
(define bt-current-element
  (lambda (bt) (car bt)))
(define move-to-left-son
  (lambda (bt) (cadr bt)))
(define move-to-right-son
  (lambda (bt) (caddr bt)))
(define at-leaf?
  (lambda (bt) (eq? bt '())))
(define bt-insert-to-left
  (lambda (new-elem bt)
    (let ([left (move-to-left-son bt)]
          [right (move-to-right-son bt)]
          [elem (bt-current-element bt)])
      (if (at-leaf? left)
          (list elem (number->bintree new-elem) right)
          (list elem (bt-insert-to-left new-elem left) right)))))
(define bt-insert-to-right
  (lambda (new-elem bt)
    (let ([left (move-to-left-son bt)]
          [right (move-to-right-son bt)]
          [elem (bt-current-element bt)])
      (if (at-leaf? right)
          (list elem left (number->bintree new-elem))
          (list elem left (bt-insert-to-right new-elem right))))))

(define t1
  (bt-insert-to-right
   14 (bt-insert-to-left
       12 (number->bintree 13))))
(check-expect (move-to-left-son t1) (list 12 '() '()))
(check-expect (bt-insert-to-left
               15 t1)
              (list 13 (list 12
                             (list 15 '() '())
                             '())
                    (list 14 '() '())))

;; ex 2.20
;; BintreeSequence ::= (Bintree BintreeSeq)
;; reversable binary tree, storing parent tree as second elem
(define number->bt-sequence
  (lambda (x) (list x '() (list '() '()))))
(define btseq-current-element
  (lambda (btseq) (car btseq)))
(define btseq-move-to-left-son
  (lambda (btseq)
    (let ([bt (car btseq)])
      (let ([left (cadr bt)])
        (list left btseq)))))
(define btseq-move-to-right-son
  (lambda (btseq)
    (let ([bt (car btseq)])
      (let ([right (caddr bt)])
        (list right btseq)))))
(define btseq-at-leaf?
  (lambda (btseq)
    (at-leaf? (btseq-current-element btseq))))
(define btseq-insert-to-left
  (lambda (new-elem btseq)
    (let ([bt (car btseq)]
          [parent (move-up btseq)])
      (list (bt-insert-to-left new-elem bt) parent))))
(define btseq-insert-to-right
  (lambda (new-elem btseq)
    (let ([bt (car btseq)]
          [parent (move-up btseq)])
      (list (bt-insert-to-right new-elem bt) parent))))
(define move-up
  (lambda (btseq) (cadr btseq)))
(define btseq-at-root?
  (lambda (btseq) (eq? (move-up btseq) '())))

(define btseq1 (list t1 '()))
(check-expect (btseq-at-root? btseq1) #t)
(check-expect (btseq-at-leaf? btseq1) #f)
(define btseq2 (btseq-move-to-right-son
                (btseq-move-to-left-son btseq1)))
(check-expect (btseq-at-leaf? btseq2) #t)
(check-expect (move-up (move-up btseq2)) btseq1)

(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

(eopl:printf "run ~s tests: all success!" success)
