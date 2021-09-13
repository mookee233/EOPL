#lang eopl

;; ex 1.15
(define duple-aux
  (lambda (n x lst)
    (if (zero? n)
        lst
        (duple-aux (- n 1) x (cons x lst)))))
(define duple
  (lambda (n x)
    (duple-aux n x '())))

;; ex 1.16
(define rev
  (lambda (lst)
    (cons (cadr lst) (car lst))))
(define invert
  (lambda (lst)
    (map (lambda (x) (rev x))
     lst)))

;; ex 1.17
(define down
  (lambda (lst)
    (if (eq? lst '())
        lst
        (cons (cons (car lst) '())
              (down (cdr lst))))))

;; ex 1.18
(define swapper-aux
  (lambda (s1 s2 lst)
    (if (eq? lst '())
        lst
        (let ([fst (car lst)]
              [rest (swapper-aux s1 s2 (cdr lst))])
          (cond [(eq? fst s1) (cons s2 rest)]
                [(eq? fst s2) (cons s1 rest)]
                [(list? fst) (cons (swapper-aux s1 s2 fst) rest)]
                [else (cons fst rest)])))))
(define swapper
  (lambda (s1 s2 slist)
    (swapper-aux s1 s2 slist)))

;; ex 1.19
(define list-set
  (lambda (lst n x)
    (if (zero? n)
        (cons x (cdr lst))
        (cons (car lst) (list-set (cdr lst) (- n 1) x)))))

;; ex 1.20
(define count-aux
  (lambda (x lst n)
    (cond [(eq? lst '()) n]
          [(eq? (car lst) x)
           (count-aux x (cdr lst) (+ n 1))]
          [(list? (car lst))
           (count-aux x (cdr lst) (+ n (count-aux (car lst) 0)))]
          [else (count-aux x (cdr lst) n)])))
(define count-occurrences
  (lambda (s slist)
    (count-aux s slist 0)))

;; ex 1.21
(define product-aux
  (lambda (s lst)
    (cond [(eq? lst '()) '()]
          [else (cons (cons s (car lst))
                   (product-aux s (cdr lst)))])))
(define product
  (lambda (sos1 sos2)
    (cond [(eq? sos1 '()) '()]
          [else (cons (product-aux (car sos1) sos2 )
                      (product (cdr sos1) sos2))])))

;; ex 1.22
(define filter-aux
  (lambda (pred lst)
    (cond [(eq? lst '()) '()]
          [else (cond [(list? (car lst)) (filter-aux pred (cdr lst))]
                      [(pred (car lst))
                       (cons (car lst) (filter-aux pred (cdr lst)))]
                      [else (filter-aux pred (cdr lst))])])))
(define filter-in
  (lambda (pred lst)
    (filter-aux pred lst)))

;; ex 1.34
(define path
  (lambda (n bst)
    (let ([fst (car bst)])
    (cond [(eq? fst n) '()]
          [(> fst n) (cons "left" (path n (cadr bst)))]
          [else (cons "right" (path n (caddr bst)))]))))
(define bst1
  '(14 (7 () (12 () ()))
       (26 (20 (17 () ())
               ())
           (31 () ()))))
