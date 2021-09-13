#lang eopl

;;;;;;;;;;;;;;;; Environment ;;;;;;;;;;;;;;;;

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

(define init-env
  (lambda ()
    (extend-env
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env))))))


;;;;;;;;;;;;;;;;;;; Program ;;;;;;;;;;;;;;;;;;;

(define-datatype program program?
  (a-program
   (exp1 expression?)))

(define report-expval-extractor-error
  (lambda (x val)
    (eopl:error x "expval-extractor-error:~s" val)))

;; run : String -> ExpVal
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

;; value-of-program : Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1) (value-of exp1 (init-env))))))


;;;;;;;;;;;;;;;; Expression ;;;;;;;;;;;;;;;;

(define identifier?
  (lambda (x) (and (not (eq? x 'lambda)) (symbol? x))))

(define-datatype expression expression?
  ;; Expression ::= Number
  (const-exp (num number?))
  ;; Expression ::= -(Expression, Expression)
  (diff-exp (exp1 expression?) (exp2 expression?))
  (add-exp (exp1 expression?) (exp2 expression?))
  (mult-exp (exp1 expression?) (exp2 expression?))
  (quot-exp (exp1 expression?) (exp2 expression?))
  (minus-exp (exp expression?))
  ;; Expression ::= zero? (Expression)
  (zero?-exp (exp1 expression?))
  ;; Expression ::= if Bool-exp then Expression else Expression
  (if-exp (exp1 bool-expr?) (exp2 expression?) (exp3 expression?))
  ;; Expression ::= Identifier
  (var-exp (var identifier?))
  ;; Expression ::= let Identifier = Expression in Expression
  (let-exp (var identifier?) (exp1 expression?) (body expression?))
  ;; Expression ::= let* {Identifier = Expression}* in Expression
  (let*-exp (vars list?) (exps list?) (body expression?))
  (cons-exp (exp1 expression?) (exp2 expression?))
  (car-exp (exp1 expression?))
  (cdr-exp (exp1 expression?))
  (null?-exp (exp1 expression?))
  (emptylist-exp)
  (lst-exp (lst expval?))
  (list-exp (exps list?))
  (bool-exp (exp1 bool-expr?))
  ;; Expression ::= cond {Expression ==> Expression}* end
  (cond-exp (exps1 list?) (exps2 list?))
  (print-exp (arg (lambda (x) #t)))
  (unpack-exp (vars list?) (exp1 expression?) (body expression?)))

(define-datatype bool-expr bool-expr?
  (equal?-exp (exp1 expression?) (exp2 expression?))
  (greater?-exp (exp1 expression?) (exp2 expression?))
  (less?-exp (exp1 expression?) (exp2 expression?)))

(define-datatype cond-cases cond-cases?
  (empty-cond-cases)
  (non-empty-cond-cases
   (case1 cond-case?) (rest-cases cond-cases?)))

(define-datatype cond-case cond-case?
  (a-cond-case (exp1 expression?) (exp2 expression?)))

(define-datatype lst lst?
  (emptylist)
  (non-emptylist
   (car expval?)
   (cdr expval?)))

(define-datatype expval expval?
  (num-val (num number?))
  (bool-val (bool boolean?))
  (list-val (lst lst?)))

;; expval->num : ExpVal -> Int
(define expval->num
  (lambda (val)
    (cases expval val
           (num-val (num) num)
           (else (report-expval-extractor-error 'num val)))))
;; expval->bool : ExpVal -> Bool
(define expval->bool
  (lambda (val)
    (cases expval val
           (bool-val (bool) bool)
           (else (report-expval-extractor-error 'bool val)))))
;; expval->lst : ExpVal -> Lst
(define expval->lst
  (lambda (val)
    (cases expval val
           (list-val (lst) lst)
           (else (report-expval-extractor-error 'lst val)))))


;;;;;;;;;;;;;;;;;;; Value-of ;;;;;;;;;;;;;;;;;;;

;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
           (const-exp (num) (num-val num))
           (var-exp (var) (apply-env env var))
           (diff-exp (exp1 exp2) (eval-arith exp1 exp2 - env))
           (zero?-exp (exp1) (eval-zero? exp1 env))
           (if-exp (exp1 exp2 exp3) (eval-if exp1 exp2 exp3 env))
           (let-exp (var exp1 body) (eval-let var exp1 body env))
           (let*-exp (vars exps body) (eval-let* vars exps body env))
           (minus-exp (exp1) (eval-minus exp1 env))
           (mult-exp (exp1 exp2) (eval-arith exp1 exp2 * env))
           (add-exp (exp1 exp2) (eval-arith exp1 exp2 + env))
           (quot-exp (exp1 exp2) (eval-arith exp1 exp2 / env))
           (lst-exp (lst) (list-val lst))
           (cons-exp (exp1 exp2) (eval-cons exp1 exp2 env))
           (car-exp (exp1) (eval-car exp1 env))
           (cdr-exp (exp1) (eval-cdr exp1 env))
           (null?-exp (exp1) (eval-null? exp1 env))
           (emptylist-exp () (list-val (emptylist)))
           (list-exp (exps) (eval-list exps env))
           (bool-exp (exp1) (eval-bool exp1 env))
           (cond-exp (exps1 exps2) (eval-cond exps1 exps2 env))
           (print-exp (arg) (begin (eopl:printf "~s" arg) (num-val 1)))
           (unpack-exp (vars exp1 body) (eval-unpack vars exp1 body env)))))

(define eval-zero?
  (lambda (exp1 env)
    (let ([val (value-of exp1 env)])
      (let ([num (expval->num val)])
        (if (zero? num)
            (bool-val #t)
            (bool-val #f))))))

(define eval-minus
  (lambda (exp1 env)
    (let ([val (expval->num (value-of exp1 env))])
      (num-val (- 0 val)))))

(define eval-arith
  (lambda (exp1 exp2 f env)
    (num-val (f (expval->num (value-of exp1 env))
                (expval->num (value-of exp2 env))))))

(define eval-comp
  (lambda (exp1 exp2 f env)
    (let ([val1 (expval->num (value-of exp1 env))]
          [val2 (expval->num (value-of exp2 env))])
      (if (f val1 val2)
          (bool-val #t)
          (bool-val #f)))))

(define eval-bool
  (lambda (exp1 env)
    (cases bool-expr exp1
           (equal?-exp (exp1 exp2) (eval-comp exp1 exp2 = env))
           (greater?-exp (exp1 exp2) (eval-comp exp1 exp2 > env))
           (less?-exp (exp1 exp2) (eval-comp exp1 exp2 <)))))

(define eval-if
  (lambda (exp1 exp2 exp3 env)
    (let ([bool (expval->bool (eval-bool exp1 env))])
      (if bool
          (value-of exp2 env)
          (value-of exp3 env)))))

(define eval-let
  (lambda (var exp1 body env)
    (let ([val (value-of exp1 env)])
      (let ([new-env (extend-env var val env)])
        (value-of body new-env)))))

(define eval-let*
  (lambda (vars exps body env)
    (if (null? vars)
        (value-of body env)
        (let ([val (value-of (car exps) env)])
          (let ([new-env (extend-env (car vars) val env)])
            (eval-let* (cdr vars) (cdr exps) body new-env))))))

(define eval-cons
  (lambda (exp1 exp2 env)
    (list-val (non-emptylist (value-of exp1 env)
                             (value-of exp2 env)))))

(define eval-car
  (lambda (exp1 env)
    (let ([val1 (expval->lst (value-of exp1 env))])
      (cases lst val1
             (emptylist () (report-empty-list-error val1))
             (non-emptylist (car cdr) car)))))

(define eval-cdr
  (lambda (exp1 env)
    (let ([val1 (expval->lst (value-of exp1 env))])
      (cases lst val1
             (emptylist () (report-empty-list-error val1))
             (non-emptylist (car cdr) cdr)))))

(define eval-null?
  (lambda (exp1 env)
    (let ([val1 (expval->lst (value-of exp1 env))])
      (cases lst val1
             (emptylist () #t)
             (else #f)))))

(define eval-list
  (lambda (exps env)
    (if (null? exps)
        (list-val (emptylist))
        (let ([val1 (value-of (car exps) env)])
          (list-val (non-emptylist val1 (eval-list (cdr exps) env)))))))

(define eval-cond
  (lambda (exps1 exps2 env)
    (if (null? exps1)
        (eopl:error 'cond-exp "cannot find matching condition")
        (let ([val1 (expval->bool (eval-bool (car exps1) env))])
          (if val1
              (value-of (car exps2) env)
              (eval-cond (cdr exps1) (cdr exps2) env))))))

(define eval-unpack
  (lambda (vars exp body env)
    (cases expression exp
           (cons-exp
            (exp1 exp2)
            (if (null? vars)
                (eopl:error 'eval-unpack "inconsistent length")
                (let ([val (value-of exp1 env)])
                  (let ([new-env (extend-env (car vars) val env)])
                    (eval-unpack (cdr vars) exp2 body new-env)))))
           (emptylist-exp
            () (if (null? vars)
                   (value-of body env)
                   (eopl:error 'eval-unpack "inconsistent length")))
           (else (eopl:error 'eval-unpack "not a list:~s" exp)))))

(define report-empty-list-error
  (lambda (val)
    (eopl:error 'empty-list-error "list:~s" val)))


;;;;;;;;;;;;;;;; Scanner & Parser ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

(define the-grammar
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression
     ("-" "(" expression "," expression ")") diff-exp)
    (expression
     ("+" "(" expression "," expression ")") add-exp)
    (expression
     ("*" "(" expression "," expression ")") mult-exp)
    (expression
     ("/" "(" expression "," expression ")") quot-exp)
    (expression
     ("zero?" "(" expression ")") zero?-exp)
    (expression
     ("if" expression "then" expression "else" expression) if-exp)
    (expression (identifier) var-exp)
    (expression
     ("let" identifier "=" expression "in" expression) let-exp)
    (expression
     ("let*" (arbno identifier "=" expression) "in" expression) let*-exp)
    (expression ("=" "(" expression "," expression ")") equal?-exp)
    (expression (">" "(" expression "," expression ")") greater?-exp)
    (expression ("<" "(" expression "," expression ")") less?-exp)
    (expression
     ("cons" "(" expression "," expression ")") cons-exp)
    (expression ("emptylist") emptylist-exp)
    (expression
     ("car" "(" expression ")") car-exp)
    (expression
     ("cdr" "(" expression ")") cdr-exp)
    (expression
     ("null?" "(" expression ")") null?-exp)
    (expression
     ("cond" (arbno expression "==>" expression) "end") cond-exp)
    (expression
     ("list" "(" (separated-list expression ",") ")") list-exp)
    (expression ("print" expression) print-exp)
    (expression
     ("unpack" (arbno identifier) "=" expression "in" expression) unpack-exp)
    ))

;; sllgen boilerplate
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))
