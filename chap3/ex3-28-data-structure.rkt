#lang eopl

;;;;;;;;;;;;;;;; Environment ;;;;;;;;;;;;;;;;

(define empty-env (list 'empty-env))
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
(define environment?
  (lambda (env)
    (and (list? env)
         (or (eq? (car env) 'extend-env) (empty-env? env)))))
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
       empty-env)))))


;;;;;;;;;;;;;;;;;;;;;; List ;;;;;;;;;;;;;;;;;;;;;;

(define foldl
  (lambda (f xs lst)
    (cond [(null? lst) xs]
          [else (foldl f (f (car lst) xs) (cdr lst))])))

;;;;;;;;;;;;;;;;;;;;;; Set ;;;;;;;;;;;;;;;;;;;;;;
(define insert
  (lambda (elem lst)
    (foldl (lambda (x xs)
             (if (eq? x elem)
                 xs
                 (cons x xs)))
           (list elem) lst)))

(define union
  (lambda (lst1 lst2)
    (foldl (lambda (x xs) (insert x xs)) lst2 lst1)))



;;;;;;;;;;;;;;;;;;; Interface ;;;;;;;;;;;;;;;;;;;

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
  (unpack-exp (vars list?) (exp1 expression?) (body expression?))
  ;; Expression ::= proc (Identifier) Expression
  (proc-exp (vars list?) (body expression?))
  ;; Expression ::= (Expression Expression)
  (call-exp (rator expression?) (rands list?)))

(define-datatype bool-expr bool-expr?
  ;; Expression ::= zero? (Expression)
  (zero?-exp (exp1 expression?))
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

(define proc
  (lambda (vars body)
    (if (expression? body)
        (if (null? (cdr vars))
            (list 'procedure (car vars) body)
            (list 'procedure (car vars)
                  (proc-exp (cdr vars) body)))
        (eopl:error 'proc "invalid args"))))

(define proc?
  (lambda (p)
    (and (list? p)
         (eq? (car p) 'procedure))))

(define proc-var
  (lambda (p) (cadr p)))

(define proc-body
  (lambda (p) (caddr p)))

(define apply-proc
  (lambda (p vals env)
    (let ([val (value-of (car vals) env)])
      (let ([new-body (value-of (replace-ocurrence
                                 (proc-var p) val (proc-body p)) env)])
        (if (null? (cdr vals))
            new-body
            (apply-proc (expval->proc new-body) (cdr vals) env))))))

(define replace-ocurrence
  (lambda (var val body)
    (cases expression body
           (const-exp (num) (const-exp num))
           (var-exp (var1)
                    (if (eq? var var1)
                        (cases expval val
                               (num-val (num) (const-exp num))
                               (bool-val (bool) (bool-exp bool))
                               (list-val (lst) (lst-exp lst))
                               (proc-val (procedure)
                                         (proc-exp (proc-var procedure)
                                                   (proc-val procedure)
                                                   (proc-body procedure))))
                        body))
           (diff-exp (exp1 exp2)
                     (diff-exp (replace-ocurrence var val exp1)
                               (replace-ocurrence var val exp2)))
           (if-exp (exp1 exp2 exp3)
                   (if-exp (replace-ocurrence var val exp1)
                           (replace-ocurrence var val exp2)
                           (replace-ocurrence var val exp3)))
           (let-exp (var1 exp1 body)
                    (let-exp var1 (replace-ocurrence var val exp1)
                             (replace-ocurrence var val body)))
           (let*-exp (vars exps body)
                     (let*-exp (vars
                                (map (lambda (x)
                                       (replace-ocurrence var val x)) exps)
                                (replace-ocurrence var val body))))
           (minus-exp (exp1) (minus-exp (replace-ocurrence var val exp1)))
           (mult-exp (exp1 exp2)
                     (mult-exp (replace-ocurrence var val exp1)
                               (replace-ocurrence var val exp2)))
           (add-exp (exp1 exp2)
                    (add-exp (replace-ocurrence var val exp1)
                             (replace-ocurrence var val exp2)))
           (quot-exp (exp1 exp2)
                    (quot-exp (replace-ocurrence var val exp1)
                              (replace-ocurrence var val exp2)))
           (lst-exp (lst) body)
           (cons-exp (exp1 exp2)
                     (cons-exp (replace-ocurrence var val exp1)
                               (replace-ocurrence var val exp2)))
           (car-exp (exp1) (car-exp (replace-ocurrence var val exp1)))
           (cdr-exp (exp1) (cdr-exp (replace-ocurrence var val exp1)))
           (null?-exp (exp1) (null?-exp (replace-ocurrence var val exp1)))
           (emptylist-exp () body)
           (list-exp (exps)
                     (list-exp (map (lambda (x) (replace-ocurrence var val x))
                                    exps)))
           (bool-exp (exp1) (replace-ocurrence var val exp1))
           (cond-exp (exps1 exps2)
                     (cond-exp (replace-ocurrence
                                (map (lambda (x) (replace-ocurrence var val x))
                                     exps1)
                                (map (lambda (x) (replace-ocurrence var val x))
                                     exps2))))
           (print-exp (arg) body)
           (unpack-exp (vars exp1 body)
                       (unpack-exp (replace-ocurrence var val exp1)
                                   (replace-ocurrence var val body)))
           (proc-exp (vars body)
                     (proc-exp vars (replace-ocurrence var val body)))
           (call-exp (rator rand)
                     (call-exp (replace-ocurrence var val rator)
                               (replace-ocurrence var val rand))))))


;; ExpVal = Int + Bool + List + Proc
(define-datatype expval expval?
  (num-val (num number?))
  (bool-val (bool boolean?))
  (list-val (lst lst?))
  (proc-val (procedure proc?)))

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

;; expval->proc : ExpVal -> Proc
(define expval->proc
  (lambda (val)
    (cases expval val
           (proc-val (proc) proc)
           (elrp (report-expval-extractor-error 'proc val)))))


;;;;;;;;;;;;;;;;;;; Value-of ;;;;;;;;;;;;;;;;;;;

;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
           (const-exp (num) (num-val num))
           (var-exp (var) (apply-env env var))
           (diff-exp (exp1 exp2) (eval-arith exp1 exp2 - env))
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
           (unpack-exp (vars exp1 body) (eval-unpack vars exp1 body env))
           (proc-exp (vars body) (eval-proc vars body))
           (call-exp (rator rand) (eval-call rator rand env)))))

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
           (zero?-exp (exp1) (eval-zero? exp1 env))
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

(define eval-proc
  (lambda (vars body)
      (proc-val (proc vars body))))

(define eval-call
  (lambda (rator rand env)
    (let ([p (expval->proc (value-of rator env))])
      (apply-proc p rand env))))

(define report-empty-list-error
  (lambda (val)
    (eopl:error 'empty-list-error "list:~s" val)))

;; ex 3.23
(define ex3.23
  "let func = proc (f) proc (n)
              if zero?(n) then 1
              else if =(1, n) then 1 else
              *(n, (f f -(n, 1))) in
       let fact = proc (x) (func func x) in
           (fact 3)")

;; give up 3.26

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
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("+" "(" expression "," expression ")") add-exp)
    (expression ("*" "(" expression "," expression ")") mult-exp)
    (expression ("/" "(" expression "," expression ")") quot-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
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
    (expression ("car" "(" expression ")") car-exp)
    (expression ("cdr" "(" expression ")") cdr-exp)
    (expression ("null?" "(" expression ")") null?-exp)
    (expression
     ("cond" (arbno expression "==>" expression) "end") cond-exp)
    (expression
     ("list" "(" (separated-list expression ",") ")") list-exp)
    (expression ("print" expression) print-exp)
    (expression
     ("unpack" (arbno identifier) "=" expression "in" expression) unpack-exp)
    (expression
     ("proc" "(" (separated-list identifier ",") ")" expression) proc-exp)
    (expression ("(" expression (arbno expression) ")") call-exp)
    ))

;; sllgen boilerplate
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))
