#lang eopl

;;;;;;;;;;;;;;;; Environment ;;;;;;;;;;;;;;;;

(define-datatype environment environment?
  (empty-env)
  (extend-env (var identifier?) (val expval?) (env environment?))
  (extend-env-rec
   (proc-name identifier?) (bound-var list?)
   (proc-body expression?) (env environment?))
  (extend-env-rec*
   (proc-names list?) (bound-var list?)
   (proc-bodies list?) (env environment?)))

(define empty-env?
  (lambda (env)
    (cases environment env
           (empty-env () #t)
           (else #f))))

(define apply-env
  (lambda (env search-var)
    (cases environment env
           (empty-env () (report-no-binding-found search-var))
           (extend-env (var val next-env)
                       (if (eq? search-var var) val
                           (apply-env next-env search-var)))
           (extend-env-rec
            (p-name b-var p-body p-env)
            (if (eq? search-var p-name)
                (proc-val (proc b-var p-body env))
                (apply-env p-env search-var)))
           (extend-env-rec*
            (p-names b-vars p-bodies p-env)
            (search-extend-env env p-names b-vars p-bodies p-env search-var))
           (else (report-invalid-arg env)))))

(define search-extend-env
  (lambda (env p-names b-vars p-bodies p-env search-var)
    (let ([p-name (car p-names)]
          [b-var (car b-vars)]
          [p-body (car p-bodies)])
      (if (null? p-names)
          (eopl:error 'apply-env "no bound variable found for: ~s" search-var)
          (if (eq? search-var p-name)
              (proc-val (proc b-var p-body env))
              (search-extend-env env (cdr p-names) (cdr b-vars) (cdr p-bodies)
                                 p-env search-var))))))

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

;; FinalAnswer = ExpVal

;; value-of-program : Program -> FinalAnswer
(define value-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1) (value-of/k exp1 (init-env) (end-cont))))))


;;;;;;;;;;;;;;;; Expression ;;;;;;;;;;;;;;;;

(define identifier?
  (lambda (x) (and (not (eq? x 'lambda)) (symbol? x))))

(define-datatype expression expression?
  ;; Expression ::= Number
  (const-exp (num number?))
  ;; Expression ::= -(Expression, Expression)
  (diff-exp (exp1 expression?) (exp2 expression?))
  (zero?-exp (exp1 expression?))
  ;; Expression ::= if Bool-exp then Expression else Expression
  ;(if-exp (exp1 bool-expr?) (exp2 expression?) (exp3 expression?))
  (if-exp (exp1 expression?) (exp2 expression?) (exp3 expression?))
  ;; Expression ::= Identifier
  (var-exp (var identifier?))
  ;; Expression ::= let Identifier = Expression in Expression
  (let-exp (var identifier?) (exp1 expression?) (body expression?))
  ;; Expression ::= proc (Identifier) Expression
  (proc-exp (var identifier?) (body expression?))
  ;; Expression ::= (Expression Expression)
  (call-exp (rator expression?) (rands expression?))
  ;; Expression ::= lecrec Identifier (Identifier) = Expression in Expression
  (letrec-exp (p-name identifier?) (b-var list?)
              (p-body expression?) (letrec-body expression?))
  (let*-exp (vars (list-of identifier?)) (vals (list-of expression?))
            (body expression?))
  ;; Expression ::= try Expression catch (Identifier) Expression
  (try-exp (exp expression?) (var identifier?) (handler-exp expression?))
  ;; Expression ::= raise Expression
  (raise-exp (exp expression?))
  )

;; (define-datatype bool-expr bool-expr?
;;   ;; Expression ::= zero? (Expression)
;;   (zero?-exp (exp1 expression?))
;;   (equal?-exp (exp1 expression?) (exp2 expression?))
;;   (greater?-exp (exp1 expression?) (exp2 expression?))
;;   (less?-exp (exp1 expression?) (exp2 expression?)))

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
  (lambda (var body env)
    (if (and (expression? body)
             (environment? env))
        (list 'procedure var body env)
        (eopl:error 'proc "invalid args"))))

(define proc?
  (lambda (p)
    (and (list? p)
         (eq? (car p) 'procedure))))

(define proc-var
  (lambda (p) (cadr p)))

(define proc-body
  (lambda (p) (caddr p)))

(define proc-env
  (lambda (p) (cadddr p)))


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


;;;;;;;;;;;;;;;;; Continuation ;;;;;;;;;;;;;;;;;

(define-datatype continuation continuation?
  (end-cont)
  (zerol-cont (cont continuation?))
  (let-exp-cont (var identifier?)
                (body expression?)
                (env environment?)
                (cont continuation?))
  (if-test-cont (exp2 expression?)
                (exp3 expression?)
                (env environment?)
                (cont continuation?))
  (diff1-cont (exp2 expression?)
             (env environment?)
             (cont continuation?))
  (diff2-cont (val expval?)
              (env environment?)
              (cont continuation?))
  (rator-cont (rand expression?)
              (env environment?)
              (cont continuation?))
  (rand-cont (val expval?)
             (cont continuation?))
  (let*-exp-cont (vars (list-of identifier?))
                 (rest-vals (list-of expression?))
                 (body expression?)
                 (env environment?)
                 (cont continuation?)))

;; apply-cont : Cont * ExpVal -> FinalAnswer
(define apply-cont
  (lambda (cont val)
    (cases continuation cont
           (end-cont ()
                     (begin (eopl:printf "End of computation.~%")
                            val))
           (zerol-cont (saved-cont)
                       (bool-val (zero? (expval->num val))))
           (let-exp-cont (var body env saved-cont)
                         (value-of/k body
                                     (extend-env var val env) saved-cont))
           (if-test-cont (exp2 exp3 env saved-cont)
                         (if (expval->bool val)
                             (value-of/k exp2 env saved-cont)
                             (value-of/k exp3 env saved-cont)))
           (diff1-cont (exp2 env saved-cont)
                       (value-of/k exp2 env
                                   (diff2-cont val env saved-cont)))
           ;; (apply-cont (diff2-cont val1 env cont) val2)
           (diff2-cont (val1 env saved-cont)
                       (let ((num1 (expval->num val1))
                             (num2 (expval->num val)))
                         (apply-cont saved-cont
                                     (num-val (- num1 num2)))))
           (rator-cont (rand env saved-cont)
                       (value-of/k rand env
                                   (rand-cont val saved-cont)))
           (rand-cont (val1 saved-cont)
                      (let ((rator (expval->proc val1)))
                        (apply-procedure/k rator val saved-cont)))
           (let*-exp-cont (vars rest-vals body env saved-cont)
                          (if (null? rest-vals)
                              (apply-cont
                               (let-exp-cont (car vars) body env saved-cont)
                               val)
                              (value-of/k
                               (car rest-vals) env
                               (let*-exp-cont (cdr vars) (cdr rest-vals) body
                                              (extend-env (car vars) val env)
                                              saved-cont)))))))
;; apply-procedure/k : Proc * ExpVal * Continuation -> FinalAnswer
(define apply-procedure/k
  (lambda (proc val cont)
    (value-of/k (proc-body proc)
                (extend-env (proc-var proc) val (proc-env proc))
                cont)))


;;;;;;;;;;;;;;;;;;; Value-of ;;;;;;;;;;;;;;;;;;;

;; value-of/k : Exp * Env * Cont -> FinalAnswer
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
           (const-exp (num) (apply-cont cont (num-val num)))
           (var-exp (var) (apply-cont cont (apply-env env var)))
           (proc-exp (var body)
                     (apply-cont cont
                                 (proc-val (proc var body env))))
           (letrec-exp (p-name b-var p-body letrec-body)
                       (value-of/k letrec-body
                                   (extend-env-rec p-name b-var p-body env)
                                   cont))
           (zero?-exp (exp1)
                      (value-of/k exp1 env (zerol-cont cont)))
           (let-exp (var exp1 body)
                    (value-of/k exp1 env
                                (let-exp-cont var body env cont)))
           (if-exp (exp1 exp2 exp3)
                   (value-of/k exp1 env
                               (if-test-cont exp2 exp3 env cont)))
           (diff-exp (exp1 exp2)
                     (value-of/k exp1 env
                                 (diff1-cont exp2 env cont)))
           (call-exp (rator rand)
                     (value-of/k rator env
                                 (rator-cont rand env cont)))
           (let*-exp (vars vals body)
                     (value-of/k (car vals) env
                                 (let*-exp-cont vars (cdr vals)
                                                body env cont))))))

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
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression
     ("if" expression "then" expression "else" expression) if-exp)
    (expression (identifier) var-exp)
    (expression
     ("let" identifier "=" expression "in" expression) let-exp)
    (expression
     ("proc" "(" identifier ")" expression) proc-exp)
    (expression ("(" expression expression ")") call-exp)
    (expression
     ("letrec" identifier "(" (separated-list identifier ",") ")" "=" expression
               "in" expression) letrec-exp)
    (expression
     ("let*" (separated-list identifier "=" expression ",") "in" expression)
     let*-exp)
    ))

;; sllgen boilerplate
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))
