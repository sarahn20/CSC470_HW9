; Enviroment Initializers
(define empty-env
  (lambda () (list 'empty-env)))
(define extend-env
  (lambda (name value env)
    (list 'extend-env name value env)))
(define extend-env*
  (lambda (lon lov env)
    (cond
      ((null? lon) env)
      (else (extend-env* (cdr lon) (cdr lov) (extend-env (car lon) (car lov) env))))))
(define get-name
  (lambda (env) (cadr env)))
(define get-value
  (lambda (env) (caddr env)))
(define get-env
  (lambda (env) (cadddr env)))
(define empty-env?
  (lambda (env) (eq? 'empty-env (car env))))
(define apply-env
  (lambda (var-name env)
    (cond
      ((empty-env? env) #f)
      (else
       (if (eq? var-name (get-name env))
           (get-value env)
           (apply-env var-name (get-env env)))))))
(define has-binding?
  (lambda (var-name env)
    (not (eq? (apply-env var-name env) #f))))
(define env (extend-env 'a 5 (extend-env 'b 7 (empty-env))))
env
(extend-env* '(c d e) '(1 2 3) env)
; Grammar Constructors
(define var-exp
  (lambda (s)
    (list 'var-exp s)))
(define lambda-exp
  (lambda (s lc-exp)
    (list 'lambda-exp s lc-exp)))
(define app-exp
  (lambda (lambda-exp param-value)
    (list 'app-exp lambda-exp param-value)))
; Grammar Extractors
(define lc-exp->type
  (lambda (lc-exp)
    (car lc-exp)))
(define var-exp->var-name
  (lambda (var-exp)
    (cadr var-exp)))
(define lambda-exp->parameter-name
  (lambda (lambda-exp)
    (cadr lambda-exp)))
(define lambda-exp->body
  (lambda (lambda-exp)
    (caddr lambda-exp)))
(define app-exp->lambda-exp
  (lambda (app-exp)
    (cadr app-exp)))
(define app-exp->parameter-input
  (lambda (app-exp)
    (caddr app-exp)))
; Grammar Predicates
(define var-exp?
  (lambda (lc-exp)
    (eq? (lc-exp->type lc-exp) 'var-exp)))
(define app-exp?
  (lambda (lc-exp)
    (eq? (lc-exp->type lc-exp) 'app-exp)))
(define lambda-exp?
  (lambda (lc-exp)
    (eq? (lc-exp->type lc-exp) 'lambda-exp)))
; Parse/Unparse
; (func gets (x) does x) ;lambda-exp
; (Run (func (x) x) ‘with parameter);app-exp
; (Get-Value ‘A) var-exp
;c0d3 Grammar Extractors
(define c0d3->type
  (lambda (c0d3)
    (car c0d3)))
(define get-value->value-name ;get-value
  (lambda (get-value)
    (cadr get-value)))
(define func->parameter-name
  (lambda (func)
    (car (caddr func))))
(define func->body
  (lambda (func)
    (car (cdr (cdddr func)))))
(define run->func
  (lambda (run)
    (cadr run)))
(define run->parameter-input
  (lambda (app-exp)
    (cadddr app-exp)))


(define parse-expression
  (lambda (c0d3)
    (cond
      ((eq? (c0d3->type c0d3) 'get-value) (var-exp (get-value->value-name c0d3)))
      ((eq? (c0d3->type c0d3) 'func) (lambda-exp (func->parameter-name c0d3) (parse-expression (func->body c0d3))))
      ((eq? (c0d3->type c0d3) 'run) (app-exp
                              (parse-expression (run->func c0d3))
                              (parse-expression (run->parameter-input c0d3)))))))



(define unparse-expression
  (lambda (lcexp)
    (cond
      ((var-exp? lcexp) (list 'get-value (var-exp->var-name lcexp)))
      ((lambda-exp? lcexp) (list 'func 'gets (list (lambda-exp->parameter-name lcexp)) 'does (unparse-expression (lambda-exp->body lcexp))))
      ((app-exp? lcexp) (list 'run
                              (unparse-expression (app-exp->lambda-exp lcexp))
                              'with
                              (unparse-expression (app-exp->parameter-input lcexp)))))))
(define myC0d3 '(run (func gets (a) does (get-value a)) with (get-value c)))
myC0d3
(unparse-expression (parse-expression myC0d3))

