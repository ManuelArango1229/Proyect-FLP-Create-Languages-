#lang eopl

#| eopl.ss:  A simple language for the book "Essentials of Programming Languages"

Creado por: Juan Manuel Arango 

Inicio: 2024-04-28

proyecto de curso de Fundamentos de lenguaje de programación (FLP)|#


;;Define the lexical specification for the language
(define lexical-specification
  '(
    (white-space (whitespace) skip)
    (comment ("#" (arbno (not #\newline))) skip)
    (identifier (letter (arbno letter digit "?")) symbol)
    (numbers (digit (arbno digit)) number)
    (numbers ("-" digit (arbno digit)) number)
    (numbers (digit (arbno digit) "." digit (arbno digit)) number)
    (numbers ("-" digit (arbno digit) "." digit (arbno digit)) number)
  )
) 




;; Define the grammar specification for the Language

(define grammar-specification
  '(
    (program (exp) a-program)
    (exp (numbers) lit-exp)
    (exp (identifier) var-exp)
    (exp (primitive "(" (separated-list exp ",") ")") prim-exp)
    (primitive ("+") sum-prim)
    (primitive ("-") menus-prim)
    (primitive ("*") mulp-prim)
    (primitive ("/") dev-prim)
    (primitive ("add1") add-prim)
    (primitive ("sub1") sub-prim)
  )
)

;; Create datatype automatically
(sllgen:make-define-datatypes lexical-specification grammar-specification)



;;Environment 
;;

(define-datatype env env?
  (empty-env)
  (extend-env (Lid (list-of symbol?)) (LVal (list-of value?)) (env env?))
)

(define value? 
  (lambda (x)
    #T
  )
)

;;Apply env, function to apply the environment to the value
(define apply-env
  (lambda (e id)
    (cases env e
      (empty-env () (eopl:error "No find the value"))
      (extend-env (lid lval e)
        (letrec
          (
            (aux (lambda (lid lval)
              (cond
                [(null? lid) (eopl:error "No find the value")]   
                [(eq? (car lid) id) (car lval)]
                [else (aux (cdr lid) (cdr lval))]
              )
              )
            )
          )
          (aux lid lval)
        )
      )
    )
  )
)

;;Inittial environment
(define init-env
  (extend-env '(x y z) '(1 2 3) (empty-env))
)

;;Job function (evaluate the expression)
(define eval-exp
  (lambda (e env)
    (cases exp e
      (lit-exp (n) n)
      (var-exp (id) (apply-env env id))
      (prim-exp (prim larg) 
        (let
          (
            (lvalues (map (lambda (e) (eval-exp e env)) larg))
          )
          (eval-prim prim lvalues)
        )
      )
    )
  )
)



(define eval-prim
  (lambda (prim lvalues)
    (cases primitive prim
      (sum-prim () (apply + lvalues))
      (menus-prim () (apply - lvalues))
      (mulp-prim () (apply * lvalues))
      (dev-prim () (apply / lvalues))
      (add-prim () (+ 1 (car lvalues)))
      (sub-prim () (- 1 (car lvalues)))
)
    )
  )
;; Build the evaluator, this is a simple evaluator that just returns the value of the expression
(define eval-program
  (lambda (prog)
    (cases program prog
      (a-program (e) (eval-exp e init-env))
    )
  )
  )
  ;; Build the REPL
(define Interpreter
  (sllgen:make-rep-loop "-->" eval-program (sllgen:make-stream-parser lexical-specification grammar-specification))
)


