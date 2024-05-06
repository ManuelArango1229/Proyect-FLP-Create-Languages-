#lang eopl

#| eopl.ss:  A simple language for the book "Essentials of Programming Languages"

Created by: Juan Manuel Arango 

Start: 2024-04-28

fundamentals of programming lenguages course project (FLP)|#


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
    ;;Expression management
    (exp (numbers) lit-exp)
    (exp (identifier) var-exp)
    (exp (primitive "(" (separated-list exp ",") ")") prim-exp)
    ;;boolean management
    (exp ("true") true-exp)
    (exp ("false") false-exp)
    ;;Conditional management
    (exp ("if" exp "then" exp "else" exp) if-exp)
    ;;ligature management
    (exp ("let" (arbno identifier "=" exp) "in" exp) let-exp)
    ;;Procedure management
    (exp ("proc" "("(separated-list identifier ",")")" exp) proc-exp)
    (exp ("(" exp (arbno exp) ")") call-exp)
    ;;Recursive Procedure management
    (exp ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" exp) "in" exp) letrec-exp)
    ;;Assignment
    ;;Begin
    (exp ("begin" exp (arbno ";" exp) "end") begin-exp)
    ;;Set
    (exp ("set" identifier "=" exp) set-exp)
    ;;Primitive management
    (primitive ("+") sum-prim)
    (primitive ("-") menus-prim)
    (primitive ("*") mulp-prim)
    (primitive ("/") dev-prim)
    (primitive ("add1") add-prim)
    (primitive ("sub1") sub-prim)
    (primitive ("<") lt-prim)
    (primitive (">") gt-prim)
    (primitive ("=") eq-prim)
    (primitive ("<=") leq-prim)
    (primitive (">=") geq-prim)
    (primitive ("and") and-prim)
    (primitive ("or") or-prim)
    (primitive ("not") not-prim)
  )
)

;; Create datatype automatically
(sllgen:make-define-datatypes lexical-specification grammar-specification)


;;Job function (evaluate the expression)
(define eval-exp
  (lambda (e env)
    (cases exp e
      (lit-exp (n) n)
      (var-exp (id) (apply-env env id))
      ;;Primitive
      (prim-exp (prim larg) 
        (let
          (
            (lvalues (map (lambda (e) (eval-exp e env)) larg))
          )
          (eval-prim prim lvalues)
        )
      )
      ;;Booolean
      (true-exp () #T)
      (false-exp () #F)
      ;;Conditional
      (if-exp (cond exp1 exp2)
        (if (eval-exp cond env) (eval-exp exp1 env) (eval-exp exp2 env))
      )
      ;ligature
      (let-exp (ids rands body)
        (let
          (
            (lvalues (map (lambda (x) (eval-exp x env)) rands))
          )
          (eval-exp body (extend-env ids lvalues env))
        )
      )
      ;;Precedure
      (proc-exp (ids body)
        (Closure ids body env)
      )
      (call-exp (rator rands)
        (let
          (
            (procV (eval-exp rator env))
            (lrands (map (lambda (x) (eval-exp x env)) rands))
          )
          (if (procval? procV) 
              (cases procval procV
                (Closure (lids body old-env)
                  (eval-exp body (extend-env lids lrands old-env))
                )
              )
            (eopl:error "Not a procedure " procV)
          )
        )
      )
      ;;letrec (only recursively procedure)
      (letrec-exp (procnames idss bodies bodyletrec)
        (eval-exp bodyletrec (extend-recursive-env procnames idss bodies env))
      )
      ;;begin
      (begin-exp (exp lexp)
        (if (null? lexp) 
          (eval-exp exp env)
          (begin 
            (eval-exp exp env)
            (letrec
              (
               (aux (lambda (lexp)
                  (cond
                    [(null? (cdr lexp)) (eval-exp (car lexp) env)]
                    [else (begin (eval-exp (car lexp) env) (aux (cdr lexp)))]

                    )
                      ))
               )
              
            (aux lexp)
              )
          )
        )
      )
      ;;Set 
      (set-exp (id exp)
        (begin
          (begin
              (setref! (apply-env-ref env id) (eval-exp exp env))
              1
          )
        )
      )
    )
  )
)

;;Environment
(define-datatype env env?
  (empty-env)
  (extend-env-ref (Lid (list-of symbol?)) (LVal vector?) (env env?))
)

(define extend-env
  (lambda (ids vals env)
    (extend-env-ref ids (list->vector vals) env)
  )
)

;;Recursive environment

(define extend-recursive-env
  (lambda (procnames idss bodies old-env)
    (let
      (
       (vec-closure (make-vector (length procnames)))
      )
      (letrec
        (
          (amb (extend-env-ref procnames vec-closure old-env))
          (get-closure (lambda (ids bodies pos)
            (cond
              [(null? ids) amb]
              [else
                (begin
                  (vector-set! vec-closure pos (Closure (car ids) (car bodies) amb)) (get-closure (cdr ids) (cdr bodies) (+ pos 1))
                )
              ]
            )
            )
          )
        )
        (get-closure idss bodies 0)
      )
    )
  )
)

(define apply-env
  (lambda (env var)
    (deref (apply-env-ref env var))
  )
)

;;Apply env, function to apply the environment to the value
(define apply-env-ref
  (lambda (e id)
    (cases env e
      (empty-env () (eopl:error "No find the value"))
      (extend-env-ref (lid vec old-env)
        (letrec
          (
            (aux (lambda (lid lval pos )
              (cond
                [(null? lid) (apply-env-ref old-env id)]
                [(eq? (car lid) id) (a-ref pos vec)]
                [else (aux (cdr lid) vec (+ pos 1))]
              )
              )
            )
          )
          (aux lid vec 0)
        )
      )
    )
  )
)



;;Inittial environment
(define init-env
  (extend-env '(x y z) '(1 2 3) (empty-env))
)


;;Function to evaluate the primitive

(define eval-prim
  (lambda (prim lvalues)
    (cases primitive prim
      (sum-prim () (apply + lvalues))
      (menus-prim () (apply - lvalues))
      (mulp-prim () (apply * lvalues))
      (dev-prim () (apply / lvalues))
      (add-prim () (+ 1 (car lvalues)))
      (sub-prim () (- 1 (car lvalues)))
      (lt-prim () (< (car lvalues) (cadr lvalues)))
      (gt-prim () (> (car lvalues) (cadr lvalues)))
      (eq-prim () (= (car lvalues) (cadr lvalues)))
      (leq-prim () (<= (car lvalues) (cadr lvalues)))
      (geq-prim () (>= (car lvalues) (cadr lvalues)))
      (and-prim () (and (car lvalues) (cadr lvalues)))
      (or-prim () (or (car lvalues) (cadr lvalues)))
      (not-prim () (not (car lvalues)))
    )
  )
)

;;Closure: Content information of procedure, linked to the place where it was created.
(define-datatype procval procval?
  (Closure
    (ids (list-of symbol?))
    (body exp?)
    (environmet env?)
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
;; References

(define-datatype reference reference?
  (a-ref (pos number?) (vec vector?))
)

;;extract references
(define deref
  (lambda (ref)
    (primitive-deref ref)
  )
)

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec) (vector-ref vec pos))
    )
  )
)
;;Asignation/Update references

(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)
  )
)

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec) (vector-set! vec pos val))
    )
  )
)

  ;; Build the REPL
(define Interpreter
  (sllgen:make-rep-loop "-->" eval-program (sllgen:make-stream-parser lexical-specification grammar-specification))
)



(Interpreter)

