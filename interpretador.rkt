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

;; Build the evaluator, this is a simple evaluator that just returns the value of the expression
(define eval-program
  (lambda (program)
    program
  )
)

;; Build the REPL
(define Interpreter
  (sllgen:make-rep-loop "-->" eval-program (sllgen:make-stream-parser lexical-specification grammar-specification))
)


