
#lang racket

; Define the syntax of our programming language
(define-syntax-rule (program stmts ...)
  (begin stmts ...))

(define-syntax-rule (assign var expr)
  (set! var expr))

(define-syntax-rule (while cond stmts ...)
  (let loop ()
    (when cond
      stmts 
      (loop))))

(define-syntax-rule (if cond then-stmts else-stmts ...)
  (if cond
      (begin then-stmts)
      (begin else-stmts )))

; Define the parser
(define (parse input)
  (define (parse-expr tokens)
    (match tokens
      ((list) (error "Unexpected end of input"))
      ((list rest ) (car tokens))))
  
  (define (parse-assign tokens)
    (match tokens
      ((list) (error "Unexpected end of input"))
      ((list id "=" rest )
       `(assign ,id ,(parse-expr rest)))
      (else (error "Invalid input from user"))))
  
  (define (parse-while tokens)
    (match tokens
      ((list) (error "Unexpected end of input"))
      ((list "while" rest ...)
       `(while ,(parse-expr rest ...)
               ,@(parse-statements (parse-block rest ...))))
      (else (error "Invalid input from user"))))
  
  (define (parse-if tokens)
    (match tokens
      ((list) (error "Unexpected end of input "))
      ((list "if" rest ...)
       `(if ,(parse-expr rest ...)
            ,@(parse-statements (parse-block rest ...))
            ,@(parse-else (parse-block rest ...))))
      (else (error "Invalid input from user check again"))))
  
  (define (parse-else tokens)
    (match tokens
      ((list) '())
      ((list "else" rest ...)
       (parse-statements (parse-block rest ...)))
      (else (error "Invalid input check again"))))
  
  (define (parse-block tokens)
    (match tokens
      ((list) (error "Unexpected end of input"))
      ((list "{" rest ...) (parse-statements rest ...))
      (else (error "Invalid input check and rewrite "))))
  
  (define (parse-statement tokens)
    (match tokens
      ((list) (error "Unexpected end of input"))
      ((list id "=" rest ...) (parse-assign tokens))
      ((list "while" rest ...) (parse-while tokens))
      ((list "if" rest ...) (parse-if tokens))
      ((list "{" rest ...) (parse-block tokens))
      (else (error "Invalid input from user"))))
  
  (define (parse-statements tokens)
    (match tokens
      ((list) '())
      (else (cons (parse-statement tokens)
                  (parse-statements (cdr tokens))))))

  ; Tokenize the input
  (define tokens (string-split input))
  
  ; Parse the input
  `(program ,@(parse-statements tokens)))

; Example usage
(parse "while a > 0 { a = a - 1 }")