#lang racket/base

(require racket/sequence
         syntax/strip-context
         "lexer.rkt")

(module* reader #f
  (provide (rename-out
            [st:read read]
            [st:read-syntax read-syntax]))

  (define (st:read in)
    (syntax->datum
     (st:read-syntax #f in))))

(define (st:read-syntax src-name in)
  (define toks
    (sequence->list
     (in-port smalltalk-lex in)))
  (strip-context
   #`(module mod racket/base
       '#,toks)))