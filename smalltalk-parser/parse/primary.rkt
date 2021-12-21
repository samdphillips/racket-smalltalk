#lang racket/base

(require microparsec
         racket/unit
         smalltalk/reader
         "interface.rkt"
         "util.rkt")

(provide (all-defined-out))

(define-unit default-st:primary@
  (import)
  (export st:primary^)

  (define st:primary/p
    (token->syntax/p
     (or/p (satisfy/p identifier?)
           (satisfy/p token-integer?)
           ))))

