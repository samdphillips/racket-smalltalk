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

  (define st:identifier/p
    (token->syntax/p (satisfy/p identifier?)))

  (define st:primary/p
    (or/p st:identifier/p
          (token->syntax/p
            (satisfy/p token-integer?)))))

