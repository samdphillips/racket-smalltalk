#lang racket/base

(require microparsec
         racket/unit
         smalltalk/reader
         "interface.rkt"
         "util.rkt")

(provide (all-defined-out))

(define-unit default-st:primary@
  (import st:block^
          st:expr^)
  (export st:primary^)

  ;; XXX move this to util
  (define st:identifier/p
    (token->syntax/p (satisfy/p identifier?)))

  (define st:nested-expr/p
    (do/p st:paren-opener/p
          [e <- st:expr/p]
          st:paren-closer/p
          (return/p e)))

  (define st:primary/p
    (or/p st:identifier/p
          (token->syntax/p
           (satisfy/p token-integer?))
          (token->syntax/p
           (satisfy/p token-string?))
          (try/p st:nested-expr/p)
          st:block/p)))

