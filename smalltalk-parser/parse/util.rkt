#lang racket/base

(require microparsec
         smalltalk/reader)

(provide (all-defined-out))

(define (token->syntax tok)
  (datum->syntax #f (token-value tok) (token-srcloc tok)))

(define (token->syntax/p p)
  (do/p [tok <- p] (return/p (token->syntax tok))))

