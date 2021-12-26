#lang racket/base

(require microparsec
         racket/function
         smalltalk/reader
         threading)

(provide (all-defined-out))

(define (token->syntax tok)
  (datum->syntax #f (token-value tok) (token-srcloc tok)))

(define (token->syntax/p p)
  (do/p [tok <- p] (return/p (token->syntax tok))))

(define (st:delimiter/p type)
  (~> (conjoin
       delimiter?
       (lambda~> token-value (eq? type)))
      satisfy/p))

