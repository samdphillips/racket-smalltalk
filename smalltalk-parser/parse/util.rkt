#lang racket/base

(require microparsec
         racket/function
         smalltalk/reader
         syntax/srcloc
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
      satisfy/p
      token->syntax/p))

;; Older Racket versions (<8.3) don't support general source-location for
;; syntax/loc and friends, so we need to only use build-source-location-syntax.
;; This also handles arguments that are `(listof syntax?)`
(define (build-source-location stx0 . stxs)
  (if (syntax? stx0)
      (for/fold ([stx stx0]) ([s (in-list stxs)])
        (cond
          [(pair? s) (apply build-source-location-syntax stx s)]
          [(null? s) stx]
          [else (build-source-location-syntax stx s)]))
      (apply build-source-location (append stx0 stxs))))

