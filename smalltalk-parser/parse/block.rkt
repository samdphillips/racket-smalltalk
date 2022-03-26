#lang racket/base

(require microparsec
         racket/function
         racket/unit
         smalltalk/reader
         "interface.rkt"
         "util.rkt")

(provide (all-defined-out))

(define-unit default-st:block@
  (import st:primary^
          st:statement^)
  (export st:block^)

  (define st:block-opener/p
    (st:opener/p "["))

  (define st:block-closer/p
    (st:closer/p "]"))

  (define st:block-args/p
    (return/p null))

  (define st:pipe/p
    (satisfy/p (conjoin
                 binary-selector?
                 (lambda (tok)
                   (eq? '\| (token-value tok))))))

  (define st:double-pipe/p
    (satisfy/p (conjoin
                 binary-selector?
                 (lambda (tok)
                   (eq? '\|\| (token-value tok))))))

  ;; XXX block temps and method temps are the same
  (define st:block-temps/p
    (or/p (do/p st:pipe/p
                [vars <- (many/p st:identifier/p)]
                st:pipe/p
                (return/p vars))
          (do/p st:double-pipe/p
                (return/p null))
          (return/p null)))

  (define (make-block-stx lb rb args temps body)
    (quasisyntax/loc (build-source-location lb rb args temps body)
      (#%st:block #,args #,temps #,@body)))

  (define st:block/p
    (do/p [lb     <- st:block-opener/p]
          [args   <- st:block-args/p]
          [temps  <- st:block-temps/p]
          [body   <- st:statements/p]
          [rb     <- st:block-closer/p]
          (return/p
           (make-block-stx lb rb args temps body))))
  )

