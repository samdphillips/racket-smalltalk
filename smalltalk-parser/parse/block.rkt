#lang racket/base

(require microparsec
         racket/function
         racket/unit
         smalltalk/reader
         (submod smalltalk/reader for-pipes)
         "interface.rkt"
         "util.rkt")

(provide (all-defined-out))

(define-unit default-st:block@
  (import st:statement^)
  (export st:block^)

  (define st:block-args/p
    (or/p (do/p [args <- (many1/p st:block-argument/p)]
                (or/p st:pipe/p
                      (do/p [p <- st:double-pipe/p]
                            (push/p (binary-selector (token-srcloc p) '\|))))
                (return/p args))
          (return/p null)))

  (define (make-block-stx lb rb args temps body)
    (quasisyntax/loc (build-source-location lb rb args temps body)
      (#%st:block #,args #,temps #,@body)))

  (define st:block/p
    (do/p [lb         <- st:block-opener/p]
          [args       <- st:block-args/p]
          [temps+body <- st:code-body/p]
          [rb         <- st:block-closer/p]
          (return/p
           (make-block-stx lb rb args (car temps+body) (cdr temps+body))))))
