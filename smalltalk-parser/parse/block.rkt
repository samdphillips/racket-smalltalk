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

  (define st:block-args/p
    (return/p null))

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
