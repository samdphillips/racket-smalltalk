#lang racket/base

(require microparsec
         racket/unit
         "interface.rkt"
         "util.rkt")

(provide (all-defined-out))

(define-unit default-st:statement@
  (import st:expr^)
  (export st:statement^)

  (define (make-return c stx)
    (quasisyntax/loc (build-source-location c stx)
      (#%st:return #,stx)))

  (define st:return/p
    (do/p [c <- (st:delimiter/p 'caret)]
          [expr <- st:expr/p]
          (return/p
           (make-return c expr))))

  (define st:statement/p
    (or/p st:return/p
          st:expr/p)))

