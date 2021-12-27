#lang racket/base

(require microparsec
         racket/unit
         "interface.rkt"
         "util.rkt")

(provide (all-defined-out))

(define-unit default-st:expr@
  (import st:primary^
          st:message^)
  (export st:expr^)

  (define (make-assignment-stx lstx rstx)
    (quasisyntax/loc (build-source-location lstx rstx)
      (#%st:assignment #,lstx #,rstx)))

  (define st:assignment/p
    (do/p [lhs <- st:identifier/p]
          (st:delimiter/p 'assignment)
          [rhs <- st:expr/p]
          (return/p
           (make-assignment-stx lhs rhs))))

  (define st:expr/p
    (or/p (try/p st:assignment/p)
          st:message/p)))

