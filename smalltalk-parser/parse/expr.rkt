#lang racket/base

(require microparsec
         racket/unit
         syntax/srcloc
         "interface.rkt"
         "util.rkt")

(provide (all-defined-out))

(define-unit default-st:expr@
  (import st:primary^
          st:message^)
  (export st:expr^)

  (define (make-assignment-stx lstx rstx)
    (define srcloc
      ;; older Racket versions (<8.3) don't support general srcloc for
      ;; syntax/loc and friends
      (build-source-location-syntax lstx rstx))
    (quasisyntax/loc srcloc
      (#%st:assignment #,lstx #,rstx)))

  (define st:assignment/p
    (do/p [lhs <- st:identifier/p]
          (st:delimiter/p 'assignment)
          [rhs <- st:expr/p]
          (return/p
           (make-assignment-stx lhs rhs))))

  (define st:expr/p
    (or/p (try/p st:assignment/p)
          st:message/p))

  )

