#lang racket/base

(require microparsec
         racket/unit
         "interface.rkt"
         "util.rkt")

(provide (all-defined-out))

(define-unit default-st:statement@
  (import st:expr^)
  (export st:statement^)

  (define st:temps/p
    (or/p (do/p st:pipe/p
                [vars <- (many/p st:identifier/p)]
                st:pipe/p
                (return/p vars))
          (do/p st:double-pipe/p
                (return/p null))
          (return/p null)))

  (define st:code-body/p
    (do/p [temps <- st:temps/p]
          [body  <- st:statements/p]
          (return/p (cons temps body))))

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
          st:expr/p))

  (define st:statements/p
    (label/p
     "statements"
     (or/p (try/p
            (do/p [s <- st:statement/p]
                  (st:delimiter/p 'dot)
                  [s* <- st:statements/p]
                  (return/p (cons s s*))))
           (try/p
            (do/p [s <- st:statement/p]
                  (st:delimiter/p 'dot)
                  (return/p (list s))))
           (do/p [s <- st:statement/p]
                 (return/p (list s)))))))

