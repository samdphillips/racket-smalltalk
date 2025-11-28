#lang racket/base

(require microparsec
         racket/function
         racket/unit
         smalltalk/reader
         "interface.rkt"
         "util.rkt")

(provide (all-defined-out))

(define-unit default-st:top-decl@
  (import st:expr^
          st:method^)
  (export st:top-decl^)

  (define subclass-kw/p
    (token->syntax/p
      (satisfy/p
        (conjoin keyword?
                 (λ (tok) (eq? 'subclass: (token-value tok)))))))

  (define (st:metaclass-decl/p name-stx)
    (do/p (st:identifier=/p name-stx)
          class-u/p
          (st:opener/p "[")
          (st:closer/p "]")
          (dbg/p
           (make-metaclass-stx name-stx))))

  (define st:decl-class/p
    (or/p st:method/p))

  (define st:class-decl/p
    (do/p [parent-class-name-stx <- st:identifier/p]
          subclass-kw/p
          [class-name-stx <- st:identifier/p]
          (st:opener/p "[")
          [methods <- (many/p st:decl-class/p)]
          (st:closer/p "]")
          (dbg/p
           (make-class-stx class-name-stx parent-class-name-stx methods))))

  (define (make-class-stx class-name-stx parent-class-name-stx methods)
    (quasisyntax/loc
        (build-source-location class-name-stx parent-class-name-stx)
      (#%st:class #,class-name-stx #,parent-class-name-stx . #,methods)))

  (define st:top-decl/p
    (or/p st:class-decl/p
          (try/p (do/p [e <- st:expr/p] (st:delimiter/p 'dot) (return/p e)))))

  (define st:module/p
    (many/p st:top-decl/p))
)