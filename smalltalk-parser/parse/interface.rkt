#lang racket/base

(require racket/unit)

(provide (all-defined-out))

(define-signature st:block^     (st:block/p))
(define-signature st:expr^      (st:expr/p))
(define-signature st:message^   (st:message/p))
(define-signature st:method^    (st:method/p))
(define-signature st:primary^   (st:identifier/p
                                 st:primary/p))
(define-signature st:statement^ (st:code-body/p
                                 st:statement/p
                                 st:statements/p))
(define-signature st:top-decl^  (st:class-decl/p
                                 st:top-decl/p))
