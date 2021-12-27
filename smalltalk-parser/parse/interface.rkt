#lang racket/base

(require racket/unit)

(provide (all-defined-out))

(define-signature st:expr^      (st:expr/p))
(define-signature st:message^   (st:message/p))
(define-signature st:primary^   (st:identifier/p
                                 st:primary/p))
(define-signature st:statement^ (st:statement/p))

