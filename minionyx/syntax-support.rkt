#lang racket/base

(require syntax/parse)

#|

Scratchpad.  Not sure how to organize this and `syntax.rkt` since they
can have cyclic dependency.

|#

(define-syntax-class :group
  #:datum-literals (group)
  [pattern (group . terms)])

(define-syntax-class :block
  #:datum-literals (block)
  [pattern (block groups ...)])

(define-syntax-class :binsel
  #:datum-literals (binsel)
  [pattern (binsel . name:id)])

(define-syntax-class :kwsel
  #:datum-literals (kwsel)
  [pattern (kwsel . name:id)])

(define-splicing-syntax-class :top-decl
  [pattern a-class::class-decl
    #:with body::block #'a-class.body
    #:attr t #'(parse-class a-class.name a-class.parent body.groups ...)]
  [pattern a-trait::trait-decl
    #:with body::block #'a-trait.body
    #:attr t #'(parse-trait a-trait.name body.groups ...)]
  [pattern e::top-expr
    #:attr t #'(parse-expr e)])

(define-syntax-class :primary
  [pattern v:id #:with t #'v])

(define-syntax-class :msg-send
  [pattern (o::primary msg:id)
    #:with t #'(#%send o.t msg ())]
  [pattern (o::primary {~seq kw::kwsel a} ...)
    #:with t #'(#%send o.t (kw.name ...) (a ...))])

(define-syntax-class :expr
  [pattern (e::primary) #:attr t #'e.t]
  [pattern e::msg-send #:attr t #'e.t])


(define-splicing-syntax-class :method-header
  [pattern {~seq name:id}
    #:attr args #'()]
  [pattern {~seq op::binsel arg:id}
    #:attr name #'op.name
    #:attr args #'(arg)]
  [pattern {~seq {~seq kws::kwsel args*:id} ...}
    #:attr name
    (datum->syntax #'(kws ...)
                   (string->symbol
                    (apply string-append
                           (for/list ([n (in-list (syntax-e #'(kws.name ...)))])
                             (symbol->string (syntax-e n))))))
    #:attr args #'(args* ...)])

(define-syntax-class :method-body
  #:datum-literals (block group binsel \|)
  [pattern (block (group {~optional {~seq (binsel . \|)
                                          temps:id ...
                                          (binsel . \|)}
                                    #:defaults ([(temps 1) #'()])}
                         . s0::expr)
                  (group . s1::expr) ...)
    #:attr stmts #'(s0.t s1.t ...)])

(define-splicing-syntax-class :method
  [pattern {~seq header::method-header body::method-body}
    #:attr name #'header.name
    #:attr args #'header.args
    #:attr stmts #'body.stmts])

#;
(syntax->datum
 (syntax-parse
     #'((kwsel . at:) anIndex (block (group false))
        (kwsel . at:) anIndex (kwsel . put:) aValue (block (group false))
        (binsel . ==) anObject
        (block (group self (kwsel . _objectEqual:) anObject))
        aSmallInt (block (group false))
        (kwsel . isKindOf:)
        aClass
        (block
         (group (binsel . \|) cls (binsel . \|) cls (delim . assign) self class)
         (group
          (block
           (group cls isNil (kwsel . ifTrue:) (block (group (delim . caret) false)))
           (group
            cls
            (binsel . ==)
            aClass
            (kwsel . ifTrue:)
            (block (group (delim . caret) true)))
           (group cls (delim . assign) cls superclass))
          repeat)))
   [(x::method ...) #'([x.name x.args x.stmts] ...)]))
