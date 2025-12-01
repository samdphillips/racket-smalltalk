#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         syntax-spec-v3)

(provide (rename-out
          [module-begin #%module-begin]))

(begin-for-syntax
  (define-syntax-class :group
    #:datum-literals (group)
    [pattern (group . terms)])

  (define-syntax-class :block
    #:datum-literals (block)
    [pattern (block groups ...)])

  (define-syntax-class :kwsel
    #:datum-literals (kwsel)
    [pattern (kwsel . sname:id)
      #:attr name (syntax-e #'sname)])

  (define-splicing-syntax-class :top-decl
    [pattern a-class::class-decl
      #:with body::block #'a-class.body
      #:attr out #'(#%class a-class.name a-class.parent body.groups ...)]
    [pattern a-trait::trait-decl
      #:with body::block #'a-trait.body
      #:attr out #'(#%trait a-trait.name body.groups ...)]
    [pattern e::top-expr
      #:attr out #'(#%expr e)])

  (define-splicing-syntax-class :class-decl
    #:datum-literals (block)
    [pattern
      {~seq parent:id subclass: name:id body::block}
      #:declare subclass: :kwsel
      #:when (eq? 'subclass: (attribute subclass:.name))])

  (define-splicing-syntax-class :trait-decl
    #:datum-literals (Trait block)
    [pattern
      {~seq Trait named: name:id body::block}
      #:declare named: :kwsel
       #:when (eq? 'named: (attribute named:.name))])

  (define-splicing-syntax-class :top-expr
    [pattern
      {~seq terms ...+}]))

(define-syntax module-begin
  (syntax-parser
    [(_ body::group ...)
     #:with ((decls::top-decl ...) ...) #'(body.terms ...)
     #'(onyx-top decls.out ... ...)]))

(syntax-spec
 (nonterminal top-decl
   class:class-decl
   trait:trait-decl)

 (nonterminal class-decl
   (#%class name:id parent:id )

 (nonterminal trait-decl)

 )