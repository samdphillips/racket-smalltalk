#lang racket/base

(require microparsec
         racket/function
         racket/unit
         smalltalk/reader
         threading
         "interface.rkt"
         "util.rkt")

(provide (all-defined-out))

(define-unit default-st:method@
  (import st:primary^
          st:statement^)
  (export st:method^)

  (define st:unary-method-header/p
    (do/p [msg <- st:identifier/p]
          (return/p (list msg))))

  (define st:binary-method-header/p
    (do/p [msg <- st:binary-selector/p]
          [arg <- st:identifier/p]
          (return/p (list msg arg))))

  (define st:keyword-method-header/p
    (seq/p 
      (many1/p (do/p [kmsg <- st:keyword/p]
                     [arg  <- st:identifier/p]
                     (return/p (cons kmsg arg))))
      (lambda (msg+args)
        (define msg (build-keyword-stx (map car msg+args)))
        (define args (map cdr msg+args))
        (return/p (cons msg args)))))

  ;; XXX duplicated from message.rkt
  (define (build-keyword-stx kws)
    (define srcloc (build-source-location kws))
    (~>> (for/list ([k (in-list kws)])
           (symbol->string (syntax->datum k)))
         (apply string-append)
         string->symbol
         (datum->syntax #f _ srcloc)))

  (define st:method-header/p
    (or/p st:unary-method-header/p
          st:binary-method-header/p
          st:keyword-method-header/p))

  (define (dbg/p v)
    (displayln v)
    (return/p v))

  (define st:method/p
    (do/p [message+args <- st:method-header/p]
          [lb         <- st:block-opener/p]
          [temps+body <- st:code-body/p]
          [rb         <- st:block-closer/p]
          (dbg/p
            (make-method-stx (build-source-location (car message+args) rb)
                             (car message+args)
                             (cdr message+args)
                             (car temps+body)
                             (cdr temps+body)))))

  (define (make-method-stx srcloc message arg* temp* body*)
    (quasisyntax/loc srcloc
      (#%st:method #,message #,arg* #,temp* #,@body*)))
)
