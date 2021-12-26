#lang racket/base

(require microparsec
         racket/function
         racket/unit
         smalltalk/reader
         syntax/srcloc
         threading
         "interface.rkt"
         "util.rkt")

(provide (all-defined-out))

;; XXX: need expected combinator to guard against bad syntax

(define-unit default-st:message@
  (import st:primary^)
  (export st:message^)

  (define st:unary-msg1/p st:identifier/p)

  (define st:unary-msg/p
    (or/p (many1/p st:unary-msg1/p)
          (return/p null)))

  (define st:binary-msg1/p
    (do/p [bmsg   <- (token->syntax/p
                      (satisfy/p binary-selector?))]
          [arg-p  <- st:primary/p]
          [arg-m* <- st:unary-msg/p]
          (return/p
            (cons bmsg (build-unary-send-stx arg-p arg-m*)))))

  (define st:binary-msg/p
    (or/p (many1/p st:binary-msg1/p)
          (return/p null)))

  (define st:keyword-msg/p
    (or/p (seq/p
            (many1/p
              (do/p [kmsg   <- (token->syntax/p (satisfy/p keyword?))]
                    [arg-p  <- st:primary/p]
                    [arg-m* <- st:binary-msg/p]
                    (return/p
                     (cons kmsg (build-binary-send-stx arg-p arg-m*)))))
            (lambda (msg+args)
              (define msg (build-keyword-stx (map car msg+args)))
              (define args (map cdr msg+args))
              (define srcloc
                (apply build-source-location-syntax msg args))
              (return/p (quasisyntax/loc srcloc (#,msg #,@args)))))
          (return/p #f)))

  (define cascade-token/p
    (satisfy/p
      (conjoin delimiter?
               (lambda~> token-value
                         (eq? 'cascade)))))

  (define st:cascade/p
    (or/p (many1/p
            (do/p cascade-token/p
                  (or/p (seq/p st:unary-msg1/p
                               (lambda (msg)
                                 (return/p
                                   (quasisyntax/loc msg (#,msg)))))
                        (seq/p st:binary-msg1/p
                               (lambda (msg+arg)
                                 (define msg (car msg+arg))
                                 (define arg (cdr msg+arg))
                                 (define srcloc
                                   (build-source-location-syntax msg arg))
                                 (return/p
                                   (quasisyntax/loc srcloc
                                     (#,msg #,arg)))))
                        st:keyword-msg/p)))
          (return/p #f)))

  (define (make-send-stx rcvr-stx msg-stx args-stx)
    (define srcloc
      ;; older Racket versions (<8.3) don't support general srcloc for
      ;; syntax/loc and friends
      (apply build-source-location-syntax rcvr-stx msg-stx args-stx))
    (quasisyntax/loc srcloc
      (#%st:send #,rcvr-stx #,msg-stx #,@args-stx)))

  (define (build-unary-send-stx rcvr msg*)
    (for/fold ([rcvr rcvr]) ([m (in-list msg*)])
      (make-send-stx rcvr m null)))

  (define (build-binary-send-stx rcvr msg+arg*)
    (for/fold ([rcvr rcvr]) ([ma (in-list msg+arg*)])
      (define m (car ma))
      (define a (cdr ma))
      (make-send-stx rcvr m (list a))))

  (define (build-keyword-stx kws)
    (define srcloc
      ;; older Racket versions (<8.3) don't support general srcloc for
      ;; syntax/loc and friends
      (apply build-source-location-syntax kws))
    (~>> (for/list ([k (in-list kws)])
           (symbol->string (syntax->datum k)))
         (apply string-append)
         string->symbol
         (datum->syntax #f _ srcloc)))

  (define (build-keyword-send-stx rcvr msg+args)
    (if msg+args
        (syntax-case msg+args ()
          [(msg . args)
           (make-send-stx rcvr #'msg (syntax->list #'args))])
        rcvr))

  (define (build-cascade-send-stx rcvr casc)
    (if casc
        (syntax-case rcvr (#%st:send)
          [(#%st:send rcvr0 msg0 . arg0)
           (let ()
             (define srcloc
               (apply build-source-location-syntax
                      #'rcvr0 #'msg0 #'arg0 casc))
             (quasisyntax/loc srcloc
               (#%st:send* rcvr0 (msg0 . arg0) #,@casc)))])
        rcvr))

  ;; XXX: combinator to set procedure name "right"
  (define st:message/p
    (do/p [p     <- st:primary/p]
          [umsg* <- st:unary-msg/p]
          [bmsg* <- st:binary-msg/p]
          [kmsg  <- st:keyword-msg/p]
          [casc  <- st:cascade/p]
          (return/p
           (~> (build-unary-send-stx p umsg*)
               (build-binary-send-stx bmsg*)
               (build-keyword-send-stx kmsg)
               (build-cascade-send-stx casc))))))

