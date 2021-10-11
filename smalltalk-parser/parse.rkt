#lang racket/base

(require microparsec
         racket/unit
         smalltalk/reader
         threading)

(define (token->syntax tok)
  (datum->syntax #f (token-value tok) (token-srcloc tok)))

(define (token->syntax/p p)
  (do/p [tok <- p] (return/p (token->syntax tok))))

(define-signature st:primary^ (st:primary/p))
(define-signature st:message^ (st:message/p))

(define-unit default-st:primary@
  (import)
  (export st:primary^)  
  
  (define st:primary/p
    (token->syntax/p
     (or/p (satisfy/p identifier?)
           (satisfy/p token-integer?)
          ))))

(define-unit default-st:message@
  (import st:primary^)
  (export st:message^)

  (define st:unary-msg/p
    (or/p (many1/p
           (token->syntax/p (satisfy/p identifier?)))
          (return/p null)))

  (define st:binary-msg/p  (return/p #f))
  (define st:keyword-msg/p (return/p #f))

  (define (build-unary-send-stx rcvr msg*)
    (for/fold ([rcvr rcvr]) ([m (in-list msg*)])
      #`(send #,rcvr #,m)))
  
  (define (build-send-stx rcvr msg+args) rcvr)
  
  (define st:message/p
    (do/p [p     <- st:primary/p]
          [umsg* <- st:unary-msg/p]
          [bmsg  <- st:binary-msg/p]
          [kmsg  <- st:keyword-msg/p]
          #;
          [casc <- st:cascade/p]
          (return/p
           (~> (build-unary-send-stx p umsg*)
               (build-send-stx bmsg)
               (build-send-stx kmsg)))))
  )


(module* test #f
  (require racket/port
           racket/stream)

  (define-values/invoke-unit/infer
    (export st:message^)
    (link default-st:primary@
          default-st:message@))

  (define (port->stream p)
    (sequence->stream
     (in-port smalltalk-read p)))

  (define (test-parse s)
    (call-with-input-string s
      (lambda (i)
        (define-values (p rest)
          (parse st:message/p (port->stream i)))
        p)))

  (test-parse "a toString")
  (test-parse "123 factorial")
  (test-parse "12345 toString size")

  )



  