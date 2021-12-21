#lang racket/base

(require microparsec
         racket/unit
         smalltalk/reader
         syntax/srcloc
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

  (define st:binary-msg/p
    (or/p (many1/p
           (do/p [bmsg   <- (token->syntax/p
                             (satisfy/p binary-selector?))]
                 [arg-p  <- st:primary/p]
                 [arg-m* <- st:unary-msg/p]
                 (return/p
                  (cons bmsg (build-unary-send-stx arg-p arg-m*)))))
          (return/p null)))

  (define st:keyword-msg/p
    (or/p (many1/p
            (do/p [kmsg   <- (token->syntax/p (satisfy/p keyword?))]
                  [arg-p  <- st:primary/p]
                  [arg-m* <- st:binary-msg/p]
                  (return/p
                    (cons kmsg (build-binary-send-stx arg-p arg-m*)))))
          (return/p #f)))

  (define (make-send rcvr-stx msg-stx args-stx)
    (define srcloc
      ;; older Racket versions (<8.3) don't support general srcloc for
      ;; syntax/loc and friends
      (apply build-source-location-syntax rcvr-stx msg-stx args-stx))
    (quasisyntax/loc srcloc
                     (#%st:send #,rcvr-stx #,msg-stx #,@args-stx)))

  (define (build-unary-send-stx rcvr msg*)
    (for/fold ([rcvr rcvr]) ([m (in-list msg*)])
      (make-send rcvr m null)))

  (define (build-binary-send-stx rcvr msg+arg*)
    (for/fold ([rcvr rcvr]) ([ma (in-list msg+arg*)])
      (define m (car ma))
      (define a (cdr ma))
      (make-send rcvr m (list a))))

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
        (make-send rcvr
                   (build-keyword-stx (map car msg+args))
                   (map cdr msg+args))
        rcvr))

  (define st:message/p
    (do/p [p     <- st:primary/p]
          [umsg* <- st:unary-msg/p]
          [bmsg* <- st:binary-msg/p]
          [kmsg  <- st:keyword-msg/p]
          #;
          [casc <- st:cascade/p]
          (return/p
           (~> (build-unary-send-stx p umsg*)
               (build-binary-send-stx bmsg*)
               (build-keyword-send-stx kmsg))))))


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

  (define (test-parse p s)
    (call-with-input-string s
      (lambda (i)
        (define-values (parsed rest)
          (parse p (port->stream i)))
        parsed)))

  (test-parse st:message/p "a toString")
  (test-parse st:message/p "123 factorial")
  (test-parse st:message/p "12345 toString size")

  (test-parse st:message/p "3 + 4")
  (test-parse st:message/p "3 + 4 * 2")
  (test-parse st:message/p "3 factorial + 4 factorial")

  (test-parse st:message/p "2 raisedTo: 5")
  (test-parse st:message/p "d at: 0 put: 1")
  (test-parse st:message/p "d at: 0 put: x + y")

  )



