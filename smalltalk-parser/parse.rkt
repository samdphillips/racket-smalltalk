#lang racket/base

(require microparsec
         racket/unit
         smalltalk/reader
         "parse/interface.rkt"
         "parse/message.rkt"
         "parse/primary.rkt")

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



