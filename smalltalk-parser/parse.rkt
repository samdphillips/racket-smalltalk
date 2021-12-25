#lang racket/base

(require microparsec
         racket/unit
         smalltalk/reader
         "parse/interface.rkt"
         "parse/message.rkt"
         "parse/primary.rkt")

(module* test #f
  (require racket/format
           racket/port
           racket/stream
           rackunit
           syntax/parse
           syntax/parse/define)

  (define-values/invoke-unit/infer
    (export st:message^)
    (link default-st:primary@
          default-st:message@))

  (define (port->stream p)
    (sequence->stream
     (in-port smalltalk-read p)))

  (define-syntax-parse-rule (test-parse test/p:id str pat)
    (test-case (~a "parse " (~s str) " with " 'test/p)
      (call-with-input-string str
        (lambda (inp)
          (define-values (parsed rest)
            (parse test/p (port->stream inp)))
          (with-check-info*
              (list (make-check-info 'parsed parsed))
            (lambda ()
              (check-true (stream-empty? rest) "unparsed tail")
              (check-true (syntax? parsed) "parsed is not syntax")
              (check-true
               (syntax-parse parsed
                 [pat #t]
                 [_ #f])
               "syntax does not match pattern")))))))

  (test-parse st:message/p "a toString"
              ({~datum #%st:send} {~datum a} {~datum toString}))

  (test-parse st:message/p "123 factorial"
              ({~datum #%st:send} 123 {~datum factorial}))
  (test-parse st:message/p "12345 toString size"
              ({~datum #%st:send}
               ({~datum #%st:send} 12345 {~datum toString})
               {~datum size}))

  (test-parse st:message/p "3 + 4"
              ({~datum #%st:send} 3 {~datum +} 4))
  (test-parse st:message/p "3 + 4 * 2"
              ({~datum #%st:send}
               ({~datum #%st:send} 3 {~datum +} 4)
               {~datum *} 2))
  (test-parse st:message/p "3 factorial + 4 factorial"
              ({~datum #%st:send}
               ({~datum #%st:send} 3 {~datum factorial})
               {~datum +}
               ({~datum #%st:send} 4 {~datum factorial})))

  (test-parse st:message/p "2 raisedTo: 5"
              ({~datum #%st:send} 2 {~datum raisedTo:} 5))
  (test-parse st:message/p "d at: 0 put: 1"
              ({~datum #%st:send} {~datum d} {~datum at:put:} 0 1))
  (test-parse st:message/p "d at: 0 put: x + y"
              ({~datum #%st:send}
               {~datum d} {~datum at:put:} 0
               ({~datum #%st:send} {~datum x} {~datum +} {~datum y})))

  (test-parse st:message/p
              "self new initialize: aCollection; add: 10; + 0; yourself"
              ({~datum #%st:send*}
               ({~datum #%st:send} {~datum self} {~datum new})
               ({~datum initialize:} {~datum aCollection})
               ({~datum add:} 10)
               ({~datum +} 0)
               ({~datum yourself})))
  )
