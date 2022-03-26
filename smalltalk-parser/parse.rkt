#lang racket/base

(module* test #f
  (require microparsec
           racket/format
           racket/port
           (only-in racket/stream stream-empty?)
           racket/unit
           rackunit
           smalltalk/reader
           syntax/parse
           syntax/parse/define
           "parse/block.rkt"
           "parse/expr.rkt"
           "parse/interface.rkt"
           "parse/message.rkt"
           "parse/primary.rkt"
           "parse/statement.rkt")

  (define-values/invoke-unit/infer
    (export st:expr^
            st:message^
            st:statement^
            st:block^)
    (link default-st:primary@
          default-st:message@
          default-st:expr@
          default-st:statement@
          default-st:block@))

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

  (define-syntax-parse-rule (test-parse* test/p:id str pats ...)
    (test-case (~a "parse " (~s str) " with " 'test/p)
      (call-with-input-string str
        (lambda (inp)
          (define-values (parsed rest)
            (parse test/p (port->stream inp)))
          (with-check-info*
              (list (make-check-info 'parsed parsed))
            (lambda ()
              (check-true (stream-empty? rest) "unparsed tail")
              (check-true (list? parsed) "parsed is not list of syntax")
              (check-true (andmap syntax? parsed)
                          "parsed is not list of syntax")
              (check-true
               (syntax-parse #`(#,@parsed)
                 [(pats ...) #t]
                 [_ #f])
               "syntax does not match patterns")))))))

  (test-parse st:message/p "42" 42)
  (test-parse st:message/p "a" {~datum a})
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

  (test-parse st:expr/p
              "Object new"
              ({~datum #%st:send} {~datum Object} {~datum new}))
  (test-parse st:expr/p
              "x := 3 "
              ({~datum #%st:assignment} {~datum x} 3))
  (test-parse st:expr/p
              "x := 3 + 4"
              ({~datum #%st:assignment}
               {~datum x}
               ({~datum #%st:send} 3 {~datum +} 4)))
  (test-parse st:expr/p
              "y := x := 3 "
              ({~datum #%st:assignment}
               {~datum y}
               ({~datum #%st:assignment} {~datum x} 3)))
  (test-parse st:expr/p
              "x := y"
              ({~datum #%st:assignment}
               {~datum x} {~datum y}))
  (test-parse st:expr/p
              "x := y squared"
              ({~datum #%st:assignment}
               {~datum x}
               ({~datum #%st:send} {~datum y} {~datum squared})))

  (test-parse st:statement/p
              "x := y"
              ({~datum #%st:assignment}
               {~datum x} {~datum y}))
  (test-parse st:statement/p
              "^42"
              ({~datum #%st:return} 42))
  (test-parse st:statement/p
              "^x := y"
              ({~datum #%st:return}
               ({~datum #%st:assignment}
                {~datum x} {~datum y})))

  (test-parse* st:statements/p "3" 3)
  (test-parse* st:statements/p "3. 4" 3 4)

  (test-parse st:block/p
              "[ 42 ]"
              ({~datum #%st:block} () () 42))
  (test-parse st:block/p
              "[ 42. ]"
              ({~datum #%st:block} () () 42))
  (test-parse st:block/p
              "[ 42. 43 ]"
              ({~datum #%st:block} () () 42 43))
  (test-parse st:block/p
              "[|x| x := 42. x + 1 ]"
              ({~datum #%st:block} () ({~datum x})
               ({~datum #%st:assignment} {~datum x} 42)
               ({~datum #%st:send} {~datum x} + 1)))
  (test-parse st:block/p
              "[| | 42 ]"
              ({~datum #%st:block} () () 42))
  (test-parse st:block/p
              "[|| 42 ]"
              ({~datum #%st:block} () () 42))
  )
