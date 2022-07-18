#lang racket/base

(module* test #f
  (require microparsec
           racket/format
           racket/match
           racket/port
           (only-in racket/stream stream-empty?)
           racket/unit
           rackunit
           smalltalk/reader
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
    (link default-st:block@
          default-st:primary@
          default-st:message@
          default-st:expr@
          default-st:statement@))

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
              (list (make-check-info 'parsed parsed)
                    (make-check-info 'pattern 'pat))
            (lambda ()
              (check-true (stream-empty? rest) "unparsed tail")
              (check-true (syntax? parsed) "parsed is not syntax")
              (check-true
               (match (syntax->datum parsed)
                 ['pat #t]
                 [_ #f])
               "syntax does not match pattern")))))))

  (define-syntax-parse-rule (test-parse* test/p:id str pats ...)
    (test-case (~a "parse " (~s str) " with " 'test/p)
      (call-with-input-string str
        (lambda (inp)
          (define-values (parsed rest)
            (parse test/p (port->stream inp)))
          (with-check-info*
              (list (make-check-info 'parsed parsed)
                    (make-check-info 'patterns '(pats ...)))
            (lambda ()
              (check-true (stream-empty? rest) "unparsed tail")
              (check-true (list? parsed) "parsed is not list of syntax")
              (check-true (andmap syntax? parsed)
                          "parsed is not list of syntax")
              (check-true
               (match (map syntax->datum parsed)
                 [(list 'pats ...) #t]
                 [_ #f])
               "syntax does not match patterns")))))))

  (test-parse st:message/p "42" 42)
  (test-parse st:message/p "a" a)
  (test-parse st:message/p "a toString"
              (#%st:send a toString))

  (test-parse st:message/p "123 factorial"
              (#%st:send 123 factorial))
  (test-parse st:message/p "12345 toString size"
              (#%st:send (#%st:send 12345 toString) size))

  (test-parse st:message/p "3 + 4"
              (#%st:send 3 + 4))
  (test-parse st:message/p "3 + 4 * 2"
              (#%st:send (#%st:send 3 + 4) * 2))
  (test-parse st:message/p "3 factorial + 4 factorial"
              (#%st:send (#%st:send 3 factorial) + (#%st:send 4 factorial)))
  (test-parse st:message/p "'x', 'y'"
              (#%st:send "x" |,| "y"))

  (test-parse st:message/p "4 raisedTo: 3 factorial"
              (#%st:send 4 raisedTo: (#%st:send 3 factorial)))
  (test-parse st:message/p "4 raisedTo: (3 factorial)"
              (#%st:send 4 raisedTo: (#%st:send 3 factorial)))
  (test-parse st:message/p "2 raisedTo: 5"
              (#%st:send 2 raisedTo: 5))
  (test-parse st:message/p "d at: 0 put: 1"
              (#%st:send d at:put: 0 1))
  (test-parse st:message/p "d at: 0 put: x + y"
              (#%st:send d at:put: 0 (#%st:send x + y)))

  (test-parse st:message/p
              "self new initialize: aCollection; add: 10; + 0; yourself"
              (#%st:send* (#%st:send self new)
                          (initialize: aCollection)
                          (add: 10)
                          (+ 0)
                          (yourself)))

  (test-parse st:expr/p
              "Object new"
              (#%st:send Object new))
  (test-parse st:expr/p
              "x := 3 "
              (#%st:assignment x 3))
  (test-parse st:expr/p
              "x := 3 + 4"
              (#%st:assignment x (#%st:send 3 + 4)))
  (test-parse st:expr/p
              "y := x := 3 "
              (#%st:assignment y (#%st:assignment x 3)))
  (test-parse st:expr/p
              "x := y"
              (#%st:assignment x y))
  (test-parse st:expr/p
              "x := y squared"
              (#%st:assignment x (#%st:send y squared)))

  (test-parse st:statement/p
              "x := y"
              (#%st:assignment x y))
  (test-parse st:statement/p
              "^42"
              (#%st:return 42))
  (test-parse st:statement/p
              "^x := y"
              (#%st:return (#%st:assignment x y)))

  (test-parse* st:statements/p "3" 3)
  (test-parse* st:statements/p "3. 4" 3 4)

  (test-parse st:block/p
              "[ 42 ]"
              (#%st:block () () 42))
  (test-parse st:block/p
              "[ 42. ]"
              (#%st:block () () 42))
  (test-parse st:block/p
              "[ 42. 43 ]"
              (#%st:block () () 42 43))
  (test-parse st:block/p
              "[|x| x := 42. x + 1 ]"
              (#%st:block () (x)
                          (#%st:assignment x 42)
                          (#%st:send x + 1)))
  (test-parse st:block/p
              "[| | 42 ]"
              (#%st:block () () 42))
  (test-parse st:block/p
              "[|| 42 ]"
              (#%st:block () () 42))
  )
