#lang racket/base

(require microparsec
         racket/unit
         (only-in racket/stream stream->list)
         threading
         smalltalk/reader
         smalltalk/parse/block
         smalltalk/parse/expr
         smalltalk/parse/interface
         smalltalk/parse/message
         smalltalk/parse/method
         smalltalk/parse/primary
         smalltalk/parse/statement
         smalltalk/parse/top-decl)

(define-values/invoke-unit/infer
  (export st:statement^
          st:method^
          st:top-decl^)
  (link default-st:block@
        default-st:primary@
        default-st:message@
        default-st:expr@
        default-st:statement@
        default-st:method@
        default-st:top-decl@))

(define (string->tokens s)
  (~> (open-input-string s)
      (in-port smalltalk-read _)
      sequence->stream))

(define (p f s)
  (define-values (parsed rest) (parse f (string->tokens s)))
  (values parsed (stream->list rest)))

(module* parse-file #f
  (call-with-input-file (vector-ref (current-command-line-arguments) 0)
    (lambda (inp)
      (port-count-lines! inp)
      (parse st:top-decl/p (sequence->stream (in-port smalltalk-read inp))))))

(module* parse-string #f
  (p st:code-body/p (vector-ref (current-command-line-arguments) 0)))
