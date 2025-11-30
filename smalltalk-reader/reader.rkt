#lang racket/base

(require racket/sequence
         racket/syntax-srcloc
         syntax/srcloc
         syntax/strip-context
         "exn.rkt"
         "lexer.rkt")

(provide st:read-syntax)

(module* reader-support #f
  (provide st:read-syntax
           read-language)

  (define (maybe-read-language in)
    (define (scan i)
      (define ch (peek-char in i))
      (cond
        [(eof-object? ch) #f]
        [(char-blank? ch) (scan (add1 i))]
        [(char=? ch #\newline) #f]
        [else i]))
    (define skip (scan 0))
    (cond
      [skip
       (read-string skip in)
       (read in)]
      [else #f]))

  (define (read-language in)
    (or (maybe-read-language in)
        '(submod smalltalk/reader print-lang))))

(module* reader syntax/module-reader
  #:language read-language
  #:read (λ (in) (syntax->datum (st:read-syntax #f in)))
  #:read-syntax st:read-syntax
  #:whole-body-readers? #t
  (require (submod ".." reader-support)))

(module print-lang racket/base
  (require racket/pretty)
  (provide (rename-out [module-begin #%module-begin]))
  (define-syntax-rule (module-begin expr ...)
    (#%module-begin
     (pretty-print (quote expr)) ...)))

(define (st:read-syntax src in)
  (define group*
    (read-top in))
  (strip-context group*))

(define (stx-cons v vs)
  (datum->syntax #f (cons v vs) (build-source-location v (syntax-srcloc vs))))

(define stx-null (datum->syntax #f null #f))

(define (wrong-closer-error tok)
  (raise-read-error (format "wrong closer: ~a" tok) (token-srcloc tok)))

(define (unexpected-eof-error)
  (raise-read-eof-error "unexpected eof"))

(define (read-top in)
  (read-group* in eof-object?))

(define (read-group* in multi-closer?)
  (define (group-closer? tok)
    (or (multi-closer? tok) (dot? tok)))
  (define (more)
    (define next (peek-token in))
    (cond
      [(multi-closer? next) (lex-token in) stx-null]
      [(closer? next) (wrong-closer-error next)]
      [else
       (define g (read-group in group-closer?))
       (define g* (more))
       (stx-cons g g*)]))
  (more))

(define (read-group in group-closer?)
  (define t* (read-term* in group-closer?))
  (when (dot? (peek-token in)) (lex-token in))
  (datum->syntax #f (cons 'group t*) t*))

(define (read-term* in group-closer?)
  (define (read-and-more rd) (stx-cons (rd in) (more)))
  (define (more)
    (define tok (peek-token in))
    (cond
      [(group-closer? tok) stx-null]
      [(literal? tok)         (read-and-more read-literal)]
      [(identifier? tok)      (read-and-more read-identifier)]
      [(binary-selector? tok) (read-and-more read-binary-selector)]
      [(keyword? tok)         (read-and-more read-keyword)]
      [(delimiter? tok)       (read-and-more read-delimiter)]
      [(block-argument? tok)  (read-and-more read-block-argument)]
      [(opener? tok)          (read-and-more read-nested)]
      [(eof-object? tok)      (unexpected-eof-error)]
      [(closer? tok)          (wrong-closer-error tok)]
      [(not tok)              (lex-token in)]
      [else
       (raise-read-error (format "unknown token: ~a" tok)
                         (token-srcloc tok))]))
  (more))

(define (read-nested in)
  (define open-tok (lex-token in))
  (define shape (token-value open-tok))
  (define (nested-closer? tok)
    (and (closer? tok)
         (eq? shape (token-value tok))))
  (define nest (read-group* in nested-closer?))
  (datum->syntax #f (cons shape nest) (syntax-srcloc nest)))

(define ((make-read-terminal [val->sexp values]) in)
  (define tok (lex-token in))
  (datum->syntax #f (val->sexp (token-value tok)) (token-srcloc tok)))

(define read-literal         (make-read-terminal))
(define read-identifier      (make-read-terminal))
(define read-binary-selector (make-read-terminal (λ (v) (cons 'binsel v))))
(define read-keyword         (make-read-terminal (λ (v) (cons 'kwsel v))))
(define read-block-argument  (make-read-terminal (λ (v) (cons 'blockarg v))))
(define read-delimiter       (make-read-terminal (λ (v) (cons 'delim v))))