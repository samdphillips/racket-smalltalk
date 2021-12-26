#lang racket/base

(require racket/contract
         (only-in microparsec gen:to-srcloc))

(provide (contract-out
          [smalltalk-read
           (->* () (input-port?) (or/c eof-object? token?))]
          [token-value    (-> token? any)])
         token?
         token-integer?
         token-string?
         identifier?
         binary-selector?
         keyword?
         delimiter?
         opener?
         closer?)

(require racket/match
         (prefix-in - syntax/readerr)
         syntax/srcloc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(module+ test
  (require rackunit
           racket/port
           syntax/parse/define))

(define (->srcloc source pos)
  (and pos
       (match-let ([(position offset line column) pos])
         (vector (if (input-port? source)
                     (object-name source)
                     source)
                 line column offset 0))))

(define ((make-raise-read-error	raise-proc) message srcloc)
  (raise-proc message
              (source-location-source   srcloc)
              (source-location-line     srcloc)
              (source-location-column   srcloc)
              (source-location-position srcloc)
              (source-location-span     srcloc)))

(define raise-read-error
  (make-raise-read-error -raise-read-error))
(define raise-read-eof-error
  (make-raise-read-error -raise-read-eof-error))

(define ZERO (char->integer #\0))
(define NINE (char->integer #\9))

(define ALPHA (char->integer #\a))
(define ZED   (char->integer #\z))

(define (parse-nrm-number srcloc numeric-string)
  (define-values (base igits)
    (match numeric-string
      [(pregexp #px"^(\\d\\d?)r(.*)" (list _ base igits))
       (values (string->number base) igits)]
      [igits (values 10 igits)]))
  (when (or (< base 2) (> base 36))
    (raise-read-error
     (format "base (~a) out of range [2,36]" base) srcloc))
  (define (char->value c)
    (define v
      (let ([cp (char->integer (char-downcase c))])
        (cond
          [(<= ZERO cp NINE) (- cp ZERO)]
          [(<= ALPHA cp ZED) (+ 10 (- cp ALPHA))])))
    (cond
      [(< v base) v]
      [else
       (raise-read-error
        (format "value (~a) out of range for base (~a)" c base)
        srcloc)]))
  (for/fold ([v 0] #:result (token srcloc v))
            ([c (in-string igits)]
             #:unless (char=? c #\_))
    (+ (char->value c) (* base v))))

(module+ test
  (test-equal? "base 2"
               (token-value (parse-nrm-number #f "2r10")) 2)
  (test-equal? "base 6"
               (token-value (parse-nrm-number #f "6r10")) 6)
  (test-equal? "explicit base 10"
               (token-value (parse-nrm-number #f "10r10")) 10)
  (test-equal? "base 16"
               (token-value (parse-nrm-number #f "16r10")) 16)
  (test-equal? "base 16"
               (token-value (parse-nrm-number #f "16rFF")) 255)
  (test-equal? "base 16 separators"
               (token-value (parse-nrm-number #f "16rFE_FF")) 65279)

  (test-equal? "implicit base 10"
               (token-value (parse-nrm-number #f "10")) 10)
  (test-equal? "with separators"
               (token-value
                (parse-nrm-number #f "1_000_000"))
               1000000)

  (test-exn "out of range igit"
            exn:fail:read?
            (lambda () (parse-nrm-number #f "10rA")))
  (test-exn "out of range base"
            exn:fail:read?
            (lambda () (parse-nrm-number #f "37rA"))))

(struct token (srcloc$ value)
  #:methods gen:to-srcloc
  [(define (token-srcloc t) (token-srcloc$ t))]
  #:transparent)
(struct identifier      token () #:transparent)
(struct binary-selector token () #:transparent)
(struct keyword         token () #:transparent)
(struct delimiter       token () #:transparent)
(struct opener          token () #:transparent)
(struct closer          token () #:transparent)

(define ((make-token-value-pred pred?) tok)
  (and (token? tok) (pred? (token-value tok))))

(define token-integer? (make-token-value-pred integer?))
(define token-string?  (make-token-value-pred string?))

(define (make-token input-port start-pos end-pos tok value)
  (tok (build-source-location-vector
        (->srcloc input-port start-pos)
        (->srcloc input-port end-pos))
       value))

(define (lex-string start-loc input-port)
  (define out-string (open-output-string))
  (define lex
    (lexer
     [(eof) (raise-read-eof-error "EOF encountered reading string"
                                  (build-source-location-vector
                                   (->srcloc input-port start-loc)
                                   (->srcloc input-port end-pos)))]
     [(:* (:~ #\'))
      (begin
        (write-string lexeme out-string)
        (lex input-port))]
     [(:: #\' #\')
      (begin
        (write-char #\' out-string)
        (lex input-port))]
     [#\'
      (make-token
       input-port start-loc end-pos
       token (get-output-string out-string))]))
  (lex input-port))

(define smalltalk-lex
  (letrec-syntax ([$token
                   (syntax-rules ()
                     [(_ value) ($token token value)]
                     [(_ make value)
                      (make-token
                       input-port start-pos end-pos make value)])])
    (lexer
     ;; whitespace
     [(eof) eof]
     [(:+  whitespace)
      (smalltalk-lex input-port)]

     ;; comments
     [(:: #\" (:* (:~ #\")) #\")
      (smalltalk-lex input-port)]

     ;; integers - nrm format
     [(:: (:** 1 2 numeric) #\r (:+ (:or #\_ alphabetic numeric)))
      ($token parse-nrm-number lexeme)]

     ;; integers - base 10
     [(:: numeric (:* (:or #\_ numeric)))
      ($token parse-nrm-number lexeme)]

     ;; keywords
     [(:: (:or #\_ alphabetic)
          (:* alphabetic numeric)
          #\:)
      ($token keyword (string->symbol lexeme))]

     ;; binary selector
     [(:+ (char-set "~!@%&*-+=\\|,<>/"))
      ($token binary-selector (string->symbol lexeme))]
     
     ;; identifiers
     [(:: (:or #\_ alphabetic)
          (:* alphabetic numeric))
      ($token identifier (string->symbol lexeme))]

     ;; strings
     [(:: #\') (lex-string start-pos input-port)]

     ;; delimiters
     [#\.  ($token delimiter 'dot)]
     [#\;  ($token delimiter 'cascade)]
     [#\^  ($token delimiter 'caret)]
     [":=" ($token delimiter 'assignment)]

     ;; openers
     [(:or (char-set "([{") (:: #\# (char-set "([{")))
      ($token opener lexeme)]

     ;; closers
     [(char-set ")]}")
      ($token closer lexeme)]
     )))

(define (smalltalk-read [input-port (current-input-port)])
  (smalltalk-lex input-port))

(module+ test
  (define-syntax-parse-rule (check-tokens s pats ...)
    (check-match
     (call-with-input-string s
       (lambda (in)
         (for/list ([tok (in-port smalltalk-lex in)]) tok)))
     (list pats ...)))

  (test-case "identifier - abc"
             (check-tokens "abc" (identifier _ 'abc)))

  (test-case "binary selectors"
             (check-tokens "+ - / * @ < > <+>"
                           (binary-selector _ '+)
                           (binary-selector _ '-)
                           (binary-selector _ '/)
                           (binary-selector _ '*)
                           (binary-selector _ '@)
                           (binary-selector _ '<)
                           (binary-selector _ '>)
                           (binary-selector _ '<+>)))
  
  (test-case "keyword - abc:"
             (check-tokens "abc:" (keyword _ 'abc:)))

  (test-case "comments"
             (check-tokens
              "\"this is a comment\" abc" (identifier _ 'abc)))

  (test-case "numbers"
             (check-tokens
              "16rFF raisedTo: 2"
              (token _ 255) (keyword _ 'raisedTo:) (token _ 2)))

  (test-case "strings"
             (check-tokens
              "'one' 'two' 'three'"
              (token _ "one") (token _ "two") (token _ "three")))

  (test-case "escaped strings"
             (check-tokens "'''one'' two three'"
                           (token _ "'one' two three")))

  (test-case "no string termination"
             (check-exn exn:fail:read:eof?
                        (lambda () (check-tokens "'oops" ""))))

  (test-case "delimiters"
             (check-tokens ". ; ^"
                           (delimiter _ 'dot)
                           (delimiter _ 'cascade)
                           (delimiter _ 'caret))))
