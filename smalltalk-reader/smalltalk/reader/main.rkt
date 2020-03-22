#lang racket/base

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
  (match-define (position offset line column) pos)
  (vector (if (input-port? source)
              (object-name source)
              source)
          line column offset 0))

(define ((make-raise-read-error	raise-proc) message start-loc end-loc)
  (let ([srcloc (build-source-location-vector start-loc end-loc)])
    (raise-proc message
                (source-location-source   srcloc)
                (source-location-line     srcloc)
                (source-location-column   srcloc)
                (source-location-position srcloc)
                (source-location-span     srcloc))))

(define raise-read-error (make-raise-read-error -raise-read-error))
(define raise-read-eof-error (make-raise-read-error -raise-read-eof-error))

(define ZERO (char->integer #\0))
(define NINE (char->integer #\9))

(define ALPHA (char->integer #\a))
(define ZED   (char->integer #\z))

(define (parse-nrm-number s)
  (define-values (base igits)
    (match s
      [(pregexp #px"^(\\d\\d?)r(.*)" (list _ base igits))
       (values (string->number base) igits)]
      [igits (values 10 igits)]))
  (define (char->value c)
    (define v
      (let ([cp (char->integer (char-downcase c))])
        (cond
          [(<= ZERO cp NINE) (- cp ZERO)]
          [(<= ALPHA cp ZED) (+ 10 (- cp ALPHA))])))
    (cond
      [(< v base) v]
      [else
        (error 'parse-nrm-number "out of range: ~s" c)]))
  (for/fold ([v 0]) ([c (in-string igits)]
                     #:unless (char=? c #\_))
    (+ (char->value c) (* base v))))

(module+ test
  (test-equal? "base 2" (parse-nrm-number "2r10") 2)
  (test-equal? "base 6" (parse-nrm-number "6r10") 6)
  (test-equal? "explicit base 10"
               (parse-nrm-number "10r10") 10)
  (test-equal? "base 16" (parse-nrm-number "16r10") 16)
  (test-equal? "base 16" (parse-nrm-number "16rFF") 255)
  (test-equal? "base 16 separators" (parse-nrm-number "16rFE_FF") 65279)

  (test-equal? "implicit base 10" (parse-nrm-number "10") 10)
  (test-equal? "with separators"  (parse-nrm-number "1_000_000") 1000000))

(struct identifier (value) #:transparent)
(struct keyword (value) #:transparent)

(define (lex-string start-loc input-port)
  (define out-string (open-output-string))
  (define lex
    (lexer
      [(eof) (raise-read-eof-error "EOF encountered reading string"
                                   start-loc
                                   (->srcloc input-port end-pos))]
      [(:* (:~ #\'))
       (begin
         (write-string lexeme out-string)
         (lex input-port))]
      [(:: #\' #\')
       (begin
         (write-char #\' out-string)
         (lex input-port))]
      [#\' (get-output-string out-string)]))
  (lex input-port))

(define smalltalk-lex
  (lexer-src-pos
    [(eof) (return-without-pos eof)]
    [(:+  whitespace)
     (return-without-pos (smalltalk-lex input-port))]

    [(:: #\" (:* (:~ #\")) #\")
     (return-without-pos (smalltalk-lex input-port))]

    [(:: (:** 1 2 numeric) #\r (:+ (:or #\_ alphabetic numeric)))
     (parse-nrm-number lexeme)]

    [(:: numeric (:* (:or #\_ numeric)))
     (parse-nrm-number lexeme)]

    [(:: (:or #\_ alphabetic)
         (:* alphabetic numeric)
         #\:)
     (keyword (string->symbol lexeme))]

    [(:: (:or #\_ alphabetic)
         (:* alphabetic numeric))
     (identifier (string->symbol lexeme))]

    [(:: #\') (lex-string (->srcloc input-port start-pos) input-port)]
  ))

(module+ test
  (define-simple-macro (check-tokens s pats ...)
    (check-match
      (call-with-input-string s
        (lambda (in)
          (for/list ([tok (in-port smalltalk-lex in)])
            (position-token-token tok))))
      (list pats ...)))

  (test-case "identifier - abc"      (check-tokens "abc" (identifier 'abc)))
  (test-case "keyword - abc:"        (check-tokens "abc:" (keyword 'abc:)))
  (test-case "comments"              (check-tokens "\"this is a comment\" abc" (identifier 'abc)))
  (test-case "numbers"               (check-tokens "16rFF raisedTo: 2" 255 (keyword 'raisedTo:) 2))
  (test-case "strings"               (check-tokens "'one' 'two' 'three'" "one" "two" "three"))
  (test-case "escaped strings"       (check-tokens "'''one'' two three'" "'one' two three"))
  (test-case "no string termination"
    (check-exn exn:fail:read:eof? (lambda () (check-tokens "'oops" ""))))
)