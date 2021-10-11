#lang info

(define name "smalltalk-parser")
(define collection "smalltalk")
(define version "0.0.1")
(define deps
  '("base"
    "smalltalk-reader"
    "threading-lib"
    "https://github.com/samdphillips/racket-microparsec.git?path=microparsec-lib"))
(define build-deps '("base" "rackunit-lib"))

