#lang racket/base

(require (prefix-in - syntax/readerr)
         syntax/srcloc)

(provide raise-read-error
         raise-read-eof-error)

(define ((make-raise-read-error raise-proc) message srcloc)
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