#lang info
(define collection "ractionary")
(define name "Ractionary")
(define blurb
  '("Racket symbol dictionary generator."))
(define compile-omit-paths '("retired"))
(define deps '(("base" #:version "6.0")
               "data-lib" "racket-index" "scribble-lib"))
(define racket-launcher-libraries
  '("ractionary-make-dict.rkt" "ractionary-make-urls.rkt"))
(define racket-launcher-names
  '("ractionary-make-dict" "ractionary-make-urls"))
