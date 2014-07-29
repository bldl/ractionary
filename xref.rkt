#lang racket/base

#|
|#

(require "util.rkt")

;;; 
;;; xref
;;;

(require net/url scribble/xref setup/xref)

(define* (path+anchor->url-string path anchor)
  (define url
    (url->string
     (path->url path)))
  (when anchor
    (set! url (string-append url "#" anchor)))
  url)

(define* (path+tag->url-string xref tag)
  (define-values (path anchor)
    (xref-tag->path+anchor xref tag))
  (path+anchor->url-string path anchor))
