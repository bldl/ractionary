#lang racket/base

#|
|#

(require "util.rkt")

;;; 
;;; Emacs Lisp
;;; 

(define* (elisp-escape-symbol s)
  (regexp-replace*
   #rx"([^a-zA-Z0-9+=*/_~!@$%^&:<>{}?-])"
   (symbol->string s)
   "\\\\\\1"))

(define* (elisp-escape-string s)
  ;; For now, until discover incompatibilities.
  (format "~s" s))
