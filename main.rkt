#lang racket

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

;; 'ix' is (hash/c sym (listof (list/c mp phase kind))).
(define* (build-url-table ix pick-best-v
                          #:xref [xref (load-collections-xref)])
  (define lst '())
  (for (((k vs) ix))
    (define best-v (pick-best-v vs))
    ;;(writeln `(best ,k ,best-v))
    (define mp (first best-v))
    (define phase (second best-v))
    (define tag (xref-binding->definition-tag 
		 xref 
		 (list mp k) phase))
    (when tag
      (define-values (path anchor)
	(xref-tag->path+anchor xref tag))
      (when path
	;; 'anchor' may be #f.
	(set! lst (cons 
		   (list k (path+anchor->url-string path anchor)) 
		   lst)))))
  (set! lst
	(sort lst string<?
	      #:key (lambda (x)
		      (symbol->string (car x)))))
  lst)

;; 'lst' is (listof (list/c sym url-string)).
(define* (write-url-table/defvar lst out)
  (displayln "(defvar racket-url-lookup-table '(" out)
  (for-each 
   (lambda (x)
     (display "(" out)
     (display (elisp-escape-symbol (first x)) out)
     (display " " out)
     (write (second x) out)
     (displayln ")" out))
   lst)
  (displayln ") \"API doc URLs for Racket symbols\")" out))
  
