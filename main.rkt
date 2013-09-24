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
;;; module paths
;;; 

(define* (rkt-string? s)
  (and (regexp-match #rx"[.]rkt$" s) #t))

(define* (rkt-basename s)
  (let ((r (regexp-match #rx"^(.*)[.]rkt$" s)))
    (and r (second r))))

(define (strip-any-main s)
  (define r (regexp-match #rx"^(.*)/main$" s))
  (if r (second r) s))

;; Output strings are only for display to the user. We do our best to
;; get a compact module path string, but we even accept malformed
;; input.
(define* (mp->string mp)
  (match mp
    ((? symbol?) (let ((s (symbol->string mp)))
                   (strip-any-main s)))
    ((list 'quote (? symbol? sym)) (format "'~a" sym))
    ((list 'lib (? string? s))
     (or (let-and bn (rkt-basename s) (strip-any-main bn))
         (format "~s" mp)))
    ((list (and (list 'lib _) sub-mp) (? symbol? sym))
     (format "container ~a in ~a" sym (mp->string sub-mp)))
    ((list 'form (? symbol? sym) kind sub-mp) ;; this is non-standard
     (format "~a ~a in ~a" (or kind 'thing) sym (mp->string sub-mp)))
    (else
     (format "~s" mp))))

#;
(begin
  (mp->string '(form m #f (lib "racket/base.rkt")))
  (mp->string '(form v def (lib "racket/base.rkt")))
  (mp->string '(lib "racket/base.rkt"))
  (mp->string '(lib "racket/main.rkt"))
  (mp->string 'racket/base)
  (mp->string 'racket/main)
  (mp->string '(quote #%kernel))
  (mp->string ''#%kernel))

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
  
;;; 
;;; blueboxes
;;; 

;; fetch-strs-for-single-tag returned strings contain odd spaces.
(define* (replace-weird-spaces s)
  (regexp-replace* #rx"\u00A0" s " "))

