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

;; Similar to mp->string but returns a datum of a "canonical" module
;; path only. For any kind of a container path (that is not separately
;; loadable), returns only the module path of the containing module.
;; Does not canonicalize relative module paths.
(define* (mp-like-extract-canonical-mp mp)
  (match mp
    ((? symbol?) (let ((s (symbol->string mp)))
                   (string->symbol (strip-any-main s))))
    ((list 'quote (? symbol? sym)) mp)
    ((list 'lib (? string? s))
     ;; Note that here 's' must be absolute within a collection.
     (if-let bn (rkt-basename s)
       (string->symbol (strip-any-main bn))
       mp))
    ((list 'submod sub-mp (? symbol? name))
     (list 'submod (mp-like-extract-canonical-mp sub-mp) name))
    ((list (and (list 'lib _) sub-mp) (? symbol? sym))
     (mp-like-extract-canonical-mp sub-mp))
    ((? path-string?) mp) ;; cannot canonicalize, may be relative
    (else
     (error 'mp-like-extract-canonical-mp
            "unsupported module-path like: ~s" mp))))
  
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

(require "blueboxes.rkt")

;; fetch-strs-for-single-tag returned strings contain odd spaces.
(define* (replace-weird-spaces s)
  (regexp-replace* #rx"\u00A0" s " "))

(define* (build-tag-index
          #:offsets [files->tag->offset #f]
          #:search-dirs [search-dirs #f])
  (unless files->tag->offset
    (set! files->tag->offset
          (if search-dirs
              (fetch-files->tag->offset #:search-dirs search-dirs)
              (fetch-files->tag->offset))))
  (define ix (make-hash))
  (for-each
   (lambda (x)
     (define h (third x))
     (for (((tag v) h))
       (match tag
	 ((list kind (list mp name))
	  (define ix-k `(,mp ,name))
	  ;; 'meth' kind does not actually have a module path, but
	  ;; mp->string is forgiving.
	  ;;(when (eq? kind 'meth) (writeln tag))
	  (define n-strs 
	    (cons
	     (format "~a in ~a:" kind (mp->string mp))
	     (map
	      replace-weird-spaces
	      (fetch-strs-for-single-tag files->tag->offset tag))))
	  (define o-strs (hash-ref ix ix-k #f))
	  (hash-set! ix ix-k (if o-strs (append o-strs n-strs) n-strs)))
	 (_ (void)))))
   files->tag->offset)
  ix)

;; Keys the index just by symbol name by choosing the "most central"
;; module that has a given symbol. The result has no module paths,
;; except possibly in the help strings.
;;
;; ix:: as generated by build-tag-index,
;;      i.e. (hash/c (list/c mp sym) (listof help-string))
;; mp-rank:: mp -> rank-number
;; Returns:: (hash/c sym (listof help-string))
(define* (tag-index-retain-best ix mp-rank)
  (define ranks (make-hasheq)) ;; (hash/c sym rank)
  (define n-ix (make-hasheq)) ;; (hash/c sym (listof help-string))
  (for (((k v) ix))
    (define mp (first k))
    (define name (second k))
    (define t-rank (mp-rank mp))
    (define o-rank (hash-ref ranks name #f))
    (unless (and o-rank (> o-rank t-rank))
      (hash-set! ranks name t-rank)
      (hash-set! n-ix name v)))
  n-ix)

(define* module-path-like/c
  (or/c module-path? pair?))

;; This function puts a help string index into a form that allows for
;; easy sorting of the help texts by module path.
;;
;; ix:: (hash/c (list/c mp sym) (listof help-string))
;; Returns:: (hash/c sym (listof (list/c mp (listof help-string))))
(define-with-contract*
  (-> (hash/c (list/c module-path-like/c symbol?) (listof string?))
      (hash/c symbol? (listof (list/c module-path-like/c (listof string?)))))
  (tag-index->by-symbol ix)
  (define n-ix (make-hasheq))
  (for (((k v) ix))
    (define mp (first k))
    (define name (second k))
    (hash-update! n-ix name
                  (lambda (lst)
                    (cons (list mp v) lst))
                  null))
  n-ix)

