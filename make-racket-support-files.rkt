#!/usr/bin/env racket
#lang racket

#|

|#

(define writeln
  (case-lambda
    ((datum) (begin (write datum) (newline)))
    ((datum out) (begin (write datum out) (newline out)))))

(define pretty-println
  (case-lambda
    ((datum) (begin (pretty-print datum) (newline)))
    ((datum out) (begin (pretty-print datum out) (newline out)))))

(define (warn msg datum)
  (printf "WARNING: ~a: ~s~n" msg datum))

;;; 
;;; module paths
;;; 

(define interesting-modules
  '('#%kernel

    racket

    racket/async-channel  
    racket/base  
    racket/block  
    racket/bool  
    racket/bytes  
    racket/class  
    racket/cmdline  
    racket/contract  
    racket/contract/combinator  
    racket/contract/parametric  
    racket/contract/region  
    racket/control  
    racket/date  
    racket/dict  
    racket/draw  
    racket/draw/draw-sig  
    racket/draw/draw-unit  
    racket/enter  
    racket/fasl  
    racket/file  
    racket/fixnum  
    racket/flonum  
    racket/function  
    racket/future  
    racket/generator  
    racket/gui  
    racket/gui/base  
    racket/gui/dynamic  
    racket/gui/init  
    racket/help  
    racket/include  
    racket/init  
    racket/language-info  
    racket/list  
    racket/load  
    racket/local  
    racket/match  
    racket/math  
    racket/mpair  
    racket/package  
    racket/path  
    racket/place  
    racket/port  
    racket/pretty  
    racket/promise  
    racket/provide  
    racket/provide-syntax  
    racket/provide-transform  
    racket/require  
    racket/require-syntax  
    racket/require-transform  
    racket/runtime-config  
    racket/runtime-path  
    racket/sandbox  
    racket/sequence  
    racket/serialize  
    racket/set  
    racket/shared  
    racket/snip  
    racket/splicing  
    racket/stream  
    racket/string  
    racket/struct-info  
    racket/stxparam  
    racket/stxparam-exptime  
    racket/surrogate  
    racket/syntax  
    racket/system  
    racket/tcp  
    racket/trace  
    racket/trait  
    racket/udp  
    racket/unit  
    racket/unit-exptime  
    racket/unsafe/ops  
    racket/vector  

    syntax/boundmap  
    syntax/context  
    syntax/define  
    syntax/docprovide  
    syntax/flatten-begin  
    syntax/free-vars  
    syntax/id-table
    syntax/kerncase  
    syntax/keyword
    syntax/location  
    syntax/modcode
    syntax/modcollapse  
    syntax/moddep
    syntax/modread  
    syntax/modresolve
    syntax/module-reader  
    syntax/name  
    syntax/parse  
    syntax/parse/debug  
    syntax/parse/define  
    syntax/path-spec  
    syntax/readerr  
    syntax/srcloc  
    syntax/strip-context  
    syntax/struct  
    syntax/stx  
    syntax/template  
    syntax/to-string  
    syntax/toplevel  
    syntax/trusted-xforms

    scribble/xref

    setup/dirs
    setup/xref

    xml
    
    net/url
    net/url-structs
    ))

(define (mp-primitive? mp)
  (match mp
    ((list 'quote _) #t)
    (_ #f)))

(define (mp-submod? mp)
  (match mp
    ((list 'submod _ ...) #t)
    (else #f)))

(define (mp/lib->symbolic mp)
  (match mp
    ((list 'quote (? symbol? sym)) mp)
    ((list 'lib s)
     (if (not (string? s))
         #f
         (let ((r (regexp-match #rx"^(.*)[.]rkt$" s)))
           (and r
                (string->symbol (second r))))))
    (_ #f)))

#;
(begin
  (mp/lib->symbolic '(lib "racket/base.rkt"))
  (mp/lib->symbolic '(quote #%kernel))
  (mp/lib->symbolic ''#%kernel))

(define (mp/symbolic->string mp)
  (match mp
    ((list 'quote sym)
     (format "'~a" sym))
    (_ (let ((s (symbol->string mp)))
	 (define r (regexp-match #rx"^(.*)/main$" s))
	 (if r (second r) s)))))

;; Output strings are only for display to the user. We do our best to
;; get a compact module path string, but we even accept malformed
;; input.
(define (mp->string mp)
  (cond
   ((symbol? mp) (symbol->string mp))
   ((mp/lib->symbolic mp) => mp/symbolic->string)
   (else
    ;;(warn "weird module path" mp)
    (format "~s" mp))))

#;
(begin
  (mp->string '(lib "racket/base.rkt"))
  (mp->string '(lib "racket/main.rkt"))
  (mp->string '(quote #%kernel))
  (mp->string ''#%kernel))

(define (mp-exclude? mp)
  (match mp
    ((list 'lib s)
     (regexp-match 
      #rx"(?:/private/|^(?:mz(?:lib|scheme)|scheme)(?:/|$))"
      s))
    ((list 'submod _ ...)
     #t)
    (_ #f)))

#;
(begin
  (mp-exclude? ''#%kernel)
  (mp-exclude? '(lib "foo/private/bar.rkt"))
  (mp-exclude? '(lib "racket.rkt"))
  )

;;; 
;;; blueboxes index
;;; 

(require "blueboxes.rkt")

(define (under? pat s)
  (and 
   (regexp-match 
    (regexp 
     (string-append "^" 
		    (regexp-quote pat)
		    "(?:/|[.]|$)")) s)
   #t))

(define (exact? pat s)
  (and 
   (regexp-match 
    (regexp 
     (string-append "^" 
		    (regexp-quote pat)
		    "(?:[.]|$)")) s)
   #t))

(define (mp-rank mp)
  (match mp
    ((list 'quote sym)
     (cond
      ((eq? '#%kernel sym) 200)
      ((eq? '#%builtin sym) 190)
      (else 10)))
    ((list 'lib str)
     (cond
      ((string=? "racket/base.rkt" str) 300)
      ((string=? "racket/main.rkt" str) 290)
      ((under? "racket/base" str) 100)
      ((under? "racket" str) 90)
      ((under? "syntax" str) 80)
      ((under? "scribble" str) 70)
      ((under? "setup" str) 60)
      ((under? "net" str) 50)
      ((under? "unstable" str) 40)
      ((under? "srfi" str) 30)
      ((under? "mzscheme" str) -10)
      ((under? "mzlib" str) -20)
      (else 0)))
    (_ -50)))

#;
(begin
  (for-each
   (lambda (mp)
     (writeln (list mp (mp-rank mp))))
   '((lib "racket/gui/init.rkt")
     (lib "racket/gui.rkt")
     (lib "racket/base.rkt")
     (lib "racket/main.rkt")
     (lib "scribble/core.rkt")
     (lib "deinprogramm/DMdA.rkt")
     '#%unsafe
     '#%kernel)))

;; fetch-strs-for-single-tag returned strings contain odd spaces.
(define (replace-weird-spaces s)
  (regexp-replace* #rx"\u00A0" s " "))

(define (build-tag-index files->tag->offset)
  (define ix (make-hash))
  (for-each
   (lambda (x)
     (define h (third x))
     (for (((tag v) h))
       ;;(writeln tag)
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
;; module that has a given symbol.
;; ix:: as generated by build-tag-index
;; n-ix:: a hasheq into which to add the results
(define (rank-tag-index ix)
  (define ranks (make-hasheq))
  (define n-ix (make-hasheq))
  (for (((k v) ix))
    (define mp (first k))
    (define name (second k))
    (define t-rank (mp-rank mp))
    (define o-rank (hash-ref ranks name #f))
    (unless (and o-rank (> o-rank t-rank))
      (hash-set! ranks name t-rank)
      (hash-set! n-ix name v)))
  n-ix)

(define (make-ranked-tag-index)
  (define files->tag->offset (fetch-files->tag->offset))
  (define ix (rank-tag-index (build-tag-index files->tag->offset)))
  ix)

;;; 
;;; symbol index
;;; 

(require syntax/moddep)

(define (new-ix)
  (make-hasheq))

;; 'sym' is an exported symbol. 'mp' is a module name. 'phase' is a
;; phase level. 'kind' is either 'def' (for a value) or 'form' (for
;; syntax).
(define (ix-add ix sym mp phase kind)
  ;;(writeln (list 'ix-add sym mp phase kind))
  (unless (or (not phase) (mp-exclude? mp))
    (define lst (hash-ref ix sym '()))
    (define v (list mp phase kind))
    (unless (member v lst)
      (hash-set! ix sym (cons v lst)))))

;; Returns a list of symbols.
(define (ix-syms-list ix)
  (hash-keys ix))

(define (get-exports-and-imports mp)
  (cond
   ((mp-primitive? mp)
    (let-values (((vals stxs) (module->exports mp)))
      (let ((imports (module->imports mp)))
	(values vals stxs imports))))
   ((mp-submod? mp)
    (values #f #f #f))
   (else
    (let ()
      ;; We need not resolve relative requires, hence #f. Normally
      ;; yields an actual path, but just a symbol for primitive
      ;; modules.
      (define path (resolve-module-path mp #f))
      
      ;; Requires actual path as an argument. Hence we cannot get
      ;; code for primitive modules like this.
      (define c-exp (get-module-code path))
      
      ;; Note that (module->exports ''#%kernel) does work.
      (let-values (((vals stxs) (module-compiled-exports c-exp)))
	(let ((imports (module-compiled-imports c-exp)))
	  (values vals stxs imports)))))))

;; Returns (values seen syms), see below.
(define (scan)
  ;; 'mods' is a list of modules still to examine.
  (define mods interesting-modules)
  ;; 'seen' is a set of modules already seen.
  (define seen (set))
  ;; 'syms' is a hash map of exported symbols, with symbols as keys,
  ;; regardless of exporting module or phase level.
  (define syms (new-ix))

  ;; 'mp' is a module name. 'kind' is either 'def' (for a value) or
  ;; 'form' (for syntax).
  (define (add-exports mp kind xs)
    (when (symbol? mp)
      (set! mp (collapse-module-path mp ''#%dummy)))
    ;;(pretty-println (list mp kind xs)) (exit)
    (for-each
     (lambda (x)
       ;;(pretty-println x) (exit)
       (define phase (car x))
       (define lst (cdr x))
       ;;(writeln (list mp kind phase x)) (exit)
       (for-each
	(lambda (y)
	  (define sym (car y))
	  (define origin-lst (cadr y))
	  ;;(writeln `(module ,mp kind ,kind phase ,phase symbol ,sym origin ,origin-lst)) (exit)
	  (ix-add syms sym mp phase kind))
	lst))
     xs))
  
  (let next ()
    (if (null? mods)
        (values seen syms)
        (let () ;; for a body context
          (define cur-mp (car mods))
	  ;;(writeln `(cur-mp ,cur-mp))

          (set! mods (cdr mods))
          (when (set-member? seen cur-mp)
            (next))
          (set! seen (set-add seen cur-mp))

	  (let-values (((vals stxs imports)
			(get-exports-and-imports cur-mp)))
	    (when vals
	      (unless (mp-exclude? cur-mp)
		(add-exports cur-mp 'def vals)
		(add-exports cur-mp 'form stxs))

	      (for-each
	       (lambda (import)
		 (define phase (car import))
		 (when phase ;; skip label phase
		   (define mpis (cdr import))
		   (define n-mods
		     (filter
		      (lambda (x)
			(and x (not (set-member? seen x))))
		      (map
		       (lambda (mpi)
			 ;; This gives us 'lib' module paths it seems,
			 ;; or (quote #%something) paths.
			 (define t-lib-mp 
			   (collapse-module-path-index mpi cur-mp))
			 ;;(writeln t-lib-mp)
			 t-lib-mp)
		       mpis)))
		   (set! mods (append mods n-mods))))
	       imports)))
          (next)))))

;;; 
;;; module/phase availability help
;;; 

;; Supports the label phase (i.e., #f) also.
(define (phase<? x y)
  (cond
   ((and (not x) (not y))
    #f)
   ((not x)
    #f)
   ((not y)
    #t)
   (else
    (< x y))))

(define (collate-by-module lst)
  (define (sort-by-phase xs)
    (sort xs phase<? #:key car))
  (define h (make-hash))
  (for ((v lst))
    (define e (hash-ref h (car v) '()))
    (set! e (cons (cdr v) e))
    (hash-set! h (car v) e))
  (for/list (((k v) h))
    (list k (sort-by-phase v))))

(define (collate-by-module-and-rank lst)
  (sort (collate-by-module lst) > 
	#:key (lambda (x) (mp-rank (car x)))
	#:cache-keys? #t))

;; name-string, (listc (mp phase kind)) -> help-string
(define (hover-text-from-ix name ix-e)
  ;;(writeln (list name ix-e)) (exit)
  (define lst (collate-by-module-and-rank ix-e))
  ;;(writeln (list 'collated name lst)) (exit)
  (define (mk-phase-text x)
    (define phase (first x))
    (define kind (second x))
    (format "~a~a" phase (if (eq? kind 'def) "v" "s")))
  (define (mk-sq-text x)
    (string-join
     (map mk-phase-text x)
     ","))
  (define (mk-mod-text x)
    (format "~a[~a]" (mp->string (first x))
	    (mk-sq-text (second x))))
  (define txt
    (string-join
     (map mk-mod-text lst)       
     "\n"))
  txt)

;;; 
;;; dictionary generation
;;; 

(define extra-words
  '(("#hash" "literal start") 
    ("#hasheq" "literal start") 
    ("#hasheqv" "literal start") 
    ("#lang" "directive") 
    ("#px" "literal start") 
    ("#reader" "directive") 
    ("#rx" "literal start") 
    ("DEPRECATED" "comment")
    ("FIXME" "comment")
    ("TODO" "comment")
    ("racket" "module")))

(define (modnames-for-dictionary mods)
  ;;(for ((mp mods) #:unless (mp-exclude? mp)) (writeln (list mp (mp->string mp))))
  (filter
   values
   (map
    (lambda (mp)
      (if (mp-exclude? mp)
	  #f
	  (mp->string mp)))
    (set->list mods))))

(define (make-dictionary-file/plain mods ix filename)
  (define modnames (modnames-for-dictionary mods))
  ;;(pretty-println modnames)
  (define exports
    (ix-syms-list ix))
  (define all-names
    (sort (append
           (filter
            (lambda (s)
              (> (string-length s) 1))
            (map symbol->string exports))
	   modnames
           (map car extra-words))
          string<?))
  (call-with-output-file* 
   filename 
   (lambda (out)
     (for-each 
      (curryr displayln out)
      all-names))
   #:exists 'truncate/replace)
  (void))

;; ix:: hasheq(name -> (list/c (mp phase kind)))
(define (make-dictionary-file/hover mods ix filename)
  (define modnames (modnames-for-dictionary mods))

  (define help-h (make-hash))
  (for (((k v) ix))
    (define name (symbol->string k))
    (when (> (string-length name) 1)
      (define txt (hover-text-from-ix name v))
      (hash-set! help-h name txt)))

  (when (blueboxes?)
    (define tag-h (make-ranked-tag-index))
    (for (((k v) tag-h))
      ;; Will override any help text already set above.
      (hash-set! help-h (symbol->string k)
		 (string-join v "\n"))))

  (for ((m modnames))
    (hash-set! help-h m "module"))
  (for ((x extra-words))
    (hash-set! help-h (first x) (second x)))

  (define help-lst
    (sort
     (for/list (((k v) help-h))
       (list k v))
     string<?
     #:key car))

  (define (el-basename path)
    (let-values 
	(((base name dir) (split-path path)))
      (path->string (path-replace-suffix name ""))))
  (define (w-f out)
    (displayln ";; generated -- do not edit" out)
    (displayln "(defvar racket-dictionary-with-help '(" out)
    (for-each (curryr writeln out) help-lst)
    (displayln ") \"public exports in Racket\")" out)
    (displayln (format "(provide '~a)" (el-basename filename)) out)
    (void))
  (call-with-output-file* 
   filename w-f
   #:exists 'truncate/replace)
  (void))

(define (make-dictionary-file mods ix filename)
  (if (hover-help?)
      (make-dictionary-file/hover mods ix filename)
      (make-dictionary-file/plain mods ix filename)))

;;; 
;;; URL lookup file generation
;;; 

(require net/url net/url-structs setup/dirs setup/xref scribble/xref)

(define (module-phase-rank v)
  (define mp (first v))
  (define phase (second v))
  (+ (mp-rank mp)
     (- 9 (abs phase))))

;; (list/c (listof mp phase kind))
(define (rank-vs vs)
  ;;(writeln (map (lambda (x) (cons (module-phase-rank x) x)) vs)) (exit)
  (sort vs > #:key module-phase-rank #:cache-keys? #t))

(define (path+anchor->url-string path anchor)
  (define url
    (url->string
     (path->url path)))
  (when anchor
    (set! url (string-append url "#" anchor)))
  url)

(define (elisp-escape-symbol s)
  (set! s (symbol->string s))
  (regexp-replace* #rx"([^a-zA-Z0-9+=*/_~!@$%^&:<>{}?-])" s "\\\\\\1"))

(define (make-url-table-file ix filename)
  (define xref (load-collections-xref))
  (define lst '())
  (for (((k v) ix))
    (define best-v (first (rank-vs v)))
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

  (call-with-output-file* 
   filename 
   (lambda (out)
     (displayln ";; generated -- do not edit" out)

     (writeln `(defvar racket-doc-dir ,(path->string (find-doc-dir))
		 "Racket installation's 'doc' directory") out)

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
   #:exists 'truncate/replace)

  (void))

;;; 
;;; main
;;; 

(define blueboxes? (make-parameter #f))
(define hover-help? (make-parameter #f))

(module* main #f
  (define dictionary-file (make-parameter #f))
  (define url-table-file (make-parameter #f))

  (command-line
   #:once-each
   (("--signatures") "use signatures as Help strings"
    (blueboxes? #t))
   (("-d" "--dictionary") filename "write dictionary"
    (dictionary-file filename))
   (("--hover") "include Help strings in dictionary"
    (hover-help? #t))
   (("-u" "--url-table") filename "write URL lookup table"
    (url-table-file filename))
   )
  (when (or (dictionary-file)
	    (url-table-file))
    (define-values (mods ix) (scan))
    ;;(pretty-println mods) (exit)
    ;;(pretty-println ix) (exit)
    (when (dictionary-file)
      (make-dictionary-file mods ix (dictionary-file)))
    (when (url-table-file)
      (make-url-table-file ix (url-table-file)))
    ))
