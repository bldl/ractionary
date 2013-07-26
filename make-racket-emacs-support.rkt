#!/usr/bin/env racket
#lang racket

#|

Currently this tool only builds a dictionary of some of the more
prominent public Racket names.

For a specified set of modules, we record all exports, and examine all
imports. The indirect dependencies of just about any given module are
huge (just use show-import-tree to see), and so we want to make sure
to avoid duplication of work.

module-compiled-imports and module-compiled-exports report all of the
relevant symbols regardless of phase level shifts. #f corresponds to
the label phase level, and said phase level is the only one we will
exclude. module->imports and module->exports provide the same
information, but the modules must have been declared.

We can tell what is syntax and what is a value. A problem here is that
because of contracts and such lots more symbols will appear to be
syntax that are actually defined as macros. This would lead to it
being confusing to highlight all syntax as keywords. So for now we are
not concerned with highlighting (let alone indentation), but just want
a dictionary.

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
  '(racket

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
    ))

(define (mp/symbolic->lib mp)
  `(lib ,(string-append (symbol->string mp) ".rkt")))

(define (mp/lib->symbolic mp)
  (match mp
    ((list 'lib s)
     (if (not (string? s))
         #f
         (let ((r (regexp-match #rx"^(.*)[.]rkt$" s)))
           (and r
                (string->symbol (second r))))))
    (_ #f)))

;;(mp/symbolic->lib 'racket/base)
;;(mp/lib->symbolic '(lib "racket/base.rkt"))

(define (mp-exclude? mp)
  (regexp-match #rx"/private/"
                (symbol->string mp)))

;;; 
;;; symbol index
;;; 

(require syntax/moddep)

(define (just-label? v)
  (andmap (lambda (x)
	    (not (second x))) v))

(define not-just-label? (negate just-label?))

(define (new-ix)
  (make-hasheq))

;; 'sym' is an exported symbol. 'mn' is a symbolic module name.
;; 'phase' is a phase level. 'kind' is either 'def' (for a value) or
;; 'form' (for syntax).
(define (ix-add ix sym mn phase kind)
  (define lst (hash-ref ix sym '()))
  (hash-set! ix sym 
	     (cons (list mn phase kind) lst)))

;; Returns a list of symbols.
(define (ix-syms ix)
  (hash-keys ix))

;; Returns a seteq of symbols.
(define (ix-syms/non-label ix)
  (for/seteq (((k v) ix)
	      #:when (not-just-label? v))
    k))

;; Returns (values seen syms), see below.
(define (scan)
  ;; 'mods' is a list of modules still to examine, by symbolic name.
  (define mods interesting-modules)
  ;; 'seen' is a set of modules already seen, by symbolic names.
  (define seen (seteq))
  ;; 'syms' is a hash map of exported symbols, with symbols as keys,
  ;; regardless of exporting module or phase level.
  (define syms (new-ix))

  ;; 'mn' is a symbolic module name. 'kind' is either 'def' (for a
  ;; value) or 'form' (for syntax).
  (define (add-exports mn kind xs)
    ;;(pretty-println (list mn kind xs))
    (for-each
     (lambda (x)
       (define phase (car x))
       (define lst (cdr x))
       ;;(writeln (list mn kind phase x)) (exit)
       (for-each
	(lambda (y)
	  (define sym (car y))
	  (define origin-lst (cadr y))
	  ;;(writeln `(module ,mn kind ,kind phase ,phase symbol ,sym origin ,origin-lst)) (exit)
	  (ix-add syms sym mn phase kind))
	lst))
     xs))
  
  (let next ()
    (if (null? mods)
        (values seen syms)
        (let () ;; for a body context
          (define cur-mp (car mods))
          (set! mods (cdr mods))
          (when (set-member? seen cur-mp)
            (next))
          (set! seen (set-add seen cur-mp))

          ;; We need not resolve relative requires, hence #f. Always yields
          ;; an actual path.
          (define path (resolve-module-path cur-mp #f))

          ;; Requires actual path as an argument.
          (define c-exp (get-module-code path))

          (unless (mp-exclude? cur-mp)
            (let-values (((vals stxs) (module-compiled-exports c-exp)))
              (add-exports cur-mp 'def vals)
              (add-exports cur-mp 'form stxs)))

          (let ((imports (module-compiled-imports c-exp)))
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
                       ;; This gives us 'lib' module paths it seems.
                       (define t-lib-mp (collapse-module-path-index mpi cur-mp))
                       (define t-sym-mp (mp/lib->symbolic t-lib-mp))
                       ;; (unless t-sym-mp
                       ;;   (warn "skipping non-'lib' module path"
                       ;;           t-lib-mp))
                       t-sym-mp)
                     mpis)))
                 (set! mods (append mods n-mods))))
             imports))
          (next)))))

;;; 
;;; dictionary generation
;;; 

(define (make-dictionary-file mods ix filename)
  (define modnames
    (set->list mods))
  (define exports
    (set->list (ix-syms/non-label ix)))
  (define extras
    (list "#t" "#f" "#lang" "DEPRECATED" "FIXME" "TODO"))
  (define all-names
    (sort (append
           (filter
            (lambda (s)
              (> (string-length s) 1))
            (map symbol->string exports))
           (map symbol->string 
                (filter (negate mp-exclude?) modnames))
           extras)
          string<?))
  (call-with-output-file* 
   filename 
   (lambda (out)
     (for-each 
      (curryr displayln out)
      all-names))
   #:exists 'truncate/replace)
  (void))

;;; 
;;; URL lookup file generation
;;; 

(require net/url net/url-structs setup/dirs setup/xref scribble/xref)

(define (under? pat s)
  (and 
   (regexp-match 
    (regexp 
     (string-append "^" 
		    (regexp-quote pat)
		    "(?:/|$)")) s)
   #t))

(define (compute-rank v)
  (define mn (first v))
  (define ms (symbol->string mn))
  (define phase (second v))
  (+
   (cond
    ((eq? mn 'racket/base) 100)
    ((eq? mn 'racket) 90)
    ((under? "racket" ms) 80)
    ((under? "syntax" ms) 70)
    ((under? "scribble" ms) 50)
    ((under? "setup" ms) 40)
    ((under? "net" ms) 30)
    ((under? "unstable" ms) 20)
    ((under? "srfi" ms) 10)
    (else 0))
   (- 9 (abs phase))))

;; (list/c (listof mn phase kind))
(define (rank-vs vs)
  ;;(writeln (map (lambda (x) (cons (compute-rank x) x)) vs)) (exit)
  (sort vs > #:key compute-rank #:cache-keys? #t))

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
  (for (((k v) ix)
	#:when (not-just-label? v))
    (define best-v (first (rank-vs v)))
    ;;(writeln `(best ,k ,best-v))
    (define mn (first best-v))
    (define phase (second best-v))
    (define tag (xref-binding->definition-tag 
		 xref 
		 (list mn k) phase))
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

(require racket/cmdline)

(define dictionary-file (make-parameter #f))
(define url-table-file (make-parameter #f))

(module* main #f
  (command-line
   #:once-each
   (("-d" "--dictionary") filename "write dictionary"
    (dictionary-file filename))
   (("-u" "--url-table") filename "write URL lookup table"
    (url-table-file filename))
   )
  (when (or (dictionary-file)
	    (url-table-file))
    (define-values (mods ix) (scan))
    (when (dictionary-file)
      (make-dictionary-file mods ix (dictionary-file)))
    (when (url-table-file)
      (make-url-table-file ix (url-table-file)))
    ))
