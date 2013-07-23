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

We presently have no hover help for the words in the dictionary. Do
not know if there is a programmatic way to query the docs for a given
symbol in a given module. But 'raco' almost does that sort of thing,
so perhaps it is possible.

|#

(require common/6/util syntax/moddep)

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

;; 'mods' is a list of modules still to examine, by symbolic name.
;; 'seen' is a set of modules already seen, by symbolic names. 'syms'
;; is a set of exported symbols, as symbols, regardless of exporting
;; module or phase level.
(struct St (mods seen syms) #:transparent #:mutable)

(define (add-exports st mn kind xs)
  (for-each
   (lambda (x)
     (define phase (car x))
     (define lst (cdr x))
     (when phase
       ;;(writeln (list mn kind phase (map car lst)))
       (for-each
        (lambda (sym)
          (define syms (St-syms st))
          (set-St-syms! st (set-add syms sym)))
        (map car lst))))
   xs))

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

(define (mp-exclude? mp)
  (regexp-match #rx"/private/"
                (symbol->string mp)))

;;(mp/symbolic->lib 'racket/base)
;;(mp/lib->symbolic '(lib "racket/base.rkt"))

(define (scan st)
  (let next ()
    (define mods (St-mods st))
    (unless (null? mods)
      (define cur-mp (car mods))
      (set-St-mods! st (cdr mods))
      (define seen (St-seen st))
      (when (set-member? seen cur-mp)
        (next))
      (set-St-seen! st (set-add seen cur-mp))

      ;; We need not resolve relative requires, hence #f. Always yields
      ;; an actual path.
      (define path (resolve-module-path cur-mp #f))

      ;; Requires actual path as an argument.
      (define c-exp (get-module-code path))

      (unless (mp-exclude? cur-mp)
        (let-values (((vals stxs) (module-compiled-exports c-exp)))
          (add-exports st cur-mp 'values vals)
          (add-exports st cur-mp 'syntaxes stxs)))

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
                   ;;   (printf "WARNING: skipping non-'lib' module path: ~s~n"
                   ;;           t-lib-mp))
                   t-sym-mp)
                 mpis)))
             ;;(writeln n-mods)
             (set-St-mods! st (append (St-mods st) n-mods))))
         imports))
      (next))))

(define (main)
  (define st (St interesting-modules (seteq) (seteq)))
  (scan st)
  (define modnames
    (set->list (St-seen st)))
  (define exports
    (set->list (St-syms st)))
  (define extras
    (list "#t" "#f" "#lang" "FIXME" "TODO"))
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
  ;;(pretty-println (list modnames exports))
  (for-each displayln all-names)
  (void))

(main)
