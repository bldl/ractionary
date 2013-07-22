#!/usr/bin/env racket
#lang racket

#|

We record all exports, and examine all imports.

The dependencies of just about any given module are huge (just use
show-import-tree to see), and so we want to make sure to avoid
duplication of work.

module-compiled-imports and module-compiled-exports report all of the
relevant symbols regardless of phase level shifts. #f corresponds to
the label phase level, and said phase level is the only one we will
exclude. module->imports and module->exports provide the same
information, but they must have been declared.

We do not want to examine imports, we only want a list of (module)
requires for each module.

We can tell what is syntax and what is a value. A problem here is that
because of contracts and such lots more symbols will appear to be
syntax that are actually defined as macros as such. So this will
probably lead to it being confusing to highlight all syntax as
keywords. We may just have to continue to declare things to highlight
by hand, for Racket itself. But at least we get our dictionary.

|#

(require common/6/util syntax/moddep)

(define interesting-modules
  '(racket
    syntax/define
    syntax/flatten-begin
    syntax/free-vars
    syntax/id-table
    syntax/kerncase
    syntax/keyword
    syntax/modcode
    syntax/modcollapse
    syntax/moddep
    syntax/modread
    syntax/modresolve
    syntax/name
    syntax/parse
    syntax/readerr
    syntax/strip-context
    syntax/stx
    syntax/to-string
    ))

;; 'mods' is a list of modules still to examine, by symbolic name.
;; 'seen' is a set of modules already seen, by symbolic names. 'syms'
;; is a set of exported symbols, as symbols, regardless of exporting
;; module or phase level.
(struct St (mods seen syms) #:transparent #:mutable)

(define (show-symbols mn kind xs)
  (for-each
   (lambda (x)
     (define phase (car x))
     (define lst (cdr x))
     (when phase
       (writeln (list mn kind phase (map car lst)))))
   xs))

(define (scan st)
  (let next ()
    (define mods (St-mods st))
    (unless (null? mods)
      (define mod (car mods))
      (set-St-mods! st (cdr mods))
      (define seen (St-seen st))
      (when (set-member? seen mod)
        (next))
      (set-St-seen! st (set-add seen mod))

      ;; We need not resolve relative requires, hence #f. Always yields
      ;; an actual path.
      (define path (resolve-module-path mod #f))

      ;; Requires actual path as an argument.
      (define c-exp (get-module-code path))
      
      (let-values (((vals stxs) (module-compiled-exports c-exp)))
        (show-symbols mod 'values vals)
        (show-symbols mod 'syntaxes stxs))
    
      ;; set-union

      (next))))
      

(define (main)
  (define st (St interesting-modules (seteq) (seteq)))
  (scan st)
  (define modnames
    (sort (map symbol->string (set->list (St-seen st))) string<?))
  (void))

(main)
