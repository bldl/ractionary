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

(define (show-symbols mn kind xs)
  (for-each
   (lambda (x)
     (define phase (car x))
     (define lst (cdr x))
     (when phase
       (writeln (list mn kind phase (map car lst)))))
   xs))

;; 'seen' is a set of modules not to scan. Returns a set of symbolic
;; names of all scanned modules and their dependencies.
(define (scan seen)
  (for ((mn interesting-modules)
        #:when (not (set-member? seen mn)))
      (set! seen (set-add seen mn))

    ;; We need not resolve relative requires, hence #f. Always yields
    ;; an actual path.
    (define path (resolve-module-path mn #f))

    ;; Requires actual path as an argument.
    (define c-exp (get-module-code path))
    
    (let-values (((vals stxs) (module-compiled-exports c-exp)))
      (show-symbols mn 'values vals)
      (show-symbols mn 'syntaxes stxs))
    
    ;; set-union
    (void))
  seen)

(define (main)
  (define modnames
    (sort (map symbol->string (set->list (scan (seteq)))) string<?))
  (void))

(main)
