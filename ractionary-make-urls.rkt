#!/usr/bin/env racket
#lang racket

#|

Generates an Emacs Lisp based URL table for looking up Racket
blueboxes data by symbol. There may be multiple URLs per symbol, and
it is up to the client software whether to immediately choose the
first (supposedly "best") choice.

|#

(require setup/xref 
         "racket-blueboxes.rkt" "elisp.rkt" "module-path.rkt"
         "ranking.rkt" "util.rkt" "xref.rkt")

;;; 
;;; symbol index
;;; 

;; Returns a hash map of exported symbols, with symbols as keys, and
;; values of the form (listof (list/c mp-description url)).
(define (scan)
  (define files->tag->offset
    (fetch-files->tag->offset))
  
  (define xref (load-collections-xref))
  
  (define syms-with-mp (make-hasheq))

  (for ([entry (in-list files->tag->offset)])
    (define file-path (first entry))
    (define tag->offset (third entry))
    (for ([(tag offset) tag->offset])
      (define kind (first tag))
      (unless (eq? kind 'idx)
        (define mp-sym (second tag))
        (when (list? mp-sym)
          (define mp (first mp-sym))
          (define sym (second mp-sym))
          (unless (symbol? sym)
            (error 'scan "unexpected: ~s in ~s" sym (list tag offset)))
          (define url (path+tag->url-string xref tag))
          (hash-update!
           syms-with-mp
           sym
           (lambda (lst)
             (cons (list mp url) lst))
           null)))))
  
  (define syms-with-desc
    (for/hash ([(sym vs) (in-hash syms-with-mp)])
      (define sorted-vs (sort vs mp<? #:key car))
      (define desc-vs (map
                       (lambda (v)
                         (list (mp->string (first v)) (second v)))
                       sorted-vs))
      (values sym desc-vs)))

  ;;(pretty-print syms-with-desc) (exit)
  syms-with-desc)

;;; 
;;; URL lookup file generation
;;; 

(require setup/dirs) ;; find-doc-dir

;; 'lst' is (listof (list/c sym (list/c mp-description url))).
(define (write-url-table lst out)
  (displayln "(defvar ractionary-url-lookup-table '(" out)
  (for-each 
   (lambda (x)
     (display "(" out)
     (display (elisp-escape-symbol (first x)) out)
     (display " " out)
     (write (second x) out)
     (displayln ")" out))
   lst)
  (displayln ") \"API doc URLs for Racket symbols.\")" out))

(define (make-url-table-file filename)
  (define ix (scan))
  ;;(pretty-print ix)
  
  (define lst 
    (sort (for/list ([(sym vs) ix])
            (list sym vs))
          symbol<? #:key car))
  
  (define (w-f out)
    (displayln ";; generated -- do not edit" out)
    (writeln `(defvar ractionary-racket-doc-dir ,(path->string (find-doc-dir))
               "Racket installation's \"doc\" directory.") out)
    (write-url-table lst out))

  (write-output filename w-f)
  (void))

;;; 
;;; main
;;; 

(module+ test
  (make-url-table-file "/tmp/urls.el"))

(module* main #f
  (define gen? #f)
  (define url-table-file (make-parameter #f))

  (command-line
   #:once-each
   [("-u" "--url-table") "generate an URL lookup table"
    (set! gen? #t)]
   [("-o" "--output") filename "write to a file"
    (url-table-file filename)])
  (when gen?
    (make-url-table-file (url-table-file)))
  (void))
