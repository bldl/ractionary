#!/usr/bin/env racket
#lang racket

#|

|#

(require syntax/moddep
         "module-path.rkt" "ranking.rkt" "util.rkt")

(define (warn msg datum)
  (printf "WARNING: ~a: ~s~n" msg datum))

;;; 
;;; index data structure
;;; 

(abstract-struct Word (word) #:transparent)
(struct BlueWord Word (mp help) #:transparent)
(struct OtherWord Word (pri help) #:transparent)

(define (word+ x y)
  (define x-w (Word-word x))
  (define y-w (Word-word y))
  (unless (string=? x-w y-w)
    (raise-arguments-error 
     'word+
     "expected the same strings"
     "x" x "y" y))
  (cond
   [(and (OtherWord? x) (OtherWord? y))
    (define x-pri (OtherWord-pri x))
    (define y-pri (OtherWord-pri y))
    (if (< x-pri y-pri) y x)]
   [(OtherWord? x) x]
   [(OtherWord? y) y]
   [(and (BlueWord? x) (BlueWord? y))
    (define x-mp (BlueWord-mp x))
    (define y-mp (BlueWord-mp y))
    (define x-lower? (mp<? x-mp y-mp))
    ;;(writeln (list (string->symbol x-w) 'CMP x-mp y-mp x-lower?))
    (if x-lower? x y)]
   [else
    (raise-arguments-error
     'word+
     "objects are not of expected types"
     "x" x "y" y)]))

(define min-word-length 2)

(define (words-put! h x)
  (define w (Word-word x))
  (when (>= (string-length w) min-word-length)
    (hash-update!
     h
     (string->symbol w)
     (lambda (y) (if y (word+ x y) x))
     #f)))

;;; 
;;; built-in word indexing
;;; 

(define builtin-words
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
    ("racket" "module")
    ("racket/base" "module")

    ("#:context" "syntax-parse keyword")
    ("#:literals" "syntax-parse keyword")
    ("#:datum-literals" "syntax-parse keyword")
    ("#:literal-sets" "syntax-parse keyword")
    ("#:conventions" "syntax-parse keyword")
    ("#:local-conventions" "syntax-parse keyword")
    ("#:disable-colon-notation" "syntax-parse keyword")
    ("#:at" "syntax-parse keyword")
    ("#:phase" "syntax-parse keyword")

    ("#:mode" "open-output-file keyword")
    ("#:exists" "open-output-file keyword")

    ("#:constructor-name" "struct option keyword")
    ("#:methods" "struct option keyword")
    ("#:mutable" "struct option keyword")
    ("#:prefab" "struct option keyword")
    ("#:property" "struct option keyword")
    ("#:reflection-name" "struct option keyword")
    ("#:transparent" "struct option keyword")

    ("#:date" "Scribble keyword")
    ("#:doc" "Scribble keyword")
    ("#:indent" "Scribble keyword")
    ("#:obfuscate?" "Scribble keyword")
    ("#:style" "Scribble keyword")
    ("#:scale" "Scribble keyword")
    ("#:suffixes" "Scribble keyword")
    ("#:tag" "Scribble keyword")
    ("#:tag-prefix" "Scribble keyword")
    ("#:tag-prefixes" "Scribble keyword")
    ("#:underline?" "Scribble keyword")
    ("#:version" "Scribble keyword")
    ))

(define (index-add-builtins! h)
  (for ((x builtin-words))
    (words-put! h (OtherWord (first x) 100 (second x)))))

;;; 
;;; module name indexing
;;; 

;; `mods` is a set of module paths
(define (index-add-modnames! h mods)
  (for ([mp (in-set mods)])
    (define s (mp->string mp))
    (words-put! h (OtherWord s 50 "module"))))

;;; 
;;; blueboxes index
;;; 

(require "racket-blueboxes.rkt")

;; fetch-strs-for-single-tag returned strings contain odd spaces.
(define (replace-weird-spaces s)
  (regexp-replace* #rx"\u00A0" s " "))

(define (index-add-blueboxes! h)
  (define files->tag->offset
    (fetch-files->tag->offset))

  (define (put! w)
    (words-put! h w)
    (void))

  (define (mk-help loc tag)
    (define n-strs 
      (cons
       loc
       (map
        replace-weird-spaces
        (fetch-strs-for-single-tag files->tag->offset tag))))
    (string-join n-strs "\n"))

  ;; It does not look like there are tags for modules (i.e., blueboxes
  ;; index entries for documented modules), so we instead just record
  ;; all referenced module paths.
  (define mp-set (mutable-set))
  (define (record-mp! mp)
    (unless (matches? mp `(submod . ,_)) 
      (set-add! mp-set mp)))
  
  (for ((entry files->tag->offset))
    (define file-path (first entry))
    (define tag->offset (third entry))
    (for (((tag offset) tag->offset))
      (match tag
        [(list (and (or 'def 'form 'constructor) kind)
               (list mp (? symbol? sym)))
         (record-mp! mp)
         (define loc (format "~a in ~a:" kind (mp->string mp)))
         (define help (mk-help loc tag))
         ;;(writeln (list sym mp (mp-rank mp)))
         (put! (BlueWord (symbol->string sym) mp help))]
        [(list 'meth (list (list mp (? symbol? cls)) (? symbol? sym)))
         (record-mp! mp)
         (define loc (format "~a of ~a in ~a:" 'meth cls (mp->string mp)))
         (define help (mk-help loc tag))
         (put! (BlueWord (symbol->string sym) mp help))]
        [(list 'sig-val (list mp (? symbol? sig) (? symbol? sym)))
         (record-mp! mp)
         (define loc (format "~a of ~a in ~a:" 'sig-val sig (mp->string mp)))
         (define help (mk-help loc tag))
         (put! (BlueWord (symbol->string sym) mp help))]
        [(list 'xrepl (? string? command))
         (put! (OtherWord command 80 "XREPL command"))]
        [(list 'idx _) ;; do not know what these are for
         (void)]
        [_
         (warn "unrecognized" tag)])))
  
  (index-add-modnames! h mp-set)
  
  (void))

;;; 
;;; dictionary generation
;;; 

(define (make-Word-help w)
  (cond
   [(OtherWord? w) (OtherWord-help w)]
   [(BlueWord? w) (BlueWord-help w)]
   [else (raise-argument-error 'make-Word-help "Word?" w)]))

;; Output words as plain text, one per line.
(define (make-dictionary-file/plain all-names)
  (define (w-f out)
    (for-each 
     (curryr displayln out)
     all-names))
  (write-output (dictionary-file) w-f)
  (void))

(define (make-dictionary-file/elisp words)
  (define (el-basename path)
    (let-values 
	(((base name dir) (split-path path)))
      (path->string (path-replace-suffix name ""))))
  (define filename (dictionary-file))
  (define bn (if filename (el-basename filename) "output"))
  (define (w-f out)
    (displayln ";; generated -- do not edit" out)
    (displayln "(defvar ractionary-dictionary '(" out)
    (for-each (curryr writeln out) words)
    (displayln ") \"Dictionary of Racket words.\")" out)
    (displayln (format "(provide '~a)" bn) out)
    (void))
  (write-output filename w-f)
  (void))

;; Output in Emacs Lisp, with (WORD HELP) elements, where HELP strings
;; are suitable for display in Auto Complete hover help boxes.
(define (make-dictionary-file/elisp-hover words)
  (define help-lst ;; (list/c string? string?)
    (for/list ((w words))
      (list (Word-word w) (make-Word-help w))))
  (define (el-basename path)
    (let-values 
	(((base name dir) (split-path path)))
      (path->string (path-replace-suffix name ""))))
  (define filename (dictionary-file))
  (define bn (if filename (el-basename filename) "output"))
  (define (w-f out)
    (displayln ";; generated -- do not edit" out)
    (display "(defvar ractionary-dictionary-with-help " out)
    (cond
      [(emit-string-prop?)
       (displayln "(list" out)
       (for ([word-help (in-list help-lst)])
         (displayln (apply format "(propertize ~s 'help ~s)" word-help) out))]
      [else
       (displayln "'(" out)
       (for-each (curryr writeln out) help-lst)])
    (displayln ") \"Dictionary of Racket words with help strings.\")" out)
    (displayln (format "(provide '~a)" bn) out)
    (void))
  (write-output filename w-f)
  (void))

(define sort-string<? (make-parameter string-ci<?))

(define (make-dictionary-file)
  (define h (make-hasheq))
  (index-add-builtins! h)
  (index-add-blueboxes! h)
  (define s<? (sort-string<?))
  (case (dictionary-format)
   [(elisp-hover)
    (make-dictionary-file/elisp-hover
     (sort (hash-values h)
           s<? #:key Word-word))]
   [(elisp)
    (make-dictionary-file/elisp
     (sort (for/list ([(k v) h])
             (Word-word v))
           s<?))]
   [(plain)
    (make-dictionary-file/plain
     (sort (for/list ([(k v) h])
             (Word-word v))
           s<?))]))

;;; 
;;; main
;;; 

(define dictionary-file (make-parameter #f))
(define dictionary-format (make-parameter 'plain))
(define emit-string-prop? (make-parameter #f))

(module* test #f
  (make-dictionary-file))

(module* main #f
  (define gen? #f)
  (command-line
   #:once-each
   [("--case-sensitive") "sort words case sensitively"
    (sort-string<? string<?)]
   [("-d" "--dictionary") "generate a dictionary"
    (set! gen? #t)]
   [("-o" "--output") filename "write to a file"
    (dictionary-file filename)]
   [("--elisp") "emit Emacs Lisp with words only"
    (dictionary-format 'elisp)]
   [("--elisp-hover") "emit Emacs Lisp with hover help"
    (dictionary-format 'elisp-hover)]
   [("--string-prop") "emit metadata as string properties"
    (emit-string-prop? #t)])
  (when gen?
    (make-dictionary-file))
  (void))
