#lang racket/base

#|
|#

(require racket/match "util.rkt")

;;; 
;;; module path ranking
;;; 

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

(define* (mp-rank mp)
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
      ((under? "pict" str) 65)
      ((under? "setup" str) 60)
      ((under? "net" str) 50)
      ((under? "unstable" str) 40)
      ((under? "srfi" str) 30)
      ((under? "lazy" str) -5)
      ((under? "mzscheme" str) -10)
      ((under? "mzlib" str) -20)
      ((under? "deinprogramm" str) -30)
      (else 0)))
    (_ -50)))

(module* main #f
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
