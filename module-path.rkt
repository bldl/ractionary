#lang racket

#|
|#

(require data/order "ranking.rkt" "util.rkt")

;;; 
;;; module path sorting
;;; 

(define reverse-number-order
  (order 'number-order number? = >))

(define* mp-order
  (let ((num-cmp (order-comparator reverse-number-order))
        (datum-cmp (order-comparator datum-order)))
    (order 'mp-order 
           any/c
           (lambda (x y)
             (define x-rank (mp-rank x))
             (define y-rank (mp-rank y))
             (define rank-r (num-cmp x-rank y-rank))
             (if (not (eq? '= rank-r))
                 rank-r
                 (datum-cmp x y))))))

(define* mp<? (order-<? mp-order))

;;; 
;;; module path manipulation
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

(module* main #f
  (mp->string '(form m #f (lib "racket/base.rkt")))
  (mp->string '(form v def (lib "racket/base.rkt")))
  (mp->string '(lib "racket/base.rkt"))
  (mp->string '(lib "racket/main.rkt"))
  (mp->string 'racket/base)
  (mp->string 'racket/main)
  (mp->string '(quote #%kernel))
  (mp->string ''#%kernel))
