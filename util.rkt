#lang racket

;;; 
;;; provide conveniences
;;; 

(provide define* define-syntax*)

(define-syntax define*
  (syntax-rules ()
    ((_ (name arg ... . rest) body ...)
     (begin
       (define (name arg ... . rest) body ...)
       (provide name)))
    ((_ (name arg ...) body ...)
     (begin
       (define (name arg ...) body ...)
       (provide name)))
    ((_ name body ...)
     (begin
       (define name body ...)
       (provide name)))))

(define-syntax define-syntax*
  (syntax-rules ()
    ((_ (name stx) body ...)
     (begin
       (define-syntax (name stx) body ...)
       (provide name)))
    ((_ name body ...)
     (begin
       (define-syntax name body ...)
       (provide name)))))

(define-syntax* define-syntax-rule*
  (syntax-rules ()
    ((_ (name rest ...) body)
     (begin
       (define-syntax-rule (name rest ...) body)
       (provide name)))))

(define-syntax* define-with-contract
  (syntax-rules ()
    ((_ contract (name . rest) body ...)
     (define (name . rest) body ...))
    ((_ contract name value)
     (define name value))))

(define-syntax* define-with-contract*
  (syntax-rules ()
    ((_ contract (name . rest) body ...)
     (begin
       (define (name . rest) body ...)
       (provide/contract [name contract])))
    ((_ contract name value)
     (begin
       (define name value)
       (provide/contract [name contract])))))

(define-syntax-rule*
  (concrete-struct* nm rest ...)
  (begin
    (struct nm rest ...)
    (provide (struct-out nm))))

(define-syntax-rule*
  (abstract-struct nm rest ...)
  (struct nm rest ... #:constructor-name ctor))

(define-syntax-rule*
  (abstract-struct* nm rest ...)
  (begin
    (struct nm rest ... #:constructor-name ctor)
    (provide (except-out (struct-out nm) ctor))))

(define-syntax-rule*
  (require* spec ...)
  (begin
    (require spec ...)
    (provide (all-from-out spec ...))))

;;; 
;;; binding conveniences
;;; 

(define-syntax* if-let
  (syntax-rules ()
    ((_ n c t e)
     (let ((n c))
       (if n t e)))))

(define-syntax* let-and
  (syntax-rules ()
    ((_ e) e)
    ((_ n v more ...)
     (let ((n v))
       (and n (let-and more ...))))))

;;; 
;;; printing conveniences
;;; 

(define* println
  (case-lambda
    ((datum) (begin (print datum) (newline)))
    ((datum out) (begin (print datum out) (newline out)))))

(define* writeln
  (case-lambda
    ((datum) (begin (write datum) (newline)))
    ((datum out) (begin (write datum out) (newline out)))))

(define* (printfln . args)
  (apply printf args) (newline))

;;; 
;;; IO conveniences
;;; 

(define* (write-output filename w-f)
  (if filename
      (call-with-output-file* 
       filename w-f
       #:exists 'truncate/replace)
      (w-f (current-output-port))))

;;; 
;;; matching conveniences
;;; 

(define-syntax-rule* (matches? e pat ...)
  (match e (pat #t) ... (_ #f)))

#|

Copyright 2009 Helsinki Institute for Information Technology (HIIT)
and the authors. All rights reserved.

Authors: Tero Hasu <tero.hasu@hut.fi>

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation files
(the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

|#
