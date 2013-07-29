#lang racket

#|

This code includes code from Racket v5.3.4, and is for extracting the
context help text displayed in DrRacket's "blue box" in the corner.

|#

(require racket/serialize setup/dirs)

(provide fetch-files->tag->offset 
	 fetch-strs-for-single-tag
	 replace-weird-spaces)

;; from Racket
;; Returns: (listof (list file-path int hash[tag -o> (cons int int)]))
;; where the hash looks something like
;; #hash(((def ((lib "racklog/main.rkt") %not)) . ((1358 . 3)))
;;       ((def ((lib "racklog/main.rkt") %if-then-else)) . ((1407 . 5)))
;;       ((def ((lib "racklog/main.rkt") %append)) . ((2433 . 5)))
;;       ((form ((lib "racklog/main.rkt") %assert!)) . ((863 . 5)))
;;       ...)
(define (fetch-files->tag->offset)
  (filter
   values
   (for*/list ([doc-search-dir (in-list (get-doc-search-dirs))]
               [doc-dir-name (in-list (if (directory-exists? doc-search-dir)
                                          (directory-list doc-search-dir)
                                          '()))])
     (define x (build-path doc-search-dir doc-dir-name "blueboxes.rktd"))
     (and (file-exists? x)
          (call-with-input-file x
            (λ (port)
              (port-count-lines! port)
              (define first-line (read-line port))
              (define pos (file-position port))
              (list x
                    (+ (string->number first-line) pos)
                    (deserialize (read port)))))))))

;; from Racket, slightly modified
;; a tag looks like (form ((lib "racklog/main.rkt") %free-vars))
(define (fetch-strs-for-single-tag files->tag->offset tag)
  (for/or ([ent (in-list files->tag->offset)])
    (define offset+lens (hash-ref (list-ref ent 2) tag #f))
    (cond
      [offset+lens
       (apply
        append
        (for/list ([offset+len (in-list offset+lens)])
          (define fn (list-ref ent 0))
          (define offset (list-ref ent 1))
          (call-with-input-file fn
            (λ (port)
              (port-count-lines! port)
              (file-position port (+ (car offset+len) offset))
              (for/list ([i (in-range (cdr offset+len))])
                (read-line port))))))]
      [else #f])))

;; fetch-strs-for-single-tag returned strings contain odd spaces.
(define (replace-weird-spaces s)
  (regexp-replace* #rx"\u00A0" s " "))

#|

Copyright (c) 2010-2013 PLT Design Inc.

This code is mostly from Racket, and is distributed under the GNU
Lesser General Public License (LGPL). See Racket's
doc/release-notes/COPYING.txt for more information.

|#
