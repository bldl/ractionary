;;; ractionary-ivy.el --- Ivy command for opening Racket docs
;; -*- lexical-binding: t; -*-

;;; Commentary:
;; 
;; A Ivy-based command for selecting a Racket definition (by name) for
;; which to view documentation. To use this feature, add its directory
;; to your `load-path', and set up an `autoload' for the
;; `ractionary-ivy-open-racket-docs' command:
;;
;;   (autoload 'ractionary-ivy-racket-docs "ractionary-ivy" nil t)
;;
;; Perhaps also bind the command to some key.

;;; Code:

(require 'ivy)
(require 'ractionary-urls)

(defun ractionary-docs-search-query-url (term)
  "Return an URL for searching for the string TERM.
The returned search URL is for the installed Racket docs."
  (concat
   "file://"
   ractionary-racket-doc-dir
   "/search/index.html?q=" (url-hexify-string term)))

(defconst ractionary-ivy-candidates
  (apply
   'append
   (mapcar
    (lambda (x)
      (let ((name (symbol-name (car x))))
	(mapcar
	 (lambda (mp-url)
	   (let ((s (concat name " [" (car mp-url) "]"))
		 (url (cadr mp-url)))
	     (list s name url)))
	 (cadr x))))
    ractionary-url-lookup-table))
  "Candidate strings for `ractionary-ivy-racket-docs'.")

(defun ractionary-ivy-open-racket-docs-for-term (candidate)
  "Open documentation for CANDIDATE.
Use `browse-url' for opening the documentation if CANDIDATE
appears in `ractionary-ivy-candidates'."
  (let* ((entry (assoc candidate ractionary-ivy-candidates)))
    (if entry
	(progn
	  (message "Opening documentation for %s" (cadr entry))
	  (browse-url (caddr entry)))
      (message "Opening a search for \"%s\"" candidate)
      (browse-url (ractionary-docs-search-query-url candidate)))))

;;;###autoload
(defun ractionary-ivy-racket-docs ()
  "Open documentation for a Racket symbol.
Query for a Racket symbol choice with `ivy-read', offering all
symbols in installed Racket documentation. Allow a free-form
search term to be entered also, in which case open a Racket
documentation search rather than opening specific documentation
directly. To select an entered string rather than a highlighted
match, use `\\<ivy-minibuffer-map>\\[ivy-immediate-done]'."
  (interactive)
  (let ((term
	 (ivy-read "Racket search term (of %d): "
		   ractionary-ivy-candidates
		   :require-match nil)))
    (when term
      (ractionary-ivy-open-racket-docs-for-term term))))

(provide 'ractionary-ivy)

;;; ractionary-ivy.el ends here
