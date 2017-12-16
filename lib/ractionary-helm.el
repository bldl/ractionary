;;; ractionary-helm.el --- Helm command for opening Racket docs
;; -*- lexical-binding: t; -*-

;;; Commentary:
;; 
;; A Helm for selecting a Racket definition (by name) for which to
;; view documentation. Unfortunately "?" and "*" require escaping in
;; match patterns, although those characters are common in Racket
;; identifiers. To use this feature, add its directory to your
;; `load-path', and set up an `autoload' for the
;; `helm-ractionary-racket-docs' command:
;;
;;   (autoload 'helm-ractionary-racket-docs "ractionary-helm" nil t)
;;
;; Perhaps also bind the command to some key.

;;; Code:

(require 'ractionary-urls)

(defvar helm-ractionary-url-table
  (make-hash-table :test 'equal)
  "URLs for the `helm-ractionary-racket-docs' choices.")

(defconst helm-ractionary-candidates
  (apply
   'append
   (mapcar
    (lambda (x)
      (let ((name (symbol-name (car x))))
	(mapcar
	 (lambda (mp-url)
	   (let ((s (concat name " [" (car mp-url) "]"))
		 (url (cadr mp-url)))
	     (puthash s url helm-ractionary-url-table)
	     s))
	 (cadr x))))
    ractionary-url-lookup-table))
  "Candidate strings for `helm-ractionary-racket-docs'.")

(defun helm-ractionary-open (candidate)
  "Open documentation for CANDIDATE.
Use `browse-url' for opening the documentation."
  (let ((url (gethash candidate helm-ractionary-url-table)))
    (when url
      (browse-url url))))

;;;###autoload
(defun helm-ractionary-racket-docs ()
  "A `helm' for Racket symbols.
Covers all Racket symbols in installed documentation. The default
action is to browse documentation for the selected symbol."
  (interactive)
  (helm :sources
	(helm-build-sync-source "Racket symbols"
	  :candidates 'helm-ractionary-candidates
	  :action '(("Open in browser" . helm-ractionary-open)))
        :buffer
	"*helm Racket*"))

(provide 'ractionary-helm)

;;; ractionary-helm.el ends here
