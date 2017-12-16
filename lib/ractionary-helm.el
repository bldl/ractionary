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

(defun helm-ractionary-candidates ()
  "Return a list of documented Racket symbol names."
  (mapcar (lambda (x) (symbol-name (car x)))
	  ractionary-url-lookup-table))

(defun helm-ractionary-open (candidate)
  "Open documentation for symbol CANDIDATE.
If multiple modules export the symbol, ask the user to pick the
module. Use `browse-url' for opening the documentation."
  (let ((entry (assq (intern candidate) ractionary-url-lookup-table)))
    (when entry
      (let* ((lst (cadr entry)))
	(when lst
	  (let ((url
		 (if (not (cdr lst))
		     (cadr (car lst)) ;; sole definition
		   (let* ((mps (mapcar #'car lst))
			  (mp (ido-completing-read "Module: " mps nil t)))
		     (when mp
		       (cadr (assoc mp lst)))))))
	    (when url
	      (browse-url url))))))))

;;;###autoload
(defun helm-ractionary-racket-docs ()
  "A `helm' for Racket symbols.
Covers all Racket symbols in installed documentation. The default
action is to browse documentation for the selected symbol."
  (interactive)
  (helm :sources
	(helm-build-sync-source "Racket symbols"
	  :candidates #'helm-ractionary-candidates
	  :action '(("Open in browser" . helm-ractionary-open)))
        :buffer
	"*helm Racket*"))

(provide 'ractionary-helm)

;;; ractionary-helm.el ends here
