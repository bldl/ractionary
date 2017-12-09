;;; company-ractionary.el --- Company Ractionary backend
;; -*- lexical-binding: t; -*-

;;; Commentary:
;; 
;; This file contains a Company backend that completes words from a
;; static, Ractionary generated dictionary. In addition, it includes a
;; backend that completes words from a local Racket namespace, but
;; only if `racket-run' (of `racket-mode') has been executed. The two
;; backends can be used together.
;;
;; To use this feature, add it to `load-path':
;;
;;   (add-to-list 'load-path "/path/to/this/files/directory")
;;
;; and generate the `ractionary-words-help' dictionary, and add it to
;; `load-path' also.
;;
;; Also add `autoload' calls for the backend functions defined in this
;; file, if necessary:
;;
;;   (autoload 'company-ractionary "company-ractionary" nil t)
;;   (autoload 'company-maybe-racket-mode "company-ractionary" nil t)
;;
;; Those autoloads may or may not be necessary, as Company should load
;; this feature automatically when the `company-ractionary' function
;; is first used.
;;
;; In your `racket-mode-hook', pick the `company-backends' you wish to
;; use. For example:
;;
;;   (setq-local company-backends
;;     '((company-ractionary
;;        :with
;;        company-maybe-racket-mode
;;        company-dabbrev-code)))
;;
;; As these backends do not check for `racket-mode' specifically, you
;; should not add them to `company-backends' globally, unless you
;; first add the appropriate check to the `prefix' operations, e.g.:
;;
;;   (prefix (and (eq major-mode 'racket-mode) (company-grab-symbol)))

;;; Code:

(require 'ractionary-words-help)

;;;###autoload
(defun company-ractionary (command &optional arg &rest ignored)
  "Ractionary backend for Company.
COMMAND, ARG, and IGNORED are as for other Company backends.
Provides hover help via the mode started by the function
`company-quickhelp-local-mode', when that minor mode is enabled."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-ractionary))
    (prefix (company-grab-symbol))
    (candidates
     (mapcar
      (lambda (w-h)
	(propertize (car w-h) 'help (cadr w-h)))
      (remove-if-not
       (lambda (c) (string-prefix-p arg (car c)))
       ractionary-dictionary-with-help)))
    (sorted t)
    (quickhelp-string (get-text-property 0 'help arg))))

;; This is essential when Company does not do the loading for us, but
;; rather our `company-maybe-racket-mode' uses `company-capf'
;; directly.
(autoload 'company-capf "company-capf" nil t)

;; Defined by racket-mode, but possibly meant to be internal. Used to
;; check whether `racket-complete-at-point' is available for use.
;; (Could really use a status bar indicator for that in
;; `racket-mode'.)
(autoload 'racket--in-repl-or-its-file-p "racket-repl")

;;;###autoload
(defun company-maybe-racket-mode (command &optional arg &rest ignored)
  "Defines a Company backend for `racket-mode'.
COMMAND, ARG, and IGNORED are as for other Company backends.
Return candidates based on `racket-complete-at-point', but only
when `racket--in-repl-or-its-file-p' holds, in which case return
an empty candidate list. Returns only candidate words, without
any meta information."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-maybe-racket-mode))
    (prefix (company-grab-symbol))
    (candidates (and (racket--in-repl-or-its-file-p)
		     (apply 'company-capf command arg ignored)))))

(provide 'company-ractionary)

;;; company-ractionary.el ends here
