;;; company-ractionary.el --- Company Ractionary backends
;; -*- lexical-binding: t; -*-

;;; Commentary:
;; 
;; This file contains two variations of a Company backend that
;; completes words from a static, Ractionary-generated dictionary. In
;; addition, it includes a backend that completes words from a local
;; Racket namespace, but only if `racket-run' (of `racket-mode') has
;; been executed.
;;
;; To use this feature, add it to `load-path':
;;
;;   (add-to-list 'load-path "/path/to/this/directory")
;;
;; and generate the `ractionary-words-help' dictionary, and add it to
;; `load-path' also.
;;
;; Also add `autoload' calls for the backend functions defined in this
;; file, if necessary:
;;
;;   (autoload 'company-ractionary "company-ractionary" nil t)
;;   (autoload 'company-ractionary/at-exp "company-ractionary" nil t)
;;   (autoload 'company-maybe-racket-mode "company-ractionary" nil t)
;;
;; Those autoloads may or may not be necessary, as Company should load
;; this feature automatically if and when the `company-ractionary'
;; function is first used.
;;
;; Depending on major mode, pick the `company-backends' you wish to
;; use. For example, for `racket-mode', in your `racket-mode-hook' you
;; might have
;;
;;   (setq-local company-backends
;;     '((company-ractionary
;;        :with
;;        company-maybe-racket-mode
;;        company-dabbrev-code)))
;;
;; For `scribble-mode', in turn, you might consider setting
;;
;;   (setq-local company-backends
;;     '(company-ractionary/at-exp
;;       (company-ractionary :with company-dabbrev)))
;;
;; where `company-ractionary/at-exp' only completes symbols prefixed
;; by "@", and the following backend group is used otherwise. While
;; `racket-complete-at-point' could also be useful for completing
;; Scribble symbols, it does not work outside `racket-mode', which is
;; why there is no use trying the `company-maybe-racket-mode' in
;; `scribble-mode'.
;;
;; As these backends do not check for Racket-specific modes, you
;; should not add them to `company-backends' globally, unless you
;; first add an appropriate check to the `prefix' operations, e.g.:
;;
;;   (prefix (and (or (eq major-mode 'racket-mode)
;;                    (eq major-mode 'scribble-mode))
;;                (company-grab-symbol)))

;;; Code:

(require 'ractionary-words-help)

;;; 
;;; Ractionary
;;; 

;;;###autoload
(defun company-ractionary (command &optional arg &rest ignored)
  "Ractionary backend for Company.
COMMAND, ARG, and IGNORED are as for other Company backends.
Return completions from a static dictionary. Provide hover help
via the mode started by the function
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

;;; 
;;; racket-mode
;;; 

;; This is essential when Company does not do the loading for us, but
;; rather our backends use `company-capf' directly.
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

;;; 
;;; Scribble
;;; 

;; Similar to `company-grab-symbol'.
(defun company-grab-at-exp-symbol ()
  "If point is at the end of an at-prefixed symbol, return it.
Return nil in any other case. Return any symbol without its \"@\"
sign."
  (when (looking-at "\\_>")
    (let ((end (point)))
      (save-excursion
	(skip-syntax-backward "w_")
	(when (eq (char-after) ?@)
	  (buffer-substring (1+ (point)) end))))))

;;;###autoload
(defun company-ractionary/at-exp (command &optional arg &rest ignored)
  "Ractionary backend for Company.
COMMAND, ARG, and IGNORED are as for other Company backends.
Only complete symbols prefixed with \"@\".
Return completions from a static dictionary. Provide hover help
via the mode started by the function
`company-quickhelp-local-mode', when that minor mode is enabled."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-ractionary/at-exp))
    (prefix (company-grab-at-exp-symbol))
    (t (apply 'company-ractionary command arg ignored))))

(provide 'company-ractionary)

;;; company-ractionary.el ends here
