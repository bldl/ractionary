(require 'ractionary-words)

;;;###autoload
(defun company-ractionary (command &optional arg &rest ignored)
  "A static Racket backend for Company, with words only.
Implements the standard interface for `company-mode' backends.
This backend implements only word completion, offering no
meta-information about the suggested words."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-ractionary))
    (prefix
     ;; We can restrict the backend to only ever get enabled for
     ;; predefined mode(s), but this is unnecessary if we only ever
     ;; add `company-ractionary' to the `company-backends'
     ;; list locally to specific modes (probably in the mode hook).
     ;; That adds flexibility if we might also want to use the backend
     ;; in `scribble-mode' and others.
     (and (eq major-mode 'racket-mode)
	  (company-grab-symbol)))
    (candidates
      ;; The generated `ractionary-dictionary' is just a list of
      ;; words, so just select those matching the prefix `arg'.
     (remove-if-not
      (lambda (c) (string-prefix-p arg c))
      ractionary-dictionary))))

;; If you name this feature `company-ractionary' (i.e., the same as
;; the function), then Company should load this file automatically on
;; demand, without an explicit `autoload'.
(provide 'company-backend-ractionary)
