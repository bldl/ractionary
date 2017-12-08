(require 'ractionary-words-help)

;;;###autoload
(defun company-ractionary (command &optional arg &rest ignored)
  "Ractionary backend for Company.
Provides hover help via `company-quickhelp-local-mode'."
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

(provide 'company-ractionary)
