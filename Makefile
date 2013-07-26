default : dict

-include local.mk

dict :
	./make-racket-emacs-support.rkt --dictionary scheme-mode

url-table :
	./make-racket-emacs-support.rkt --url-table racket-urls.el
	emacs -Q -L . -batch -f batch-byte-compile racket-urls.el
