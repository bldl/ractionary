default : dict

-include local.mk

dict :
	./make-racket-emacs-support.rkt --dictionary scheme-mode

