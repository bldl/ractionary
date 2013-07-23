default : dict

-include local.mk

dict :
	./make-racket-emacs-support.rkt > scheme-mode

