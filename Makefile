default : dict

-include local.mk

all : dict hover-dict url-table

dict :
	./make-racket-support-files.rkt --dictionary scheme-mode

hover-dict :
	./make-racket-support-files.rkt --dictionary racket-exports.el --hover --signatures
	emacs -Q -L . -batch -f batch-byte-compile racket-exports.el

url-table :
	./make-racket-support-files.rkt --url-table racket-urls.el
	emacs -Q -L . -batch -f batch-byte-compile racket-urls.el
