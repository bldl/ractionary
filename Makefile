default : dict

-include local.mk

all : dict hover-dict url-table

TOOL := ./make-racket-support-files.rkt

dict :
	$(TOOL) --dictionary scheme-mode

hover-dict :
	$(TOOL) --dictionary racket-exports.el --hover --signatures
	emacs -Q -L . -batch -f batch-byte-compile racket-exports.el

url-table :
	$(TOOL) --url-table racket-urls.el
	emacs -Q -L . -batch -f batch-byte-compile racket-urls.el

doc:
	scribble ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/ --dest-name README.html README.scrbl
