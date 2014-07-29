default : setup

-include local.mk

setup :
	raco setup ractionary

all : dict hover-dict url-table

dict :
	./ractionary-make-dict.rkt --dictionary --output racket-mode

hover-dict :
	./ractionary-make-dict.rkt --dictionary --output racket-exports.el --hover
	emacs -Q -L . -batch -f batch-byte-compile racket-exports.el

url-table :
	./ractionary-make-urls.rkt --url-table --output racket-urls.el
	emacs -Q -L . -batch -f batch-byte-compile racket-urls.el

clean :
	find -name compiled -type d -print0 | xargs -0 --no-run-if-empty rm -r

doc : html-doc markdown-doc

# nicer, but not supported for a GitHub README
html-doc :
	scribble ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/ --html --dest-name README.html README.scrbl

markdown-doc :
	scribble ++xref-in setup/xref load-collections-xref --markdown --dest-name README.md README.scrbl
