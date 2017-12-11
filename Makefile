default : setup

-include local.mk

install :
	raco pkg install --name ractionary

setup :
	raco setup ractionary

check-pkg-deps :
	raco setup --check-pkg-deps ractionary

all : plain-dict elisp-dict elisp-dict-help url-table

plain-dict :
	./ractionary-make-dict.rkt --dictionary --output racket-mode

elisp-dict :
	./ractionary-make-dict.rkt --dictionary --output ractionary-words.el --elisp
	emacs -Q -L . -batch -f batch-byte-compile ractionary-words.el

elisp-dict-help :
	./ractionary-make-dict.rkt --dictionary --output ractionary-words-help.el --elisp-hover
	emacs -Q -L . -batch -f batch-byte-compile ractionary-words-help.el

# Emacs appears to be choking on this rule
elisp-dict-help-prop :
	./ractionary-make-dict.rkt --dictionary --output ractionary-words-help.el --elisp-hover --string-prop
	emacs -Q -L . -batch -f batch-byte-compile ractionary-words-help.el

url-table :
	./ractionary-make-urls.rkt --url-table --output ractionary-urls.el
	emacs -Q -L . -batch -f batch-byte-compile ractionary-urls.el

clean :
	find -name compiled -type d -print0 | xargs -0 --no-run-if-empty rm -r

doc : html-doc markdown-doc

# nicer, but not supported for a GitHub README
html-doc :
	mkdir -p web/examples
	scribble ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/ --html --dest web --dest-name web/index.html README.scrbl
	rsync -av --delete --include='*.el' --include='*/' --exclude='*' examples/ web/examples/

markdown-doc :
	scribble ++xref-in setup/xref load-collections-xref --markdown --dest-name README.md README.scrbl
