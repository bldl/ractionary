#lang scribble/manual

@(require (for-label racket/base))

@title{Ractionary}
@bold{Racket Dictionary Generator}

@author+email["Tero Hasu" "tero at ii.uib.no"]

@section{Introduction}

This is a tool for generating dictionaries for Racket language aware tools support. The generation is done based on information available through Racket's own facilities, including: 
@racket[module-compiled-exports] and @racket[module-compiled-imports]
(and @racket[module->exports] and @racket[module->imports]);
@racketmodname[scribble/xref] provided documentation cross-reference information;
and DrRacket "blue boxes" data (see @racketmodname[scribble/contract-render]).

The tool does not cover all the modules that ship with Racket; if your favorite is missing, you may want to try editing the value of @racketidfont{interesting-modules} in the @filepath{make-racket-support-files.rkt} script.

Currently the focus is on Emacs support. One of the generated dictionary files is just a plain list of words, whereas the others contain Emacs Lisp declarations.

@section{Supported Dictionaries}

The following files may currently be generated using the included @filepath{Makefile}:

@itemlist[

@item{@filepath{scheme-mode} is suitable for use as a drop-in Emacs Auto Complete Mode dictionary, for example, without any specific Emacs configuration required. It is just a list of symbols without any additional information, and could potentially be used for auto completion in other editors as well.}

@item{@filepath{racket-exports.el} is like the above, but also includes a "Help" text for each symbol, where available. (Where not available, a machine generated Help text is still included, basically just naming the modules that export the symbol.) For a given symbol there may be multiple possible definitions, in which case documented identifiers are preferred. Where the same symbol has multiple documented definitions, these are ranked according to the "importance" of the module name, and the highest ranked one is picked. For example, @racketidfont{car} in @racketmodname[r5rs] or @racketmodname[srfi/1] probably should not be considered as important as the @racket[car] in Racket proper.}

@item{@filepath{racket-urls.el} associates API documentation URLs (for a local Racket installation) with symbols, where documentation is available. Again, as above, only the most highly ranked one of each symbol with get a URL; the generated URL table can hence be used to implement an "I'm feeling lucky" search for the symbols in Racket.}

]

@section{Bugs}

The @filepath{racket-exports.el} dictionary includes associated, brief "Help" documentation with each symbol. The documentation names a module exporting the symbol, but many symbols are exported (or re-exported) from multiple modules; only one of these is included in the Help text, and it is not always the most appropriate choice. For example, for symbol @racket[car] the module @racketmodname[racket/base] is probably a more appropriate and future-proof choice than @racketmodname['#%kernel]. @racket[module-compiled-exports] and @racket[module->exports] do return origin information for exported symbols, and it would probably be possible to use this information to find the "best" module exporting a given symbol.

@section{Missing Features}

We would probably also want to generate a table of symbols naming Racket syntactic forms, i.e. macros. This could then easily be used for syntax highlighting, e.g. with @code{font-lock-add-keywords} in Emacs. Indeed, we can tell which export is syntax and which is a value. Alas, because of contracts and such lots more symbols will appear to be syntax that are actually programmer defined as macros. This would lead to it being confusing to highlight all syntax as keywords. So for now we are not concerned with highlighting.

@section{License}

Except where otherwise noted, the following license applies:

Copyright (C) 2013 University of Bergen and the authors.

Authors: Tero Hasu

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation files
(the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

@section{See Also}

@itemlist[

@item{a @link["http://terohasu.net/blog/2013-08-24-ractionary.html"]{blog post} describing one way to set up Emacs to use the generated dictionary files}

]
