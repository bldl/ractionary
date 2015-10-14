#lang scribble/manual

@(require (for-label racket/base))

@title{Ractionary}
@bold{Racket Dictionary Generator}

@author+email["Tero Hasu" "tero at ii.uib.no"]

@section{Introduction}

This is a tool for generating dictionaries for Racket language aware tools support. The generation is done based on information available through Racket's own facilities, including: 
@racketmodname[scribble/xref] provided documentation cross-reference information;
and DrRacket "blue boxes" data (see @racketmodname[scribble/contract-render]).

Currently the focus is on Emacs support. One of the generated dictionary files is just a plain list of words, whereas the others contain Emacs Lisp declarations.

@section{Supported Dictionaries}

The following files may currently be generated using as shown in the included @filepath{Makefile}:

@itemlist[

@item{@filepath{racket-mode} is suitable for use as a drop-in Emacs Auto Complete Mode dictionary, for example, without any specific Emacs configuration required. It is just a list of symbols without any additional information, and could potentially be used for auto completion in other editors as well.}

@item{@filepath{racket-exports.el} is like the above, but also includes a "Help" text for each symbol. For a given symbol there may be multiple possible definitions; these are ranked according to the "importance" of the module name, and the highest ranked one is picked. For example, @racketidfont{car} in @racketmodname[r5rs] or @racketmodname[srfi/1] probably should not be considered as important as the @racket[car] in Racket proper.}

@item{@filepath{racket-urls.el} associates API documentation URLs (for a local Racket installation) with symbols. Where the same symbol has multiple documented definitions, each of them will get a URL. The generated URL table can hence be used either to implement an "I'm feeling lucky" search for the symbols in Racket, or a set of choices can be presented to the user.}

]

@section{Missing Features}

We would probably also want to generate a table of symbols naming Racket syntactic forms, i.e. macros. This could then easily be used for syntax highlighting, e.g. with @code{font-lock-add-keywords} in Emacs. Indeed, we can tell which export is syntax and which is a value. Alas, because of contracts and such lots more symbols will appear to be syntax than are actually programmer defined as macros. This would lead to it being confusing to highlight all syntax as keywords. So for now we are not concerned with highlighting.

@section{License}

Except where otherwise noted, the following license applies:

Copyright (C) 2013-2015 University of Bergen and the authors.

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

@item{a @link["http://terohasu.net/blog/2013-08-24-ractionary.html"]{blog post} describing one way to set up Emacs to use the generated dictionary files (somewhat out of date wrt this version)}

@item{@link["https://github.com/greghendershott/racket-mode"]{racket-mode} for Emacs, which also now supports completion, of a dynamic and context sensitive kind---it should be possible to use both: a static dictionary for faster completion as you type, but setting up a trigger to escape to context-sensitive completion (possibly by calling @code{ac-stop}, then @code{completion-at-point}) where the fixed dictionary does not have the desired symbol}

]
