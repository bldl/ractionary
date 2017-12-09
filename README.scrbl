#lang scribble/manual

@(require (for-label racket/base))

@title{Ractionary}

@author[@hyperlink["http://terohasu.net/"]{Tero Hasu}]

@section{Introduction}

Ractionary is a set of tools for generating dictionaries for Racket language aware tool support. The generation is done based on information available through Racket's own facilities, including: 
@racketmodname[scribble/xref] provided documentation cross-reference information;
and DrRacket "blue boxes" data (see @racketmodname[scribble/contract-render]).

Currently the focus is on Emacs support. One of the generated dictionary file formats is just a plain list of words, whereas the others contain Emacs Lisp declarations.

@section{Supported Dictionaries}

The following files may currently be generated as shown in the included @filepath{Makefile}:

@itemlist[

@item{@filepath{racket-mode} is suitable for use as a drop-in Emacs Auto Complete Mode dictionary, for example, without any specific Emacs configuration required. It is just a list of bare symbols (one per line) without any additional information, and could potentially be used for auto completion in other editors as well.}

@item{@filepath{ractionary-words.el} is like the above, but it declares an Emacs Lisp variable @tt{ractionary-dictionary} whose value is the list of strings constituting a Racket vocabulary.}

@item{@filepath{ractionary-words-help.el} is like the above, but its @tt{ractionary-dictionary-with-help} variable also includes a "Help" text for each symbol. For a given symbol there may be multiple possible definitions; these are ranked according to the "importance" of the Racket module providing it (as defined by @filepath{ranking.rkt}), and the highest ranked one is picked. For example, @racketidfont{car} in @racketmodname[r5rs] or @racketmodname[srfi/1] probably should not be considered as important as the @racket[car] in Racket proper.}

@item{@filepath{ractionary-urls.el} associates API documentation URLs (for a local Racket installation) with symbols. Where the same symbol has multiple documented definitions, each of them will get its own URL. The generated URL table can hence be used either to implement an "I'm feeling lucky" search for the symbols in Racket, or a set of choices can be presented to the user.}

]

@section{See Also}

@itemlist[

@item{The @hyperlink["https://github.com/greghendershott/racket-mode"]{racket-mode} major mode for Emacs, which also supports completion, of a dynamic and context sensitive kind---it is possible to use both: a static dictionary for readily available context-insensitive completion, and racket-mode's completion for when a program has been analyzed for information about its top-level namespace.}

@item{The @filepath{examples} directory contains example @hyperlink["https://company-mode.github.io/"]{Company} mode backend implementations making use of Ractionary-generated dictionaries. Most interestingly, the @filepath{emacs-company-racket-mode} example shows how to achieve completion that combines (1) a Ractionary-generated dictionary, (2) @tt{racket-complete-at-point}, and (3) non-comment symbols in the current buffer.}

@item{A @hyperlink["http://terohasu.net/blog/2013-08-24-ractionary.html"]{blog post} describing one way to set up Emacs to use the generated dictionary files, including the use of @hyperlink["https://github.com/auto-complete/auto-complete"]{Auto Complete} mode for completion. The post is somewhat out of date with respect to the current version of Ractionary.}

]

@section{Related Software}

@itemlist[

@item{@hyperlink["https://github.com/greghendershott/racket-mode"]{racket-mode}}

@item{@hyperlink["http://www.neilvandyke.org/scribble-emacs/"]{scribble.el}}

@item{@hyperlink["https://company-mode.github.io/"]{Company}}

@item{@hyperlink["https://github.com/expez/company-quickhelp"]{Company quickhelp}}

@item{@hyperlink["https://github.com/auto-complete/auto-complete"]{Auto Complete}}

]

@section{License}

Except where otherwise noted, the following license applies:

Copyright (C) 2013-2017 University of Bergen and the authors.

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
