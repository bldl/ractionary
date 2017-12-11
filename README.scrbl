#lang scribble/manual

@(require (for-label racket/base))

@title{Ractionary}

@author[@hyperlink["http://terohasu.net/"]{Tero Hasu}]

@section{Introduction}

Ractionary is a set of tools for generating dictionaries for Racket language aware tool support. The generation is done based on information available through Racket's own facilities, including: 
@racketmodname[scribble/xref] provided documentation cross-reference information;
and DrRacket "blue boxes" data (see @racketmodname[scribble/contract-render]).

Currently the focus is on @seclink["emacs-use"]{Emacs support}. One of the generated dictionary file formats is just a plain list of words, whereas the others contain Emacs Lisp declarations.

@section{Source Code}

The Ractionary source code repository is at
@nested[#:style 'inset]{@url{https://github.com/bldl/ractionary}}

@section{Generated Dictionaries}

The following files may currently be generated as shown in the included @filepath{Makefile}:

@itemlist[

@item{@filepath{racket-mode} is suitable for use as a drop-in Emacs Auto Complete Mode dictionary, for example, without any specific Emacs configuration required. It is just a list of bare symbols (one per line) without any additional information, and could potentially be used for auto completion in other editors as well.}

@item{@filepath{ractionary-words.el} is like the above, but it declares an Emacs Lisp variable @tt{ractionary-dictionary} whose value is the list of strings constituting a Racket vocabulary.}

@item{@filepath{ractionary-words-help.el} is like the above, but its @tt{ractionary-dictionary-with-help} variable also includes a "Help" text for each symbol. For a given symbol there may be multiple possible definitions; these are ranked according to the "importance" of the Racket module providing it (as defined by @filepath{ranking.rkt}), and the highest ranked one is picked. For example, @racketidfont{car} in @racketmodname[r5rs] or @racketmodname[srfi/1] probably should not be considered as important as the @racket[car] in Racket proper.}

@item{@filepath{ractionary-urls.el} associates API documentation URLs (for a local Racket installation) with symbols. Where the same symbol has multiple documented definitions, each of them will get its own URL. The generated URL table can hence be used either to implement an "I'm feeling lucky" search for the symbols in Racket, or a set of choices can be presented to the user.}

]

@section[#:tag "emacs-use"]{Dictionary Data Use in Emacs}

Some examples of Ractionary-generated dictionary uses include:

@itemlist[

@item{@hyperlink["examples/emacs-company-vanilla/company-ractionary.el"]{emacs-company-vanilla}:
A basic @hyperlink["https://company-mode.github.io/"]{Company} mode backend that does nothing but offer completions from among symbols an a Ractionary-generated dictionary.}

@item{@hyperlink["examples/emacs-company-quickhelp/company-ractionary.el"]{emacs-company-quickhelp}:
A backend similar to the above, but adds help strings for possible display with @hyperlink["https://github.com/expez/company-quickhelp"]{Company quickhelp}.}

@item{@hyperlink["examples/emacs-company-racket-mode/company-ractionary.el"]{emacs-company-racket-mode}:
Complements the above Company backend with another one, which wraps the symbol completion functionality of @hyperlink["https://github.com/greghendershott/racket-mode"]{racket-mode}, whose @tt{racket-complete-at-point} function does completion in a dynamic and context sensitive manner.

This example shows that it is possible to combine both a static dictionary for readily available context-insensitive completion, and more delicate and processing-intensive dynamic and contextual completion.
The two backends of this example can further be complemented with Company's own @tt{company-dabbrev-code} backend, which provides generic-yet-contextual completion of non-comment symbols (e.g., those in the current buffer, or in same-mode buffers).}

@item{@hyperlink["examples/emacs-company-scribble/company-ractionary.el"]{emacs-company-scribble}:
This example adapts the earlier @hyperlink["examples/emacs-company-quickhelp/company-ractionary.el"]{emacs-company-quickhelp} backend for completion of @"@"-expressions' initial symbols.
While @hyperlink["http://www.neilvandyke.org/scribble-emacs/"]{@tt{scribble-mode}} comes built in with completion for common Scribble symbols, a more extensive dictionary might be worth having when editing Scribble documents that make heavy use of additional abstractions, or particularly in languages such as @racketmodname[at-exp] @racketmodname[racket].}

@item{@hyperlink["http://terohasu.net/blog/2013-08-24-ractionary.html"]{``Dictionary-Enabled Racket Support for Emacs''}:
A blog post describing one way to set up Emacs to use the generated dictionary files for @hyperlink["https://github.com/auto-complete/auto-complete"]{Auto Complete} mode based symbol completion, and for quick opening of documentation for a symbol. The post is somewhat out of date with respect to the current version of Ractionary.}

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
