# Ractionary

[Tero Hasu](http://terohasu.net/)

## 1. Introduction

Ractionary is a set of tools for generating dictionaries for Racket
language aware tool support. The generation is done based on information
available through Racket’s own facilities, including: `scribble/xref`
provided documentation cross-reference information; and DrRacket "blue
boxes" data \(see `scribble/contract-render`\).

Currently the focus is on Emacs support. One of the generated dictionary
file formats is just a plain list of words, whereas the others contain
Emacs Lisp declarations.

## 2. Supported Dictionaries

The following files may currently be generated as shown in the included
`"Makefile"`:

* `"racket-mode"` is suitable for use as a drop-in Emacs Auto Complete
  Mode dictionary, for example, without any specific Emacs configuration
  required. It is just a list of bare symbols \(one per line\) without
  any additional information, and could potentially be used for auto
  completion in other editors as well.

* `"ractionary-words.el"` is like the above, but it declares an Emacs
  Lisp variable `ractionary-dictionary` whose value is the list of
  strings constituting a Racket vocabulary.

* `"ractionary-words-help.el"` is like the above, but its
  `ractionary-dictionary-with-help` variable also includes a "Help" text
  for each symbol. For a given symbol there may be multiple possible
  definitions; these are ranked according to the "importance" of the
  Racket module providing it \(as defined by `"ranking.rkt"`\), and the
  highest ranked one is picked. For example, `car` in `r5rs` or `srfi/1`
  probably should not be considered as important as the `car` in Racket
  proper.

* `"ractionary-urls.el"` associates API documentation URLs \(for a local
  Racket installation\) with symbols. Where the same symbol has multiple
  documented definitions, each of them will get its own URL. The
  generated URL table can hence be used either to implement an "I’m
  feeling lucky" search for the symbols in Racket, or a set of choices
  can be presented to the user.

## 3. See Also

* The [racket-mode](https://github.com/greghendershott/racket-mode)
  major mode for Emacs, which also supports completion, of a dynamic and
  context sensitive kind—it is possible to use both: a static dictionary
  for readily available context-insensitive completion, and
  racket-mode’s completion for when a program has been analyzed for
  information about its top-level namespace.

* The `"examples"` directory contains example
  [Company](https://company-mode.github.io/) mode backend
  implementations making use of Ractionary-generated dictionaries. Most
  interestingly, the `"emacs-company-racket-mode"` example shows how to
  achieve completion that combines \(1\) a Ractionary-generated
  dictionary, \(2\) `racket-complete-at-point`, and \(3\) non-comment
  symbols in the current buffer.

* A [blog post](http://terohasu.net/blog/2013-08-24-ractionary.html)
  describing one way to set up Emacs to use the generated dictionary
  files, including the use of [Auto
  Complete](https://github.com/auto-complete/auto-complete) mode for
  completion. The post is somewhat out of date with respect to the
  current version of Ractionary.

## 4. Related Software

* [racket-mode](https://github.com/greghendershott/racket-mode)

* [scribble.el](http://www.neilvandyke.org/scribble-emacs/)

* [Company](https://company-mode.github.io/)

* [Company quickhelp](https://github.com/expez/company-quickhelp)

* [Auto Complete](https://github.com/auto-complete/auto-complete)

## 5. License

Except where otherwise noted, the following license applies:

Copyright \(C\) 2013-2017 University of Bergen and the authors.

Authors: Tero Hasu

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files \(the
"Software"\), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
