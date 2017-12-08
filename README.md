# Ractionary

**Racket Dictionary Generator**

Tero Hasu <[tero at hasu dot is](mailto:tero at hasu dot is)>

## 1. Introduction

This is a tool for generating dictionaries for Racket language aware
tools support. The generation is done based on information available
through Racket’s own facilities, including: `scribble/xref` provided
documentation cross-reference information; and DrRacket "blue boxes"
data \(see `scribble/contract-render`\).

Currently the focus is on Emacs support. One of the generated dictionary
files is just a plain list of words, whereas the others contain Emacs
Lisp declarations.

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

* The `"examples"` directory contains example
  [Company](https://company-mode.github.io/) mode backend
  implementations making use of Ractionary-generated dictionaries.

* A [blog post](http://terohasu.net/blog/2013-08-24-ractionary.html)
  describing one way to set up Emacs to use the generated dictionary
  files, including the use of [Auto
  Complete](http://www.cx4a.org/software/auto-complete/) mode for
  completion. The post is somewhat out of date with respect to the
  current version of Ractionary.

* The [racket-mode](https://github.com/greghendershott/racket-mode)
  major mode for Emacs, which also supports completion, of a dynamic and
  context sensitive kind—it should be possible to use both: a static
  dictionary for faster completion as you type, but setting up a trigger
  to escape to context-sensitive completion \(possibly by calling
  `ac-stop`, then `completion-at-point`, if using Auto Complete mode\)
  where the static dictionary does not have the desired symbol.

## 4. Missing Features

We might also want to generate a table of symbols naming Racket
syntactic forms, i.e., macros. This could then easily be used for syntax
highlighting, e.g. with `font-lock-add-keywords` in Emacs. Indeed, we
can tell which export is syntax and which is a value. Alas, because of
contracts and such lots more symbols will appear to be syntax than are
actually programmer defined as macros. This would lead to it being
confusing to highlight all macro names as keywords. So for now we are
not concerned with highlighting.

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
