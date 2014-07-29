#lang racket/base

#|

A selection of "root modules" to use when searching recursively for
exported symbols to enter into a dictionary.

|#

(provide interesting-modules)

(define interesting-modules
  '('#%kernel

    racket

    racket/async-channel  
    racket/base  
    racket/block  
    racket/bool  
    racket/bytes  
    racket/class  
    racket/cmdline  
    racket/contract  
    racket/contract/combinator  
    racket/contract/parametric  
    racket/contract/region  
    racket/control  
    racket/date  
    racket/dict  
    racket/draw  
    racket/draw/draw-sig  
    racket/draw/draw-unit  
    racket/enter  
    racket/fasl  
    racket/file  
    racket/fixnum  
    racket/flonum  
    racket/function  
    racket/future  
    racket/generator  
    racket/gui  
    racket/gui/base  
    racket/gui/dynamic  
    racket/gui/init  
    racket/help  
    racket/include  
    racket/init  
    racket/language-info  
    racket/list  
    racket/load  
    racket/local  
    racket/match  
    racket/math  
    racket/mpair  
    racket/package  
    racket/path  
    racket/place  
    racket/port  
    racket/pretty  
    racket/promise  
    racket/provide  
    racket/provide-syntax  
    racket/provide-transform  
    racket/require  
    racket/require-syntax  
    racket/require-transform  
    racket/runtime-config  
    racket/runtime-path  
    racket/sandbox  
    racket/sequence  
    racket/serialize  
    racket/set  
    racket/shared  
    racket/snip  
    racket/splicing  
    racket/stream  
    racket/string  
    racket/struct-info  
    racket/stxparam  
    racket/stxparam-exptime  
    racket/surrogate  
    racket/syntax  
    racket/system  
    racket/tcp  
    racket/trace  
    racket/trait  
    racket/udp  
    racket/unit  
    racket/unit-exptime  
    racket/unsafe/ops  
    racket/vector  

    syntax/boundmap  
    syntax/context  
    syntax/define  
    syntax/docprovide  
    syntax/flatten-begin  
    syntax/free-vars  
    syntax/id-table
    syntax/kerncase  
    syntax/keyword
    syntax/location  
    syntax/modcode
    syntax/modcollapse  
    syntax/moddep
    syntax/modread  
    syntax/modresolve
    syntax/module-reader  
    syntax/name  
    syntax/parse  
    syntax/parse/debug  
    syntax/parse/define  
    syntax/path-spec
    syntax/quote
    syntax/readerr  
    syntax/srcloc  
    syntax/strip-context  
    syntax/struct  
    syntax/stx  
    syntax/template  
    syntax/to-string  
    syntax/toplevel  
    syntax/trusted-xforms

    pict
    scribble/manual
    scribble/xref

    setup/dirs
    setup/xref

    data/order
    data/queue
    data/skip-list
    data/splay-tree

    rackunit
    
    xml

    json

    net/url
    net/url-structs

    unstable/error
    ))

