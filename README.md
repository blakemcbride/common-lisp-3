
# Common Lisp 3

This repository represents the author's view on a new dialect
of Common Lisp called Common Lisp 3.  It is an effort to modernize and 
clean up the ANSI Common Lisp standard.  

Common Lisp 3 consists of ANSI Common Lisp with the following changes:

1. The function and variable namespaces have been collapsed into a single
namespace. This simplifies the language and makes it easier to perform
functional programming.

2. Although CLOS is untouched, Common Lisp 3 adds a layer on top of CLOS
that facilitates the creation of object-oriented code.  This makes it
easier to do the things that most programmers do most of the time.

3. Common Lisp 3 supports case-sensitive symbols.

4. Common Lisp 3 supports native threads.

5. Common Lisp 3 supports tail recursion elimination.

This repository contains code that adds items 1, 2, and 3 to ANSI
Common Lisp.

Items 4 and 5 are already a part of the SBCL Common Lisp implementation.

## Usage

Although this package works correctly in most versions of Common Lisp,
you would not get the native threads and tail recursion enhancements
offered by SBCL.  SBCL is therefore the recommended implementation.

The system can be loaded with the following Lisp commands:

```
(load "cl3")
(use-package :cl3)
```

## Portability

This code works on:

* SBCL
* CLISP
* ABCL
* CCL

It is known not to work on:

* ECL
* MKCL

## Needed

It is known that a comprehensive manual is needed.

## Source

The source code for this system is located at https://github.com/blakemcbride/common-lisp-3

It was written by Blake McBride (blake@mcbridemail.com)
