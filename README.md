# QUILC

QUILC comprises two projects. The first, `cl-quil`, does the heavy
lifting of parsing, compiling, and optimizing Quil code. The second,
`quilc`, presents an external interface for using `cl-quil`, either
using the binary `quilc` application directly, or alternatively by
communicating with a server.

## `CL-QUIL`

`CL-QUIL` is the library that implements lexing, parsing, and
compiling of Quil code. The code can be found under `./src/`.

### Usage

Follow the instructions in `docs/lisp-setup.md` to satisfy the
dependencies required to load the `CL-QUIL` library. Afterwhich, the
library can be loaded 

``` shell
$ sbcl

* (ql:quickload :cl-quil)
;;; <snip>compilation output</snip>
(:CL-QUIL)
* (cl-quil:parse-quil-string "H 0")
#<CL-QUIL:PARSED-PROGRAM {100312C643}>
```

A few good entry points to exploring the library are:

* The functions `cl-quil::parse-quil` in
   [`src/parser.lisp`](src/parser.lisp), and
   `cl-quil:parse-quil-string` in
   [`src/cl-quil.lisp`](src/cl-quil.lisp) and the various transforms
   therein.
* The function `cl-quil:compiler-hook` which constructs a control-flow
  graph (CFG) and then performs various optimizations on the CFG.

## Quil Compiler

This directory contains the `quilc` application. `quilc` takes as
input arbitrary Quil code, either provided directly to the binary or
to the `quilc` server, and produces optimized Quil code. The compiled
code is optimized for the configured instruction set architecture
(ISA), targeting the native gates specified by the ISA.

### Building the Quil Compiler

Prerequisites to building `quilc` are:
0. Standard unix build tools
1. [SBCL (a recent version)](http://www.sbcl.org/): Common Lisp
   compiler
2. [Quicklisp](https://www.quicklisp.org/beta/): Common Lisp library
   manager
3. [buildapp](https://github.com/xach/buildapp): Builds executable
   binaries from Common Lisp software
4. [CL-QUIL](https://github.com/rigetti/cl-quil): The Common Lisp Quil
   compiler

Building the `quilc` binary is automated using the `Makefile`:

    make quilc

### Using the Quil Compiler

The Quil Compiler provides two modes of interaction: (1) communicating
directly with the `quilc` binary, providing your Quil code over
`stdin`; or (2) communicating with the `quilc` server.

#### quilc

The `quilc` binary reads Quil code provided on `stdin`:

    echo "H 0" | quilc
    cat large_file.quil | quilc

#### Server

For various reasons (e.g. not having to repeatedly load the `quilc`
binary into memory, communicating over a network) `quilc` provides a
server interface:

    quilc -S

This provides high-level languages such as Python a way to communicate
with the Quil compiler, thus enabling high-level abstractions and
tools that are not directly available in Quil. The `pyquil` library
provides such an interface to `quilc`.
