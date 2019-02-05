# QUILC

[![pipeline status](https://gitlab.com/rigetti/quilc/badges/master/pipeline.svg)](https://gitlab.com/rigetti/quilc/commits/master)

Quilc is an advanced optimizing compiler for the quantum instruction
language Quil, licensed under the [Apache 2.0 license](LICENSE.txt).

Quilc comprises two projects. The first, `cl-quil`, does the heavy
lifting of parsing, compiling, and optimizing Quil code. The second,
`quilc`, presents an external interface for using `cl-quil`, either using
the binary `quilc` application directly, or alternatively by
communicating with a server (HTTP or [RPCQ](https://github.com/rigetti/rpcq/)).

Quil is the [quantum instruction language](https://arxiv.org/pdf/1608.03355.pdf) developed at
[Rigetti Computing](https://rigetti.com). In Quil quantum algorithms are expressed using Quil's
native gates and instructions. One can also use Quil's `DEFGATE` to
define new non-native gates, and `DEFCIRCUIT` to build a named circuit
that can be referenced elsewhere in Quil code (analogous to a function
in most other programming languages).

## `CL-QUIL`

`CL-QUIL` is the library that implements parsing and compiling
of Quil code. The code can be found under `./src/`.

### Usage

To get up and running quickly using the `quilc` Docker image, head directly to the
section "Running the Quil Compiler with Docker" below. Otherwise, the following steps
will walk you through how to build the compiler from source.

Follow the instructions in QVM's
[doc/lisp-setup.md](https://github.com/rigetti/qvm/blob/master/doc/lisp-setup.md) to satisfy the
dependencies required to load the `CL-QUIL` library. Afterwhich, the
library can be loaded


``` shell
$ sbcl

```

``` common-lisp
* (ql:quickload :cl-quil)
;;; <snip>compilation output</snip>
(:CL-QUIL)
* (cl-quil:parse-quil-string "H 0")
#<CL-QUIL:PARSED-PROGRAM {100312C643}>
```

A few good entry points to exploring the library are:

* The functions `cl-quil::parse-quil` in [`src/parser.lisp`](src/parser.lisp), and
  `cl-quil:parse-quil-string` in [`src/cl-quil.lisp`](src/cl-quil.lisp) and the various
  transforms therein.
* The function `cl-quil:compiler-hook` which constructs a control-flow
  graph (CFG) and then performs various optimizations on the CFG.

## Quil Compiler

This directory contains the `quilc` application. `quilc` takes as input
arbitrary Quil code, either provided directly to the binary or to the
`quilc` server, and produces optimized Quil code. The compiled code is
optimized for the configured instruction set architecture (ISA),
targeting the native gates specified by the ISA.

### Building the Quil Compiler

Prerequisites to building `quilc` are:

1. Standard UNIX build tools
2. [SBCL (a recent version)](http://www.sbcl.org/): Common Lisp compiler
3. [Quicklisp](https://www.quicklisp.org/beta/): Common Lisp library manager
4. [buildapp](https://github.com/xach/buildapp): Builds executable binaries from Common Lisp software

Building the `quilc` binary is automated using the `Makefile`:

``` shell
$ make quilc
```

This will create a binary `quilc` in the current directory

``` shell
$ ./quilc --version
```

To install system-wide issue the command

``` shell
$ make install
```

### Using the Quil Compiler

The Quil Compiler provides two modes of interaction: (1) communicating
directly with the `quilc` binary, providing your Quil code over `stdin`;
or (2) communicating with the `quilc` server.

#### quilc

The `quilc` binary reads Quil code provided on `stdin`:

``` shell
$ echo "H 0" | quilc
$ cat large_file.quil | quilc
```

#### Server

For various reasons (e.g. not having to repeatedly load the `quilc`
binary into memory, communicating over a network) `quilc` provides a
server interface. `quilc` currently supports two server modes:

##### HTTP

The HTTP server was the original implementation of the server mode. It is now deprecated in favour
of the RPCQ server mode. Do not depend on it. You can create the HTTP server with the `-S` flag
``` shell
$ quilc -S
+-----------------+
|  W E L C O M E  |
|   T O   T H E   |
|  R I G E T T I  |
|     Q U I L     |
| C O M P I L E R |
+-----------------+
Copyright (c) 2016-2019 Rigetti Computing.



>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> IMPORTANT NOTICE <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
The HTTP endpoint has been deprecated in favor of the RPCQ endpoint.  In the
future, it will be removed.  You're advised to modify your client code to talk
to the RPCQ version instead.
>>>>>>>>>>>>>>>>>>>>>>>>>>>>> END IMPORTANT NOTICE <<<<<<<<<<<<<<<<<<<<<<<<<<<<<


[2019-01-29 13:59:18] Starting server: 0.0.0.0 : 6000.
```

##### RPCQ

[RPCQ](https://github.com/rigetti/rpcq/) is an open-source RPC framework developed at Rigetti for
efficient network communication through the QCS stack. The server is started in RPCQ-mode using
the `-R` flag

``` shell
$ quilc -R
+-----------------+
|  W E L C O M E  |
|   T O   T H E   |
|  R I G E T T I  |
|     Q U I L     |
| C O M P I L E R |
+-----------------+
Copyright (c) 2016-2019 Rigetti Computing.

<134>1 2019-01-29T22:03:08Z workstation.local ./quilc 4077 LOG0001 - Launching quilc.
<134>1 2019-01-29T22:03:08Z workstation.local ./quilc 4077 - - Spawning server at (tcp://*:5555) .
```

The server-mode provides to high-level languages such as Python a way
to communicate with the Quil compiler, thus enabling high-level
abstractions and tools that are not directly available in Quil. The
[`pyquil`](https://github.com/rigetti/pyquil) library provides such an interface to `quilc`.

# Automated Packaging with Docker

The CI pipeline for `quilc` produces a Docker image, available at
[`rigetti/quilc`](https://hub.docker.com/r/rigetti/quilc). To get the latest stable
version of `quilc`, run `docker pull rigetti/quilc`.

# Running the Quil Compiler with Docker

As outlined above, the Quil Compiler supports two modes of operation: stdin and server.

To run `quilc` in stdin mode, do one either of the following:

1) `docker run --rm -it rigetti/quilc`

The containerized compiler will then read whatever newline-separated Quil instructions you
enter, waiting for an EOF signal (C+d) to compile it.

2) `echo "H 0" | docker run --rm -i rigetti/quilc`

You can pipe Quil instructions into the `quilc` container if you drop the `-t`.

To run `quilc` in server mode, do the following:

`docker run --rm -it -p 5555:5555 rigetti/quilc -R`

This will spawn an RPCQ-mode `quilc` server, that you can communicate with over TCP. If
you would like to change the port of the server to PORT, you can alter the command as follows:

`docker run --rm -it -p PORT:PORT rigetti/quilc -R -p PORT`

# Release Process

1. Update `VERSION.txt` and dependency versions (if applicable) and push the commit to `master`.
2. Push a git tag `vX.Y.Z` that contains the same version number as in `VERSION.txt`.
3. Verify that the resulting build (triggered by pushing the tag) completes successfully.
4. Publish a [release](https://github.com/rigetti/quilc/releases) using the tag as the name.
5. Close the [milestone](https://github.com/rigetti/quilc/milestones) associated with this release,
   and migrate incomplete issues to the next one.

# Get involved!

We welcome and encourage community contributions! Peruse our
[guidelines for contributing](CONTRIBUTING.md) to get you up to speed on
expectations. Once that's clear, a good place to start is the
[good first issue](https://github.com/rigetti/quilc/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22)
section. If you have any find bugs, please create an
[issue](https://github.com/rigetti/quilc/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22).
If you need help with some code or want to discuss some technical issues, you can find us in the
`#dev` channel on [slack](https://slack.rigetti.com/).

We look forward to meeting and working with you!
