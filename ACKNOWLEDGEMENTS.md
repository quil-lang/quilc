# Acknowledgements

## Pre-Release History

Within the first half of 2016, the first version of Quil was being
created, originally called "QIL" for "Quantum Instruction
Language". Around May, it was renamed to Quil because it both looks
and sounds nicer. The concrete development of Quil was started by
Robert Smith and continues to be developed by him. Development began
along the efforts contained within two software projects. The first
project is CL-QUIL, which sought to provide syntax to Quil, and the
second project is the [QVM](https://github.com/rigetti/qvm), which
sought to provide semantics to Quil.

CL-QUIL started off as a parser for Quil. Quil programs used to be
represented as S-expressions, and was written like so:

```
((H       0)
 (CNOT    0 1)
 (MEASURE 0 0))
```

The parser was a combination of the Lisp function
[`READ`](http://www.lispworks.com/documentation/lw50/CLHS/Body/f_rd_rd.htm)
along with a simple walker to translate the forms into primitive
structure objects. Eventually, a more assembly-like syntax was
preferred, and a recursive descent parser was written. This included
the development of a new lexical analyzer generator called
[ALEXA](https://github.com/rigetti/alexa) which was open-sourced in
May 2016.

With the introduction of the `DEFCIRCUIT` and `LABEL` constructs, more
sophisticated program analysis was required (e.g., for circuit
expansion and label resolution). Around July 2016, the CL-QUIL project
was extended to be a compilation framework for Quil, starting off with
many concepts in classical compiler theory like control flow graphs,
resource parallelization, and [nano-pass
concepts](https://www.cs.indiana.edu/~dyb/pubs/nano-jfp.pdf). Development
of these concepts continues.

Around a year later, in July 2017, Rigetti hired interns, some of whom
worked on what has now become the front-end of a Quil compiler. Aaron
Vontell (@vontell) contributed new analysis and optimization passes,
including dead-code elimination, program reconstitution,
pretty-printing, and gate fusion. Joseph Lin (@joelin0) brought MAGICL
to life, getting complex double-precision arithmetic functions
working, getting a Fortran parser working (Sorry, Joe...), and
improving the compiler's static error detection facilities.

At the same time, Eric Peterson (@ecp-rigetti) joined and started
working on the quantum aspect of compilation. This was the birth of
`quilc`, a user-application and front-end to CL-QUIL used for
compiling between different quantum abstract machines. In particular,
he has implemented gate decomposition schemes, peephole optimization,
CFG improvements, and a lot more. Eric has been a main developer of
CL-QUIL and quilc since.

Toward the end of 2017, Robert Smith (@tarballs-are-good), Nikolas
Tezak (@ntezak), and Anthony Polloreno (@ampolloreno) collaborated to
include the Pauli and Clifford groups, with both an algebraic and
symplectic representation. This provides both a polynomial-time
simulatable representation of some common quantum operators, as well
as a method to calculate sequences for [randomized
benchmarking](https://arxiv.org/abs/0707.0963).

In the intern group of 2018, Corwin de Boor (@Strikeskids) implemented
a variety of new addressing and rewiring schemes, akin to register
allocation of qubits. He also contributed ideas and improvements to
the syntax, parsing, and analysis of Quil, including precise resource
tracking.

In the fall of 2018, `quilc` was released in binary form for Windows,
Mac, and Linux as a part of the Forest SDK, compatible with Rigetti's
Quantum Cloud Services. This couldn't have been done without Peter
Karalekas's (@karalekas) and Zach Beane's (@xach) extensive help
versioning, testing, releasing, and deploying `quilc`.

In the back half of 2018, Mark Skilbeck contributed the implementation
of `DAGGER` expansion of circuits, maturing the benchmark suite, and
porting & packaging `quilc` for Windows. Lauren Capelluto contributed
the implementation of arithmetic rewriting.

CL-QUIL and quilc benefited extensively from internal testers at
Rigetti, as well as the thousands of users of Quantum Cloud Services.

## Credits Roll

For security and privacy, the git history has been removed. The
contributors of the project, in approximately the order of their
commit count according to `git shortlog --summary --numbered`, prior
to open-sourcing were:

* **Eric Peterson** (decomposition and optimization framework, too many contributions to count)
* **Robert Smith** (parser, CFG generation, symplectic representations)
* **Mark Skilbeck** (`DAGGER` expansion, benchmarking, Windows port, Google & IBM architecture ports)
* **Peter Karalekas** (release management, automated testing, versioning, and deployment)
* **Nikolas Tezak** (Pauli and Clifford group algebra, gate resolution, `PRAGMA` formalization)
* **Zach Beane** (porting to CCL & LispWorks, release management, Quicklisp & `buildapp`)
* **Corwin de Boor** (resource tracking, implementation and comparative study of rewiring schemes)
* **Lauren Capelluto** (arithmetic rewriting)
* **Erik Davis** (optimization of resource tracking)
* **Aaron Vontell** (CFG optimizations, gate fusion)
* **Anthony Polloreno** (Clifford groups, randomized benchmarking, SWAP quotient)
* **Chris Osborn** (dead qubit detection)
* **Joseph Lin** (MAGICL, static arity and gate resolution analysis)

## Open Source

CL-QUIL and quilc proudly depend on the work of the open source
community, as well as [SBCL](http://www.sbcl.org/),
[ASDF](https://common-lisp.net/project/asdf/), and Zach Beane's
continued maintenance of the [Quicklisp](https://www.quicklisp.org/)
project and repository.