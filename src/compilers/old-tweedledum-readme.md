The following README used to be included with the Tweedledum contrib, which we used to efficiently compile classical logic gates. This functionality has since been replaced with native quilc (Common Lisp) code, replacing our only use of Tweedledum; however this exposition of how quilc handles classical logic gates with and without a specialized permutation gate synthesis routine is still valuable.

====================================================================
# Efficient compilation of classical logic gates

The Quil compiler (quilc), being a compiler of quantum programs, must be able to
operate on the group of unitary matrices. In the general case, quilc produces
efficient and optimized output that can be run on some QPU architecture. Often
times, however, quilc produces inefficient output.

## How quilc compiles the Toffoli gate

Take for example the compilation of the [Toffoli gate](https://en.wikipedia.org/wiki/Toffoli_gate) (also known as the
`CCNOT`). It is known that the Toffoli gate --- itself being gate of three qubits
--- can be decomposed some number of 1-qubit gates (rotations, etc.) and a
minimum of six 2-qubit gates (e.g. controlled-not). So does quilc give optimal
output when compiling `CCNOT 2 1 0`? Check it out:

```common-lisp
(print-parsed-program (compiler-hook (parse-quil "CCNOT 2 1 0") (build-nq-fully-connected-chip 3 :architecture '(:CNOT))))
```
```
RZ(pi/2) 1                              # Entering rewiring: #(0 1 2)
# ...
CNOT 0 1
# ...
CNOT 0 2
# ...
CNOT 0 1
# ...
CNOT 0 2
# ...
CNOT 1 2
# ...
CNOT 1 2
# ...
HALT                                    # Exiting rewiring: #(0 1 2)
```

That's six 2-qubit gates. Optimal! Neat! But not quite --- there is a translation
rule within the compiler that says: if the input instruction is of the form
`CCNOT a b c` then immediately replace it by the known optimal decomposition.
That is, this compilation is hard-coded (and hand-coded) into the compiler. If
we disable that translator, then we get the less-than-optimal result:

```common-lisp
(print-parsed-program (compiler-hook (parse-quil "CCNOT 2 1 0") (build-nq-fully-connected-chip 3 :architecture '(:CNOT))))
```
```
RZ(-3*pi/4) 0                           # Entering rewiring: #(0 1 2)
# ...
CNOT 0 1
# ...
CNOT 0 1
# ...
CNOT 0 2
# ...
CNOT 1 2
# ...
CNOT 0 2
# ...
CNOT 1 2
# ...
CNOT 0 1
# ...
HALT                                    # Exiting rewiring: #(0 1 2)
```

A whopping seven 2-qubit gates. Well that's not so bad, but that was for a
somewhat simple 3-qubit gate. Much larger gates regularly show up, and quilc's
`DEFGATE` allows arbitrarily large gates, so we ought to do the best job we can;
and we ought to do it automatically, without relying on hard-coded translators.

## Classical logic gates, quilc, and Tweedledum

One of those cases in which quilc tends to produce non-optimal output is
compilation of classical logic gates. A classical logic gate is one whose matrix
representation has in any column or row one *and only one* `1` (that's an ugly
mouthful), and the rest of the matrix is made up of zeros. These are called
permutation matrices, because they have the effect of re-ordering the vectors to
which they're applied. For example, the Toffoli gate is a classical logic gate:
it is defined by the matrix

```
         / 1 0 0 0 0 0 0 0 \
         | 0 1 0 0 0 0 0 0 |
         | 0 0 1 0 0 0 0 0 |
CCNOT =: | 0 0 0 1 0 0 0 0 |
         | 0 0 0 0 1 0 0 0 |
         | 0 0 0 0 0 1 0 0 |
         | 0 0 0 0 0 0 0 1 |
         \ 0 0 0 0 0 0 1 0 /.
```

Quilc is designed with this larger goal in mind: as noted before, quilc will
usually compile anything you can throw at it --- any *unitary* matrix, that is.
Tweedledum, on the other hand, is/was developed to specifically compile
classical logic gates. This is good news because (1) classical logic
synthesis/decomposition is a better understood problem, allowing its authors to
produce very efficient compilation output; and (2) classical logic gates are
*unitary* gates and so are candidates for quantum computation. In fact, since
classical logic gates are so well understood, they are often used in
benchmarking quantum computers. Naturally then it makes sense that our compiler
produce optimal code so that QPU benchmarks are not bottlenecked by poor code.

## tweedledum.lisp

Rather than reinvent the wheel, we have chosen to write a small C wrapper to
Tweedledum's C++ code that exposes a subset of Tweedledum's functionality. We
are then able to call into this C library using Common Lisp's gnarly CFFI
tooling.

## Installation

To build the `cl-quil/tweedledum` package, you must first make sure that the
tweedledum submodule is initialized
```
$ git submodule init
$ git submodule update --init
```

You can then load the `cl-quil/tweedledum` package
```
CL-USER> (ql:quickload :cl-quil/tweedledum)
CL-USER> (cl-quil/tweedledum:synthesis-dbs '(0 1 3 2))
"H 0
RZ(1.57079637050628662) 1
RZ(1.57079637050628662) 0
CNOT 1 0
RZ(-1.57079637050628662) 0
CNOT 1 0
H 0
"
```

The shared library has been automagically compiled, CFFI has installed its
hooks, and now `cl-quil` has some more neat-o compilation routines.

For an example of how this improves compilation, we can compile a notoriously
difficult test-case which uses a 8-qubit oracle for the Bernstein-Vazirani
algorithm.
 - Without `cl-quil/tweedledum`:
 ```
 CL-USER> (time (cl-quil-tests::compare-compiled #P"./tests/compiler-hook-test-files/sohaib.quil" ':cnot))
WARNING: Condition FIASCO::IS-ASSERTION was signalled.
.
Evaluation took:
  6.439 seconds of real time
  6.470664 seconds of total run time (6.283246 user, 0.187418 system)
  [ Run times consist of 0.420 seconds GC time, and 6.051 seconds non-GC time. ]
  100.50% CPU
  2,051 forms interpreted
  17,462,268,343 processor cycles
  4,891,270,304 bytes consed

T
 ```
 - With `cl-quil/tweedledum`:
 ```
 CL-USER> (ql:quickload :cl-quil/tweedledum)
 CL-USER> (time (cl-quil-tests::compare-compiled #P"./tests/compiler-hook-test-files/sohaib.quil" ':cnot))
WARNING: Condition FIASCO::IS-ASSERTION was signalled.
.
Evaluation took:
  0.416 seconds of real time
  0.418083 seconds of total run time (0.384058 user, 0.034025 system)
  [ Run times consist of 0.042 seconds GC time, and 0.377 seconds non-GC time. ]
  100.48% CPU
  2,055 forms interpreted
  1,128,897,003 processor cycles
  259,116,096 bytes consed

T
 ```

A big reduction in both the time and memory used!
