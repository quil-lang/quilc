# QUILC BACKENDS

## What is a backend?

A backend is a mechanism to emit device-specific code, usually but not necessarily a binary. The code that a backend receives is guaranteed to comport to a particular CHIP-SPECIFICATION.

## This directory

The `backends/` folder contains the implementations of the backends, as well as common code useful for constructing a backend. Every backend should have its own folder in this directory.

## How to add a new backend

The `quil` backend is a minimal, dummy backend that emits Quil, and can be used to model a new, more complicated backend. The basic steps are this:

1. Make a new directory for your backend.

2. Implement the `BACKEND`, `EXECUTABLE`, and `COMPILATION` protocols in `common.lisp`.

## Example usage

```
(let* ((chip (build-nq-fully-connected-chip 5))
       (orig (parse-quil "CONTROLLED CONTROLLED H 0 1 2") )
       (comp (compiler-hook orig chip))
       (back (make-instance 'quil-backend))
       (exec (compile-to-backend comp chip back)))
  ;; optionally SIGN-EXECUTABLE here with keys generated with GENERATE-KEY-PAIR.
  (with-open-file (stream "~/Scratch/test.quil" :direction ':output
                                                :if-does-not-exist ':create
                                                :if-exists ':supersede
                                                :element-type '(unsigned-byte 8))
    (write-executable exec stream)))
```
