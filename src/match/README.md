# `cl-quil/match`: Pattern Matching and Replacement

## Description

This system implements the quantum circuit pattern matching algorithm of Iten et al. It exposes two primary functions as the interface: `pattern-match` and `pattern-replace`.

`pattern-match` takes a circuit and a pattern that realises the identity function as input. The circuit is then searched for matches of the pattern. The list of these matches is returned.

`pattern-replace` takes a circuit and a list of patterns as input. `pattern-match` is called with each pattern. Each match with the pattern in the circuit is then replaced with the unmatched section of the pattern. Then `pattern-replace` repeats. The return value of `pattern-replace` is the optimized circuit.

It can be useful to print out the current state of `pattern-match` and `pattern-replace`. To do so, three methods are provided:
- `(print-circ circuit)` takes a circuit as input and prints a visual representation of the circuit
- `(print-circ-as-canon circuit)` takes a circuit as input and prints a visual representation of the canonical form of the circuit
- `(print-canon-data canonical-form)` takes a `canonical-form` as input and prints a visual representation of the canonical form

## Caveats

The pattern matching algorithm is currently implemented as a standalone tool, and is not yet able to be used as a pattern matcher in QUILC.

## How To Use

1. Load `"cl-quil/match"`.
2. Implement the gate interface by creating a subclass of the class `gate`. An example of how to do this can be found in `test-gate.lisp`
3. Realise a circuit and a pattern as lists of objects that subclass `gate`.
4. Run `(pattern-match circuit pattern)` or `(pattern-replace circuit patterns)`.

## How To Test

Run `(asdf:test-system "cl-quil/match")`.

## References

Iten, R., Moyard, R., Metger, T., Sutter, D. and Woerner, S., 2020.
Exact and practical pattern matching for quantum circuit optimization.
[`arXiv:1909.05270`](https://arxiv.org/abs/1909.05270)
