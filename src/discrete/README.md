# QUILC Discrete Compilation

## Usage

To build Quilc with this module use `make quilc-discrete` and run tests with `make test-discrete`.
The discrete compilers will only trigger on discrete chip ISAs. An example call is:

``` sh
echo 'PRAGMA TOLERANCE "1E-4"; RZ(0.01) 0' | ./quilc -Pm --isa 4Q-cliffordt
```

The optional `TOLERANCE` pragma is specifies how close the approximation should be (this definition is intentionally left vague as specifying tolerance provides little guarantees).

## Compilers

### RZ Approximation

  - Approximation of RZ as unitaries over Z[i,1/sqrt(2)]
    - Reference [arXiv:1212.6253v2](https://arxiv.org/abs/1212.6253)
    - Source: `src/discrete/rz-approx/`
  - Decomposes unitaries representable by Clifford+T
    - Reference: [arXiv:1206.5236](https://arxiv.org/abs/1206.5236v4)
    - Source: `src/discrete/operators/hadamardt.lisp`
  - Outputs as Matsumoto Amano Normal Form
    - Reference: [arXiv:0806.3834](https://arxiv.org/abs/0806.3834)
    - Source: `/src/discrete/operators/ma-normal-form.lisp`
