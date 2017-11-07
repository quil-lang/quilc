# `quilc` release notes

## Version 0.3.0

* add command line arguments
* offload compiler-hook logic into CL-Quil
* die on error rather than dropping into the Lisp debugger
* fix various issues in matrix representation output

## Version 0.2.0

Initial in-house release. Maps a ProtoQuil program onto physical hardware, which is currently fixed as a 8Q chip with a ring topology.  All 1Q instructions are converted to lie in the gate set {±X/2, RZ} and all 2Q instructions are converted to CZ gates.  Any nQ instructions, n ≥ 3, are converted to 1Q and 2Q instructions by Cosine-Sine compilation.
