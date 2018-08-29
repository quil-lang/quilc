# `quilc` release notes

## Version 0.12.0

* TODO

## Version 0.11.0

* TODO

## Version 0.10.0

* TODO

## Version 0.9.0

* reply with unused qubits
* reply with raw logical schedule in the JSON output
* change socket timeout to match process timeout, allowing longer server runtimes
* emit JSONified error messages
* various bugfixes

## Version 0.8.0

* added options to calculate fidelity and multiqubit gate depth
* updated server-mode payloads to match Forest 1.3 spec
* updated QPU parser to match new file format
* added Makefile conveniences: source dependencies and `make deps`

## Version 0.7.1

* improved protoquil support
* support for server-mode timeouts
* new server communication schema

## Version 0.7.0

* introduce --server-mode

## Version 0.5.0

No new features; just some fiddling to match the new requirements of the underlying CL-Quil library.

## Version 0.4.0

* introduce --isa command-line option

## Version 0.3.0

* add command line arguments
* offload compiler-hook logic into CL-Quil
* die on error rather than dropping into the Lisp debugger
* fix various issues in matrix representation output

## Version 0.2.0

Initial in-house release. Maps a ProtoQuil program onto physical hardware, which is currently fixed as a 8Q chip with a ring topology.  All 1Q instructions are converted to lie in the gate set {±X/2, RZ} and all 2Q instructions are converted to CZ gates.  Any nQ instructions, n ≥ 3, are converted to 1Q and 2Q instructions by Cosine-Sine compilation.
