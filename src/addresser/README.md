## Overview and Motivation

The quilc addresser is responsible for translating source programs, which may
involve arbitrary "logical qubits" interacting in arbitrary ways (e.g. 5q
gates), to programs which conform to the topological constraints of the hardware
(e.g. using only physical qubits, and with 2Q gates only on live hardware
links). The main entry point is `DO-GREEDY-ADDRESSING`.

At the core of the addresser are two data structures:
-  a "logical to physical rewiring" (cf. `rewiring.lisp`), which determines what
   physical qubit a logical qubit is presently assigned to
- a "logical schedule" (cf. `logical-schedule.lisp`), which represents the
  source instructions as a partially ordered set, with resource conflicts
  determining whether one instruction may occur before another. The addresser
  walks the logical scheduler in topological order, updating the logical to
  physical rewiring as need be, and emitting instructions (to a "chip
  schedule").

The addresser handles gates on > 2 qubits by first translating them with
`APPLY-TRANSLATION-COMPILERS`. This is relatively straightforward.

The main difficulty in addressing is managing the logical to physical rewiring.
At any given moment, logical qubits 
- might not have an assigment to a physical qubit (and hence an assignment must
  be made), or
- might be assigned to physical qubits which are not adjacent (in which case
  something must be done in order for a gate involving them to be executed).

The main technique for dealing with the second problem is to introduce SWAP
operations, which can be used to shuffle the quantum state around in a way that
reflects an update to the logical to physical assignment. This is a challenging
task, because it can incur a sizeable overhead in the 2Q cost of the compiled
program. The quilc addresser makes a lot of effort to assign qubits and select
swaps intelligently.

The approach taken by quilc is to apply a few heuristics, which can be tuned or
adapted as we see fit. These are split along two axes. The first axis is how one
measures the "cost" of an instruction or set of instructions. The second is how
one finds "good" swap operations.

## Cost Heuristics

Right now there are two sorts of costs: a duration-based cost
(cf. `temporal-addresser.lisp`) and a fidelity-based cost
(cf. `fidelity-addreser.lisp`). New cost heuristics may be implemented by
subclassing `ADDRESSER-STATE` (cf. `addresser-state.lisp`), and then implementing
methods on a few associated generics:
- `COST-FUNCTION`, which computes the cost of the current logical schedule.
- `WEIGHTED-FUTURE-GATES`, which constructs a mapping from a gate in the current
  logical schedule to a value indicating "how far" into the future they will
  occur.
- `BUILD-WORST-COST`, which is basically a stand-in for `most-positive-fixnum`
  if the heuristic uses a custom or compound cost value.

In practice, both the temporal and fidelity addresser go a bit further than this
(e.g. with non-real cost values), but the above reflects the bare minimum needed
to add a new cost heuristic.

As a note, the reason for `WEIGHTED-FUTURE-GATES` is to allow parts of the
addresser to effectively do a look-ahead. For example, in swap selection,
choosing swaps which look good "right now" might be a poor strategy if the next
few instructions force additional swaps to be inserted. As it stands, both the
temporal and fidelity cost functions incorporate this look-ahead information.

## Search Heuristics

When the addresser decides that swaps are needed, there are usually a large
number of candidates. In order to select the best, it now relies on the
following two generics
- `SELECT-SWAPS-FOR-GATES`, used by the addresser to select swaps so that it may
  assign logical gates to physical qubits, and
- `SELECT-SWAPS-FOR-REWIRING`, used by the addresser to select swaps in order to
  update the current rewiring to a desired rewiring.

These represent the two main pieces of functionality needed to add a new search
heuristic to the addresser.

The search type is represented as a keyword of type `ADDRESSER-SEARCH-TYPE` in
`addresser-common.lisp`, corresponding to three implementations
- `:GREEDY-PATH` in `path-heuristic.lisp`,
- `:GREEDY-QUBIT` in `qubit-heuristic.lisp`, 
- `:A*`in `astar-rewiring-search.lisp`.

The default type may be customized (cf. `addresser-common.lisp`):
- `*addresser-gates-swap-search-type*` specifies the default type for `SELECT-SWAP-FOR-GATES`,
- `*addresser-rewiring-swap-search-type*` specifies the default type for `SELECT-SWAPS-FOR-REWIRING`.


## Additional Customization

The flag `*ADDRESSER-USE-1Q-QUEUES*` specifies whether to stash 1Q gates away so
that they will not influence qubit allocation, swap selection, and so on.
Previously this was hardcoded as default behavior for the temporal cost
heuristic but not for the fidelity cost heuristic (this was because the fidelity
heuristic can actually place 1Q gates in intelligent manner, on good qubits,
whereas the duration heuristic cannot, and so for the duration heuristic all
effort is made to not get in the way of 2Q addressing).
