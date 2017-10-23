# `quilc` README

## Specification

`quilc` is a program that reads arbitrary[^qubit-restriction] ProtoQuil from standard in and writes Quil to standard out which can be directly instantiated on an 8Q chip arranged in a ring topology with the native gate set {±X/2, RZ, CZ}.[^future-plans]

[^qubit-restriction]: Currently, the input program is required to use only qubits 0-7.

[^future-plans]: We will soon loosen this considerably. A user will be able to specify a chip design and ISA by passing a command-line parameter, and `quilc` will target this instead.

## Compilation

After installing and compiling `CL-QUIL`, the `quilc` binary can be built by running

```
$ make quilc
```

from within the `CL-QUIL` root directory.  This deposits the binary into the root directory as well.

## Invocation

`quilc` takes no command-line arguments.  It reads ProtoQuil in from standard-in and writes ProtoQuil to standard-out. The program is precedes and followed by a pragma of the form

```
PRAGMA REWIRING "#(n0 n1 ... n7)"
```

which indicates that the `j`th logical qubit from standard-in can be found on the `nj`th physical qubit on standard-out.  The preceding pragma indicates the remapping for state preparation, and the following pragma indicates the remapping for read-out.

## An example Python hook

The following Python snippet indicates how to use the `quilc` compiler from within pyQuil.  The function `compile_program` takes a pyQuil `Program` object as its argument and returns the output of `quilc` on that object as another `Program` object.

```
from subprocess import Popen, PIPE, STDOUT
from pyquil.quil import Program

# old_program is a pyQuil program object.
# This returns a new pyQuil program object
def compile_program(old_program):
    old_program_text = "{}".format(old_program)
    p = Popen(['/path/to/cl-quil/quilc'], stdout=PIPE, stdin=PIPE, stderr=STDOUT)
    out_pipe = p.communicate(input=old_program_text)[0]
    return Program(out_pipe.decode().encode('ascii').split('\n'))
```