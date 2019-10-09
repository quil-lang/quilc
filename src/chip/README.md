## Instruction set architecture

An instruction set architecture describes the hardware restrictions that the compiler must satisfy. The available quantum resources are encoded in an `"isa"` layer, with qubit information in the `"1Q"` dictionary, and qubit-qubit information in the `"2Q"` dictionary. Each of these dictionaries has keys for their respective hardware objects, which in turn are dictionaries of hardware resource information.

### Specification

The serialized ISA is specified in JSON, with the following structure:


```
{
    "name": name,
    "version": version,
    
    "isa": {
        "1Q": {
            label: {
                "type": "Xhalves",
                "gates": gates,
                "dead": dead
            },
            .
            .
            .
        },
        "2Q": {
            label: {
                "type": [ "CZ" | "ISWAP" | "CPHASE" | "PISWAP" ],
                "gates": gates,
                "dead": dead
            }
            .
            .
            .
        }
    }
    
    "specs": {
        spec-type: spec-value,
        .
        .
        .
    }
}
```

Where the above fields have the following permitted types and/or values:

```
name := STRING
version := STRING

label := STRING
type := STRING | [ STRING ]
gates := [ gate ]
gate := measure-gate | other-gate
measure-gate := {
    "operator": "MEASURE",
    "qubit": "_" | INTEGER,
    "target": "_" | STRING,
    "duration": REAL,
    "fidelity": REAL
}
other-gate := {
    "operator": STRING,
    "parameters": [ INTEGER | "_" ],
    "arguments": [ INTEGER | "_" ],
    "duration": REAL,
    "fidelity": REAL
}

dead := true | false
```

Qubits (in the `"1Q"` layer) that have unspecified `"type"` or `"gates"` fields default to supporting arbitrary `Z`-rotations, discrete `X`-rotations (namely multiples of ±π), and measurements (`RZ`, `RX`, and `MEASURE` respectively). This combination of permitted gates is often called "Xhalves".

Edges (in the `"2Q"` layer) that have unspecified `"type"` or `"gates"` fields default to supporting only the `CZ` gate.

_Note_: the ISA `"type"` field is deprecated for both the `"1Q"` and `"2Q"` layers. In its place, one should use the `"gates"` field. See below for an example of using `"gates"`.


### Overview and examples

For example, a chip with a single qubit chip may be specified with:

```
{
    "isa": {
        "1Q": {
            "0": {
                "type": "Xhalves"
            }
        }
    },
    "specs": {}
}
```

This chip has a single physical qubit (labeled `"0"`) which natively supports the operation type `"Xhalves"`.

To permit qubit-qubit interactions, in the `"2Q"` layer one must provide keys of the form `"a-b"` where `a` and `b` are integers with `a < b`. For example, a two qubit chip with a qubit-qubit interaction may be specified with:

```
{
    "isa": {
        "1Q": {
            "0": {
                "type": "Xhalves"
            },
            "1": {
                "type": "Xhalves"
            }
        },
        "2Q": {
            "0-1": {
                "type": "CZ"
            }
        }
    },
    "specs": {}
}
```

To encode a chip that supports a directed qubit-qubit interaction, use the `"arguments"` field. For example, if a particular `CNOT` requires that its control qubit be `0` and its target be `1`

```
{
    "isa": {
        "1Q": {
            "0": {
            },
            "1": {
            }
        },
        "2Q": {
            "0-1": {
                "gates": [{
                    "operator": "CNOT",
                    "parameters": [],
                    "arguments": [0, 1],
                    "duration": 100,
                    "fidelity": 0.999
                }]
            }
        }
    },
    "specs": {}
}
```
If we were to compile the instruction `CZ 0 1` or `CZ 1 0` or indeed any program, the resulting program would only contain `CNOT`s with control `0` and target `1`.

The ISA object also provides the compiler with metadata that may be used to optimize its output. For example, the compiler may elect to use resources with higher qubit-qubit interaction fidelity over those with lower fidelity. Benchmark metrics (such as `T1` and `T2`) can also be provided in the `"specs"` layer:

```
{
    "isa": {
        "1Q": {
            "0": {
                "type": "Xhalves"
            },
            "1": {
                "type": "Xhalves"
            }
        },
        "2Q": {
            "0-1": {
                "type": "CZ"
            }
        }
    },
    "specs": {
        "T1": 0.000028,
        "T2": 0.000026
    }
}

```
