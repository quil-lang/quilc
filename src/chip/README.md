## Instruction set architecture

### Overview and examples

An instruction set architecture describes the hardware restrictions that the compiler must satisfy. The available quantum resources are encoded in an `"isa"` layer, with qubit information in the `"1Q"` dictionary, and qubit-qubit information in the `"2Q"` dictionary. Each of these dictionaries has keys for their respective hardware objects, which in turn are dictionaries of hardware resource information.

For example, a single qubit chip may be specified with:

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

To permit qubit-qubit interactions, one must provide keys of the form `"a-b"` where `a` and `b` are integers with `a < b`. For example, a two qubit chip with a qubit-qubit interaction may be specified with:

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


### Specification

The serialized ISA is specified in JSON, with the following structure:


```
{
    "name": name,
    "version": version,
    
    "isa": {
        "1Q": {
            label: {
                "type": oneq-type,
                "gates": oneq-gates,
                "dead": dead
            },
            .
            .
            .
        },
        "2Q": {
            label: {
                "type": type,
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

