from ctypes import *
import json
import sys

import libquilc


def die(msg):
    print(msg)
    exit(1)


if __name__ == "__main__":
    gateset = "PHASE(pi/2) 0, H 0"
    sequence = c_char_p()

    if (
        libquilc.quilc_generate_rb_sequence(
            2, 1, gateset.encode("utf-8"), 52, None, byref(sequence)
        )
        != 0
    ):
        die("unable to generate RB sequence")

    print(json.loads(sequence.value.decode("utf-8")))

    indices = "0"
    symbols = "X"
    clifford = "H 0"
    pauli = c_char_p()

    if (
        libquilc.quilc_conjugate_pauli_by_clifford(
            indices.encode("utf-8"),
            symbols.encode("utf-8"),
            clifford.encode("utf-8"),
            byref(pauli),
        )
        != 0
    ):
        die("unable to conjugate pauli")

    result = json.loads(pauli.value.decode("utf-8"))
    result.pop("_type")
    print(result)

    quil = "RX((2+1/2)*pi/7) 0"
    raw_result = c_char_p()

    if libquilc.quilc_rewrite_arithmetic(quil.encode("utf-8"), byref(raw_result)) != 0:
        die("unable to rewrite arithmetic")

    result = json.loads(raw_result.value.decode("utf-8"))
    result.pop("_type")
    print(result)
