from ctypes import *
import libquilc
import sys

def die(msg):
    print(msg)
    exit(1)

if __name__ == '__main__':
    source = input()

    program = libquilc.quil_program()
    processed_program = libquilc.quil_program()
    chip_spec = libquilc.chip_specification()

    if (libquilc.quilc_parse_quil(source.encode('utf-8'), byref(program)) != 0):
        die('unable to parse quil')

    if (libquilc.quilc_build_nq_linear_chip(8, byref(chip_spec)) != 0):
        die('unable to create chip spec');

    if (libquilc.quilc_compile_quil(program, chip_spec, byref(processed_program)) != 0):
        die('unable to compile program')

    libquilc.quilc_print_program(processed_program);
    libquilc.lisp_release_handle(program)
    libquilc.lisp_release_handle(processed_program)
    libquilc.lisp_release_handle(chip_spec)
