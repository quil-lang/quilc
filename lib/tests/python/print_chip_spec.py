from ctypes import *
import libquilc
import sys

def die(msg):
    print(msg)
    exit(1)

if __name__ == '__main__':
    chip_spec = libquilc.chip_specification()

    if (libquilc.quilc_build_nq_linear_chip(8, byref(chip_spec)) != 0):
        die('unable to create chip spec');

    libquilc.quilc_print_chip_spec(chip_spec);

    if (libquilc.quilc_chip_spec_from_isa_descriptor('8Q'.encode('utf-8'), byref(chip_spec)) != 0):
        die('unable to create chip spec');

    libquilc.quilc_print_chip_spec(chip_spec);
    libquilc.quilc_release_handle(chip_spec)
