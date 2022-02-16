#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libquilc.h"

void die(char *msg) {
  printf("%s\n", msg);
  exit(1);
}

int main(int argc, char **argv) {
  init("libquilc.core");

  chip_specification chip_spec;

  if (quilc_build_nq_linear_chip(8, &chip_spec) != ERROR_SUCCESS)
    die("unable to build chip");

  quilc_print_chip_spec(chip_spec);

  if (quilc_chip_spec_from_isa_descriptor("8Q", &chip_spec) != ERROR_SUCCESS)
    die("unable to build chip");

  quilc_print_chip_spec(chip_spec);
  lisp_release_handle(chip_spec);

  return 0;
}
