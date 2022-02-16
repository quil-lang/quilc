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

  char source[256];
  quil_program program, processed_program;
  chip_specification chip_spec;

  fgets(source, 256, stdin);

  if (quilc_parse_quil(source, &program) != ERROR_SUCCESS)
    die("unable to parse quil");

  if (quilc_build_nq_linear_chip(8, &chip_spec) != ERROR_SUCCESS)
    die("unable to build chip");

  if (quilc_compile_protoquil(program, chip_spec, &processed_program) != ERROR_SUCCESS)
    die("unable to compile program");

  quilc_print_program(processed_program);
  lisp_release_handle(program);
  lisp_release_handle(processed_program);
  lisp_release_handle(chip_spec);

  return 0;
}
