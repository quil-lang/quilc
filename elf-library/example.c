#include <ecl/ecl.h>

/* This name corresponds to the argument :init-name to make-build when
   we are building the shared object. Function may take time to return
   because top-level forms are replayed from a vector. */
extern void quil_init(cl_object);

extern int test_error(char *text, char **output);
extern int test_result(char *text, char **output);
extern int parse_quil_from_string(char *text, char **output);

typedef int (*lisp_fun)(char *text, char **output);

/* marshalling/demarshalling between char* and cl_object could be put
   in this functin. Then the prototypes would be:
   
   typedef cl_object (*lisp_fun) (cl_object input); */
int call_the_function(lisp_fun fun, char *text, char **output) {
  cl_object error = ecl_make_symbol("ERROR","CL");
  cl_env_ptr the_env = ecl_process_env();
  ECL_HANDLER_CASE_BEGIN(the_env, ecl_list1(error)) {
    fun(text, output);
  } ECL_HANDLER_CASE(1, condition) {
    printf("Error when executing whatever on '%s'.\n", text);
    return -1;
  } ECL_HANDLER_CASE_END;
  printf("Function call result is: '%s'\n", *output);
  return 0;
}

int main (int argc, char **argv) {
  printf(";;; Booting ECL.\n");
  cl_boot(argc, argv);
  printf(";;; Initializing QUILC.\n");
  ecl_init_module(NULL, quil_init);
  printf(";;; We are good to go. Have fun.\n");

  /* Simple eval */
  cl_eval(c_string_to_object("(print 'hello-quil-world)"));
  printf("\n\n");

  char *foobar = NULL;
  call_the_function(test_result, "Test input!", &foobar);
  call_the_function(test_error, "Test input!", &foobar);
  /* empty program */
  call_the_function(parse_quil_from_string, "", &foobar);
  
  printf("\n");
  printf(";;; Shutting down ECL.\n");
  cl_shutdown();
  return 0;
}

/* called lisp form signals an error */
int test_error(char *text, char **output) {
  cl_eval(c_string_to_object("(error \"hakuna matata\")"));
  return 0;
}

/* called lisp form returns a string */
int test_result(char *text, char **output) {
  cl_object input_text = ecl_make_simple_base_string(text, -1);
  cl_object result = cl_funcall(4,
                                ecl_make_symbol("CONCATENATE","CL"),
                                ecl_make_symbol("STRING","CL"),
                                ecl_make_simple_base_string("Processed: ", -1),
                                input_text);
  result = ecl_null_terminated_base_string(result);
  *output = ecl_base_string_pointer_safe(result);
  return 0;
}

int parse_quil_from_string(char *string, char **out_code_ptr) {
  cl_object input_code = ecl_make_simple_base_string(string, -1);
  cl_object parse_quil_fun = ecl_make_symbol("PARSE-QUIL", "QUIL");
  cl_object result = cl_funcall(2, parse_quil_fun, input_code);
  /* function does not return a string, for sake of example we make it so. */
  result = cl_format(3, ECL_NIL, ecl_make_simple_base_string("result: ~s", -1), result);
  result = ecl_null_terminated_base_string(result);
  *out_code_ptr = ecl_base_string_pointer_safe(result);
  return 0;
}
