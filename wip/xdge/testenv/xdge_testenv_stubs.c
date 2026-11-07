#include <stdlib.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>

value xdge_testenv_setenv(value name, value contents)
{
  if (!caml_string_is_c_safe(name) || !caml_string_is_c_safe(contents))
    caml_invalid_argument("Xdge_testenv.set");
  if (setenv(String_val(name), String_val(contents), 1) != 0)
    caml_failwith("Xdge_testenv.set: setenv failed");
  return Val_unit;
}

value xdge_testenv_unsetenv(value name)
{
  if (!caml_string_is_c_safe(name))
    caml_invalid_argument("Xdge_testenv.set");
  if (unsetenv(String_val(name)) != 0)
    caml_failwith("Xdge_testenv.set: unsetenv failed");
  return Val_unit;
}
