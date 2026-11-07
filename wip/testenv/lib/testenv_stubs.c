#include <stdlib.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>

value testenv_setenv(value name, value contents)
{
  if (!caml_string_is_c_safe(name) || !caml_string_is_c_safe(contents))
    caml_invalid_argument("Testenv.set");
  if (setenv(String_val(name), String_val(contents), 1) != 0)
    caml_failwith("Testenv.set: setenv failed");
  return Val_unit;
}

value testenv_unsetenv(value name)
{
  if (!caml_string_is_c_safe(name))
    caml_invalid_argument("Testenv.set");
  if (unsetenv(String_val(name)) != 0)
    caml_failwith("Testenv.set: unsetenv failed");
  return Val_unit;
}
