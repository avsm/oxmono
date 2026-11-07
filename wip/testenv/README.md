# Testenv

Scoped process environment changes for tests that run in a single domain.

- `Testenv.set` binds or unsets one variable. Unsetting really removes the
  variable, which `Unix.putenv` cannot do.
- `Testenv.with_bindings` applies a list of bindings around a call and
  restores the previous environment afterwards, including after an exception.
- `Testenv.with_xdg` binds `HOME` and the XDG base directories to a fresh
  temporary root for the duration of a call, then restores the environment
  and deletes the root.

Changing the environment while another domain reads it is a data race. Call
these functions from the main domain before any other domain starts, or
between tests once every domain has finished.
