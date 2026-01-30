Current_exe
===========

A library for getting the path to the currently running executable, with support for
overriding in special environments.

## Problem

In some environments, `argv[0]` doesn't reliably contain the path to the current
executable:

- The executable might have been deleted after launch
- The path might be relative and the working directory has changed
- Some wrapper scripts modify argv before execution

## Quickstart

```ocaml
(* By default, returns something like "/proc/12345/exe" *)
let exe_path = Current_exe.get_path ()
```

The `Expert` module allows overriding the default behavior for special environments.
