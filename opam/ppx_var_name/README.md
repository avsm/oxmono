ppx_var_name
============

Allows you to reference the OCaml variable name in RHS expressions.

Usage
-----

```ocaml
let foo_bar = [%VAR_NAME]      in
let foo_bar = [%Var_name]      in
let foo_bar = [%var_name] 
and foo_bar = [%var_dash_name] in

let%map       foo_bar = [%Var_name] in
let%bind      foo_bar = [%Var_name] in

let%map_open.Command  foo_bar = [%var_dash_name] in
let%bind_open.Command foo_bar = [%var_dash_name] in

let%map_open.Command foo_bar = [%var_dash_name]
and                  baz_qux = [%var_dash_name]
in

let%mapn foo = [%Var_name]
and      bar = [%Var_name]
and      baz = [%Var_name] in

[%map_open.Command
  let foo_bar = [%var_dash_name]
  and baz_qux = [%var_dash_name] in
  ...
  ]

module Foo_Bar = struct
  let name = [%Module_name]
  let name = [%MODULE_NAME]
  let name = [%module_name]
  let name = [%module_dash_name]
end
```

produces:

```ocaml
let foo_bar = "FOO_BAR" in
let foo_bar = "Foo_bar" in
let foo_bar = "foo_bar"
and foo_bar = "foo-bar" in

let%map       foo_bar = "Foo_bar" in
let%bind      foo_bar = "Foo_bar" in

let%map_open.Command  foo_bar = "foo-bar" in
let%bind_open.Command foo_bar = "foo-bar" in

let%map_open.Command foo_bar = "foo-bar"
and                  baz_qux = "baz-qux"
in

let%mapn foo = "Foo"
and      bar = "Bar"
and      baz = "Baz" in

[%map_open.Command
  let foo_bar = "foo-bar"
  and baz_qux = "baz-qux" in
  ...
  ]

module Foo_bar = struct
  let name = "Foo_Bar"
  let name = "FOO_BAR"
  let name = "foo_bar"
  let name = "foo-bar"
end
```

For typical usage you can apply this in bulk to existing code with a command like:
```bash
sed -i -E 's/let ([a-zA-Z_][a-zA-Z0-9_]*) = "\1"/let \1 = [%var_name]/g' *.ml
```
