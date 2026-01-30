# ppx_with

A ppx rewriter for building _scoped_ operations, which must do cleanup or similar work
after a main body, while maintaining readability.


Example:

```ocaml
let with_connection ~instance f =
  let connection = Connection.create ~instance in
  Monitor.protect (fun () -> f connection) ~finally:(fun () -> Connection.close connection)
;;

let make_query ~query =
  let%with connection = with_connection ~instance:"instance-0" in
  Connection.make_query ~query connection
;;
```

is equivalent to

```ocaml
let make_query ~query =
  with_connection ~instance:"instance-0" (fun connection ->
    Connection.make_query ~query connection)
;;
```

To ensure the generated closure is stack-allocated, use`let%with.stack`:

```ocaml
let make_query ~query =
  let%with.stack connection = with_connection ~instance:"instance-0" in
  Connection.make_query ~query connection
;;
```

which is equivalent to

```ocaml
let make_query ~query =
  with_connection ~instance:"instance-0" (stack_ fun connection ->
    Connection.make_query ~query connection) [@nontail]
;;
```


When you have a labeled `~f` argument, you can use `let%with.tilde`
(or `let%with.tilde.stack`):

```ocaml
let with_connection ~instance ~f = (* ... *)

let make_query ~query =
  let%with.tilde connection = with_connection ~instance:"instance-0" in
  Connection.make_query ~query connection
;;
```

You can also use `let%with ... and ... in` or multiple consecutive `let%with`s to wrap the
main body in multiple layers:

```ocaml
let make_query ~query =
  let%with connection0 = with_connection ~instance:"instance-0"
  and connection1 = with_connection ~instance:"instance-1"
  in
  let%bind result0 = Connection.make_query ~query connection0
  and result1 = Connection.make_query ~query connection1
  in
  return (result0, result1)
;;
```

Additionally, it adds a `[%with.const]` expression which can be used if you want to apply
your function to constant argument, for example:

```ocaml
let run ?extra_runner_params () =
  let%with () =
    match extra_runner_params with
    | Some params -> Runner.with_enabled_params params
    | None -> [%with.const ()]
  in
  Runner.run ()
;;
```

There is also support for `match%with` and `match%with.tilde` (and additionally `.stack`),
which are like `let%with` and `let%with.tilde` but allowing matching on an anonymous
argument. For example:

```ocaml
let iter_pipe pipe (f : [`Snapshot of int list | `Update of int] -> unit) =
  (* ... *)
;;

let pipe = (* ... *)

let () =
  match%with iter_pipe pipe with
  | `Snapshot lst -> print_s [%sexp (lst : int list)]
  | `Update n     -> print_s [%sexp (n   : int     )]
;;
```
