type ('t, 'impl, 'tags) pi = ..
type _ binding = H : ('t, 'impl, 'tags) pi * 'impl -> 't binding
[@@unsafe_allow_any_mode_crossing]

(* The record wrapper exists to carry the mode-crossing assertion, which
   is not allowed on a type abbreviation. The explicit kind is also
   needed: the attribute alone does not change the declared kind that
   signature matching checks (see oxcaml-repro/02). A handler is written
   once when a resource is created and read-only afterwards, and
   implementations are module-level functions, so sharing across domains
   cannot race. *)
type 't ops : value mod portable contended = { ops : 't binding array }
[@@unboxed] [@@unsafe_allow_any_mode_crossing]

type ('t, 'tags) handler = 't ops
type -'a t = T : ('t * 't ops) -> 'a t

let not_supported () = failwith "Operation not supported!"

let handler l = { ops = Array.of_list l }
let bindings h = Array.to_list h.ops

let get : 't ops -> ('t, 'impl, 'tags) pi -> 'impl = fun { ops } op ->
  let rec aux i =
    if i = Array.length ops then not_supported ();
    let H (k, v) = ops.(i) in
    if Obj.repr k == Obj.repr op then Obj.magic v
    else aux (i + 1)
  in
  aux 0

let get_opt : 't ops -> ('t, 'impl, 'tags) pi -> 'impl option = fun { ops } op ->
  let rec aux i =
    if i = Array.length ops then None
    else (
      let H (k, v) = ops.(i) in
      if Obj.repr k == Obj.repr op then Some (Obj.magic v)
      else aux (i + 1)
    )
  in
  aux 0

type close_ty = [`Close]
type (_, _, _) pi += Close : ('t, 't -> unit, [> close_ty]) pi

let close (T (t, ops)) = get ops Close t
