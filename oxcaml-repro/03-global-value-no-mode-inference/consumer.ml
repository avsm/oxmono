(* [Lib.get] is portable and usable. [Lib.v], a module-level constant of
   an abstract type, reads as [contended] from a portable function, so
   any library exposing constants of abstract types (event descriptors,
   sentinel values, [Mtime.Span.zero], [Hmap.empty]) is unusable from
   portable code until the library itself declares a crossing kind.
   There is no way to declare or assert the kind from outside the
   library. *)
let use : (unit -> int) @ portable = fun () -> Lib.get Lib.v
let () = ignore (use ())
