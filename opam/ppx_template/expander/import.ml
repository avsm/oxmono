include struct
  open Ppxlib
  include Ast

  module Ast_pattern = struct
    include Ast_pattern

    let fail = Ppxlib__.Ast_pattern0.fail
    let pexp_hole = pexp_hole
  end

  module Attribute = Attribute
  module Context_free = Context_free
  module Driver = Driver
  module Extension = Extension
  module Loc = Loc

  module Location : sig
    include module type of Location

    val raise_errorf : ?loc:location -> ('a, Format.formatter, unit, _) format4 -> 'a
    [@@alert
      do_not_raise
        "Create an error node instead of raising. See [Location.error_extensionf], or \
         alternately [Location.Error.createf] and [Location.Error.to_extension]."]
  end =
    Location
end

include struct
  open Ppxlib_jane
  module Ast_traverse = Ast_traverse

  type jkind_annotation = Shim.jkind_annotation
  type mode = Shim.Mode.t = Mode of string [@@unboxed]
  type modality = Shim.Modality.t = Modality of string [@@unboxed]
end

module Ast_builder = struct
  include Ppxlib.Ast_builder.Default
  include Ppxlib_jane.Ast_builder.Default
end

include struct
  open Sexplib0
  module Sexp = Sexp
  include Sexp_conv
end

(* Re-export from [Import] to shadow [Result] from [open Stdppx] *)
module Result = Result

(* Convenience functions *)
let map_snd (x, y) ~f = x, f y

(* Make [Sexp.t] constructors always in scope *)
type _sexp = Sexplib0.Sexp.t =
  | Atom of string
  | List of Sexplib0.Sexp.t list
