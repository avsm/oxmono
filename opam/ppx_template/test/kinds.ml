[@@@disable_unused_warnings]

[@@@expand_inline
  module%template M : sig
    val id : ('a : k) -> 'a [@@kind k = (value, value & bits64, value mod contended)]
  end = struct
    let id x = x [@@kind k = (value, value & bits64, value mod contended)]
  end

  let%template id = (M.id [@kind k])
  [@@kind k = (value, value & bits64, value mod contended)]
  ;;

  [%%template
  [@@@kind.default k1 = (value, value & bits64)]
  [@@@kind.default k2 = (value, value mod contended)]

  let const2 _ _ x = x]]

module M : sig
  val id : ('a : value) -> 'a

  [@@@ocaml.text "/*"]

  val id__'value_bits64' : ('a : value & bits64) -> 'a
  val id__'value_mod_contended' : ('a : value mod contended) -> 'a

  [@@@ocaml.text "/*"]
end = struct
  let id x = x
  and id__'value_bits64' x = x
  and id__'value_mod_contended' x = x
end

let id = M.id
and id__'value_bits64' = M.id__'value_bits64'
and id__'value_mod_contended' = M.id__'value_mod_contended'

include struct
  include struct
    let const2 _ _ x = x
  end [@@ocaml.doc " @inline "]

  include struct
    let const2__value__'value_mod_contended' _ _ x = x
  end [@@ocaml.doc " @inline "]
end [@@ocaml.doc " @inline "]

include struct
  include struct
    let const2__'value_bits64'__value _ _ x = x
  end [@@ocaml.doc " @inline "]

  include struct
    let const2__'value_bits64'__'value_mod_contended' _ _ x = x
  end [@@ocaml.doc " @inline "]
end [@@ocaml.doc " @inline "]

[@@@end]
