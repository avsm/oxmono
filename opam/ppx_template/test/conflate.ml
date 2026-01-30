open! Ppx_template_test_common

[@@@disable_unused_warnings]

(* We always conflate modes/modalities for portability, contention, visibiltiy, and access
   axes *)

[@@@expand_inline
  module type Y = sig
    val f : unit -> unit
  end

  module%template.portable [@modality p] P (Y : Y) = struct
    let g @ p = Y.f
  end

  module%template.stateless [@modality v] S (Y : Y) = struct
    let g @ v = Y.f
  end]

module type Y = sig
  val f : unit -> unit
end

module P__portable (Y : sig
  @@ portable
    include Y
  end) =
struct
  let g @ portable = Y.f
end

module P (Y : sig
  @@ nonportable
    include Y
  end) =
struct
  let g @ nonportable = Y.f
end

module S__stateless (Y : sig
  @@ stateless
    include Y
  end) =
struct
  let g @ stateless = Y.f
end

module S__observing (Y : sig
  @@ observing
    include Y
  end) =
struct
  let g @ observing = Y.f
end

module S (Y : sig
  @@ stateful
    include Y
  end) =
struct
  let g @ stateful = Y.f
end

[@@@end]

[@@@expand_inline
  [%%template
  [@@@mode p = (nonportable, portable)]

  type u = { field : unit -> unit @@ p } [@@mode p]

  [@@@kind.default k = (value mod p, bits64 mod p)]

  type t : k]]

include struct
  type u = { field : unit -> unit @@ nonportable }

  include struct
    type t__'value_mod_nonportable' : value mod nonportable
  end [@@ocaml.doc " @inline "]

  include struct
    type t__'bits64_mod_nonportable' : bits64 mod nonportable
  end [@@ocaml.doc " @inline "]
end [@@ocaml.doc " @inline "]

include struct
  type u__portable = { field : unit -> unit @@ portable }

  include struct
    type t__'value_mod_portable' : value mod portable
  end [@@ocaml.doc " @inline "]

  include struct
    type t__'bits64_mod_portable' : bits64 mod portable
  end [@@ocaml.doc " @inline "]
end [@@ocaml.doc " @inline "]

[@@@end]

[@@@expand_inline
  [%%template
  [@@@modality s = (stateful, observing, stateless)]

  type u = { field : unit @ s -> unit @ s } [@@modality s]

  [@@@kind.default k = (value mod s, bits64 mod s)]

  type t : k]]

include struct
  type u = { field : unit @ stateful -> unit @ stateful }

  include struct
    type t__'value_mod_stateful' : value mod stateful
  end [@@ocaml.doc " @inline "]

  include struct
    type t__'bits64_mod_stateful' : bits64 mod stateful
  end [@@ocaml.doc " @inline "]
end [@@ocaml.doc " @inline "]

include struct
  type u__observing = { field : unit @ observing -> unit @ observing }

  include struct
    type t__'value_mod_observing' : value mod observing
  end [@@ocaml.doc " @inline "]

  include struct
    type t__'bits64_mod_observing' : bits64 mod observing
  end [@@ocaml.doc " @inline "]
end [@@ocaml.doc " @inline "]

include struct
  type u__stateless = { field : unit @ stateless -> unit @ stateless }

  include struct
    type t__'value_mod_stateless' : value mod stateless
  end [@@ocaml.doc " @inline "]

  include struct
    type t__'bits64_mod_stateless' : bits64 mod stateless
  end [@@ocaml.doc " @inline "]
end [@@ocaml.doc " @inline "]

[@@@end]
