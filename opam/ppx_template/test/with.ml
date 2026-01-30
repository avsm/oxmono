[@@@disable_unused_warnings]

(* basic use *)

[@@@expand_inline
  [%%template
  [@@@kind_set.default ks = (value, value_with_imm)]

  module type S = sig
    type t [@@kind k = ks]

    module M : sig
      type t [@@kind k = ks]
    end
  end

  module type S_int = S
  [@kind_set ks]
  [@with:
    type t = int [@@kind k = ks]

    module M : sig
      type t = int [@@kind k = ks]
    end]

  module type S_mono = sig
    type t

    include
      S
    [@kind_set ks]
    [@with:
      type t := t [@@kind k = ks]

      module M : sig
        type t := t [@@kind k = ks]
      end]
  end]]

include struct
  module type S = sig
    type t

    module M : sig
      type t
    end
  end

  module type S_int = S with type t = int with type M.t = int

  module type S_mono = sig
    type t

    include S with type t := t with type M.t := t
  end
end [@@ocaml.doc " @inline "]

include struct
  module type S__''value_with_imm'' = sig
    type t__immediate
    and t__immediate64
    and t

    module M : sig
      type t__immediate
      and t__immediate64
      and t
    end
  end

  module type S_int__''value_with_imm'' =
    S__''value_with_imm''
    with type t__immediate = int
     and type t__immediate64 = int
     and type t = int
    with type M.t__immediate = int
     and type M.t__immediate64 = int
     and type M.t = int

  module type S_mono__''value_with_imm'' = sig
    type t

    include
      S__''value_with_imm''
      with type t__immediate := t
       and type t__immediate64 := t
       and type t := t
      with type M.t__immediate := t
       and type M.t__immediate64 := t
       and type M.t := t
  end
end [@@ocaml.doc " @inline "]

[@@@end]

(* more interesting module types in a mono context *)

module P = struct end

[@@@expand_inline
  module type S_extended = sig
    include S [@kind_set value_with_imm]

    module type S
    module type T

    module N : sig end
    module O : sig end
  end
  [@with:
    module type S = S [@kind_set value_with_imm]

    module type T := sig
      type t [@@kind k = ks]
    end

    module N = P
    module O := P

    [@@@kind.default k = ks]]]

module type S_extended = sig
    include S__''value_with_imm''

    module type S
    module type T

    module N : sig end
    module O : sig end
  end
  with
    module type S = S__''value_with_imm''
  with
    module type T := sig
      [@@@ocaml.text "/*"]

      type t__ks

      [@@@ocaml.text "/*"]
    end
  with module N = P
  with module O := P
 [@ocaml.doc " @inline "]

[@@@end]
