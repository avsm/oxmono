(* Test the behavior of nested template extension nodes *)

module _ : sig
  [@@@warning "-32-60"]

  [@@@expand_inline:
    [%%template:
    [@@@mode.default m1 = (m1a, m1b)]

    val x : unit

    val%template y : unit

    module%template.portable M () : sig end]]

  include sig
    [@@@ocaml.text "/*"]

    val x__m1a : unit

    [@@@ocaml.text "/*"]

    val y : unit

    [@@@ocaml.text "/*"]

    module M__m1a__portable : functor () -> sig @@ portable end
    module M__m1a : functor () -> sig @@ nonportable end

    [@@@ocaml.text "/*"]
  end
  [@@ocaml.doc " @inline "]

  include sig
    [@@@ocaml.text "/*"]

    val x__m1b : unit

    [@@@ocaml.text "/*"]

    val y : unit

    [@@@ocaml.text "/*"]

    module M__m1b__portable : functor () -> sig @@ portable end
    module M__m1b : functor () -> sig @@ nonportable end

    [@@@ocaml.text "/*"]
  end
  [@@ocaml.doc " @inline "]

  [@@@end]
end = struct
  [@@@expand_inline
    [%%template
    [@@@mode.default m1 = (m1a, m1b)]

    let x = ()
    let%template y = () [@@warning "-32"]

    module%template.portable M () = struct end]]

  include struct
    let x__m1a = ()
    let y = () [@@warning "-32"]

    module M__m1a__portable () = struct end
    module M__m1a () = struct end
  end [@@ocaml.doc " @inline "]

  include struct
    let x__m1b = ()
    let y = () [@@warning "-32"]

    module M__m1b__portable () = struct end
    module M__m1b () = struct end
  end [@@ocaml.doc " @inline "]

  [@@@end]
end
