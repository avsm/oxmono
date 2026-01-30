open! Ppx_template_test_common

[@@@disable_unused_warnings]

[@@@expand_inline
  module%template List : sig
    type 'a t = 'a list

    [%%template:
    [@@@alloc.default _ @ m_out = (heap_global, stack_local)]

    (* [cons] can't implement [local -> heap] *)
    [@@@mode.default m_in = (global, m_out)]

    val cons : 'a @ m_in -> 'a t @ m_in -> 'a t @ m_out [@@zero_alloc_if_local m_out]]

    [%%template:
    [@@@alloc.default a @ m_out = (heap_global, stack_local)]
    [@@@mode.default m_in = (local, global)]

    val map : 'a t @ m_in -> f:('a @ m_in -> 'a @ m_out) -> 'a t @ m_out
    [@@zero_alloc_if_stack a]

    val fold
      :  'a t @ m_in
      -> init:'acc @ m_out
      -> f:('acc @ m_out -> 'a @ m_in -> 'acc @ m_out)
      -> 'acc @ m_out
    [@@zero_alloc_if_local m_out] [@@zero_alloc_if_stack a]]
  end = struct
    type 'a t = 'a list

    [%%template
    [@@@alloc.default a @ m_out = (heap_global, stack_local)]
    [@@@mode.default m_in = (global, m_out)]

    let cons (hd @ m_in) (tl @ m_in) = hd :: tl [@exclave_if_stack a]]

    [%%template
    [@@@alloc.default a @ m_out = (heap_global, stack_local)]
    [@@@mode.default m_in = (local, global)]

    let rec map (t @ m_in) ~f =
      match[@exclave_if_stack a] t with
      | [] -> []
      | hd :: tl ->
        (f [@zero_alloc_if_local m_out assume]) hd :: (map [@mode m_in] [@alloc a]) tl ~f
    ;;

    let rec fold (t @ m_in) ~(init @ m_out) ~f =
      match[@exclave_if_stack a] t with
      | [] -> init
      | hd :: tl ->
        (fold [@mode m_in] [@alloc a])
          tl
          ~init:((f [@zero_alloc_if_local m_out assume]) init hd)
          ~f
    ;;]
  end]

module List : sig
  type 'a t = 'a list

  include sig
    include sig
      val cons : 'a @ global -> 'a t @ global -> 'a t @ global
    end
    [@@ocaml.doc " @inline "]
  end
  [@@ocaml.doc " @inline "]

  include sig
    include sig
      [@@@ocaml.text "/*"]

      val cons__stack : 'a @ global -> 'a t @ global -> local_ 'a t [@@zero_alloc]

      [@@@ocaml.text "/*"]
    end
    [@@ocaml.doc " @inline "]

    include sig
      [@@@ocaml.text "/*"]

      val cons__local__stack : local_ 'a -> local_ 'a t -> local_ 'a t [@@zero_alloc]

      [@@@ocaml.text "/*"]
    end
    [@@ocaml.doc " @inline "]
  end
  [@@ocaml.doc " @inline "]

  include sig
    include sig
      [@@@ocaml.text "/*"]

      val map__local : local_ 'a t -> f:(local_ 'a -> 'a @ global) -> 'a t @ global

      [@@@ocaml.text "/*"]

      [@@@ocaml.text "/*"]

      val fold__local
        :  local_ 'a t
        -> init:'acc @ global
        -> f:('acc @ global -> local_ 'a -> 'acc @ global)
        -> 'acc @ global

      [@@@ocaml.text "/*"]
    end
    [@@ocaml.doc " @inline "]

    include sig
      val map : 'a t @ global -> f:('a @ global -> 'a @ global) -> 'a t @ global

      val fold
        :  'a t @ global
        -> init:'acc @ global
        -> f:('acc @ global -> 'a @ global -> 'acc @ global)
        -> 'acc @ global
    end
    [@@ocaml.doc " @inline "]
  end
  [@@ocaml.doc " @inline "]

  include sig
    include sig
      [@@@ocaml.text "/*"]

      val map__local__stack : local_ 'a t -> f:(local_ 'a -> local_ 'a) -> local_ 'a t
      [@@zero_alloc]

      [@@@ocaml.text "/*"]

      [@@@ocaml.text "/*"]

      val fold__local__stack
        :  local_ 'a t
        -> init:local_ 'acc
        -> f:(local_ 'acc -> local_ 'a -> local_ 'acc)
        -> local_ 'acc
      [@@zero_alloc]

      [@@@ocaml.text "/*"]
    end
    [@@ocaml.doc " @inline "]

    include sig
      [@@@ocaml.text "/*"]

      val map__stack : 'a t @ global -> f:('a @ global -> local_ 'a) -> local_ 'a t
      [@@zero_alloc]

      [@@@ocaml.text "/*"]

      [@@@ocaml.text "/*"]

      val fold__stack
        :  'a t @ global
        -> init:local_ 'acc
        -> f:(local_ 'acc -> 'a @ global -> local_ 'acc)
        -> local_ 'acc
      [@@zero_alloc]

      [@@@ocaml.text "/*"]
    end
    [@@ocaml.doc " @inline "]
  end
  [@@ocaml.doc " @inline "]
end = struct
  type 'a t = 'a list

  include struct
    include struct
      let cons (hd @ global) (tl @ global) = hd :: tl
    end [@@ocaml.doc " @inline "]
  end [@@ocaml.doc " @inline "]

  include struct
    include struct
      let cons__stack (hd @ global) (tl @ global) = exclave_ hd :: tl
    end [@@ocaml.doc " @inline "]

    include struct
      let cons__local__stack (local_ hd) (local_ tl) = exclave_ hd :: tl
    end [@@ocaml.doc " @inline "]
  end [@@ocaml.doc " @inline "]

  include struct
    include struct
      let rec map__local (local_ t) ~f =
        match t with
        | [] -> []
        | hd :: tl -> f hd :: map__local tl ~f
      ;;

      let rec fold__local (local_ t) ~(init @ global) ~f =
        match t with
        | [] -> init
        | hd :: tl -> fold__local tl ~init:(f init hd) ~f
      ;;
    end [@@ocaml.doc " @inline "]

    include struct
      let rec map (t @ global) ~f =
        match t with
        | [] -> []
        | hd :: tl -> f hd :: map tl ~f
      ;;

      let rec fold (t @ global) ~(init @ global) ~f =
        match t with
        | [] -> init
        | hd :: tl -> fold tl ~init:(f init hd) ~f
      ;;
    end [@@ocaml.doc " @inline "]
  end [@@ocaml.doc " @inline "]

  include struct
    include struct
      let rec map__local__stack (local_ t) ~f = exclave_
        match t with
        | [] -> []
        | hd :: tl -> (f [@zero_alloc assume]) hd :: map__local__stack tl ~f
      ;;

      let rec fold__local__stack (local_ t) ~(local_ init) ~f = exclave_
        match t with
        | [] -> init
        | hd :: tl -> fold__local__stack tl ~init:((f [@zero_alloc assume]) init hd) ~f
      ;;
    end [@@ocaml.doc " @inline "]

    include struct
      let rec map__stack (t @ global) ~f = exclave_
        match t with
        | [] -> []
        | hd :: tl -> (f [@zero_alloc assume]) hd :: map__stack tl ~f
      ;;

      let rec fold__stack (t @ global) ~(local_ init) ~f = exclave_
        match t with
        | [] -> init
        | hd :: tl -> fold__stack tl ~init:((f [@zero_alloc assume]) init hd) ~f
      ;;
    end [@@ocaml.doc " @inline "]
  end [@@ocaml.doc " @inline "]
end

[@@@end]

(* [[@alloc]] with no payload disables the default. *)

[@@@expand_inline
  [%%template
  [@@@alloc.default a @ m = (heap_global, stack_local)]

  let[@alloc] f x = x]]

include struct
  let f x = x
end [@@ocaml.doc " @inline "]

include struct
  let f x = x
end [@@ocaml.doc " @inline "]

[@@@end]

(* Further examples of using the alloc-poly syntax. *)

[@@@expand_inline
  [%%template
  (* Normal *)
  [@@@alloc.default a @ m = (heap_global, stack_local)]

  module Outer1 = struct
    (* Left-hand-side can be short-form [alloc] if the mode isn't needed *)
    let f x = x [@exclave_if_stack a'] [@@alloc a' = (heap, stack)]

    (* Right-hand-side can be in long-form [alloc @ mode] *)
    [@@@alloc a' @ m' = (heap_global, a @ m)]

    (* You can pun on an [alloc] *)
    module [@alloc a'] Inner1 = struct
      let f (x @ m') = x [@exclave_if_stack a']
    end

    (* You can pun on an [alloc @ mode] *)
    module [@alloc a' @ m'] Inner2 = struct
      let f (x @ m') = x [@exclave_if_stack a']
    end
  end

  (* You can pun on [heap] and [stack] directly. *)
  module [@alloc heap] Outer2 = struct
    let f x = x
  end

  module [@alloc stack] Outer3 = struct
    let f x = exclave_ x
  end]]

include struct
  module Outer1 = struct
    let f x = x
    and f__stack x = exclave_ x

    include struct
      module Inner1 = struct
        let f (x @ global) = x
      end

      module Inner2 = struct
        let f (x @ global) = x
      end
    end [@@ocaml.doc " @inline "]
  end

  module Outer2 = struct
    let f x = x
  end

  module Outer3__stack = struct
    let f x = exclave_ x
  end
end [@@ocaml.doc " @inline "]

include struct
  module Outer1__stack = struct
    let f x = x
    and f__stack x = exclave_ x

    include struct
      module Inner1 = struct
        let f (x @ global) = x
      end

      module Inner2 = struct
        let f (x @ global) = x
      end
    end [@@ocaml.doc " @inline "]

    include struct
      module Inner1__stack = struct
        let f (local_ x) = exclave_ x
      end

      module Inner2__stack = struct
        let f (local_ x) = exclave_ x
      end
    end [@@ocaml.doc " @inline "]
  end

  module Outer2 = struct
    let f x = x
  end

  module Outer3__stack = struct
    let f x = exclave_ x
  end
end [@@ocaml.doc " @inline "]

[@@@end]
