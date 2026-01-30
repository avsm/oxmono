type ('a : value_or_null) global : value_or_null = { global : 'a @@ aliased global }
[@@unboxed]

module Access : sig
  type 'k t : void mod aliased external_ global many portable
  type packed = P : 'k t -> packed [@@unboxed]
  type 'k boxed : value mod aliased external_ global many portable

  val box : 'k t -> 'k boxed
  val unbox : 'k boxed -> 'k t

  (* Can break soundness. *)
  val unsafe_mk : unit -> 'k t @@ portable
  val equality_witness : 'k t -> 'j t -> ('k, 'j) Type.eq @@ portable
end = struct
  type inner : void mod everything

  external mk_inner : unit -> inner @@ portable = "%unbox_unit"

  type dummy
  type 'k t = T : inner -> dummy t [@@unboxed]
  type packed = P : 'k t -> packed [@@unboxed]
  type 'k boxed = Box : dummy boxed

  external unsafe_rebrand : 'k t -> 'j t @@ portable = "%identity"
  external unsafe_rebrand_boxed : 'k boxed -> 'j boxed @@ portable = "%identity"

  let[@inline] unsafe_mk (type k) () : k t = unsafe_rebrand (T (mk_inner ()))
  let box _ = unsafe_rebrand_boxed Box
  let unbox _ = unsafe_mk ()

  let[@inline] equality_witness (type k j) (T _ : k t) (T _ : j t) : (k, j) Type.eq =
    Type.Equal
  ;;
end

let[@inline] current () = Access.P (Access.unsafe_mk ())

type initial

let initial = Access.(box (unsafe_mk ()))

module TLS = Basement.Stdlib_shim.Domain.Safe.TLS

let initial_key =
  let key = TLS.new_key (fun _ -> false) in
  TLS.set key true;
  key
;;

let access_initial f = exclave_
  if TLS.get initial_key then f (Some Access.(box (unsafe_mk ()))) else f None
;;

let access_initial_domain =
  if Basement.Stdlib_shim.runtime5 ()
  then
    fun [@inline] f -> exclave_
    if Stdlib.Domain.is_main_domain ()
    then f (Some Access.(box (unsafe_mk ())))
    else f None
  else fun [@inline] f -> exclave_ f (Some Access.(box (unsafe_mk ())))
;;

module Password : sig
  type 'k t : void mod contended external_ portable unyielding
  type 'k boxed : value mod contended external_ portable unyielding

  (* Can break the soundness of the API. *)
  val unsafe_mk : unit -> 'k t @ local @@ portable
  val box : 'k t @ local -> 'k boxed @ local
  val unbox : 'k boxed @ local -> 'k t @ local

  module Shared : sig
    type 'k t : void mod contended external_ portable unyielding
    type 'k boxed : value mod contended external_ portable unyielding

    val box : 'k t @ local -> 'k boxed @ local @@ portable
    val unbox : 'k boxed @ local -> 'k t @ local @@ portable

    val borrow
      : ('a : value_or_null) 'k.
      'k t @ local
      -> ('k t @ forkable local -> 'a @ unique) @ forkable local once
      -> 'a @ unique
      @@ portable

    (* Can break the soundness of the API. *)
    val unsafe_mk : unit -> 'k t @ local @@ portable
  end

  val shared : 'k t @ local -> 'k Shared.t @ local @@ portable

  val with_current
    : ('a : value_or_null) 'k.
    'k Access.t
    -> ('k t @ local -> 'a @ forkable local once unique) @ local once
    -> 'a @ forkable local once unique
    @@ portable
end = struct
  type 'k t : void mod contended external_ portable unyielding
  type 'k boxed = unit

  external unsafe_mk : unit -> 'k t @@ portable = "%unbox_unit"

  let box _ = ()
  let unbox () = unsafe_mk ()

  module Shared = struct
    type 'k t : void mod contended external_ portable unyielding
    type 'k boxed = unit

    external unsafe_mk : unit -> 'k t @@ portable = "%unbox_unit"

    let box _ = ()
    let unbox () = unsafe_mk ()
    let[@inline] borrow _ f = f (unsafe_mk ())
  end

  external shared : 'k t @ local -> 'k Shared.t @ local @@ portable = "%identity"

  let[@inline] with_current _ f = exclave_ f (unsafe_mk ()) [@nontail]
end

module Data = struct
  type ('a, 'k) t : value mod everything with 'a @@ contended portable

  external unsafe_mk
    :  ('a[@local_opt])
    -> (('a, 'k) t[@local_opt])
    @@ portable
    = "%identity"

  external unsafe_get
    :  (('a, 'k) t[@local_opt])
    -> ('a[@local_opt])
    @@ portable
    = "%identity"

  external unsafe_mk_unique
    :  ('a[@local_opt]) @ unique
    -> (('a, 'k) t[@local_opt]) @ unique
    @@ portable
    = "%identity"

  external unsafe_get_unique
    :  (('a, 'k) t[@local_opt]) @ unique
    -> ('a[@local_opt]) @ unique
    @@ portable
    = "%identity"

  external unsafe_mk_once
    :  ('a[@local_opt]) @ once
    -> (('a, 'k) t[@local_opt]) @ once
    @@ portable
    = "%identity"

  external unsafe_get_once
    :  (('a, 'k) t[@local_opt]) @ once
    -> ('a[@local_opt]) @ once
    @@ portable
    = "%identity"

  external unsafe_mk_once_unique
    :  ('a[@local_opt]) @ once unique
    -> (('a, 'k) t[@local_opt]) @ once unique
    @@ portable
    = "%identity"

  external unsafe_get_once_unique
    :  (('a, 'k) t[@local_opt]) @ once unique
    -> ('a[@local_opt]) @ once unique
    @@ portable
    = "%identity"

  let[@inline] wrap ~access:_ t = unsafe_mk t
  let[@inline] unwrap ~access:_ t = unsafe_get t
  let[@inline] wrap_unique ~access:_ t = unsafe_mk_unique t
  let[@inline] unwrap_unique ~access:_ t = unsafe_get_unique t
  let[@inline] wrap_once ~access:_ t = unsafe_mk_once t
  let[@inline] unwrap_once ~access:_ t = unsafe_get_once t
  let[@inline] wrap_once_unique ~access:_ t = unsafe_mk_once_unique t
  let[@inline] unwrap_once_unique ~access:_ t = unsafe_get_once_unique t
  let[@inline] unwrap_shared ~access:_ t = unsafe_get t
  let[@inline] create f = unsafe_mk (f ())
  let[@inline] create_once f = unsafe_mk_once (f ())
  let[@inline] create_unique f = unsafe_mk_unique (f ())
  let[@inline] map ~password:_ ~f t = unsafe_mk (f (unsafe_get t))

  let[@inline] fst t =
    let t1, _ = unsafe_get t in
    unsafe_mk t1
  ;;

  let[@inline] snd t =
    let _, t2 = unsafe_get t in
    unsafe_mk t2
  ;;

  let[@inline] both t1 t2 = unsafe_mk (unsafe_get t1, unsafe_get t2)
  let[@inline] extract ~password:_ ~f t = f (unsafe_get t)
  let inject = unsafe_mk
  let project = unsafe_get
  let[@inline] project_shared ~key:_ t = unsafe_get t
  let[@inline] project_shared_unique ~key:_ t = unsafe_get_unique t
  let[@inline] bind ~password:_ ~f t = f (unsafe_get t)
  let[@inline] iter ~password:_ ~f t = f (unsafe_get t)

  module Shared = struct
    type ('a, 'k) data = ('a, 'k) t
    type ('a, 'k) t = ('a, 'k) data

    let[@inline] wrap ~access:_ v = unsafe_mk v
    let[@inline] unwrap ~access:_ t = unsafe_get t
    let[@inline] expose ~key:_ t = unsafe_get t
    let[@inline] create f = unsafe_mk (f ())
    let[@inline] map ~password:_ ~f t = unsafe_mk (f (unsafe_get t))
    let[@inline] both t1 t2 = unsafe_mk (unsafe_get t1, unsafe_get t2)

    let[@inline] fst t =
      let x, _ = unsafe_get t in
      unsafe_mk x
    ;;

    let[@inline] snd t =
      let _, y = unsafe_get t in
      unsafe_mk y
    ;;

    let[@inline] extract ~password:_ ~f t = f (unsafe_get t)
    let[@inline] inject v = unsafe_mk v
    let[@inline] project t = unsafe_get t
    let[@inline] bind ~password:_ ~f t = f (unsafe_get t)
    let[@inline] iter ~password:_ ~f t = f (unsafe_get t)
    let[@inline] map_into ~password:_ ~f t = unsafe_mk (f (unsafe_get t))

    module Local = struct
      let[@inline] wrap ~access:_ v = exclave_ unsafe_mk v
      let[@inline] unwrap ~access:_ t = exclave_ unsafe_get t
      let[@inline] create f = exclave_ unsafe_mk (f ())
      let[@inline] map ~password:_ ~f t = exclave_ unsafe_mk (f (unsafe_get t))
      let[@inline] both t1 t2 = exclave_ unsafe_mk (unsafe_get t1, unsafe_get t2)

      let[@inline] fst t = exclave_
        let x, _ = unsafe_get t in
        unsafe_mk x
      ;;

      let[@inline] snd t = exclave_
        let _, y = unsafe_get t in
        unsafe_mk y
      ;;

      let[@inline] extract ~password:_ ~f t = exclave_ f (unsafe_get t)
      let[@inline] inject v = exclave_ unsafe_mk v
      let[@inline] project t = exclave_ unsafe_get t
      let[@inline] bind ~password:_ ~f t = exclave_ f (unsafe_get t)
      let[@inline] iter ~password:_ ~f t = f (unsafe_get t) [@nontail]
      let[@inline] map_into ~password:_ ~f t = exclave_ unsafe_mk (f (unsafe_get t))
    end
  end

  let[@inline] map_shared ~password:_ ~f t = unsafe_mk (f (unsafe_get t))
  let[@inline] extract_shared ~password:_ ~f t = f (unsafe_get t)

  module Local = struct
    let[@inline] wrap ~access:_ t = exclave_ unsafe_mk t
    let[@inline] unwrap ~access:_ t = exclave_ unsafe_get t
    let[@inline] wrap_unique ~access:_ t = exclave_ unsafe_mk_unique t
    let[@inline] unwrap_unique ~access:_ t = exclave_ unsafe_get_unique t
    let[@inline] wrap_once ~access:_ t = exclave_ unsafe_mk_once t
    let[@inline] unwrap_once ~access:_ t = exclave_ unsafe_get_once t
    let[@inline] unwrap_shared ~access:_ t = exclave_ unsafe_get t
    let[@inline] create f = exclave_ unsafe_mk (f ())
    let[@inline] map ~password:_ ~f t = exclave_ unsafe_mk (f (unsafe_get t))

    let[@inline] fst t = exclave_
      let t1, _ = unsafe_get t in
      unsafe_mk t1
    ;;

    let[@inline] snd t = exclave_
      let _, t2 = unsafe_get t in
      unsafe_mk t2
    ;;

    let[@inline] both t1 t2 = exclave_ unsafe_mk (unsafe_get t1, unsafe_get t2)
    let[@inline] extract ~password:_ ~f t = exclave_ f (unsafe_get t)
    let[@inline] inject v = exclave_ unsafe_mk v
    let[@inline] project t = exclave_ unsafe_get t
    let[@inline] project_shared ~key:_ t = exclave_ unsafe_get t
    let[@inline] bind ~password:_ ~f t = exclave_ f (unsafe_get t)
    let[@inline] iter ~password:_ ~f t = f (unsafe_get t) [@nontail]
    let[@inline] map_shared ~password:_ ~f t = exclave_ unsafe_mk (f (unsafe_get t))
    let[@inline] extract_shared ~password:_ ~f t = exclave_ f (unsafe_get t)
  end

  module Or_null = struct
    type ('a : value_or_null
         , 'k)
         t :
         value_or_null mod everything with 'a @@ contended portable

    external unsafe_mk
      : ('a : value_or_null) 'k.
      ('a[@local_opt]) -> (('a, 'k) t[@local_opt])
      @@ portable
      = "%identity"

    external unsafe_get
      : ('a : value_or_null) 'k.
      (('a, 'k) t[@local_opt]) -> ('a[@local_opt])
      @@ portable
      = "%identity"

    let[@inline] wrap ~access:_ t = unsafe_mk t
    let[@inline] unwrap ~access:_ t = unsafe_get t
    let[@inline] create f = unsafe_mk (f ())
    let project = unsafe_get
  end
end

module Key : sig
  type 'k t : void mod contended external_ forkable many portable unyielding
  type packed = P : 'k t -> packed [@@unboxed]
  type 'k boxed : value mod contended external_ forkable many portable

  val unsafe_mk : unit -> 'k t @ unique @@ portable
  val box : 'k t @ unique -> 'k boxed @ unique
  val unbox : 'k boxed @ unique -> 'k t @ unique
  val box_aliased : 'k t -> 'k boxed
  val unbox_aliased : 'k boxed -> 'k t

  val with_password
    : ('a : value_or_null) 'k.
    'k t @ unique
    -> f:('k Password.t @ local -> 'a @ unique) @ local once
    -> #('a * 'k t) @ unique
    @@ portable

  val with_password_local
    : ('a : value_or_null) 'k.
    'k t @ unique -> f:('k Password.t @ local -> 'a @ local) @ local once -> 'a @ local
    @@ portable

  val with_password_shared
    : ('a : value_or_null) 'k.
    'k t -> f:('k Password.Shared.t @ local -> 'a @ unique) @ local once -> 'a @ unique
    @@ portable

  val with_password_shared_local
    : ('a : value_or_null) 'k.
    'k t -> f:('k Password.Shared.t @ local -> 'a @ local) @ local once -> 'a @ local
    @@ portable

  val access
    : ('a : value_or_null) 'k.
    'k t @ unique
    -> f:('k Access.t -> 'a @ contended once portable unique) @ local once portable
    -> #('a * 'k t) @ contended once portable unique
    @@ portable

  val access_local
    : ('a : value_or_null) 'k.
    'k t @ unique
    -> f:('k Access.t -> 'a @ contended local once portable unique) @ local once portable
    -> #('a * 'k t) @ contended local once portable unique
    @@ portable

  val access_shared
    : ('a : value_or_null) 'k.
    'k t
    -> f:('k Access.t @ shared -> 'a @ contended once portable unique)
       @ local once portable
    -> 'a @ contended once portable unique
    @@ portable

  val access_shared_local
    : ('a : value_or_null) 'k.
    'k t
    -> f:('k Access.t @ shared -> 'a @ contended local once portable unique)
       @ local once portable
    -> 'a @ contended local once portable unique
    @@ portable

  val globalize_unique : 'k t @ local unique -> 'k t @ unique @@ portable
  val destroy : 'k t @ local unique -> 'k Access.t @@ portable
end = struct
  type 'k t : void mod contended external_ forkable many portable unyielding
  type packed = P : 'k t -> packed [@@unboxed]
  type 'k boxed = unit

  external unsafe_mk : unit -> 'k t @ unique @@ portable = "%unbox_unit"

  let box _ = ()
  let unbox () = unsafe_mk ()
  let box_aliased _ = ()
  let unbox_aliased () = unsafe_mk ()

  let[@inline] with_password_shared (type k) _ ~f =
    let password : k Password.Shared.t = Password.Shared.unsafe_mk () in
    f password [@nontail]
  ;;

  let[@inline] with_password_shared_local (type k) _ ~f = exclave_
    let password : k Password.Shared.t = Password.Shared.unsafe_mk () in
    f password
  ;;

  let[@inline] with_password (type k) k ~f =
    let password : k Password.t = Password.unsafe_mk () in
    #(f password, k)
  ;;

  let[@inline] with_password_local (type k) _ ~f = exclave_
    let password : k Password.t = Password.unsafe_mk () in
    f password
  ;;

  let[@inline] access k ~f = #(f (Access.unsafe_mk ()), k)
  let[@inline] access_local k ~f = exclave_ #(f (Access.unsafe_mk ()), k)

  let[@inline] access_shared _ ~f =
    let c : 'k Access.t = Access.unsafe_mk () in
    f c
  ;;

  let[@inline] access_shared_local _ ~f =
    let c : 'k Access.t = Access.unsafe_mk () in
    exclave_ f c
  ;;

  let[@inline] globalize_unique _ = unsafe_mk ()
  let[@inline] destroy _ = Access.unsafe_mk ()
end

let[@inline] create () = Key.P (Key.unsafe_mk ())

let[@inline] access_local ~password:_ ~f = exclave_
  let c : _ Access.t = Access.unsafe_mk () in
  f c
;;

let[@inline] access ~password:_ ~f =
  let c : _ Access.t = Access.unsafe_mk () in
  f c
;;

let[@inline] access_shared_local ~password:_ ~f = exclave_
  let c : _ Access.t = Access.unsafe_mk () in
  f c
;;

let[@inline] access_shared ~password ~f =
  (access_shared_local ~password ~f:(fun access -> { global = f access })).global
;;
