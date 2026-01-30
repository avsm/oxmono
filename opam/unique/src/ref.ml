type (!'a : value_or_null) t : mutable_data with 'a

external make
  : ('a : value_or_null).
  'a @ once unique -> ('a t[@local_opt]) @ unique
  @@ portable
  = "%makemutable"

external get
  : ('a : value_or_null).
  ('a t[@local_opt]) @ unique -> 'a @ once unique
  @@ portable
  = "%field0"

external set
  : ('a : value_or_null).
  ('a t[@local_opt]) -> 'a @ once unique -> unit
  @@ portable
  = "%setfield0"

let[@inline] exchange t a =
  let a' = get (Basement.Stdlib_shim.Obj.magic_unique t) in
  set t a;
  a'
;;

module Local = struct
  type (!'a : value_or_null) t : mutable_data with 'a

  external make
    : ('a : value_or_null).
    'a @ local once unique -> 'a t @ local unique
    @@ portable
    = "%makemutable"

  external get
    : ('a : value_or_null).
    'a t @ local unique -> 'a @ local once unique
    @@ portable
    = "%field0"

  external set_global
    : ('a : value_or_null).
    'a t @ local -> 'a @ once unique -> unit
    @@ portable
    = "%setfield0"

  let[@inline] exchange_global t a = exclave_
    let a' = get (Basement.Stdlib_shim.Obj.magic_unique t) in
    set_global t a;
    a'
  ;;
end
