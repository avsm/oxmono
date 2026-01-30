type 'a t = 'a Base.Or_null.t Ref.t

let[@inline] make v = Ref.make (Base.This v)
let[@inline] get_or_null t = Ref.exchange t Base.Null

let[@inline] get_exn t =
  match Ref.exchange t Base.Null with
  | Null -> Base.failwith "Once.get_exn failed: already accessed"
  | This v -> v
;;

module Atomic = struct
  type 'a t = 'a option Atomic.t

  let[@inline] make v = Atomic.make (Some v)
  let[@inline] get_opt t = Atomic.exchange t None

  let[@inline] get_exn t =
    match Atomic.exchange t None with
    | None -> Base.failwith "Once.get_exn failed: already accessed"
    | Some v -> v
  ;;
end

module Local = struct
  type 'a t = 'a Base.Or_null.t Ref.Local.t

  let make v = exclave_ Ref.Local.make (Base.This v)
  let get_or_null t = exclave_ Ref.Local.exchange_global t Base.Null

  let get_exn t = exclave_
    match get_or_null t with
    | Null -> failwith "Once.Local.get_exn failed: already accessed"
    | This v -> v
  ;;
end
