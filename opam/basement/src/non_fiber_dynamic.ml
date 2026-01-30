module Domain = struct
  include Domain
  include Stdlib_shim.Domain.Safe
end

(** [Cell.t] is an abstraction over a mutable cell (backed by an atomic) with simple
    permissioning. This makes it a little easier to understand how the fields of
    [Dynamic.t] are used, which can otherwise be a bit confusing due to an optimization
    described below. *)
module Cell : sig @@ portable
  type ('a, +'perm) t : value mod contended portable

  val make : ('a : value mod contended). 'a @ portable -> ('a, [< `read | `write ]) t
  val make_readonly : ('a : value mod contended). 'a @ portable -> ('a, [< `read ]) t
  val get : ('a : value mod contended). ('a, [> `read ]) t -> 'a @ portable
  val set : ('a : value mod contended). ('a, [> `write ]) t -> 'a @ portable -> unit
end = struct
  type ('a, +_) t = 'a Portable_atomic.t

  let[@inline] make x = Portable_atomic.make x
  let[@inline] make_readonly x = make x
  let[@inline] get t = Portable_atomic.get t
  let[@inline] set t x = Portable_atomic.set t x
end

(** A ['a t] represents a dynamically scoped variable.

    Conceptually, this could just be
    {[
      type 'a t =
        { root : 'a Modes.Portable.t Atomic.t
        ; current : 'a Modes.Portable.t option Domain.TLS.key
        }
    ]}
    where the current value is [v] when [current = Some v], and [root] otherwise. This
    requires a conditional jump, though, which we avoid by having [current] be an alias
    for [root] rather than [None]. *)
type 'a t : value mod contended portable =
  { root : ('a, [ `write ]) Cell.t
  (** The default value when not inside a [with_temporarily]. This is shared by all
      domains and can be set at any time. *)
  ; current : ('a, [ `read ]) Cell.t Domain.TLS.key
  (** The value of the dynamic variable for the current domain (conceptually, fiber).
      Inside the scope of a [with_temporarily], [current] is set to a constant [Cell.t];
      outside the scope of all [with_temporarily]'s, [current] is an alias for [root]. *)
  }

let make x =
  let root = Cell.make x in
  let current =
    Domain.TLS.new_key
      (fun () ->
        (* Any already-existing domains are initialized to point at [root]. *)
        root)
      ~split_from_parent:(fun (current : _ Cell.t) () ->
        (* If the dynamic is currently explicitly set by the user, then we want to
           preserve that set when we split. If it hasn't been set by the user, then
           [current] is just [root]. *)
        current)
  in
  { root; current }
;;

let get t = Cell.get (Domain.TLS.get t.current)
let set_root t x = Cell.set t.root x

let with_temporarily t x ~f =
  let new_cell = Cell.make_readonly x in
  let restore_to =
    (* Set the [current] TLS value to [new_cell], and return the previous cell. *)
    let restore_to = Domain.TLS.get t.current in
    Domain.TLS.set t.current new_cell;
    restore_to
  in
  let local_ restore () =
    (* Set the [current] TLS value back to [restore_to]. *)
    Domain.TLS.set t.current restore_to
  in
  match f () with
  | res ->
    restore ();
    res
  | exception exn ->
    let bt = Stdlib.Printexc.get_raw_backtrace () in
    restore ();
    Stdlib.Printexc.raise_with_backtrace exn bt
;;
