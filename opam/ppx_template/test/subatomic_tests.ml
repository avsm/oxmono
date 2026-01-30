open! Ppx_template_test_common

[@@@disable_unused_warnings]

[@@@expand_inline
  module%template M : sig
    type 'a t

    val grow : 'a t -> fresh:(unit -> 'a) -> unit

    val iter : 'a t @ s -> f:('a @ s -> unit) -> unit
    [@@synchro __ @ s = (unsync_uncontended, sync_shared)]
  end = struct
    type 'a t =
      { mutable data : 'a array
      ; iterators : int Subatomic.t
      }

    let grow t ~fresh =
      assert (Subatomic.get t.iterators = 0);
      let new_prefix = Array.init (Array.length t.data) ~f:(fun _ -> fresh ()) in
      t.data <- Array.append new_prefix t.data
    ;;

    let iter t ~f =
      (Subatomic.incr [@synchro s]) t.iterators;
      for i = 0 to Array.length t.data do
        f ((Array.get [@mode m]) t.data i)
      done;
      (Subatomic.decr [@synchro s]) t.iterators
    [@@synchro s @ m = (unsync_uncontended, sync_shared)]
    ;;
  end]

module M : sig
  type 'a t

  val grow : 'a t -> fresh:(unit -> 'a) -> unit
  val iter : 'a t @ uncontended -> f:('a @ uncontended -> unit) -> unit

  [@@@ocaml.text "/*"]

  val iter__sync : 'a t @ shared -> f:('a @ shared -> unit) -> unit

  [@@@ocaml.text "/*"]
end = struct
  type 'a t =
    { mutable data : 'a array
    ; iterators : int Subatomic.t
    }

  let grow t ~fresh =
    assert (Subatomic.get t.iterators = 0);
    let new_prefix = Array.init (Array.length t.data) ~f:(fun _ -> fresh ()) in
    t.data <- Array.append new_prefix t.data
  ;;

  let iter t ~f =
    Subatomic.incr t.iterators;
    for i = 0 to Array.length t.data do
      f t.data.(i)
    done;
    Subatomic.decr t.iterators

  and iter__sync t ~f =
    Subatomic.incr__sync t.iterators;
    for i = 0 to Array.length t.data do
      f (Array.get__shared t.data i)
    done;
    Subatomic.decr__sync t.iterators
  ;;
end

[@@@end]
