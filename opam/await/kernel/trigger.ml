open! Base
open! Portable_kernel

(*=The underlying state machine of a trigger:

     [create]
        |
        v
      Initial---------------[signal]-----------------+
        |                                            |
        |                                            v
        +-[on_signal]-> Awaiting -[signal|drop]-> Signaled
                           ^
                           |
                  [create_with_action]

   The [Signaled] state is terminal. *)
type state =
  | Initial
  | Awaiting :
      { action : 'k @ contended portable unique -> unit @@ portable
      ; k : 'k @@ portable
      }
      -> state
  | Signaled

type t = state Atomic.t

let is_signalled t =
  match Atomic.get t with
  | Signaled -> true
  | Awaiting _ | Initial -> false
;;

module Source = struct
  type nonrec t = t

  let same = Base.phys_equal

  let signal t =
    match Atomic.get t with
    | Signaled -> ()
    | Awaiting current_r as current ->
      (match
         Atomic.compare_and_set t ~if_phys_equal_to:current ~replace_with:Signaled
       with
       | Set_here ->
         current_r.action ((Obj.magic_unique [@mode contended portable]) current_r.k)
       | Compare_failed -> ())
    | Initial as current ->
      (match
         Atomic.compare_exchange t ~if_phys_equal_to:current ~replace_with:Signaled
       with
       | Signaled | Initial -> ()
       | Awaiting current_r as current ->
         (match
            Atomic.compare_and_set t ~if_phys_equal_to:current ~replace_with:Signaled
          with
          | Set_here ->
            current_r.action ((Obj.magic_unique [@mode contended portable]) current_r.k)
          | Compare_failed -> ()))
  ;;

  let is_signalled = is_signalled

  module For_testing = struct
    let signal_if_awaiting t =
      match Atomic.get t with
      | Signaled | Initial -> ()
      | Awaiting current_r as current ->
        (match
           Atomic.compare_and_set t ~if_phys_equal_to:current ~replace_with:Signaled
         with
         | Set_here ->
           current_r.action ((Obj.magic_unique [@mode contended portable]) current_r.k)
         | Compare_failed -> ())
    ;;
  end
end

let on_signal t ~f:action k =
  let k = (Obj.magic_many [@mode contended portable]) k in
  match Atomic.get t with
  | Signaled -> This ((Obj.magic_unique [@mode contended portable]) k)
  | Awaiting _ -> failwith "Trigger.on_signal: already awaiting"
  | Initial as if_phys_equal_to ->
    (match
       Atomic.compare_exchange
         t
         ~if_phys_equal_to
         ~replace_with:(Awaiting { action = (Obj.magic_many [@mode portable]) action; k })
     with
     | Initial -> Null
     | Signaled -> This ((Obj.magic_unique [@mode contended portable]) k)
     | Awaiting _ -> failwith "Trigger.on_signal: already awaiting")
;;

let drop t =
  match Atomic.get t with
  | Signaled -> false
  | Initial -> failwith "Trigger.drop: not awaiting"
  | Awaiting _ as if_phys_equal_to ->
    (match Atomic.compare_and_set t ~if_phys_equal_to ~replace_with:Signaled with
     | Set_here -> true
     | Compare_failed -> false)
;;

let%template source t = t [@@mode m = (global, local)]
let create () = Atomic.make Initial

let create_with_action ~f:action k =
  let k = (Obj.magic_many [@mode contended portable]) k in
  Atomic.make (Awaiting { action = (Obj.magic_many [@mode portable]) action; k })
;;
