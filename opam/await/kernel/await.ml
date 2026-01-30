open Base
open Portable_kernel

exception Terminated

let () =
  Stdlib.Printexc.Safe.register_printer (function
    | Terminated -> Some "Terminated"
    | _ -> None)
;;

type ('c, 'k) local_handler =
  { password : 'k Capsule.Expert.Password.t
  ; context : ('c, 'k) Capsule.Data.t
  ; await : ('c @ local -> Trigger.t -> unit, 'k) Capsule.Data.t
  ; yield : ('c @ local -> unit, 'k) Capsule.Data.t or_null
  }

type ('c, 'k) portable_handler =
  { context : 'c @@ contended global portable
  ; await : 'c @ contended portable -> Trigger.t -> unit @@ global portable
  ; yield : ('c @ contended portable -> unit) or_null @@ global portable
  }

type%fuelproof t : value mod contended portable =
  | Local_await :
      { terminator : Terminator.t
      ; handler : ('c, 'k) local_handler
      }
      -> t
  | Portable_await :
      { terminator : Terminator.t
      ; handler : ('c, 'k) portable_handler
      }
      -> t

let with_ ~terminator ~yield ~await context ~f = exclave_
  let (P access) = Capsule.Access.current () in
  let await = Capsule.Data.(wrap [@mode local]) ~access await in
  let yield =
    match yield with
    | Null -> Null
    | This yield -> This (Capsule.Data.(wrap [@mode local]) ~access yield)
  in
  let context = Capsule.Data.(wrap [@mode local]) ~access context in
  Capsule.Expert.Password.with_current access (fun password -> exclave_
    let await =
      Local_await { terminator; handler = { yield; await; context; password } }
    in
    f await)
;;

let create_global ~terminator ~yield ~await context =
  Portable_await { terminator; handler = { context; await; yield } }
;;

let[@inline] terminator = function
  | Local_await { terminator; _ } -> exclave_ terminator
  | Portable_await { terminator; _ } -> exclave_ terminator
;;

module Call_await_safe_unoptimized : sig
  val call_await : t @ local -> Trigger.t -> unit @@ portable
end = struct
  let call_await t (trigger : Trigger.t) =
    match t with
    | Local_await { handler = { password; context; await; yield = _ }; terminator = _ } ->
      Capsule.Expert.Data.Local.iter
        ~password
        (Capsule.Expert.Data.Local.both await context)
        ~f:(fun [@inline] (await, context) -> await context trigger [@nontail])
      [@nontail]
    | Portable_await { handler = { context; await; yield = _ }; terminator = _ } ->
      await context trigger
  ;;
end

module Call_await_unsafe_optimized : sig
  include module type of struct
    include Call_await_safe_unoptimized
  end
end = struct
  external magic_unwrap_capsule
    :  (('a, 'k) Capsule.Data.t[@local_opt])
    -> ('a[@local_opt])
    @@ portable
    = "%identity"

  let[@inline] call_await t (trigger : Trigger.t) =
    match t with
    | Local_await
        { handler = { context; await; yield = _; password = _ }; terminator = _ } ->
      (magic_unwrap_capsule await) (magic_unwrap_capsule context) trigger [@nontail]
    | Portable_await { handler = { context; await; yield = _ }; terminator = _ } ->
      await context trigger [@nontail]
  ;;
end

include Call_await_unsafe_optimized

let await t ~on_terminate ~await_on =
  if not (Trigger.is_signalled await_on)
  then (
    (match Terminator.add_trigger (terminator t) on_terminate with
     | Terminated -> Trigger.Source.signal on_terminate
     | Attached | Signaled -> ());
    call_await t await_on [@nontail])
;;

let await_until_terminated t trigger =
  match Terminator.add_trigger (terminator t) (Trigger.source trigger) with
  | Attached -> call_await t trigger
  | Terminated -> Trigger.Source.signal (Trigger.source trigger)
  | Signaled -> ()
;;

let await_until_terminated_or_canceled w cancellation trigger =
  match Cancellation.add_trigger cancellation (Trigger.source trigger) with
  | Attached -> await_until_terminated w trigger
  | Canceled -> Trigger.Source.signal (Trigger.source trigger)
  | Signaled -> ()
;;

let await_never_terminated t trigger =
  if not (Trigger.is_signalled trigger) then call_await t trigger
;;

let await_with_terminate t trigger ~terminate r =
  if not (Trigger.is_signalled trigger)
  then (
    let on_terminate = Trigger.create_with_action ~f:terminate r in
    match Terminator.add_trigger (terminator t) (Trigger.source on_terminate) with
    | Terminated ->
      Trigger.Source.signal (Trigger.source on_terminate);
      call_await t trigger
    | Attached | Signaled ->
      call_await t trigger;
      let _ : bool = Trigger.drop on_terminate in
      ())
;;

let await_with_terminate_or_cancel t cancellation trigger ~terminate_or_cancel request =
  if not (Trigger.is_signalled trigger)
  then (
    let on_terminate_or_cancel =
      Trigger.create_with_action ~f:terminate_or_cancel request
    in
    match
      Cancellation.add_trigger cancellation (Trigger.source on_terminate_or_cancel)
    with
    | Canceled ->
      Trigger.Source.signal (Trigger.source on_terminate_or_cancel);
      call_await t trigger
    | Attached | Signaled ->
      (match
         Terminator.add_trigger (terminator t) (Trigger.source on_terminate_or_cancel)
       with
       | Terminated ->
         Trigger.Source.signal (Trigger.source on_terminate_or_cancel);
         call_await t trigger
       | Attached | Signaled ->
         call_await t trigger;
         let _ : bool = Trigger.drop on_terminate_or_cancel in
         ()))
;;

let is_terminated t = Terminator.is_terminated (terminator t) [@nontail]

let with_terminator t terminator =
  match t with
  | Local_await t -> exclave_ Local_await { t with terminator }
  | Portable_await t -> exclave_ Portable_await { t with terminator }
;;

let yield t =
  if Terminator.is_terminated (terminator t) then raise Terminated;
  match t with
  | Local_await { handler = { password; context; yield; await = _ }; terminator = _ } ->
    (match yield with
     | Null -> ()
     | This yield ->
       Capsule.Expert.Data.Local.iter
         ~password
         (Capsule.Expert.Data.Local.both yield context)
         ~f:(fun [@inline] (yield, context) -> yield context [@nontail])
       [@nontail])
  | Portable_await { handler = { context; yield; await = _ }; terminator = _ } ->
    (match yield with
     | Null -> ()
     | This yield -> yield context [@nontail])
;;

module For_testing = struct
  let with_never ~f = exclave_
    let await () _ =
      failwith
        "[await never] was called. Usually this means that an operation blocked which \
         was expected to never block"
    in
    let (P access) = Capsule.Access.current () in
    let await = Capsule.Data.(wrap [@mode local]) ~access await in
    let context = Capsule.Data.(wrap [@mode local]) ~access () in
    Capsule.Expert.Password.with_current access (fun password -> exclave_
      let await =
        Local_await
          { terminator = Terminator.never
          ; handler = { await; context; password; yield = Null }
          }
      in
      f await)
  ;;
end
