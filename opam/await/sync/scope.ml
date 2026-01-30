open Base
open Basement
open Portable_kernel
open Await_kernel

module Live : sig @@ portable
  type count : immediate

  (** Live count starts at [1] to include the thread of control that created the count. *)
  val one : count

  type t = #(count Atomic.Loc.t * Trigger.Source.t)

  (** [incr t] increments the live count [t] unless it is zero.

      @raise Invalid_argument in case the count is zero. *)
  val incr : t @ contended local -> unit

  (** [decr t] decrements the live count [t] and signals the associated trigger in case
      the count reached zero.

      It is a fatal error to decrement the count after it has reached zero. *)
  val decr : t @ contended local -> unit
end = struct
  type count = int

  let one = 1

  type t = #(int Atomic.Loc.t * Trigger.Source.t)

  let[@inline] incr #(count, _) =
    (* We allow non-local counts and need to ensure that we don't increment after 0. *)
    let[@inline] rec loop count backoff =
      let n = Atomic.Loc.get count in
      if n = 0
      then invalid_arg "Scope: already finished"
      else (
        match
          Atomic.Loc.compare_and_set count ~if_phys_equal_to:n ~replace_with:(n + 1)
        with
        | Set_here -> ()
        | Compare_failed -> loop count (Backoff.once backoff))
    in
    loop count Backoff.default
  ;;

  let[@inline] decr #(count, finished) =
    let prior = Atomic.Loc.fetch_and_add count (-1) in
    assert%debug (1 <= prior);
    if prior = 1 then Trigger.Source.signal finished
  ;;
end

type 'a inner =
  { mutable tasks_count : Live.count [@atomic]
  ; tasks_finished : Trigger.Source.t
  ; mutable daemons_count : Live.count [@atomic]
  ; daemons_finished : Trigger.Source.t
  ; daemon_cancellation : Cancellation.t
  ; mutable failure : (exn * Backtrace.t) or_null [@atomic]
  ; context : 'a @@ contended portable
  ; terminator : Terminator.t
  }

type 'a t = { inner : 'a inner @@ contended global portable } [@@unboxed]

let terminator { inner = { terminator; _ } } = terminator

let terminate t =
  Terminator.Source.terminate
    (Terminator.source (terminator t)
     (* Scopes always have a terminatable terminator *)
     |> Or_null.value_exn)
;;

let context { inner = { context; _ } } = context

let raised { inner = { terminator; _ } as inner } exn bt =
  match (Atomic.Loc.get [%atomic.loc inner.failure] : (exn * Backtrace.t) or_null) with
  | Null ->
    (match
       Atomic.Loc.compare_and_set
         [%atomic.loc inner.failure]
         ~if_phys_equal_to:Null
         ~replace_with:(This (exn, bt))
     with
     | Compare_failed -> ()
     | Set_here ->
       Or_null.iter ~f:Terminator.Source.terminate (Terminator.source terminator))
  | This _ -> ()
;;

let[@inline] tasks t = #([%atomic.loc t.inner.tasks_count], t.inner.tasks_finished)
let[@inline] daemons t = #([%atomic.loc t.inner.daemons_count], t.inner.daemons_finished)

module Task_handle = struct
  type 'a inner =
    { scope : 'a t
    ; mutable am_daemon : bool [@atomic]
    }

  type 'a t = { inner : 'a inner @@ aliased contended }

  let into_scope { inner = { scope; _ } } = scope

  let become_daemon { inner = { scope; _ } as inner } =
    Live.incr (daemons scope);
    Atomic.Loc.set [%atomic.loc inner.am_daemon] true;
    Live.decr (tasks scope);
    #(scope, scope.inner.daemon_cancellation)
  ;;
end

module Token = struct
  type nonrec 'a t = 'a t aliased many

  let use { many = { aliased = t } } ~f =
    let task_handle = stack_ { Task_handle.scope = t; am_daemon = false } in
    let[@inline] finish () =
      if task_handle.am_daemon then Live.decr (daemons t) else Live.decr (tasks t)
    in
    match (f [@inlined hint]) t.inner.terminator { Task_handle.inner = task_handle } with
    | res ->
      finish ();
      res
    | exception exn ->
      let bt = Backtrace.Exn.most_recent () in
      raised t exn bt;
      finish () [@nontail]
  ;;

  let drop { many = { aliased = t } } = Live.decr (tasks t)
end

let globalize { inner } = { inner }

let add (t @ local) : _ Token.t @ unique =
  Live.incr (tasks t);
  { many = { aliased = globalize t } }
;;

let failure t = Atomic.Loc.get [%atomic.loc t.inner.failure]

module Global = struct
  let create_with_trigger context ~finished:both_finished =
    let tasks_finished = Trigger.create () in
    let t =
      { inner =
          { tasks_count = Live.one
          ; tasks_finished = Trigger.source tasks_finished
          ; daemons_count = Live.one
          ; daemons_finished = both_finished
          ; daemon_cancellation = Cancellation.never
          ; failure = Null
          ; context
          ; terminator = Terminator.Expert.create ()
          }
      }
    in
    let _ : _ =
      Trigger.on_signal
        tasks_finished
        ~f:(fun { many = { aliased = t } } -> Live.decr (daemons t))
        { many = { aliased = t } }
    in
    let _ : _ =
      Terminator.add_trigger
        t.inner.terminator
        (Trigger.source
           (Trigger.create_with_action
              ~f:(fun { many = { aliased = t } } -> Live.decr (tasks t))
              { many = { aliased = t } }))
    in
    t
  ;;

  let create context ~on_exit =
    let finished = Trigger.create () in
    let t = create_with_trigger context ~finished:(Trigger.source finished) in
    let _ : _ =
      Trigger.on_signal
        finished
        ~f:(fun { many = { aliased = t } } -> on_exit t (failure t))
        { many = { aliased = t } }
    in
    t
  ;;
end

let finish w t ~tasks_finished ~daemons_finished =
  Live.decr (tasks t);
  Await.await_never_terminated w tasks_finished;
  Cancellation.Source.cancel
    (Cancellation.source t.inner.daemon_cancellation |> Or_null.value_exn);
  Live.decr (daemons t);
  Await.await_never_terminated w daemons_finished;
  Or_null.iter
    (Atomic.Loc.get [%atomic.loc t.inner.failure])
    ~f:(fun (exn, bt) -> Exn.raise_with_original_backtrace exn bt)
;;

let with_ w context ~f =
  Terminator.with_linked (Await.terminator w) (fun terminator ->
    Cancellation.with_ (fun [@inline] daemon_cancellation ->
      let tasks_finished = Trigger.create () in
      let daemons_finished = Trigger.create () in
      let t @ local =
        { inner =
            { tasks_count = Live.one
            ; tasks_finished = Trigger.source tasks_finished
            ; daemons_count = Live.one
            ; daemons_finished = Trigger.source daemons_finished
            ; daemon_cancellation = Cancellation.Expert.globalize daemon_cancellation
            ; failure = Null
            ; context
            ; terminator = Terminator.Expert.globalize terminator
            }
        }
      in
      match f t with
      | result ->
        finish w t ~tasks_finished ~daemons_finished;
        result
      | exception exn ->
        let bt = Backtrace.Exn.most_recent () in
        raised t exn bt;
        finish w t ~tasks_finished ~daemons_finished;
        Exn.raise_with_original_backtrace exn bt)
    [@nontail])
  [@nontail]
;;
