@@ portable

(** A trigger represents the ability to await for or react to a signal.

    To use a trigger, one [create]s a trigger and arranges for [signal] to be called on
    the [source] of the trigger.

    To use a trigger to react to a signal, one may call [on_signal] to register a callback
    with the trigger. Only a single callback can be registered with each trigger. Because
    the registered callback may be called from an unknown context, the callback should be
    essentially wait-free and should not raise exceptions.

    The most common way to use a trigger is to await on it with an [Await.t]. To do so,
    one must not register an action with the sink: the scheduler will instead register its
    own callback to resume the awaiter. *)

module Source : sig
  (** [t] is the type of sources of triggers. A source can be used to signal the
      associated trigger. *)
  type t : value mod contended portable

  (** [same l r] determines whether [l] and [r] are the one and the same trigger. *)
  val same : t @ local -> t @ local -> bool

  (** [signal t] signals [t], running the callback registered with [on_signal t] if it
      exists. If [t] is already signalled then [signal t] does nothing. *)
  val signal : t @ local -> unit

  (** [is_signalled t] is [true] if [t] has been signalled and [false] if it is
      unsignalled. *)
  val is_signalled : t @ local -> bool

  (**/**)

  module For_testing : sig
    (** [signal_if_awaiting t] signals [t] only if the [t] is in the waiting state. This
        is intended for testing / benchmarking the overheads of await implementations and
        should not be used in other contexts. *)
    val signal_if_awaiting : t @ local -> unit
  end
end

(** [t] is the type of triggers. A sink can be used to register a callback with the
    associated trigger. *)
type t : value mod contended portable

(** [is_signalled t] is [true] if [t] has been signalled and [false] if it is unsignalled. *)
val is_signalled : t @ local -> bool

(** [on_signal t ~f k] registers [f k] to be run when [t] is signalled. It is [Null] if
    [t] was unsignalled and the function was registered. It is [This k] if [t] was already
    signalled and so the function was not registered.

    Only one callback can be registered with a trigger: [on_signal t f k] raises [Failure]
    if [t] already has a registered callback. *)
val on_signal
  :  t @ local
  -> f:('k @ contended once portable unique -> unit) @ once portable
  -> 'k @ contended once portable unique
  -> 'k or_null @ contended once portable unique

(** [drop t] unregisters the callback registered with [t] and marks [t] as signalled. It
    is [true] if [t] was previously unsignalled and a callback was successfully
    unregistered. It is [false] if [t] was already signalled.

    This should only be used after a callback has been registered: [drop t] raises
    [Failure] if [t] is unsignalled and there was no registered callback. *)
val drop : t @ local -> bool

(** [create ()] creates a new unsignalled trigger.

    If a trigger is potentially added to a cancellation or termination token, then the
    trigger must be used. To use a trigger, either [Source.signal] or [drop] must be
    called on it. *)
val create : unit -> t

(** [create_with_action ~f k] creates a new trigger that already has the given [f] and
    resource [k] registered with it.

    If a trigger is potentially added to a cancellation or termination token, then the
    trigger must be used. To use a trigger, either [Source.signal] or [drop] must be
    called on it.

    Equivalent to:
    {[
      let create_with_action action k =
        let trigger = create () in
        let _ : bool = on_signal trigger action k in
        trigger
      ;;
    ]} *)
val create_with_action
  :  f:('k @ contended once portable unique -> unit) @ once portable
  -> 'k @ contended once portable unique
  -> t

(** [source t] returns the associated source of the trigger [t]. *)
val%template source : t @ m -> Source.t @ m
[@@mode m = (global, local)]
