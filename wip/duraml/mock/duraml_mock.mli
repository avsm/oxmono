(** Scripted queues and request-boundary fault injection.

    {1 NAME}
    Duraml_mock - replace or observe the Duraml provider protocol in tests.

    {1 DESCRIPTION}
    Use the memory backend when a test needs real queue semantics. Use {!make}
    to script typed responses, and {!wrap} to observe or interrupt a real store.
    Callbacks execute in the calling fiber and may suspend or raise. These
    mocks do not simulate SQLite crash recovery, fsync or power loss.

    {1 SYNOPSIS}
    {[
      let seen = ref [] in
      let before = { Duraml_mock.observe = (fun request ->
        seen := Duraml.Pi.operation request :: !seen) } in
      let observed = Duraml_mock.wrap ~before store in
      Duraml.enqueue observed spec
    ]}

    {1 LIFETIME}
    Scripted providers own no switch or resources. Callers own any state
      captured
    by callbacks. Wrappers retain the underlying store's lifetime. Closing it
    makes forwarded requests fail normally. Neither constructor starts fibers.
      *)

type handler = {call : 'a. 'a Duraml.Pi.request -> 'a}
(** A rank-2 callback returning the result required by each request constructor.
    Pattern-match on the request GADT to refine its result type. The record is
    concrete so callers can provide a polymorphic implementation. *)
val make : await:(Duraml.waiting -> unit) -> handler -> Duraml.plain
(** [make ~await handler] constructs a scripted resource. The handler owns all
    semantics, including identity and claim production. Use {!Duraml.Pi.waiting}
    to create scripted wait observations. [await] must arrange suspension when
    testing blocking {!Duraml.take}, or an [Await] script can spin
      indefinitely. *)
type observer = {observe : 'a. 'a Duraml.Pi.request -> unit}
(** A rank-2 observer that can inspect every request without changing its type.
  *)
val wrap : ?before:observer -> ?after:observer -> _ Duraml.t -> Duraml.plain
(** [wrap ~before ~after store] invokes [before], forwards the request, then
    invokes [after] only on success. Raising in [before] prevents dispatch.
    Raising in [after] models losing the response to a committed operation.
    Wait operations delegate directly and are not passed to observers. *)
