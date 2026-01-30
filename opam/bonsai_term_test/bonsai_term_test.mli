open! Core
open Bonsai_test
open Bonsai_term

module Result_spec : sig
  type ('a, 'incoming) t
  type 'a incoming
end

module Capability : sig
  type t =
    | Ansi
    | Not_ansi
end

val create_handle_generic
  :  ?initial_dimensions:Dimensions.t
  -> ?capability:Capability.t
  -> to_view_with_handler:('a -> View.With_handler.t)
  -> handle_incoming:('a -> 'incoming -> unit Effect.t)
  -> (dimensions:Dimensions.t Bonsai.t -> local_ Bonsai.graph -> 'a Bonsai.t)
  -> (('a, 'incoming) Result_spec.t, 'incoming Result_spec.incoming) Handle.t

val create_handle
  :  ?initial_dimensions:Dimensions.t
  -> ?capability:Capability.t
  -> (dimensions:Dimensions.t Bonsai.t
      -> local_ Bonsai.graph
      -> view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t)
  -> ( (View.With_handler.t, Nothing.t) Result_spec.t
       , Nothing.t Result_spec.incoming )
       Handle.t

val create_handle_without_handler
  :  ?initial_dimensions:Dimensions.t
  -> ?capability:Capability.t
  -> (dimensions:Dimensions.t Bonsai.t -> local_ Bonsai.graph -> View.t Bonsai.t)
  -> ((View.t, Nothing.t) Result_spec.t, Nothing.t Result_spec.incoming) Handle.t

val set_dimensions : (_, _ Result_spec.incoming) Handle.t -> Dimensions.t -> unit
val send_event : (_, _ Result_spec.incoming) Handle.t -> Event.t -> unit
val do_actions : (_, 'incoming Result_spec.incoming) Handle.t -> 'incoming list -> unit

module For_minimal_mocking_test_suite : sig
  val cap_to_notty_cap : Capability.t -> Notty.Cap.t
  val prettify_ansi : string -> string
end
