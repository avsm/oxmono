open! Core
open Ppxlib

module Passthroughs : sig
  (** Attributes whose payloads get directly passed to the runtime; e.g.
      [[@config my_config]] passes [~config:my_config] to
      [Ppx_quick_test_config.run_quick_test]. *)
  type t = expression Map.M(Quick_test_parameter).t
end

module Remember_failures : sig
  module One : sig
    (** The ppx-time representation of a [Ppx_quick_test_runtime.Sexp_examples.One.t];
        represents one item in a [@remember_failures] attribute *)
    type t =
      { expr : expression (** One expression, as appears in the attribute *)
      ; body : string (** The syntactic string contents of the expression *)
      ; body_location : location (** The location of [body] (not including delimiters) *)
      ; delimiters : string option
      (** [None] for ["asdf"], [Some "xxx"] for [{xxx|asdf|xxx}] *)
      }
  end

  type t =
    | Ignore (** Either no attribute, or [@remember_failures.ignore _] *)
    | Empty of location
    (** [@remember_failures], with no payload; the [location] is the location of the full
        attribute *)
    | List of One.t Nonempty_list.t (** [@remember_failures _ ...] *)
end

module Examples : sig
  (** [@examples _]; specially handled to turn [(x, y, z)] into [(x, (y, (z, ())))], which
      is the expected shape for [Ppx_quick_test_runtime_lib.Params] *)
  type t = expression option
end

type t =
  { passthroughs : Passthroughs.t
  ; remember_failures : Remember_failures.t
  ; examples : Examples.t
  }

val consume : value_binding -> value_binding * t
