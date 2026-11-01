(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Runtime selection of model-specific agent constructors.

    A driver creates its own tools with its backend's native codecs. Agentkit
    only selects the driver and exposes the resulting {!Agent.S} operations. *)

type model = { name : string; description : string }
(** One model name accepted by a driver. *)

type availability =
  | Ready
  | Needs_download
  | Unavailable
  | Managed
      (** Whether a model can be used now, fetched, or is managed outside
          Agentkit. *)

type listing = { model : model; availability : availability }
(** A qualified model and its current availability. *)

type session
(** An agent with its backend-specific type hidden. *)

val session :
  ?prefill_progress:(unit -> int * int) ->
  (module Agent.S with type t = 'a) ->
  'a ->
  session
(** [session backend agent] exposes [agent] through the common operations.
    [prefill_progress] reports tokens prepared and tokens required, when the
    backend provides a live progress counter. *)

val send : session -> on_event:(Agent.event -> unit) -> string -> unit
(** [send session ~on_event prompt] runs one exchange. *)

val stats : session -> Agent.stats
(** [stats session] is the last exchange's accounting. *)

val cancel : session -> unit
(** [cancel session] interrupts its active exchange. *)

val close : session -> unit
(** [close session] releases its backend resources. *)

val prefill_progress : session -> int * int
(** [prefill_progress session] is the current prefill count and target. It is
    [(0, 0)] when the backend does not report progress. *)

type 'a t
(** A model family and its native constructor, returning ['a]. *)

val v :
  name:string -> models:(unit -> model list) -> create:(string -> 'a) -> 'a t
(** [v ~name ~models ~create] registers a model family. [create model] builds
    native tools and an agent for [model]. [models ()] lists names the driver
    knows about. A driver may also accept a path or another model name not in
    that list. *)

val manage :
  ?availability:(string -> availability) ->
  ?fetch:(string -> token:string option -> (unit, string) result) ->
  ?canonical:(string -> string) ->
  'a t ->
  'a t
(** [manage driver] adds model availability and an explicit fetch operation
    without changing how the driver constructs agents. [canonical] maps a
    driver's aliases to listed model names for [lookup]. *)

val name : 'a t -> string
(** [name driver] is the prefix used before the slash in a model choice. *)

type 'a registry
(** A set of drivers available to one program. *)

val merge : 'a t list -> 'a registry
(** [merge drivers] is a registry of [drivers]. Duplicate names are refused. *)

val models : 'a registry -> model list
(** [models registry] lists choices as [driver/model] names. *)

val catalog : 'a registry -> listing list
(** [catalog registry] lists qualified choices and their current availability.
*)

val lookup : 'a registry -> string -> (listing, string) result
(** [lookup registry choice] describes a listed model, resolving driver aliases.
*)

val fetch :
  'a registry -> string -> token:string option -> (unit, string) result
(** [fetch registry choice ~token] explicitly installs [driver/model]. It does
    not run during model selection. A driver without a fetch operation is
    refused. *)

type 'a selection
(** A selected driver and model before its constructor runs. *)

val select : 'a registry -> string -> ('a selection, string) result
(** [select registry choice] resolves [driver/model] without constructing the
    model or starting its tools. *)

val driver_name : 'a selection -> string
(** [driver_name selection] is the selected driver prefix. *)

val model_name : 'a selection -> string
(** [model_name selection] is the name passed to its constructor. *)

val start : 'a selection -> 'a
(** [start selection] calls the selected driver's constructor. *)

val create : 'a registry -> string -> ('a, string) result
(** [create registry choice] selects [driver/model] and builds its value. The
    model name after the first slash is passed unchanged to the driver. Errors
    selecting a driver are returned. Exceptions from its constructor are allowed
    to escape. *)

val model_arg : string option Cmdliner.Term.t
(** [model_arg] reads [--model DRIVER/MODEL]. *)

val model_term :
  default:string -> ?short_driver:string -> unit -> string Cmdliner.Term.t
(** [model_term ~default ()] reads [--model DRIVER/MODEL] and uses [default]
    when absent. [short_driver] keeps older unqualified model names and paths
    usable under that driver. An unknown explicit driver is checked by
    {!create}. *)

module Cli : sig
  val models_cmd : unit registry -> (unit, string) result Cmdliner.Cmd.t
  (** [models_cmd registry] provides [models list], [models show MODEL], and
      [models fetch MODEL]. Model selection itself never fetches weights. *)
end
