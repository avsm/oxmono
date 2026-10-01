(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Events shared by Agentkit model adapters.

    An adapter reports one {!event} stream for each exchange. Text may arrive in
    small pieces. A tool call is followed by one result, statistics finish a
    model turn, and {!Done} finishes the exchange. {!Trace} turns this stream
    into durable journal records. *)

type tool_call = {
  id : string;  (** backend call identifier, when supplied *)
  name : string;
  arguments : string;
}

module Tool : sig
  type t
  val v : name:string -> description:string -> parameters:Jsont.json -> t
  val with_invoke : t -> (tool_call -> string) -> t
  val invoke : t -> tool_call -> string
  val name : t -> string
  val description : t -> string
  val parameters : t -> Jsont.json
end

type stats = {
  ctx_used : int;  (** tokens the session holds *)
  ctx_size : int;  (** the context window they must fit in *)
  prompt_tokens : int;  (** tokens in the last model prompt *)
  generated : int;  (** tokens generated during the exchange *)
  generate_seconds : float;  (** time spent generating them *)
  prefill_seconds : float;  (** time spent preparing prompts *)
  tool_calls : int;  (** tools called during the exchange *)
  turns : int;  (** model turns in the exchange *)
  drafted : int;  (** generated tokens accepted from a draft model *)
  total_generated : int;  (** tokens generated since the session began *)
  total_generate_seconds : float;  (** time spent generating them *)
}
(** Model-independent accounting for one exchange. An adapter uses zero for a
    measurement its backend does not provide. *)

(** A request to run one tool. *)

type cut = {
  tokens : int;  (** the reply's token ceiling *)
  tool_call : bool;  (** whether an unfinished tool call was discarded *)
}
(** A reply stopped by its token ceiling. *)

type compaction = {
  before : int;  (** tokens held before compaction *)
  after : int;  (** tokens held afterwards *)
  summary : string;  (** the summary that replaced earlier turns *)
}
(** A conversation shortened by its backend. *)

(** Progress from a model adapter. *)
type event =
  | Reasoning of string
  | Content of string
  | Tool_call of tool_call
  | Tool_result of string * string
  | Stats of stats
  | Expanded of int
  | Cut_off of cut
  | Squeezed of int
  | Compacted of compaction
  | Done

(** Operations common to Agentkit model adapters. Agent construction remains
    backend-specific because model selection and transport are not common. *)
module type S = sig
  type t
  (** A persistent model conversation. *)

  val send : t -> on_event:(event -> unit) -> string -> unit
  (** [send agent ~on_event prompt] runs one complete exchange and reports its
      progress through [on_event]. *)

  val stats : t -> stats
  (** [stats agent] is the accounting for the most recent exchange. *)

  val cancel : t -> unit
  (** [cancel agent] interrupts its active exchange. *)

  val close : t -> unit
  (** [close agent] releases its backend resources. *)
end
