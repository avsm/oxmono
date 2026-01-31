(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Control messages for Claude session management.

    Control messages are used to manage the interaction flow with Claude,
    including session control, cancellation requests, and other operational
    commands. *)

val src : Logs.Src.t
(** The log source for control message operations *)

type t
(** The type of control messages. *)

val jsont : t Jsont.t
(** [jsont] is the jsont codec for control messages. *)

val create : request_id:string -> subtype:string -> data:Jsont.json -> t
(** [create ~request_id ~subtype ~data] creates a new control message.
    @param request_id Unique identifier for this control request
    @param subtype The specific type of control message
    @param data Additional JSON data for the control message *)

val request_id : t -> string
(** [request_id t] returns the unique request identifier. *)

val subtype : t -> string
(** [subtype t] returns the control message subtype. *)

val data : t -> Jsont.json
(** [data t] returns the additional data associated with the control message. *)

val to_json : t -> Jsont.json
(** [to_json t] converts the control message to its JSON representation. *)

val of_json : Jsont.json -> t
(** [of_json json] parses a control message from JSON.
    @raise Invalid_argument if the JSON is not a valid control message. *)

(** {1 Logging} *)

val log_received : t -> unit
(** [log_received t] logs that a control message was received. *)

val log_sending : t -> unit
(** [log_sending t] logs that a control message is being sent. *)
