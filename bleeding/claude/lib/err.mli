(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Error handling for claudeio. *)

type t =
  | Cli_not_found of string
  | Process_error of string
  | Connection_error of string
  | Protocol_error of string
  | Timeout of string
  | Permission_denied of { tool_name : string; message : string }
  | Hook_error of { callback_id : string; message : string }
  | Control_error of { request_id : string; message : string }

exception E of t

val pp : Format.formatter -> t -> unit
(** Pretty-print an error. *)

val to_string : t -> string
(** Convert error to string. *)

val raise : t -> 'a
(** [raise err] raises [E err]. *)

(** {1 Convenience Raisers} *)

val cli_not_found : string -> 'a
val process_error : string -> 'a
val connection_error : string -> 'a
val protocol_error : string -> 'a
val timeout : string -> 'a
val permission_denied : tool_name:string -> message:string -> 'a
val hook_error : callback_id:string -> message:string -> 'a
val control_error : request_id:string -> message:string -> 'a

(** {1 Result Helpers} *)

val get_ok : msg:string -> ('a, string) result -> 'a
(** [get_ok ~msg result] returns the Ok value or raises Protocol_error with msg
    prefix. *)

val get_ok' : msg:string -> ('a, string) result -> 'a
(** [get_ok' ~msg result] returns the Ok value or raises Protocol_error with
    string error. *)
