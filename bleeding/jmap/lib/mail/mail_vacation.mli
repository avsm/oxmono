@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Vacation responses.

    {{:https://datatracker.ietf.org/doc/html/rfc8621#section-8} RFC 8621 Section
     8} defines the VacationResponse object, the message a server sends back
    automatically while the user is away. There is exactly one per account,
    whose id is {!singleton_id}, so it is changed with a [VacationResponse/set]
    [update] rather than a create.

    @canonical Jmap.Proto.Vacation *)

type t = {
  id : Proto_id.t option;
      (** The id, always {!singleton_id}, or [None] if the property was not
          asked for. *)
  is_enabled : bool option;
      (** [true] while the server is sending the response. *)
  from_date : Ptime.t option;
      (** The time to start sending the response at, or [None] to start at once.
      *)
  to_date : Ptime.t option;
      (** The time to stop sending the response at, or [None] to go on until
          [is_enabled] is set to [false]. *)
  subject : string option;
      (** The subject of the response, or [None] to let the server choose one.
      *)
  text_body : string option;  (** The plain text body of the response. *)
  html_body : string option;  (** The HTML body of the response. *)
}
(** The type for VacationResponse objects. *)

val v :
  ?is_enabled:bool ->
  ?from_date:Ptime.t ->
  ?to_date:Ptime.t ->
  ?subject:string ->
  ?text_body:string ->
  ?html_body:string ->
  unit ->
  t
(** [v ()] is a VacationResponse with only the properties given set, every other
    one being [None] and left out of the JSON. [id] is always unset, the account
    having one VacationResponse whose id the server already knows. *)

val singleton_id : Proto_id.t
(** [singleton_id] is ["singleton"], the id of the one VacationResponse of an
    account. *)

val jsont : t Jsont.t
(** [jsont] is the codec for a VacationResponse. Every property may be absent,
    since RFC 8621 Section 8.1 lets a client restrict the [properties] of the
    [/get]. *)
