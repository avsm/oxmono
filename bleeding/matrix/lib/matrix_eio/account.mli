(** account — third-party identifiers, the password and deactivation, raising
    instead of returning.

    Every function raises [Eio.Io] carrying [Error.E e] where
    {!Matrix_client.Account} returns [Error e]. That module documents what each
    call does, which endpoint it uses and which errors it produces. *)

(** {1 Third-party identifiers} *)

(** The kinds of third-party identifier an account may be bound to. *)
type medium = Matrix_client.Account.medium =
  | Email
  | Msisdn  (** A phone number in E.164 form without the leading plus. *)

type threepid = Matrix_client.Account.threepid = {
  medium : medium;
  address : string;
  validated_at : Matrix_proto.Event.Timestamp.t;
  added_at : Matrix_proto.Event.Timestamp.t;
}
(** An email address or phone number bound to the account. *)

val get_threepids : Client.t -> threepid list
(** [get_threepids c] is {!Matrix_client.Account.get_threepids} with the result
    unwrapped. *)

val request_email_token :
  Client.t -> email:string -> client_secret:string -> send_attempt:int -> string
(** [request_email_token c ~email ~client_secret ~send_attempt] is
    {!Matrix_client.Account.request_email_token} with the result unwrapped. *)

val request_msisdn_token :
  Client.t ->
  country:string ->
  phone_number:string ->
  client_secret:string ->
  send_attempt:int ->
  string
(** [request_msisdn_token c ~country ~phone_number ~client_secret ~send_attempt]
    is {!Matrix_client.Account.request_msisdn_token} with the result unwrapped.
*)

val add_threepid : Client.t -> client_secret:string -> sid:string -> unit
(** [add_threepid c ~client_secret ~sid] is
    {!Matrix_client.Account.add_threepid} with the result unwrapped. *)

val delete_threepid : Client.t -> medium:medium -> address:string -> unit
(** [delete_threepid c ~medium ~address] is
    {!Matrix_client.Account.delete_threepid} with the result unwrapped. *)

(** {1 Password and deactivation} *)

val change_password :
  Client.t ->
  new_password:string ->
  ?logout_devices:bool ->
  ?auth:Matrix_client.Uiaa.auth_data ->
  unit ->
  unit
(** [change_password c ~new_password ()] is
    {!Matrix_client.Account.change_password} with the result unwrapped. *)

val deactivate :
  Client.t -> ?erase:bool -> ?auth:Matrix_client.Uiaa.auth_data -> unit -> unit
(** [deactivate c ()] is {!Matrix_client.Account.deactivate} with the result
    unwrapped. The account can never be used again. *)

(** {1 Ignored users} *)

val get_ignored_users : Client.t -> Matrix_proto.Id.User_id.t list
(** [get_ignored_users c] is {!Matrix_client.Account.get_ignored_users} with the
    result unwrapped. *)

val ignore_user : Client.t -> user_id:Matrix_proto.Id.User_id.t -> unit
(** [ignore_user c ~user_id] is {!Matrix_client.Account.ignore_user} with the
    result unwrapped. *)

val unignore_user : Client.t -> user_id:Matrix_proto.Id.User_id.t -> unit
(** [unignore_user c ~user_id] is {!Matrix_client.Account.unignore_user} with
    the result unwrapped. *)
