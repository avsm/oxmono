(** push — push rules and pushers.

    A push rule tells the homeserver which events are worth notifying about and
    a pusher tells it where to deliver the notification. The rules themselves
    are {!Matrix_proto.Push}, and evaluating them locally is {!Push_evaluator}.
    Every endpoint here works on the [global] scope, the only one the
    specification defines.

    @see <https://spec.matrix.org/v1.11/client-server-api/#push-notifications>
      Push Notifications *)

(** {1 Push rules} *)

val get_push_rules : Client.t -> (Matrix_proto.Push.Ruleset.t, Error.t) result
(** [get_push_rules t] is every rule the user has, in every kind. Uses
    [GET /_matrix/client/v3/pushrules/]. *)

val get_push_rule :
  Client.t ->
  Matrix_proto.Push.Rule_id.t ->
  (Matrix_proto.Push.Rule.t, Error.t) result
(** [get_push_rule t rule_id] is the rule at [rule_id]. Uses
    [GET /_matrix/client/v3/pushrules/global/{kind}/{ruleId}]. An unknown
    [rule_id] is [M_NOT_FOUND]. *)

val delete_push_rule :
  Client.t -> Matrix_proto.Push.Rule_id.t -> (unit, Error.t) result
(** [delete_push_rule t rule_id] removes the rule at [rule_id]. Uses
    [DELETE /_matrix/client/v3/pushrules/global/{kind}/{ruleId}]. Deleting a
    rule the server supplies is [M_FORBIDDEN]. *)

val set_push_rule :
  Client.t ->
  Matrix_proto.Push.Rule_id.t ->
  actions:Matrix_proto.Push.Action.t list ->
  ?conditions:Matrix_proto.Push.Condition.t list ->
  ?pattern:string ->
  ?before:string ->
  ?after:string ->
  unit ->
  (unit, Error.t) result
(** [set_push_rule t rule_id ~actions ()] adds the rule at [rule_id] or replaces
    it. Uses [PUT /_matrix/client/v3/pushrules/global/{kind}/{ruleId}].

    [conditions] applies to override and underride rules and defaults to none.
    [pattern] applies to content rules and defaults to none. The server rejects
    either where it does not belong. [before] and [after] name another rule of
    the same kind to insert this one next to, and both default to absent, which
    puts the rule at the head of its kind. *)

val set_enabled :
  Client.t ->
  Matrix_proto.Push.Rule_id.t ->
  enabled:bool ->
  (unit, Error.t) result
(** [set_enabled t rule_id ~enabled] turns the rule at [rule_id] on or off. Uses
    [PUT /_matrix/client/v3/pushrules/global/{kind}/{ruleId}/enabled]. A
    disabled rule is skipped without ending evaluation, so the rules after it
    still apply. *)

val set_actions :
  Client.t ->
  Matrix_proto.Push.Rule_id.t ->
  actions:Matrix_proto.Push.Action.t list ->
  (unit, Error.t) result
(** [set_actions t rule_id ~actions] replaces the actions of the rule at
    [rule_id]. Uses
    [PUT /_matrix/client/v3/pushrules/global/{kind}/{ruleId}/actions]. It is the
    one way to change the behaviour of a rule the server supplies without
    deleting it. *)

(** {1 Pushers} *)

(** The type for the transport a homeserver reaches a push gateway over. *)
type pusher_kind = Http | Email

type pusher_data = {
  url : string option;
      (** The gateway's [/_matrix/push/v1/notify] endpoint. Required for an
          {!Http} pusher and absent for an {!Email} one. *)
  format : string option;
      (** ["event_id_only"] to have the gateway sent identifiers rather than
          event content. *)
}
(** The type for where and how a pusher delivers. *)

type pusher = {
  pushkey : string;
      (** Identifies the device to the gateway, and identifies the pusher. A
          second pusher with the same [pushkey] and [app_id] replaces this one.
      *)
  kind : pusher_kind;
  app_id : string;  (** Identifies the application, at most 64 characters. *)
  app_display_name : string;
  device_display_name : string;
  profile_tag : string option;
      (** Selects which of the user's rulesets applies to this device. *)
  lang : string;  (** Language for notification text, as an ISO 639 code. *)
  data : pusher_data;
}
(** The type for a destination the homeserver delivers notifications to. *)

val get_pushers : Client.t -> (pusher list, Error.t) result
(** [get_pushers t] is every pusher on the user's account. Uses
    [GET /_matrix/client/v3/pushers]. *)

val set_pusher :
  Client.t ->
  pushkey:string ->
  kind:pusher_kind ->
  app_id:string ->
  app_display_name:string ->
  device_display_name:string ->
  ?profile_tag:string ->
  lang:string ->
  data:pusher_data ->
  ?append:bool ->
  unit ->
  (unit, Error.t) result
(** [set_pusher t ~pushkey ~kind ~app_id ~app_display_name ~device_display_name
     ~lang ~data ()] adds a pusher or replaces the one with the same [pushkey]
    and [app_id]. Uses [POST /_matrix/client/v3/pushers/set].

    [profile_tag] defaults to absent. [append] defaults to [false], which makes
    the server drop any other user's pusher holding the same [pushkey], the
    right behaviour when a device changes hands. Pass [true] to leave them
    alone. *)

val delete_pusher :
  Client.t -> pushkey:string -> app_id:string -> (unit, Error.t) result
(** [delete_pusher t ~pushkey ~app_id] stops delivery to that pusher. It is
    [POST /_matrix/client/v3/pushers/set] with a null [kind], which is how the
    specification spells removal. *)
