(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** What the [jmap] and [jmapq] commands share.

    Both tools read the same configuration, resolve the same account, page the
    same queries and report the same kinds of failure. Everything here is used
    by both, so that a fix lands once. *)

(** {1 Capabilities} *)

val mail_capabilities : string list
(** [mail_capabilities] is the [using] array of a mail request, which is the
    core and mail capabilities of RFC 8620 Section 2 and RFC 8621 Section 1. It
    is the default of every function here that sends a request. *)

(** {1 Reporting} *)

val die : ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [die fmt] prints the message on stderr under a red ["Error:"] prefix and
    exits with status 1. It is the one error shape both tools print. *)

val warn : ('a, Format.formatter, unit) format -> 'a
(** [warn fmt] prints the message on stderr under a yellow ["Warning:"] prefix.
    It is for an answer that is incomplete but still worth printing, so stdout
    stays usable in a pipe and the exit status stays 0. *)

val fail_client : Jmap_eio.Client.error -> 'a
(** [fail_client e] is {!die} over {!Jmap_eio.Client.error_to_string}. *)

val fail_sync : Jmap_eio.Sync.error -> 'a
(** [fail_sync e] is {!die} over {!Jmap_eio.Sync.pp_error}. *)

val debug_json : Jmap_eio.Cli.config -> string -> 'a Jsont.t -> 'a -> unit
(** [debug_json cfg name jsont v] prints [v] as indented JSON on stderr under
    the heading [name] when [cfg] has debugging enabled, and prints nothing
    otherwise. A value that will not encode prints as a note naming the encoding
    error, since a diagnostic dump is not worth failing a command for. *)

(** {1 Command line arguments} *)

val positive_int : int Cmdliner.Arg.conv
(** [positive_int] parses an integer between 1 and 2{^ 53}-1, which is the range
    RFC 8620 Section 1.3 allows a JMAP number to take. *)

val id : Jmap.Proto.Id.t Cmdliner.Arg.conv
(** [id] parses a JMAP id, rejecting on the command line what the server would
    reject. *)

val unique_ids : Jmap.Proto.Id.t list -> (Jmap.Proto.Id.t list, string) result
(** [unique_ids ids] is [ids] when no id appears twice, and otherwise a message
    naming the first repeat. A [/set] keys its arguments by id, so the same id
    twice in one call silently loses one of the two. *)

(** {1 Formatting} *)

val terminal_text : string -> string
(** [terminal_text text] is {!Jmap_eio.Cli.terminal_text}, which renders control
    characters visibly. Apply it to server-supplied text before printing it. *)

val ptime_to_string : Ptime.t -> string
(** [ptime_to_string t] is [t] as ["YYYY-MM-DD hh:mm:ss"] in UTC. It is exactly
    19 characters wide, so a column of them lines up. *)

val truncate_string : int -> string -> string
(** [truncate_string budget value] is {!terminal_text} of [value] shortened to
    at most [budget] bytes, ending in ["..."] when anything was dropped and
    [budget] is over 3. A [budget] of 3 or less has no room for the ellipsis and
    cuts silently; a non-positive one is the empty string. A value already
    within [budget] is returned unchanged.

    The budget is bytes rather than characters because the callers pad their
    columns with [%-20s] and the like, which counts bytes. A multi-byte UTF-8
    sequence is never cut in half, so the result may be shorter than [budget]
    even when the value was not. *)

val limit_note : shown:int -> limit:int -> string
(** [limit_note ~shown ~limit] is [", limit reached"] when [shown] is at least
    [limit], and the empty string otherwise. A listing that filled the limit it
    asked for is a prefix of the answer rather than the whole of it, and saying
    so is the difference between a count the user can trust and one they cannot.
*)

val listing_header : string -> string -> unit
(** [listing_header title summary] opens a vertical box on stdout and prints
    [title] in bold followed by [summary] in brackets and a blank line. Close
    the box with [Fmt.pr "@[@]@."] once the rows are printed. Every listing of
    both tools has this one shape. *)

(** {1 Sessions and accounts} *)

val resolve_account_id :
  ?capability:string ->
  Jmap_eio.Cli.config ->
  Jmap_eio.Client.t ->
  Jmap.Proto.Id.t
(** [resolve_account_id ~capability cfg client] is the account named by [cfg],
    or the primary account for [capability], which defaults to the mail
    capability. It {!die}s when no account can be resolved. *)

(** {1 Requests} *)

val call :
  ?capabilities:string list ->
  Jmap_eio.Client.t ->
  (_, 'r) Jmap.Chain.handle Jmap.Chain.t ->
  'r
(** [call ~capabilities client chain] is the decoded response of the call
    [chain] ends in. [capabilities] defaults to {!mail_capabilities}. It {!die}s
    on a transport, method or decoding failure. *)

val query_ids :
  Jmap_eio.Client.t ->
  ?capabilities:string list ->
  ?max:int ->
  (position:int64 ->
  limit:int64 ->
  ('q, Jmap.Proto.Method.query_response) Jmap.Chain.handle Jmap.Chain.t) ->
  Jmap.Proto.Id.t list
(** [query_ids client ~capabilities ~max query] is {!Jmap_eio.Sync.all_ids},
    which pages [query] under the limit the server reports rather than the one
    asked for. [capabilities] defaults to {!mail_capabilities} and [max]
    defaults to no cap. It {!die}s on failure. *)

val get_objects :
  Jmap_eio.Client.t ->
  ?capabilities:string list ->
  kind:string ->
  id:('r -> Jmap.Proto.Id.t option) ->
  Jmap.Proto.Id.t list ->
  (ids:Jmap.Proto.Id.t list ->
  ('g, 'r Jmap.Proto.Method.get_response) Jmap.Chain.handle Jmap.Chain.t) ->
  'r list
(** [get_objects client ~capabilities ~kind ~id ids get] is
    {!Jmap_eio.Sync.get_all} over [ids], reordered to match [ids]. [kind] names
    the objects in the warning printed for any id the server did not resolve,
    such as ["emails"]. [capabilities] defaults to {!mail_capabilities}.

    An id a query returned that the following [/get] does not know is a record
    destroyed between the two requests. It is reported with {!warn} rather than
    dropped, since the rows printed are then fewer than the ids asked for. It
    {!die}s on failure. *)

val get_emails :
  Jmap_eio.Client.t ->
  ?capabilities:string list ->
  account_id:Jmap.Proto.Id.t ->
  properties:Jmap.Proto.Email.property list ->
  Jmap.Proto.Id.t list ->
  Jmap.Proto.Email.t list
(** [get_emails client ~capabilities ~account_id ~properties ids] is
    {!get_objects} over an [Email/get] of [properties]. *)

val chunks : int -> 'a list -> 'a list list
(** [chunks size values] is [values] cut into consecutive lists of [size]
    elements, the last holding the remainder. An empty [values] is no chunks.

    @raise Invalid_argument if [size] is not positive. *)

(** {1 Batching a /set} *)

type set_labels = {
  succeeded : string;
      (** The past participle heading the successes, such as ["Deleted"]. *)
  action : string;
      (** The infinitive of the failure line, such as ["delete"] in
          ["Failed to delete"]. *)
  activity : string;
      (** The noun heading an interrupted run, such as ["Deletion"]. *)
}
(** The type for how a {!type-set_report} is worded. *)

type set_report = {
  changed : Jmap.Proto.Id.t list;  (** The ids the server acted on. *)
  refused : (Jmap.Proto.Id.t * Jmap.Proto.Error.Set_error.t) list;
      (** The ids the server refused, with why. *)
  unmentioned : Jmap.Proto.Id.t list;
      (** The ids of an answered call that the response named in neither list,
          whose outcome the server therefore did not report. *)
  interrupted : (Jmap.Proto.Id.t list * Jmap.Proto.Id.t list * string) option;
      (** The ids of the request that failed, whose outcome is unknown, the ids
          of the requests never sent, and why the run stopped. *)
}
(** The type for the outcome of a batched [/set]. *)

val run_set :
  Jmap_eio.Client.t ->
  ?capabilities:string list ->
  Jmap_eio.Cli.config ->
  outcome:
    ('r ->
    Jmap.Proto.Id.t list * (Jmap.Proto.Id.t * Jmap.Proto.Error.Set_error.t) list) ->
  Jmap.Proto.Id.t list ->
  (ids:Jmap.Proto.Id.t list -> ('s, 'r) Jmap.Chain.handle Jmap.Chain.t) ->
  set_report
(** [run_set client ~capabilities cfg ~outcome ids set] applies [set] to [ids]
    in as many calls as the server's [maxObjectsInSet] requires, and is what
    became of every id.

    [outcome result] is the ids one call changed paired with the ids it refused,
    which for a destroy is [destroyed] and [notDestroyed] and for an update is
    [updated] and [notUpdated]. [capabilities] defaults to {!mail_capabilities},
    and the request and response of each call are dumped through {!debug_json}
    under [cfg].

    The first call that fails stops the run, since a later batch acting on a
    server that just refused one is worth neither the request nor the report. It
    {!die}s when the server advertises a [maxObjectsInSet] of zero, as nothing
    can then be changed. *)

val report_set : set_labels -> set_report -> unit
(** [report_set labels report] prints [report] over emails, the successes on
    stdout and everything else on stderr, worded by [labels]. It exits with
    status 1 when any id was refused, was left unmentioned, or was in or after
    an interrupted request, and returns normally otherwise. *)
