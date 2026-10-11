(** Dedicated local setup channel. Secrets are accepted only in [Save], never
    returned in responses or sent to the review worker. *)

type profile = {
  name : string;
  jmap_profile : string;
  url : string;
  account : string;
  chat_model : string;
  has_token : bool;
}
[@@deriving sexp]

(** [Save (profile, token, fresh)] creates a profile when [fresh] is true,
    otherwise edits an existing one. An empty token retains the shared token
    only when the server URL is unchanged. Never log serialized requests. *)
type request = List | Save of profile * string * bool [@@deriving sexp]

type response = Profiles of profile list | Saved of profile | Error of string
[@@deriving sexp]

val request : request -> string
val response : response -> string
val parse_request : string -> (request, string) result
val parse_response : string -> response

val read : in_channel -> string
(** [read ic] reads one setup message, bounded to 64 KiB. *)
