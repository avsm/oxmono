module Response = Bushel_knowledge_store.Response

type model = {
  name : string;
  repo : string;
  revision : string;
  dimensions : int;
  size_bytes : int64;
  cached : bool;
  model_dir : string;
  is_default : bool;
}

type response =
  | Error of string
  | Configured of string
  | Models of { default : string; models : model list }
  | Status of Response.status
  | Result of Response.t

val json : response -> Jsont.json
val encode : response -> string

val print_value : ?elapsed:float -> command:string -> response -> unit
(** [print_value ~command response] renders a typed service response. *)

val print : ?elapsed:float -> command:string -> string -> unit
(** [print ~command response] strictly decodes a JSON status, search, source or
    contact response before rendering it. Configuration and model lists use
    [print_value]. Errors go to stderr. Terminal controls are sanitized.
    Malformed data and unsupported commands raise [Failure]. *)
