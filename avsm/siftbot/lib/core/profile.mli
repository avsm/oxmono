type t = {
  name : string;
  jmap_profile : string;
  account : string option;
  sortal_roots : string list;
  body_limit : int;
  max_steps : int;
  chat_model : string option;
  embedding_url : string option;
  embedding_model : string option;
  embedding_dimensions : int;
}
(** XDG profile configuration. JMAP credentials remain in the shared store. *)

val init : name:string -> jmap_profile:string -> unit
val load : string -> t
val data_path : string -> string

val connect : sw:Eio.Switch.t -> Eio_unix.Stdenv.base -> t -> Import.remote
(** [connect ~sw env profile] opens the restricted reader from a shared bearer
    profile. No secret enters domain values or worker messages. *)

val defaults : name:string -> jmap_profile:string -> t
(** [defaults ~name ~jmap_profile] supplies the initial local settings. *)

val exists : string -> bool

val names : unit -> string list
(** [names ()] lists local profile names in bytewise order. *)

val save : t -> unit
(** [save profile] atomically replaces the local configuration, preserving
    credentials in the separate shared JMAP store. *)
