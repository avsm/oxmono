(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Loader - converts parser events to YAML data structures *)

(** {1 String-based loading} *)

val value_of_string :
  ?resolve_aliases:bool -> ?max_nodes:int -> ?max_depth:int -> string -> Value.t
(** Load single document as Value.

    @param resolve_aliases Whether to resolve aliases (default true)
    @param max_nodes Maximum nodes during alias expansion (default 10M)
    @param max_depth Maximum alias nesting depth (default 100) *)

val yaml_of_string :
  ?resolve_aliases:bool -> ?max_nodes:int -> ?max_depth:int -> string -> Yaml.t
(** Load single document as Yaml.

    @param resolve_aliases Whether to resolve aliases (default false)
    @param max_nodes Maximum nodes during alias expansion (default 10M)
    @param max_depth Maximum alias nesting depth (default 100) *)

val documents_of_string : string -> Document.t list
(** Load all documents from a string *)

(** {1 Reader-based loading} *)

val value_of_reader :
  ?resolve_aliases:bool ->
  ?max_nodes:int ->
  ?max_depth:int ->
  Bytesrw.Bytes.Reader.t ->
  Value.t
(** Load single document as Value from a Bytes.Reader *)

val yaml_of_reader :
  ?resolve_aliases:bool ->
  ?max_nodes:int ->
  ?max_depth:int ->
  Bytesrw.Bytes.Reader.t ->
  Yaml.t
(** Load single document as Yaml from a Bytes.Reader *)

val documents_of_reader : Bytesrw.Bytes.Reader.t -> Document.t list
(** Load all documents from a Bytes.Reader *)

(** {1 Parser-based loading} *)

val load_value :
  ?resolve_aliases:bool ->
  ?max_nodes:int ->
  ?max_depth:int ->
  Parser.t ->
  Value.t option
(** Load single Value from parser *)

val load_yaml : Parser.t -> Yaml.t option
(** Load single Yaml from parser *)

val load_document : Parser.t -> Document.t option
(** Load single Document from parser *)

val iter_documents : (Document.t -> unit) -> Parser.t -> unit
(** Iterate over documents from parser *)

val fold_documents : ('a -> Document.t -> 'a) -> 'a -> Parser.t -> 'a
(** Fold over documents from parser *)

(** {1 Event function-based loading}

    These functions accept a [unit -> Event.spanned option] function instead of
    a [Parser.t], allowing them to work with any event source. *)

val value_of_parser :
  ?resolve_aliases:bool ->
  ?max_nodes:int ->
  ?max_depth:int ->
  (unit -> Event.spanned option) ->
  Value.t
(** Load single Value from event source function *)

val yaml_of_parser :
  ?resolve_aliases:bool ->
  ?max_nodes:int ->
  ?max_depth:int ->
  (unit -> Event.spanned option) ->
  Yaml.t
(** Load single Yaml from event source function *)

val document_of_parser : (unit -> Event.spanned option) -> Document.t option
(** Load single Document from event source function *)

val documents_of_parser : (unit -> Event.spanned option) -> Document.t list
(** Load all documents from event source function *)

val iter_documents_parser :
  (Document.t -> unit) -> (unit -> Event.spanned option) -> unit
(** Iterate over documents from event source function *)

val fold_documents_parser :
  ('a -> Document.t -> 'a) -> 'a -> (unit -> Event.spanned option) -> 'a
(** Fold over documents from event source function *)
