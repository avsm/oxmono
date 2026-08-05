(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Yamlrw Unix - Channel and file I/O for YAML

    This module provides channel and file operations for parsing and emitting
    YAML using bytesrw for efficient streaming I/O. *)

(** {1 Types} *)

type value = Yamlrw.Value.t
type yaml = Yamlrw.Yaml.t
type document = Yamlrw.Document.t

(** {1 Channel Input} *)

val value_of_channel :
  ?resolve_aliases:bool ->
  ?max_nodes:int ->
  ?max_depth:int ->
  in_channel ->
  value
(** Parse a JSON-compatible value from an input channel.

    @param resolve_aliases Whether to expand aliases (default: true)
    @param max_nodes Maximum nodes during alias expansion (default: 10M)
    @param max_depth Maximum alias nesting depth (default: 100) *)

val yaml_of_channel :
  ?resolve_aliases:bool ->
  ?max_nodes:int ->
  ?max_depth:int ->
  in_channel ->
  yaml
(** Parse a full YAML value from an input channel.

    @param resolve_aliases Whether to expand aliases (default: false)
    @param max_nodes Maximum nodes during alias expansion (default: 10M)
    @param max_depth Maximum alias nesting depth (default: 100) *)

val documents_of_channel : in_channel -> document list
(** Parse multiple YAML documents from an input channel. *)

(** {1 Channel Output} *)

val value_to_channel :
  ?encoding:Yamlrw.Encoding.t ->
  ?scalar_style:Yamlrw.Scalar_style.t ->
  ?layout_style:Yamlrw.Layout_style.t ->
  out_channel ->
  value ->
  unit
(** Write a JSON-compatible value to an output channel. *)

val yaml_to_channel :
  ?encoding:Yamlrw.Encoding.t ->
  ?scalar_style:Yamlrw.Scalar_style.t ->
  ?layout_style:Yamlrw.Layout_style.t ->
  out_channel ->
  yaml ->
  unit
(** Write a full YAML value to an output channel. *)

val documents_to_channel :
  ?encoding:Yamlrw.Encoding.t ->
  ?scalar_style:Yamlrw.Scalar_style.t ->
  ?layout_style:Yamlrw.Layout_style.t ->
  ?resolve_aliases:bool ->
  out_channel ->
  document list ->
  unit
(** Write multiple YAML documents to an output channel. *)

(** {1 File Input} *)

val value_of_file :
  ?resolve_aliases:bool -> ?max_nodes:int -> ?max_depth:int -> string -> value
(** Parse a JSON-compatible value from a file. *)

val yaml_of_file :
  ?resolve_aliases:bool -> ?max_nodes:int -> ?max_depth:int -> string -> yaml
(** Parse a full YAML value from a file. *)

val documents_of_file : string -> document list
(** Parse multiple YAML documents from a file. *)

(** {1 File Output} *)

val value_to_file :
  ?encoding:Yamlrw.Encoding.t ->
  ?scalar_style:Yamlrw.Scalar_style.t ->
  ?layout_style:Yamlrw.Layout_style.t ->
  string ->
  value ->
  unit
(** Write a JSON-compatible value to a file. *)

val yaml_to_file :
  ?encoding:Yamlrw.Encoding.t ->
  ?scalar_style:Yamlrw.Scalar_style.t ->
  ?layout_style:Yamlrw.Layout_style.t ->
  string ->
  yaml ->
  unit
(** Write a full YAML value to a file. *)

val documents_to_file :
  ?encoding:Yamlrw.Encoding.t ->
  ?scalar_style:Yamlrw.Scalar_style.t ->
  ?layout_style:Yamlrw.Layout_style.t ->
  ?resolve_aliases:bool ->
  string ->
  document list ->
  unit
(** Write multiple YAML documents to a file. *)
