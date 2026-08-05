(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Yamlrw Unix - Channel and file I/O for YAML

    This module provides channel and file operations for parsing and emitting
    YAML using bytesrw for efficient streaming I/O. *)

open Bytesrw
open Yamlrw

(** {1 Types} *)

type value = Value.t
type yaml = Yaml.t
type document = Document.t

(** {1 Channel Input} *)

let value_of_channel ?(resolve_aliases = true)
    ?(max_nodes = Yaml.default_max_alias_nodes)
    ?(max_depth = Yaml.default_max_alias_depth) ic =
  let reader = Bytes.Reader.of_in_channel ic in
  Loader.value_of_reader ~resolve_aliases ~max_nodes ~max_depth reader

let yaml_of_channel ?(resolve_aliases = false)
    ?(max_nodes = Yaml.default_max_alias_nodes)
    ?(max_depth = Yaml.default_max_alias_depth) ic =
  let reader = Bytes.Reader.of_in_channel ic in
  Loader.yaml_of_reader ~resolve_aliases ~max_nodes ~max_depth reader

let documents_of_channel ic =
  let reader = Bytes.Reader.of_in_channel ic in
  Loader.documents_of_reader reader

(** {1 Channel Output} *)

let value_to_channel ?(encoding = `Utf8) ?(scalar_style = `Any)
    ?(layout_style = `Any) oc (v : value) =
  let config =
    { Emitter.default_config with encoding; scalar_style; layout_style }
  in
  let writer = Bytes.Writer.of_out_channel oc in
  Serialize.value_to_writer ~config writer v

let yaml_to_channel ?(encoding = `Utf8) ?(scalar_style = `Any)
    ?(layout_style = `Any) oc (v : yaml) =
  let config =
    { Emitter.default_config with encoding; scalar_style; layout_style }
  in
  let writer = Bytes.Writer.of_out_channel oc in
  Serialize.yaml_to_writer ~config writer v

let documents_to_channel ?(encoding = `Utf8) ?(scalar_style = `Any)
    ?(layout_style = `Any) ?(resolve_aliases = true) oc docs =
  let config =
    { Emitter.default_config with encoding; scalar_style; layout_style }
  in
  let writer = Bytes.Writer.of_out_channel oc in
  Serialize.documents_to_writer ~config ~resolve_aliases writer docs

(** {1 File Input} *)

let value_of_file ?(resolve_aliases = true)
    ?(max_nodes = Yaml.default_max_alias_nodes)
    ?(max_depth = Yaml.default_max_alias_depth) path =
  In_channel.with_open_bin path (fun ic ->
      value_of_channel ~resolve_aliases ~max_nodes ~max_depth ic)

let yaml_of_file ?(resolve_aliases = false)
    ?(max_nodes = Yaml.default_max_alias_nodes)
    ?(max_depth = Yaml.default_max_alias_depth) path =
  In_channel.with_open_bin path (fun ic ->
      yaml_of_channel ~resolve_aliases ~max_nodes ~max_depth ic)

let documents_of_file path = In_channel.with_open_bin path documents_of_channel

(** {1 File Output} *)

let value_to_file ?(encoding = `Utf8) ?(scalar_style = `Any)
    ?(layout_style = `Any) path v =
  Out_channel.with_open_bin path (fun oc ->
      value_to_channel ~encoding ~scalar_style ~layout_style oc v)

let yaml_to_file ?(encoding = `Utf8) ?(scalar_style = `Any)
    ?(layout_style = `Any) path v =
  Out_channel.with_open_bin path (fun oc ->
      yaml_to_channel ~encoding ~scalar_style ~layout_style oc v)

let documents_to_file ?(encoding = `Utf8) ?(scalar_style = `Any)
    ?(layout_style = `Any) ?(resolve_aliases = true) path docs =
  Out_channel.with_open_bin path (fun oc ->
      documents_to_channel ~encoding ~scalar_style ~layout_style
        ~resolve_aliases oc docs)
