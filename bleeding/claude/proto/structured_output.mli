(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Structured output configuration using JSON Schema.

    This module provides the wire format types for structured output support,
    allowing specification of expected output formats using JSON schemas. When a
    structured output format is configured, Claude will return its response in
    the specified JSON format, validated against the provided schema.

    This is the protocol-level module. For the high-level API with logging and
    additional features, see {!Claude.Structured_output}. *)

(** {1 Output Format Configuration} *)

type t
(** The type of structured output format configurations.

    This wraps a JSON Schema that specifies the expected output format. *)

val of_json_schema : Jsont.json -> t
(** [of_json_schema schema] creates an output format from a JSON Schema.

    The schema should be a valid JSON Schema Draft 7 as a {!type:Jsont.json}
    value.

    Example:
    {[
      let meta = Jsont.Meta.none in
      let schema =
        Jsont.Object
          ( [
              (("type", meta), Jsont.String ("object", meta));
              ( ("properties", meta),
                Jsont.Object
                  ( [
                      ( ("name", meta),
                        Jsont.Object
                          ([ (("type", meta), Jsont.String ("string", meta)) ], meta)
                      );
                      ( ("age", meta),
                        Jsont.Object
                          ([ (("type", meta), Jsont.String ("integer", meta)) ], meta)
                      );
                    ],
                    meta ) );
              ( ("required", meta),
                Jsont.Array
                  ([ Jsont.String ("name", meta); Jsont.String ("age", meta) ], meta)
              );
            ],
            meta )
      in

      let format = Structured_output.of_json_schema schema
    ]} *)

val to_json_schema : t -> Jsont.json
(** [to_json_schema t] extracts the JSON Schema from the output format. *)

val jsont : t Jsont.t
(** Codec for structured output format.

    Encodes/decodes the structured output configuration to/from the wire format
    JSON representation used by the Claude CLI protocol. *)
