(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Structured output configuration using JSON Schema.

    This module provides structured output support for Claude, allowing you to
    specify the expected output format using JSON schemas. When a structured
    output format is configured, Claude will return its response in the
    specified JSON format, validated against your schema.

    {2 Overview}

    Structured outputs ensure that Claude's responses conform to a specific JSON
    schema, making it easier to parse and use the results programmatically. This
    is particularly useful for:

    - Extracting structured data from unstructured text
    - Building APIs that require consistent JSON responses
    - Integrating Claude into data pipelines
    - Ensuring type-safe parsing of Claude's outputs

    {2 Creating Output Formats}

    Use {!of_json_schema} to specify a JSON Schema as a {!type:Jsont.json}
    value:
    {[
      let meta = Jsont.Meta.none in
      let schema = Jsont.Object ([
        (("type", meta), Jsont.String ("object", meta));
        (("properties", meta), Jsont.Object ([
          (("name", meta), Jsont.Object ([
            (("type", meta), Jsont.String ("string", meta))
          ], meta));
          (("age", meta), Jsont.Object ([
            (("type", meta), Jsont.String ("integer", meta))
          ], meta));
        ], meta));
        (("required", meta), Jsont.Array ([
          Jsont.String ("name", meta);
          Jsont.String ("age", meta)
        ], meta));
      ], meta) in

      let format = Structured_output.of_json_schema schema
    ]}

    {3 Helper Functions for Building Schemas}

    For complex schemas, you can use helper functions to make construction
    easier:
    {[
      let json_object fields = Jsont.Object (fields, Jsont.Meta.none)
      let json_string s = Jsont.String (s, Jsont.Meta.none)
      let json_array items = Jsont.Array (items, Jsont.Meta.none)
      let json_field name value = ((name, Jsont.Meta.none), value)

      let person_schema =
        json_object
          [
            json_field "type" (json_string "object");
            json_field "properties"
              (json_object
                 [
                   json_field "name"
                     (json_object [ json_field "type" (json_string "string") ]);
                   json_field "age"
                     (json_object [ json_field "type" (json_string "integer") ]);
                 ]);
            json_field "required"
              (json_array [ json_string "name"; json_string "age" ]);
          ]

      let format = Structured_output.of_json_schema person_schema
    ]}

    {2 Usage with Claude Client}

    {[
      let options = Options.default
        |> Options.with_output_format format

      let client = Client.create ~sw ~process_mgr ~options () in
      Client.query client "Extract person info from: John is 30 years old";

      let messages = Client.receive_all client in
      List.iter (function
        | Message.Result result ->
            (match Message.Result.structured_output result with
            | Some json -> (* Process validated JSON *)
                let json_str = match Jsont_bytesrw.encode_string' Jsont.json json with
                  | Ok s -> s
                  | Error err -> Jsont.Error.to_string err
                in
                Printf.printf "Structured output: %s\n" json_str
            | None -> ())
        | _ -> ()
      ) messages
    ]}

    {2 JSON Schema Support}

    The module supports standard JSON Schema Draft 7, including:
    - Primitive types (string, integer, number, boolean, null)
    - Objects with properties and required fields
    - Arrays with item schemas
    - Enumerations
    - Nested objects and arrays
    - Complex validation rules

    @see <https://json-schema.org/> JSON Schema specification
    @see <https://erratique.ch/software/jsont> jsont documentation *)

val src : Logs.Src.t
(** The log source for structured output operations *)

(** {1 Output Format Configuration} *)

type t
(** The type of structured output format configurations. *)

val of_json_schema : Jsont.json -> t
(** [of_json_schema schema] creates an output format from a JSON Schema.

    The schema should be a valid JSON Schema Draft 7 as a {!type:Jsont.json}
    value.

    Example:
    {[
      let meta = Jsont.Meta.none in
      let schema = Jsont.Object ([
        (("type", meta), Jsont.String ("object", meta));
        (("properties", meta), Jsont.Object ([
          (("name", meta), Jsont.Object ([
            (("type", meta), Jsont.String ("string", meta))
          ], meta));
          (("age", meta), Jsont.Object ([
            (("type", meta), Jsont.String ("integer", meta))
          ], meta));
        ], meta));
        (("required", meta), Jsont.Array ([
          Jsont.String ("name", meta);
          Jsont.String ("age", meta)
        ], meta));
      ], meta) in

      let format = Structured_output.of_json_schema schema
    ]} *)

val json_schema : t -> Jsont.json
(** [json_schema t] returns the JSON Schema. *)

val jsont : t Jsont.t
(** Codec for structured output format. *)

(** {1 Serialization}

    Internal use for encoding/decoding with the CLI. *)

val to_json : t -> Jsont.json
(** [to_json t] converts the output format to its JSON representation. Internal
    use only. *)

val of_json : Jsont.json -> t
(** [of_json json] parses an output format from JSON. Internal use only.
    @raise Invalid_argument if the JSON is not a valid output format. *)
