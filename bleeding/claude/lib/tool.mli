(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Custom tool definitions for MCP servers.

    Tools are functions that Claude can invoke. They run in-process within your
    OCaml application via the MCP (Model Context Protocol).

    {2 Basic Usage}

    {[
      let greet =
        Tool.create ~name:"greet" ~description:"Greet a user by name"
          ~input_schema:
            (`O
               [
                 ("type", `String "object");
                 ( "properties",
                   `O [ ("name", `O [ ("type", `String "string") ]) ] );
                 ("required", `A [ `String "name" ]);
               ])
          ~handler:(fun args ->
            match Tool_input.get_string args "name" with
            | Some name ->
                Ok
                  (`A
                     [
                       `O
                         [
                           ("type", `String "text");
                           ("text", `String (Printf.sprintf "Hello, %s!" name));
                         ];
                     ])
            | None -> Error "Missing 'name' parameter")
    ]}

    {2 Tool Response Format}

    Tool handlers return MCP-compatible content:
    - Success: [Ok content] where content is JSON array of content blocks
    - Error: [Error message] for error responses

    Content blocks are typically:
    {[
      `A [ `O [ ("type", `String "text"); ("text", `String "result") ] ]
    ]} *)

type t
(** Abstract type for tool definitions. *)

val create :
  name:string ->
  description:string ->
  input_schema:Jsont.json ->
  handler:(Tool_input.t -> (Jsont.json, string) result) ->
  t
(** [create ~name ~description ~input_schema ~handler] creates a custom tool.

    @param name
      Unique tool identifier. Claude uses this in function calls. When
      registered with an MCP server named "foo", the full tool name becomes
      [mcp__foo__<name>].
    @param description
      Human-readable description. Helps Claude understand when to use the tool.
    @param input_schema
      JSON Schema defining input parameters. Should be a valid JSON Schema
      object with "type", "properties", etc.
    @param handler
      Function that executes the tool. Receives tool input, returns content
      array or error message. *)

val name : t -> string
(** [name t] returns the tool's name. *)

val description : t -> string
(** [description t] returns the tool's description. *)

val input_schema : t -> Jsont.json
(** [input_schema t] returns the JSON Schema for inputs. *)

val call : t -> Tool_input.t -> (Jsont.json, string) result
(** [call t input] invokes the tool handler with the given input. *)

(** {1 Convenience Constructors}

    Helper functions for common tool patterns. *)

val text_result : string -> Jsont.json
(** [text_result s] creates a text content result:
    [\`A [\`O ["type", \`String "text"; "text", \`String s]]] *)

val error_result : string -> Jsont.json
(** [error_result s] creates an error content result with is_error flag. *)

(** {2 Schema Helpers}

    Build JSON Schema objects more easily. *)

val schema_object :
  (string * Jsont.json) list -> required:string list -> Jsont.json
(** [schema_object props ~required] creates an object schema.
    {[
      schema_object
        [ ("name", schema_string); ("age", schema_int) ]
        ~required:[ "name" ]
    ]} *)

val schema_string : Jsont.json
(** String type schema: [{"type": "string"}] *)

val schema_int : Jsont.json
(** Integer type schema: [{"type": "integer"}] *)

val schema_number : Jsont.json
(** Number type schema: [{"type": "number"}] *)

val schema_bool : Jsont.json
(** Boolean type schema: [{"type": "boolean"}] *)

val schema_array : Jsont.json -> Jsont.json
(** [schema_array item_schema] creates array schema with given item type. *)

val schema_string_enum : string list -> Jsont.json
(** [schema_string_enum values] creates enum schema for string values. *)
