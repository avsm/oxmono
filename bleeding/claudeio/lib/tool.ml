(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module J = Jsont.Json

type t = {
  name : string;
  description : string;
  input_schema : Jsont.json;
  handler : Tool_input.t -> (Jsont.json, string) result;
}

let create ~name ~description ~input_schema ~handler =
  { name; description; input_schema; handler }

let name t = t.name
let description t = t.description
let input_schema t = t.input_schema
let call t input = t.handler input

(* Convenience constructors using Jsont.Json builders *)

let text_result s =
  J.list
    [
      J.object'
        [
          J.mem (J.name "type") (J.string "text");
          J.mem (J.name "text") (J.string s);
        ];
    ]

let error_result s =
  J.list
    [
      J.object'
        [
          J.mem (J.name "type") (J.string "text");
          J.mem (J.name "text") (J.string s);
          J.mem (J.name "is_error") (J.bool true);
        ];
    ]

(* Schema helpers *)

let schema_string = J.object' [ J.mem (J.name "type") (J.string "string") ]
let schema_int = J.object' [ J.mem (J.name "type") (J.string "integer") ]
let schema_number = J.object' [ J.mem (J.name "type") (J.string "number") ]
let schema_bool = J.object' [ J.mem (J.name "type") (J.string "boolean") ]

let schema_array item_schema =
  J.object'
    [
      J.mem (J.name "type") (J.string "array");
      J.mem (J.name "items") item_schema;
    ]

let schema_string_enum values =
  J.object'
    [
      J.mem (J.name "type") (J.string "string");
      J.mem (J.name "enum") (J.list (List.map J.string values));
    ]

let schema_object props ~required =
  J.object'
    [
      J.mem (J.name "type") (J.string "object");
      J.mem (J.name "properties")
        (J.object' (List.map (fun (k, v) -> J.mem (J.name k) v) props));
      J.mem (J.name "required") (J.list (List.map J.string required));
    ]
