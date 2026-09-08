(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type result_reference = {
  result_of : string;
  name : string;
  path : Json_pointer.t;
}

let result_reference ~result_of ~name ~path = { result_of; name; path }

let result_reference_of_strings ~result_of ~name ~path =
  match Json_pointer.of_string path with
  | path -> Ok { result_of; name; path }
  | exception Jsont.Error e -> Error e

let result_reference_make result_of name path = { result_of; name; path }

let result_reference_jsont =
  let kind = "ResultReference" in
  Jsont.Object.map ~kind result_reference_make
  |> Jsont.Object.mem "resultOf" Jsont.string ~enc:(fun r -> r.result_of)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "path" Json_pointer.jsont ~enc:(fun r -> r.path)
  |> Jsont.Object.finish

type t = { name : string; arguments : Jsont.json; method_call_id : string }

let create ~name ~arguments ~method_call_id =
  { name; arguments; method_call_id }

let jsont =
  let kind = "Invocation" in
  let dec json =
    match json with
    | Jsont.Array ([ name_json; arguments; call_id_json ], _) ->
        let name =
          match name_json with
          | Jsont.String (s, _) -> s
          | j ->
              Jsont.Error.msg (Jsont.Json.meta j)
                "Invocation[0] must be a string"
        in
        (* RFC 8620 Section 3.2: "A String[*] object containing named
           arguments". *)
        (match arguments with
        | Jsont.Object _ -> ()
        | j ->
            Jsont.Error.msg (Jsont.Json.meta j)
              "Invocation[1] must be an object");
        let method_call_id =
          match call_id_json with
          | Jsont.String (s, _) -> s
          | j ->
              Jsont.Error.msg (Jsont.Json.meta j)
                "Invocation[2] must be a string"
        in
        { name; arguments; method_call_id }
    | Jsont.Array (_, meta) ->
        Jsont.Error.msg meta "Invocation must be a 3-element array"
    | j -> Jsont.Error.msg (Jsont.Json.meta j) "Invocation must be an array"
  in
  let enc t =
    Jsont.Array
      ( [
          Jsont.String (t.name, Jsont.Meta.none);
          t.arguments;
          Jsont.String (t.method_call_id, Jsont.Meta.none);
        ],
        Jsont.Meta.none )
  in
  Jsont.map ~kind ~dec ~enc Jsont.json
