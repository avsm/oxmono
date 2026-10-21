(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A typed tool: a Dsml invoke codec (schema + decoder) plus a handler over the
   decoded OCaml value. The codec hides the JSON layer from the handler. *)

module Json = Dsml.Json

type result = { text : string; images : string list }

type t = {
  name : string;
  description : string;
  schema : Json.t;
  invoke : Dsml.tool_call -> result;
}

let text text = { text; images = [] }

let v_result ~description codec handler =
  {
    name = Dsml.Codec.name codec;
    description;
    schema = Dsml.Codec.schema codec;
    invoke =
      (fun call ->
        match Dsml.Codec.decode_arguments codec call.arguments with
        | Ok args -> (
            (* A tool that fails reports it and the turn continues. Resource
               exhaustion is not a tool failure, though, and describing it to
               the model as one would hide it while the program is already in
               trouble. *)
            try handler args with
            | (Eio.Cancel.Cancelled _ | Out_of_memory | Stack_overflow) as e ->
                raise e
            | e -> text ("Error: " ^ Printexc.to_string e))
        | Error msg -> text ("Error: " ^ msg));
  }

let v ~description codec handler =
  v_result ~description codec (fun args -> text (handler args))

let raw ~name ~description ~schema handler =
  if name = "" then invalid_arg "Ds4.Tool.raw: empty name";
  { name; description; schema; invoke = fun call -> text (handler call) }

let name t = t.name
let description t = t.description
let schema t = t.schema
let invoke_result t call = t.invoke call
let invoke t call = (invoke_result t call).text

let pp_result ppf result =
  Format.fprintf ppf "@[<hov 2>{ text = %S;@ images = %d }@]" result.text
    (List.length result.images)

let pp ppf t =
  Format.fprintf ppf "@[<hov 2>{ name = %S;@ description = %S;@ schema = %s }@]"
    t.name t.description
    (Dsml.Json.Value.to_string t.schema)
