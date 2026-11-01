(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  method_responses : Proto_invocation.t list;
  created_ids : (Proto_id.t * Proto_id.t) list option;
  session_state : string;
  source : string option;
}

let make method_responses created_ids session_state =
  { method_responses; created_ids; session_state; source = None }

let jsont =
  let kind = "Response" in
  Jsont.Object.map ~kind make
  |> Jsont.Object.mem "methodResponses" (Jsont.list Proto_invocation.jsont)
       ~enc:(fun r -> r.method_responses)
  |> Jsont.Object.opt_mem "createdIds" (Proto_json_map.of_id Proto_id.jsont)
       ~enc:(fun r -> r.created_ids)
  |> Jsont.Object.mem "sessionState" Jsont.string ~enc:(fun r ->
      r.session_state)
  |> Jsont.Object.finish

let media =
  Proto_json.media
    ~with_source:(fun ~source response -> { response with source = Some source })
    jsont

let source r = r.source

let source_fragment r meta =
  Option.bind r.source (fun text ->
      let loc = Jsont.Meta.textloc meta in
      let first = Jsont.Textloc.first_byte loc in
      let last = Jsont.Textloc.last_byte loc in
      if first < 0 || last < first || last >= String.length text then None
      else Some (String.sub text first (last - first + 1)))

let is_for method_call_id inv =
  String.equal inv.Proto_invocation.method_call_id method_call_id

let find_response method_call_id response =
  List.find_opt (is_for method_call_id) response.method_responses

let find_responses method_call_id response =
  List.filter (is_for method_call_id) response.method_responses

let get_response method_call_id response =
  List.find (is_for method_call_id) response.method_responses

let is_error (invocation : Proto_invocation.t) =
  String.equal invocation.Proto_invocation.name "error"

let error (invocation : Proto_invocation.t) =
  if is_error invocation then
    Some
      (Jsont.Json.decode' Proto_error.Method_error.jsont
         invocation.Proto_invocation.arguments)
  else None

let pp ppf r = Proto_json.pp jsont ppf r
