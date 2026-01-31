(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module J = Jsont.Json

type t = {
  name : string;
  version : string;
  tools : Tool.t list;
  tool_map : (string, Tool.t) Hashtbl.t;
}

let create ~name ?(version = "1.0.0") ~tools () =
  let tool_map = Hashtbl.create (List.length tools) in
  List.iter (fun tool -> Hashtbl.add tool_map (Tool.name tool) tool) tools;
  { name; version; tools; tool_map }

let name t = t.name
let version t = t.version
let tools t = t.tools

(* JSONRPC helpers using Jsont.Json builders *)

let jsonrpc_success ~id result =
  J.object'
    [
      J.mem (J.name "jsonrpc") (J.string "2.0");
      J.mem (J.name "id") id;
      J.mem (J.name "result") result;
    ]

let jsonrpc_error ~id ~code ~message =
  J.object'
    [
      J.mem (J.name "jsonrpc") (J.string "2.0");
      J.mem (J.name "id") id;
      J.mem (J.name "error")
        (J.object'
           [
             J.mem (J.name "code") (J.number (Float.of_int code));
             J.mem (J.name "message") (J.string message);
           ]);
    ]

(* Extract string from JSON *)
let get_string key (obj : Jsont.json) =
  match obj with
  | Jsont.Object (mems, _) -> (
      match J.find_mem key mems with
      | Some (_, Jsont.String (s, _)) -> Some s
      | _ -> None)
  | _ -> None

(* Extract object from JSON *)
let get_object key (obj : Jsont.json) : Jsont.json option =
  match obj with
  | Jsont.Object (mems, _) -> (
      match J.find_mem key mems with
      | Some (_, (Jsont.Object _ as o)) -> Some o
      | _ -> None)
  | _ -> None

(* Get ID from JSON message *)
let get_id (msg : Jsont.json) : Jsont.json =
  match msg with
  | Jsont.Object (mems, _) -> (
      match J.find_mem "id" mems with Some (_, id) -> id | None -> J.null ())
  | _ -> J.null ()

(* Handle initialize request *)
let handle_initialize t ~id =
  jsonrpc_success ~id
    (J.object'
       [
         J.mem (J.name "protocolVersion") (J.string "2024-11-05");
         J.mem (J.name "capabilities")
           (J.object' [ J.mem (J.name "tools") (J.object' []) ]);
         J.mem (J.name "serverInfo")
           (J.object'
              [
                J.mem (J.name "name") (J.string t.name);
                J.mem (J.name "version") (J.string t.version);
              ]);
       ])

(* Handle tools/list request *)
let handle_tools_list t ~id =
  let tools_json =
    List.map
      (fun tool ->
        J.object'
          [
            J.mem (J.name "name") (J.string (Tool.name tool));
            J.mem (J.name "description") (J.string (Tool.description tool));
            J.mem (J.name "inputSchema") (Tool.input_schema tool);
          ])
      t.tools
  in
  jsonrpc_success ~id (J.object' [ J.mem (J.name "tools") (J.list tools_json) ])

(* Handle tools/call request *)
let handle_tools_call t ~id ~params =
  match get_string "name" params with
  | None -> jsonrpc_error ~id ~code:(-32602) ~message:"Missing 'name' parameter"
  | Some tool_name -> (
      match Hashtbl.find_opt t.tool_map tool_name with
      | None ->
          jsonrpc_error ~id ~code:(-32601)
            ~message:(Printf.sprintf "Tool '%s' not found" tool_name)
      | Some tool -> (
          let arguments =
            match get_object "arguments" params with
            | Some args -> args
            | None -> J.object' []
          in
          let input = Tool_input.of_json arguments in
          match Tool.call tool input with
          | Ok content ->
              jsonrpc_success ~id
                (J.object' [ J.mem (J.name "content") content ])
          | Error msg ->
              (* Return error as content with is_error flag *)
              jsonrpc_success ~id
                (J.object'
                   [
                     J.mem (J.name "content")
                       (J.list
                          [
                            J.object'
                              [
                                J.mem (J.name "type") (J.string "text");
                                J.mem (J.name "text") (J.string msg);
                              ];
                          ]);
                     J.mem (J.name "isError") (J.bool true);
                   ])))

let handle_request t ~method_ ~params ~id =
  match method_ with
  | "initialize" -> handle_initialize t ~id
  | "tools/list" -> handle_tools_list t ~id
  | "tools/call" -> handle_tools_call t ~id ~params
  | _ ->
      jsonrpc_error ~id ~code:(-32601)
        ~message:(Printf.sprintf "Method '%s' not found" method_)

let handle_json_message t (msg : Jsont.json) =
  let method_ = match get_string "method" msg with Some m -> m | None -> "" in
  let params =
    match get_object "params" msg with Some p -> p | None -> J.object' []
  in
  let id = get_id msg in
  handle_request t ~method_ ~params ~id
