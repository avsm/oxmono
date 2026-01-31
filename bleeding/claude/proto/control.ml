(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Control protocol wire format for SDK communication. *)

module Request = struct
  (* Individual record types for each request variant - private to this module *)
  type permission_r = {
    tool_name : string;
    input : Jsont.json;
    permission_suggestions : Permissions.Update.t list option;
    blocked_path : string option;
    unknown : Unknown.t;
  }

  type initialize_r = {
    hooks : (string * Jsont.json) list option;
    unknown : Unknown.t;
  }

  type set_permission_mode_r = {
    mode : Permissions.Mode.t;
    unknown : Unknown.t;
  }

  type hook_callback_r = {
    callback_id : string;
    input : Jsont.json;
    tool_use_id : string option;
    unknown : Unknown.t;
  }

  type mcp_message_r = {
    server_name : string;
    message : Jsont.json;
    unknown : Unknown.t;
  }

  type set_model_r = { model : string; unknown : Unknown.t }

  type t =
    | Interrupt
    | Permission of permission_r
    | Initialize of initialize_r
    | Set_permission_mode of set_permission_mode_r
    | Hook_callback of hook_callback_r
    | Mcp_message of mcp_message_r
    | Set_model of set_model_r
    | Get_server_info

  let interrupt () = Interrupt

  let permission ~tool_name ~input ?permission_suggestions ?blocked_path () =
    Permission
      {
        tool_name;
        input;
        permission_suggestions;
        blocked_path;
        unknown = Unknown.empty;
      }

  let initialize ?hooks () = Initialize { hooks; unknown = Unknown.empty }

  let set_permission_mode ~mode () =
    Set_permission_mode { mode; unknown = Unknown.empty }

  let hook_callback ~callback_id ~input ?tool_use_id () =
    Hook_callback { callback_id; input; tool_use_id; unknown = Unknown.empty }

  let mcp_message ~server_name ~message () =
    Mcp_message { server_name; message; unknown = Unknown.empty }

  let set_model ~model () = Set_model { model; unknown = Unknown.empty }
  let get_server_info () = Get_server_info

  (* Individual record codecs *)
  let interrupt_jsont : unit Jsont.t =
    Jsont.Object.map ~kind:"Interrupt" (fun _unknown -> ())
    |> Jsont.Object.keep_unknown Unknown.mems ~enc:(fun () -> Unknown.empty)
    |> Jsont.Object.finish

  let permission_jsont : permission_r Jsont.t =
    let make tool_name input permission_suggestions blocked_path unknown :
        permission_r =
      { tool_name; input; permission_suggestions; blocked_path; unknown }
    in
    Jsont.Object.map ~kind:"Permission" make
    |> Jsont.Object.mem "toolName" Jsont.string ~enc:(fun (r : permission_r) ->
        r.tool_name)
    |> Jsont.Object.mem "input" Jsont.json ~enc:(fun (r : permission_r) ->
        r.input)
    |> Jsont.Object.opt_mem "permissionSuggestions"
         (Jsont.list Permissions.Update.jsont) ~enc:(fun (r : permission_r) ->
           r.permission_suggestions)
    |> Jsont.Object.opt_mem "blockedPath" Jsont.string
         ~enc:(fun (r : permission_r) -> r.blocked_path)
    |> Jsont.Object.keep_unknown Unknown.mems ~enc:(fun (r : permission_r) ->
        r.unknown)
    |> Jsont.Object.finish

  let initialize_jsont : initialize_r Jsont.t =
    (* The hooks field is an object with string keys and json values *)
    let hooks_map_jsont = Jsont.Object.as_string_map Jsont.json in
    let module StringMap = Map.Make (String) in
    let hooks_jsont =
      Jsont.map
        ~dec:(fun m -> StringMap.bindings m)
        ~enc:(fun l -> StringMap.of_seq (List.to_seq l))
        hooks_map_jsont
    in
    let make hooks unknown = { hooks; unknown } in
    Jsont.Object.map ~kind:"Initialize" make
    |> Jsont.Object.opt_mem "hooks" hooks_jsont ~enc:(fun (r : initialize_r) ->
        r.hooks)
    |> Jsont.Object.keep_unknown Unknown.mems ~enc:(fun (r : initialize_r) ->
        r.unknown)
    |> Jsont.Object.finish

  let set_permission_mode_jsont : set_permission_mode_r Jsont.t =
    let make mode unknown = { mode; unknown } in
    Jsont.Object.map ~kind:"SetPermissionMode" make
    |> Jsont.Object.mem "mode" Permissions.Mode.jsont
         ~enc:(fun (r : set_permission_mode_r) -> r.mode)
    |> Jsont.Object.keep_unknown Unknown.mems
         ~enc:(fun (r : set_permission_mode_r) -> r.unknown)
    |> Jsont.Object.finish

  let hook_callback_jsont : hook_callback_r Jsont.t =
    let make callback_id input tool_use_id unknown =
      { callback_id; input; tool_use_id; unknown }
    in
    Jsont.Object.map ~kind:"HookCallback" make
    |> Jsont.Object.mem "callbackId" Jsont.string
         ~enc:(fun (r : hook_callback_r) -> r.callback_id)
    |> Jsont.Object.mem "input" Jsont.json ~enc:(fun (r : hook_callback_r) ->
        r.input)
    |> Jsont.Object.opt_mem "toolUseId" Jsont.string
         ~enc:(fun (r : hook_callback_r) -> r.tool_use_id)
    |> Jsont.Object.keep_unknown Unknown.mems ~enc:(fun (r : hook_callback_r) ->
        r.unknown)
    |> Jsont.Object.finish

  let mcp_message_jsont : mcp_message_r Jsont.t =
    let make server_name message unknown = { server_name; message; unknown } in
    Jsont.Object.map ~kind:"McpMessage" make
    |> Jsont.Object.mem "serverName" Jsont.string
         ~enc:(fun (r : mcp_message_r) -> r.server_name)
    |> Jsont.Object.mem "message" Jsont.json ~enc:(fun (r : mcp_message_r) ->
        r.message)
    |> Jsont.Object.keep_unknown Unknown.mems ~enc:(fun (r : mcp_message_r) ->
        r.unknown)
    |> Jsont.Object.finish

  let set_model_jsont : set_model_r Jsont.t =
    let make model unknown = { model; unknown } in
    Jsont.Object.map ~kind:"SetModel" make
    |> Jsont.Object.mem "model" Jsont.string ~enc:(fun (r : set_model_r) ->
        r.model)
    |> Jsont.Object.keep_unknown Unknown.mems ~enc:(fun (r : set_model_r) ->
        r.unknown)
    |> Jsont.Object.finish

  let get_server_info_jsont : unit Jsont.t =
    Jsont.Object.map ~kind:"GetServerInfo" (fun _unknown -> ())
    |> Jsont.Object.keep_unknown Unknown.mems ~enc:(fun () -> Unknown.empty)
    |> Jsont.Object.finish

  (* Main variant codec using subtype discriminator *)
  let jsont : t Jsont.t =
    let case_interrupt =
      Jsont.Object.Case.map "interrupt" interrupt_jsont ~dec:(fun () ->
          Interrupt)
    in
    let case_permission =
      Jsont.Object.Case.map "canUseTool" permission_jsont ~dec:(fun v ->
          Permission v)
    in
    let case_initialize =
      Jsont.Object.Case.map "initialize" initialize_jsont ~dec:(fun v ->
          Initialize v)
    in
    let case_set_permission_mode =
      Jsont.Object.Case.map "setPermissionMode" set_permission_mode_jsont
        ~dec:(fun v -> Set_permission_mode v)
    in
    let case_hook_callback =
      Jsont.Object.Case.map "hookCallback" hook_callback_jsont ~dec:(fun v ->
          Hook_callback v)
    in
    let case_mcp_message =
      Jsont.Object.Case.map "mcpMessage" mcp_message_jsont ~dec:(fun v ->
          Mcp_message v)
    in
    let case_set_model =
      Jsont.Object.Case.map "setModel" set_model_jsont ~dec:(fun v ->
          Set_model v)
    in
    let case_get_server_info =
      Jsont.Object.Case.map "getServerInfo" get_server_info_jsont
        ~dec:(fun () -> Get_server_info)
    in

    let enc_case = function
      | Interrupt -> Jsont.Object.Case.value case_interrupt ()
      | Permission v -> Jsont.Object.Case.value case_permission v
      | Initialize v -> Jsont.Object.Case.value case_initialize v
      | Set_permission_mode v ->
          Jsont.Object.Case.value case_set_permission_mode v
      | Hook_callback v -> Jsont.Object.Case.value case_hook_callback v
      | Mcp_message v -> Jsont.Object.Case.value case_mcp_message v
      | Set_model v -> Jsont.Object.Case.value case_set_model v
      | Get_server_info -> Jsont.Object.Case.value case_get_server_info ()
    in

    let cases =
      Jsont.Object.Case.
        [
          make case_interrupt;
          make case_permission;
          make case_initialize;
          make case_set_permission_mode;
          make case_hook_callback;
          make case_mcp_message;
          make case_set_model;
          make case_get_server_info;
        ]
    in

    Jsont.Object.map ~kind:"Request" Fun.id
    |> Jsont.Object.case_mem "subtype" Jsont.string ~enc:Fun.id ~enc_case cases
         ~tag_to_string:Fun.id ~tag_compare:String.compare
    |> Jsont.Object.finish
end

module Response = struct
  (* Standard JSON-RPC 2.0 error codes using polymorphic variants *)
  module Error_code = struct
    type t =
      [ `Parse_error
      | `Invalid_request
      | `Method_not_found
      | `Invalid_params
      | `Internal_error
      | `Custom of int ]

    let to_int : [< t ] -> int = function
      | `Parse_error -> -32700
      | `Invalid_request -> -32600
      | `Method_not_found -> -32601
      | `Invalid_params -> -32602
      | `Internal_error -> -32603
      | `Custom n -> n

    let of_int = function
      | -32700 -> `Parse_error
      | -32600 -> `Invalid_request
      | -32601 -> `Method_not_found
      | -32602 -> `Invalid_params
      | -32603 -> `Internal_error
      | n -> `Custom n
  end

  (* Structured error similar to JSON-RPC *)
  type error_detail = { code : int; message : string; data : Jsont.json option }

  let error_detail ~code ~message ?data () =
    { code = Error_code.to_int code; message; data }

  let error_detail_jsont : error_detail Jsont.t =
    let make code message data = { code; message; data } in
    Jsont.Object.map ~kind:"ErrorDetail" make
    |> Jsont.Object.mem "code" Jsont.int ~enc:(fun e -> e.code)
    |> Jsont.Object.mem "message" Jsont.string ~enc:(fun e -> e.message)
    |> Jsont.Object.opt_mem "data" Jsont.json ~enc:(fun e -> e.data)
    |> Jsont.Object.finish

  (* Individual record types for each response variant *)
  type success_r = {
    request_id : string;
    response : Jsont.json option;
    unknown : Unknown.t;
  }

  type error_r = {
    request_id : string;
    error : error_detail;
    unknown : Unknown.t;
  }

  type t = Success of success_r | Error of error_r

  let success ~request_id ?response () =
    Success { request_id; response; unknown = Unknown.empty }

  let error ~request_id ~error () =
    Error { request_id; error; unknown = Unknown.empty }

  (* Individual record codecs *)
  let success_jsont : success_r Jsont.t =
    let make request_id response unknown = { request_id; response; unknown } in
    Jsont.Object.map ~kind:"Success" make
    |> Jsont.Object.mem "requestId" Jsont.string ~enc:(fun (r : success_r) ->
        r.request_id)
    |> Jsont.Object.opt_mem "response" Jsont.json ~enc:(fun (r : success_r) ->
        r.response)
    |> Jsont.Object.keep_unknown Unknown.mems ~enc:(fun (r : success_r) ->
        r.unknown)
    |> Jsont.Object.finish

  let error_jsont : error_r Jsont.t =
    let make request_id error unknown = { request_id; error; unknown } in
    Jsont.Object.map ~kind:"Error" make
    |> Jsont.Object.mem "requestId" Jsont.string ~enc:(fun (r : error_r) ->
        r.request_id)
    |> Jsont.Object.mem "error" error_detail_jsont ~enc:(fun (r : error_r) ->
        r.error)
    |> Jsont.Object.keep_unknown Unknown.mems ~enc:(fun (r : error_r) ->
        r.unknown)
    |> Jsont.Object.finish

  (* Main variant codec using subtype discriminator *)
  let jsont : t Jsont.t =
    let case_success =
      Jsont.Object.Case.map "success" success_jsont ~dec:(fun v -> Success v)
    in
    let case_error =
      Jsont.Object.Case.map "error" error_jsont ~dec:(fun v -> Error v)
    in

    let enc_case = function
      | Success v -> Jsont.Object.Case.value case_success v
      | Error v -> Jsont.Object.Case.value case_error v
    in

    let cases = Jsont.Object.Case.[ make case_success; make case_error ] in

    Jsont.Object.map ~kind:"Response" Fun.id
    |> Jsont.Object.case_mem "subtype" Jsont.string ~enc:Fun.id ~enc_case cases
         ~tag_to_string:Fun.id ~tag_compare:String.compare
    |> Jsont.Object.finish
end

type request_envelope = {
  request_id : string;
  request : Request.t;
  unknown : Unknown.t;
}

type response_envelope = { response : Response.t; unknown : Unknown.t }

let create_request ~request_id ~request () =
  { request_id; request; unknown = Unknown.empty }

let create_response ~response () = { response; unknown = Unknown.empty }

(* Envelope codecs *)
let request_envelope_jsont : request_envelope Jsont.t =
  let make request_id request unknown = { request_id; request; unknown } in
  Jsont.Object.map ~kind:"RequestEnvelope" make
  |> Jsont.Object.mem "requestId" Jsont.string
       ~enc:(fun (r : request_envelope) -> r.request_id)
  |> Jsont.Object.mem "request" Request.jsont
       ~enc:(fun (r : request_envelope) -> r.request)
  |> Jsont.Object.keep_unknown Unknown.mems ~enc:(fun (r : request_envelope) ->
      r.unknown)
  |> Jsont.Object.finish

let response_envelope_jsont : response_envelope Jsont.t =
  let make response unknown = { response; unknown } in
  Jsont.Object.map ~kind:"ResponseEnvelope" make
  |> Jsont.Object.mem "response" Response.jsont
       ~enc:(fun (r : response_envelope) -> r.response)
  |> Jsont.Object.keep_unknown Unknown.mems ~enc:(fun (r : response_envelope) ->
      r.unknown)
  |> Jsont.Object.finish

(** Server information *)
module Server_info = struct
  type t = {
    version : string;
    capabilities : string list;
    commands : string list;
    output_styles : string list;
    unknown : Unknown.t;
  }

  let create ~version ~capabilities ~commands ~output_styles () =
    { version; capabilities; commands; output_styles; unknown = Unknown.empty }

  let version t = t.version
  let capabilities t = t.capabilities
  let commands t = t.commands
  let output_styles t = t.output_styles
  let unknown t = t.unknown

  let jsont : t Jsont.t =
    let make version capabilities commands output_styles unknown =
      { version; capabilities; commands; output_styles; unknown }
    in
    Jsont.Object.map ~kind:"ServerInfo" make
    |> Jsont.Object.mem "version" Jsont.string ~enc:(fun r -> r.version)
    |> Jsont.Object.mem "capabilities" (Jsont.list Jsont.string)
         ~enc:(fun r -> r.capabilities)
         ~dec_absent:[]
    |> Jsont.Object.mem "commands" (Jsont.list Jsont.string)
         ~enc:(fun r -> r.commands)
         ~dec_absent:[]
    |> Jsont.Object.mem "outputStyles" (Jsont.list Jsont.string)
         ~enc:(fun r -> r.output_styles)
         ~dec_absent:[]
    |> Jsont.Object.keep_unknown Unknown.mems ~enc:(fun r -> r.unknown)
    |> Jsont.Object.finish
end
