(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let src =
  Logs.Src.create "claude.sdk_control" ~doc:"Claude SDK control protocol"

module Log = (val Logs.src_log src : Logs.LOG)

module Request = struct
  type interrupt = { subtype : [ `Interrupt ]; unknown : Unknown.t }

  type permission = {
    subtype : [ `Can_use_tool ];
    tool_name : string;
    input : Jsont.json;
    permission_suggestions : Proto.Permissions.Update.t list option;
    blocked_path : string option;
    unknown : Unknown.t;
  }

  type initialize = {
    subtype : [ `Initialize ];
    hooks : (string * Jsont.json) list option;
    unknown : Unknown.t;
  }

  type set_permission_mode = {
    subtype : [ `Set_permission_mode ];
    mode : Proto.Permissions.Mode.t;
    unknown : Unknown.t;
  }

  type hook_callback = {
    subtype : [ `Hook_callback ];
    callback_id : string;
    input : Jsont.json;
    tool_use_id : string option;
    unknown : Unknown.t;
  }

  type mcp_message = {
    subtype : [ `Mcp_message ];
    server_name : string;
    message : Jsont.json;
    unknown : Unknown.t;
  }

  type set_model = {
    subtype : [ `Set_model ];
    model : string;
    unknown : Unknown.t;
  }

  type get_server_info = { subtype : [ `Get_server_info ]; unknown : Unknown.t }

  type t =
    | Interrupt of interrupt
    | Permission of permission
    | Initialize of initialize
    | Set_permission_mode of set_permission_mode
    | Hook_callback of hook_callback
    | Mcp_message of mcp_message
    | Set_model of set_model
    | Get_server_info of get_server_info

  let interrupt ?(unknown = Unknown.empty) () =
    Interrupt { subtype = `Interrupt; unknown }

  let permission ~tool_name ~input ?permission_suggestions ?blocked_path
      ?(unknown = Unknown.empty) () =
    Permission
      {
        subtype = `Can_use_tool;
        tool_name;
        input;
        permission_suggestions;
        blocked_path;
        unknown;
      }

  let initialize ?hooks ?(unknown = Unknown.empty) () =
    Initialize { subtype = `Initialize; hooks; unknown }

  let set_permission_mode ~mode ?(unknown = Unknown.empty) () =
    Set_permission_mode { subtype = `Set_permission_mode; mode; unknown }

  let hook_callback ~callback_id ~input ?tool_use_id ?(unknown = Unknown.empty)
      () =
    Hook_callback
      { subtype = `Hook_callback; callback_id; input; tool_use_id; unknown }

  let mcp_message ~server_name ~message ?(unknown = Unknown.empty) () =
    Mcp_message { subtype = `Mcp_message; server_name; message; unknown }

  let set_model ~model ?(unknown = Unknown.empty) () =
    Set_model { subtype = `Set_model; model; unknown }

  let get_server_info ?(unknown = Unknown.empty) () =
    Get_server_info { subtype = `Get_server_info; unknown }

  (* Individual record codecs *)
  let interrupt_jsont : interrupt Jsont.t =
    let make (unknown : Unknown.t) : interrupt =
      { subtype = `Interrupt; unknown }
    in
    Jsont.Object.map ~kind:"Interrupt" make
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (r : interrupt) ->
        r.unknown)
    |> Jsont.Object.finish

  let permission_jsont : permission Jsont.t =
    let make tool_name input permission_suggestions blocked_path
        (unknown : Unknown.t) : permission =
      {
        subtype = `Can_use_tool;
        tool_name;
        input;
        permission_suggestions;
        blocked_path;
        unknown;
      }
    in
    Jsont.Object.map ~kind:"Permission" make
    |> Jsont.Object.mem "tool_name" Jsont.string ~enc:(fun (r : permission) ->
        r.tool_name)
    |> Jsont.Object.mem "input" Jsont.json ~enc:(fun (r : permission) ->
        r.input)
    |> Jsont.Object.opt_mem "permission_suggestions"
         (Jsont.list Proto.Permissions.Update.jsont)
         ~enc:(fun (r : permission) -> r.permission_suggestions)
    |> Jsont.Object.opt_mem "blocked_path" Jsont.string
         ~enc:(fun (r : permission) -> r.blocked_path)
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (r : permission) ->
        r.unknown)
    |> Jsont.Object.finish

  let initialize_jsont : initialize Jsont.t =
    (* The hooks field is an object with string keys and json values *)
    let hooks_map_jsont = Jsont.Object.as_string_map Jsont.json in
    let module StringMap = Map.Make (String) in
    let hooks_jsont =
      Jsont.map
        ~dec:(fun m -> StringMap.bindings m)
        ~enc:(fun l -> StringMap.of_seq (List.to_seq l))
        hooks_map_jsont
    in
    let make hooks (unknown : Unknown.t) : initialize =
      { subtype = `Initialize; hooks; unknown }
    in
    Jsont.Object.map ~kind:"Initialize" make
    |> Jsont.Object.opt_mem "hooks" hooks_jsont ~enc:(fun (r : initialize) ->
        r.hooks)
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (r : initialize) ->
        r.unknown)
    |> Jsont.Object.finish

  let set_permission_mode_jsont : set_permission_mode Jsont.t =
    let make mode (unknown : Unknown.t) : set_permission_mode =
      { subtype = `Set_permission_mode; mode; unknown }
    in
    Jsont.Object.map ~kind:"SetPermissionMode" make
    |> Jsont.Object.mem "mode" Proto.Permissions.Mode.jsont
         ~enc:(fun (r : set_permission_mode) -> r.mode)
    |> Jsont.Object.keep_unknown Jsont.json_mems
         ~enc:(fun (r : set_permission_mode) -> r.unknown)
    |> Jsont.Object.finish

  let hook_callback_jsont : hook_callback Jsont.t =
    let make callback_id input tool_use_id (unknown : Unknown.t) : hook_callback
        =
      { subtype = `Hook_callback; callback_id; input; tool_use_id; unknown }
    in
    Jsont.Object.map ~kind:"HookCallback" make
    |> Jsont.Object.mem "callback_id" Jsont.string
         ~enc:(fun (r : hook_callback) -> r.callback_id)
    |> Jsont.Object.mem "input" Jsont.json ~enc:(fun (r : hook_callback) ->
        r.input)
    |> Jsont.Object.opt_mem "tool_use_id" Jsont.string
         ~enc:(fun (r : hook_callback) -> r.tool_use_id)
    |> Jsont.Object.keep_unknown Jsont.json_mems
         ~enc:(fun (r : hook_callback) -> r.unknown)
    |> Jsont.Object.finish

  let mcp_message_jsont : mcp_message Jsont.t =
    let make server_name message (unknown : Unknown.t) : mcp_message =
      { subtype = `Mcp_message; server_name; message; unknown }
    in
    Jsont.Object.map ~kind:"McpMessage" make
    |> Jsont.Object.mem "server_name" Jsont.string
         ~enc:(fun (r : mcp_message) -> r.server_name)
    |> Jsont.Object.mem "message" Jsont.json ~enc:(fun (r : mcp_message) ->
        r.message)
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (r : mcp_message) ->
        r.unknown)
    |> Jsont.Object.finish

  let set_model_jsont : set_model Jsont.t =
    let make model (unknown : Unknown.t) : set_model =
      { subtype = `Set_model; model; unknown }
    in
    Jsont.Object.map ~kind:"SetModel" make
    |> Jsont.Object.mem "model" Jsont.string ~enc:(fun (r : set_model) ->
        r.model)
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (r : set_model) ->
        r.unknown)
    |> Jsont.Object.finish

  let get_server_info_jsont : get_server_info Jsont.t =
    let make (unknown : Unknown.t) : get_server_info =
      { subtype = `Get_server_info; unknown }
    in
    Jsont.Object.map ~kind:"GetServerInfo" make
    |> Jsont.Object.keep_unknown Jsont.json_mems
         ~enc:(fun (r : get_server_info) -> r.unknown)
    |> Jsont.Object.finish

  (* Main variant codec using subtype discriminator *)
  let jsont : t Jsont.t =
    let case_interrupt =
      Jsont.Object.Case.map "interrupt" interrupt_jsont ~dec:(fun v ->
          Interrupt v)
    in
    let case_permission =
      Jsont.Object.Case.map "can_use_tool" permission_jsont ~dec:(fun v ->
          Permission v)
    in
    let case_initialize =
      Jsont.Object.Case.map "initialize" initialize_jsont ~dec:(fun v ->
          Initialize v)
    in
    let case_set_permission_mode =
      Jsont.Object.Case.map "set_permission_mode" set_permission_mode_jsont
        ~dec:(fun v -> Set_permission_mode v)
    in
    let case_hook_callback =
      Jsont.Object.Case.map "hook_callback" hook_callback_jsont ~dec:(fun v ->
          Hook_callback v)
    in
    let case_mcp_message =
      Jsont.Object.Case.map "mcp_message" mcp_message_jsont ~dec:(fun v ->
          Mcp_message v)
    in
    let case_set_model =
      Jsont.Object.Case.map "set_model" set_model_jsont ~dec:(fun v ->
          Set_model v)
    in
    let case_get_server_info =
      Jsont.Object.Case.map "get_server_info" get_server_info_jsont
        ~dec:(fun v -> Get_server_info v)
    in

    let enc_case = function
      | Interrupt v -> Jsont.Object.Case.value case_interrupt v
      | Permission v -> Jsont.Object.Case.value case_permission v
      | Initialize v -> Jsont.Object.Case.value case_initialize v
      | Set_permission_mode v ->
          Jsont.Object.Case.value case_set_permission_mode v
      | Hook_callback v -> Jsont.Object.Case.value case_hook_callback v
      | Mcp_message v -> Jsont.Object.Case.value case_mcp_message v
      | Set_model v -> Jsont.Object.Case.value case_set_model v
      | Get_server_info v -> Jsont.Object.Case.value case_get_server_info v
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
  (* Re-export Error_code from Proto *)
  module Error_code = Proto.Control.Response.Error_code

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

  type success = {
    subtype : [ `Success ];
    request_id : string;
    response : Jsont.json option;
    unknown : Unknown.t;
  }

  type error = {
    subtype : [ `Error ];
    request_id : string;
    error : error_detail;
    unknown : Unknown.t;
  }

  type t = Success of success | Error of error

  let success ~request_id ?response ?(unknown = Unknown.empty) () =
    Success { subtype = `Success; request_id; response; unknown }

  let error ~request_id ~error ?(unknown = Unknown.empty) () =
    Error { subtype = `Error; request_id; error; unknown }

  (* Individual record codecs *)
  let success_jsont : success Jsont.t =
    let make request_id response (unknown : Unknown.t) : success =
      { subtype = `Success; request_id; response; unknown }
    in
    Jsont.Object.map ~kind:"Success" make
    |> Jsont.Object.mem "request_id" Jsont.string ~enc:(fun (r : success) ->
        r.request_id)
    |> Jsont.Object.opt_mem "response" Jsont.json ~enc:(fun (r : success) ->
        r.response)
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (r : success) ->
        r.unknown)
    |> Jsont.Object.finish

  let error_jsont : error Jsont.t =
    let make request_id error (unknown : Unknown.t) : error =
      { subtype = `Error; request_id; error; unknown }
    in
    Jsont.Object.map ~kind:"Error" make
    |> Jsont.Object.mem "request_id" Jsont.string ~enc:(fun (r : error) ->
        r.request_id)
    |> Jsont.Object.mem "error" error_detail_jsont ~enc:(fun (r : error) ->
        r.error)
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (r : error) ->
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

type control_request = {
  type_ : [ `Control_request ];
  request_id : string;
  request : Request.t;
  unknown : Unknown.t;
}

type control_response = {
  type_ : [ `Control_response ];
  response : Response.t;
  unknown : Unknown.t;
}

type t = Request of control_request | Response of control_response

let create_request ~request_id ~request ?(unknown = Unknown.empty) () =
  Request { type_ = `Control_request; request_id; request; unknown }

let create_response ~response ?(unknown = Unknown.empty) () =
  Response { type_ = `Control_response; response; unknown }

(* Individual record codecs *)
let control_request_jsont : control_request Jsont.t =
  let make request_id request (unknown : Unknown.t) : control_request =
    { type_ = `Control_request; request_id; request; unknown }
  in
  Jsont.Object.map ~kind:"ControlRequest" make
  |> Jsont.Object.mem "request_id" Jsont.string
       ~enc:(fun (r : control_request) -> r.request_id)
  |> Jsont.Object.mem "request" Request.jsont ~enc:(fun (r : control_request) ->
      r.request)
  |> Jsont.Object.keep_unknown Jsont.json_mems
       ~enc:(fun (r : control_request) -> r.unknown)
  |> Jsont.Object.finish

let control_response_jsont : control_response Jsont.t =
  let make response (unknown : Unknown.t) : control_response =
    { type_ = `Control_response; response; unknown }
  in
  Jsont.Object.map ~kind:"ControlResponse" make
  |> Jsont.Object.mem "response" Response.jsont
       ~enc:(fun (r : control_response) -> r.response)
  |> Jsont.Object.keep_unknown Jsont.json_mems
       ~enc:(fun (r : control_response) -> r.unknown)
  |> Jsont.Object.finish

(* Main variant codec using type discriminator *)
let jsont : t Jsont.t =
  let case_request =
    Jsont.Object.Case.map "control_request" control_request_jsont ~dec:(fun v ->
        Request v)
  in
  let case_response =
    Jsont.Object.Case.map "control_response" control_response_jsont
      ~dec:(fun v -> Response v)
  in

  let enc_case = function
    | Request v -> Jsont.Object.Case.value case_request v
    | Response v -> Jsont.Object.Case.value case_response v
  in

  let cases = Jsont.Object.Case.[ make case_request; make case_response ] in

  Jsont.Object.map ~kind:"Control" Fun.id
  |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
       ~tag_to_string:Fun.id ~tag_compare:String.compare
  |> Jsont.Object.finish

let log_request req =
  Log.debug (fun m ->
      m "SDK control request: %a" (Jsont.pp_value Request.jsont ()) req)

let log_response resp =
  Log.debug (fun m ->
      m "SDK control response: %a" (Jsont.pp_value Response.jsont ()) resp)

(** Server information *)
module Server_info = struct
  type t = {
    version : string;
    capabilities : string list;
    commands : string list;
    output_styles : string list;
    unknown : Unknown.t;
  }

  let create ~version ~capabilities ~commands ~output_styles
      ?(unknown = Unknown.empty) () =
    { version; capabilities; commands; output_styles; unknown }

  let version t = t.version
  let capabilities t = t.capabilities
  let commands t = t.commands
  let output_styles t = t.output_styles
  let unknown t = t.unknown

  let jsont : t Jsont.t =
    let make version capabilities commands output_styles (unknown : Unknown.t) :
        t =
      { version; capabilities; commands; output_styles; unknown }
    in
    Jsont.Object.map ~kind:"ServerInfo" make
    |> Jsont.Object.mem "version" Jsont.string ~enc:(fun (r : t) -> r.version)
    |> Jsont.Object.mem "capabilities" (Jsont.list Jsont.string)
         ~enc:(fun (r : t) -> r.capabilities)
         ~dec_absent:[]
    |> Jsont.Object.mem "commands" (Jsont.list Jsont.string)
         ~enc:(fun (r : t) -> r.commands)
         ~dec_absent:[]
    |> Jsont.Object.mem "outputStyles" (Jsont.list Jsont.string)
         ~enc:(fun (r : t) -> r.output_styles)
         ~dec_absent:[]
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (r : t) -> r.unknown)
    |> Jsont.Object.finish
end
