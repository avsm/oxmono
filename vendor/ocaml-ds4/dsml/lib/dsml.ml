(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The prompt encodings and reply grammars, DSML being DeepSeek-V4's, with the
   V4.1, GLM and Qwen markups beside it.

   The DSML markup resembles XML but is not XML. Tags embed the DSML token,
   and parameter bodies are raw text scanned for a closing sentinel. V4.1
   spells the same grammar with different tag names. JSON is handled by jsont,
   wrapped as [Json]. The tool-call grammar is the bidirectional codec [Codec].
   The GLM and Qwen sections sit after their DeepSeek counterparts, and the
   [dialect] passed to the entry points picks between them. *)

(* ===================================================================== *)
(* Special tokens                                                         *)
(* ===================================================================== *)

let bos_token =
  "<\xef\xbd\x9cbegin\xe2\x96\x81of\xe2\x96\x81sentence\xef\xbd\x9c>"

let eos_token =
  "<\xef\xbd\x9cend\xe2\x96\x81of\xe2\x96\x81sentence\xef\xbd\x9c>"

let thinking_start_token = "<think>"
let thinking_end_token = "</think>"
let dsml_token = "\xef\xbd\x9cDSML\xef\xbd\x9c" (* ｜DSML｜ *)
let user_token = "<\xef\xbd\x9cUser\xef\xbd\x9c>"
let assistant_token = "<\xef\xbd\x9cAssistant\xef\xbd\x9c>"
let latest_reminder_token = "<\xef\xbd\x9clatest_reminder\xef\xbd\x9c>"

(* The GLM marker strings. The engine's rendered-chat tokenizer maps each to
   its reserved token wherever it appears in the rendered text, so they are
   structure to the prompt and contraband in content, exactly as the DeepSeek
   markers above are. *)
let glm_bos_token = "[gMASK]"
let glm_sop_token = "<sop>"
let glm_eos_token = "<|endoftext|>"
let glm_system_token = "<|system|>"
let glm_user_token = "<|user|>"
let glm_assistant_token = "<|assistant|>"
let glm_observation_token = "<|observation|>"
let glm_tool_call_open = "<tool_call>"
let glm_tool_call_close = "</tool_call>"
let glm_tool_response_open = "<tool_response>"
let glm_tool_response_close = "</tool_response>"
let glm_arg_key_open = "<arg_key>"
let glm_arg_key_close = "</arg_key>"
let glm_arg_value_open = "<arg_value>"
let glm_arg_value_close = "</arg_value>"

(* V4.1's system marker, which the V4 tokenizer does not have. *)
let deepseek41_system_token = "<\xef\xbd\x9cSystem\xef\xbd\x9c>"

(* The Qwen ChatML markers. Qwen shares GLM's spelling of <tool_call> and
   <tool_response>, and its function and parameter tags are plain text rather
   than reserved tokens. *)
let qwen_im_start_token = "<|im_start|>"
let qwen_im_end_token = "<|im_end|>"
let qwen_endoftext_token = "<|endoftext|>"
let qwen_function_open = "<function="
let qwen_function_close = "</function>"
let qwen_parameter_open = "<parameter="
let qwen_parameter_close = "</parameter>"

(* Which model family's markup to speak. The conversation representation is
   shared, and the dialect chooses the rendering and the reply grammar. *)
type dialect = Deepseek | Deepseek41 | Glm | Qwen

let reasoning_effort_max =
  "Reasoning Effort: Absolute maximum with no shortcuts permitted.\n\
   You MUST be very thorough in your thinking and comprehensively decompose \
   the problem to resolve the root cause, rigorously stress-testing your logic \
   against all potential paths, edge cases, and adversarial scenarios.\n\
   Explicitly write out your entire deliberation process, documenting every \
   intermediate step, considered alternative, and rejected hypothesis to \
   ensure absolutely no assumption is left unchecked.\n\n"

exception Parse_error of string

(* ===================================================================== *)
(* Small helpers                                                          *)
(* ===================================================================== *)

let starts_with s ~prefix = String.starts_with ~prefix s
let ends_with s ~suffix = String.ends_with ~suffix s
let sub_from s i = String.sub s i (String.length s - i)

(* First index >= [start] at which [sub] occurs in [s], or None. *)
let find_from s start sub =
  let sl = String.length sub and n = String.length s in
  if sl = 0 then Some start
  else begin
    let matches_at i =
      let rec c j = j >= sl || (s.[i + j] = sub.[j] && c (j + 1)) in
      c 0
    in
    let last = n - sl in
    let rec loop i =
      if i > last then None else if matches_at i then Some i else loop (i + 1)
    in
    loop start
  end

let contains s sub = find_from s 0 sub <> None

(* Strip leading whitespace: space, tab, NL, CR, FF, VT. *)
let strip_left_ws s =
  let n = String.length s in
  let i = ref 0 in
  while
    !i < n
    &&
    match s.[!i] with
    | ' ' | '\t' | '\n' | '\r' | '\012' | '\011' -> true
    | _ -> false
  do
    incr i
  done;
  String.sub s !i (n - !i)

(* JSON values in jsont's representation. The constructors are re-exported so
   that the matches below read directly. [Value] provides only what this module
   needs, being compact printing, parsing and member lookup. *)
module Json = struct
  type t = Jsont.json =
    | Null of unit Jsont.node
    | Bool of bool Jsont.node
    | Number of float Jsont.node
    | String of string Jsont.node
    | Array of t list Jsont.node
    | Object of Jsont.object' Jsont.node

  module Value = struct
    let string s = Jsont.Json.string s
    let number f = Jsont.Json.number f
    let name n = Jsont.Json.name n
    let member n v = Jsont.Json.mem n v
    let object' mems = Jsont.Json.object' mems
    let list l = Jsont.Json.list l
    let member_key k mems = Jsont.Json.find_mem k mems
    let of_string s = Jsont_bytesrw.decode_string Jsont.json s

    let of_string_exn s =
      match of_string s with Ok j -> j | Error e -> invalid_arg ("Json: " ^ e)

    (* Encoding a generic JSON value is total in practice (jsont renders
       non-finite numbers as null), so the error arm is unreachable. *)
    let to_string j =
      match Jsont_bytesrw.encode_string Jsont.json j with
      | Ok s -> s
      | Error _ -> "null"
  end
end

let json_member k = function
  | Json.Object (mems, _) -> Option.map snd (Json.Value.member_key k mems)
  | _ -> None

(* ===================================================================== *)
(* DSML markup scanner                                                    *)
(* ===================================================================== *)

(* The names that follow the DSML token in each tag. V4.1 writes the same
   grammar as V4 with these renamed, the leading space included. *)
type tags = { calls : string; invoke : string; parameter : string }

let v4_tags =
  { calls = "tool_calls"; invoke = "invoke"; parameter = "parameter" }

let v41_tags =
  { calls = " calls"; invoke = " invoke"; parameter = " parameter" }

let invoke_start g = "<" ^ dsml_token ^ g.invoke
let invoke_end g = "</" ^ dsml_token ^ g.invoke
let parameter_start g = "<" ^ dsml_token ^ g.parameter
let parameter_slash g = "/" ^ dsml_token ^ g.parameter
let tool_calls_open g = "<" ^ dsml_token ^ g.calls
let tool_calls_end g = "</" ^ dsml_token ^ g.calls ^ ">"
let tool_calls_start g = "\n\n" ^ tool_calls_open g
let parameter_close g = "<" ^ parameter_slash g ^ ">"

(* A string value runs to its closing tag, so a value that holds that tag is
   written with the tag's [<] as [&lt;], as upstream's agent and server do. An
   ampersand that begins such a spelling is itself written [&amp;], so that
   [&lt;] followed by the tag can be said literally too. Nothing else is
   escaped: a value is not HTML, and [&lt;] followed by anything but the tag
   stays as it is. [escaped_close s i close] is the length of the escaped
   spelling of [close] at [i], counting any [amp;] after its ampersand. *)
let escaped_close s i close =
  let n = String.length s in
  let at j sub =
    let m = String.length sub in
    j + m <= n && String.sub s j m = sub
  in
  if i >= n || s.[i] <> '&' then None
  else
    let rec amps j = if at j "amp;" then amps (j + 4) else j in
    let j = amps (i + 1) in
    let tail = String.sub close 1 (String.length close - 1) in
    if at j "lt;" && at (j + 3) tail then Some (j + 3 + String.length tail - i)
    else None

let escape_text ~close s =
  let n = String.length s and m = String.length close in
  let b = Buffer.create (n + 8) in
  for i = 0 to n - 1 do
    if i + m <= n && String.sub s i m = close then Buffer.add_string b "&lt;"
    else if escaped_close s i close <> None then Buffer.add_string b "&amp;"
    else Buffer.add_char b s.[i]
  done;
  Buffer.contents b

(* The inverse of [escape_text]: each escaped spelling loses one level, [&lt;]
   becoming [<] and a leading [&amp;] becoming [&]. *)
let unescape_text ~close s =
  let n = String.length s in
  let b = Buffer.create n in
  let rec go i =
    if i < n then
      match escaped_close s i close with
      | Some _ when i + 5 <= n && String.sub s i 5 = "&amp;" ->
          Buffer.add_char b '&';
          go (i + 5)
      | Some _ ->
          Buffer.add_char b '<';
          go (i + 4)
      | None ->
          Buffer.add_char b s.[i];
          go (i + 1)
  in
  go 0;
  Buffer.contents b

(* Every marker this module gives a meaning to, paired with an inert form of
   itself: the five it refuses to see in content, and the three that open a
   turn.  No replacement is an entry of the tokenizer's special table (see
   [special_token_at] in csrc/ds4.c), so each tokenizes as ordinary words, and
   no replacement contains [<] or the fullwidth bar, so the text beside one
   cannot complete a marker the rewrite has just taken apart.  [<think>] and
   [</think>] are ASCII entries of that table, and [<｜User｜>] is the one with
   teeth, since the tokenizer reads it as the token that opens a user turn. *)
let inert_forms =
  [
    (bos_token, "[begin_of_sentence]");
    (eos_token, "[end_of_sentence]");
    (thinking_start_token, "[think]");
    (thinking_end_token, "[/think]");
    (dsml_token, "|DSML|");
    (user_token, "[User]");
    (assistant_token, "[Assistant]");
    (latest_reminder_token, "[latest_reminder]");
  ]

(* The GLM markers and their inert forms. A GLM tokenizer's rendered-chat
   table also maps the DeepSeek-style bracket markers onto its own reserved
   tokens, so the GLM list extends the DeepSeek one rather than replacing it.
   The same rules hold: no replacement is a marker, and none contains [<] or
   the fullwidth bar. *)
let glm_inert_forms =
  inert_forms
  @ [
      (glm_bos_token, "(gMASK)");
      (glm_sop_token, "[sop]");
      (glm_eos_token, "[endoftext]");
      (glm_system_token, "[system]");
      (glm_user_token, "[user]");
      (glm_assistant_token, "[assistant]");
      (glm_observation_token, "[observation]");
      (glm_tool_call_open, "[tool_call]");
      (glm_tool_call_close, "[/tool_call]");
      (glm_tool_response_open, "[tool_response]");
      (glm_tool_response_close, "[/tool_response]");
      (glm_arg_key_open, "[arg_key]");
      (glm_arg_key_close, "[/arg_key]");
      (glm_arg_value_open, "[arg_value]");
      (glm_arg_value_close, "[/arg_value]");
    ]

(* Text the decoder refused, rewritten so that it can be shown and stored as
   text.  It stays readable as evidence, but no longer says to the tokenizer
   what it said, so re-encoding a conversation that carries it cannot break the
   next prompt apart. *)
let neutralise_with forms s =
  let n = String.length s in
  let b = Buffer.create n in
  let rec go i =
    if i >= n then ()
    else
      match
        (* The first-byte check keeps ordinary text from paying an allocating
           comparison per marker per character. *)
        List.find_opt
          (fun (tok, _) ->
            let l = String.length tok in
            tok.[0] = s.[i] && i + l <= n && String.sub s i l = tok)
          forms
      with
      | Some (tok, inert) ->
          Buffer.add_string b inert;
          go (i + String.length tok)
      | None ->
          Buffer.add_char b s.[i];
          go (i + 1)
  in
  go 0;
  Buffer.contents b

(* V4.1 adds a system marker to V4's set. *)
let deepseek41_inert_forms =
  inert_forms @ [ (deepseek41_system_token, "[System]") ]

(* The Qwen markers, over the DeepSeek set for the reason the GLM list is. *)
let qwen_inert_forms =
  inert_forms
  @ [
      (qwen_im_start_token, "[im_start]");
      (qwen_im_end_token, "[im_end]");
      (qwen_endoftext_token, "[endoftext]");
      (glm_tool_call_open, "[tool_call]");
      (glm_tool_call_close, "[/tool_call]");
      (glm_tool_response_open, "[tool_response]");
      (glm_tool_response_close, "[/tool_response]");
    ]

let neutralise_specials ?(dialect = Deepseek) s =
  neutralise_with
    (match dialect with
    | Deepseek -> inert_forms
    | Deepseek41 -> deepseek41_inert_forms
    | Glm -> glm_inert_forms
    | Qwen -> qwen_inert_forms)
    s

(* Read from [index] until the earliest of [stops]; on a positional tie the
   first stop in the list wins.  Returns (index-after-stop, content-before-stop,
   matched-stop). *)
let read_until_stop text index stops =
  let n = String.length text in
  let min_pos = ref n and matched = ref None in
  List.iter
    (fun s ->
      match find_from text index s with
      | Some pos when pos < !min_pos ->
          min_pos := pos;
          matched := Some s
      | _ -> ())
    stops;
  match !matched with
  | Some s ->
      ( !min_pos + String.length s,
        String.sub text index (!min_pos - index),
        Some s )
  | None -> (n, String.sub text index (n - index), None)

(* Validate ' name="NAME">\n' (with optional leading whitespace).  An invoke
   with no parameters has an empty body, which leaves a second newline before
   the closing tag, so that form is accepted as well.  Without it the decoder
   would refuse what the encoder writes for a call that takes no arguments. *)
let parse_tool_name content =
  let s = strip_left_ws content in
  let prefix = "name=\"" in
  let suffix = if ends_with s ~suffix:"\">\n\n" then "\">\n\n" else "\">\n" in
  if
    starts_with s ~prefix && ends_with s ~suffix
    && String.length s >= String.length prefix + String.length suffix
  then
    String.sub s (String.length prefix)
      (String.length s - String.length prefix - String.length suffix)
  else
    raise (Parse_error (Printf.sprintf "Tool name format error: '%s'" content))

(* Validate ' name="K" string="true|false">VALUE<' -> (K, "true|false", VALUE). *)
let parse_param content =
  if not (ends_with content ~suffix:"<") then
    raise (Parse_error (Printf.sprintf "Parameter format error: '%s'" content));
  let body = String.sub content 0 (String.length content - 1) in
  let p1 = " name=\"" in
  if not (starts_with body ~prefix:p1) then
    raise (Parse_error (Printf.sprintf "Parameter format error: '%s'" content));
  let start = String.length p1 in
  let sep = "\" string=\"" in
  let rec find_sep from =
    match find_from body from sep with
    | None ->
        raise
          (Parse_error (Printf.sprintf "Parameter format error: '%s'" content))
    | Some pos ->
        let after = sub_from body (pos + String.length sep) in
        if starts_with after ~prefix:"true\">" then
          (String.sub body start (pos - start), "true", sub_from after 6)
        else if starts_with after ~prefix:"false\">" then
          (String.sub body start (pos - start), "false", sub_from after 7)
        else find_sep (pos + 1)
  in
  find_sep start

(* Read one invocation, positioned just after the [invoke_start] token.  Returns
   (tool name, parameters in order, index past [</｜DSML｜invoke>]). *)
let read_one_invoke g text index =
  let parameter_start = parameter_start g and invoke_end = invoke_end g in
  let j, name_content, st =
    read_until_stop text index [ parameter_start; invoke_end ]
  in
  let name = parse_tool_name name_content in
  let params = ref [] and i = ref j and st = ref st in
  while match !st with Some s -> s = parameter_start | None -> false do
    (* The value ends at the whole closing tag. Its tail alone also ends the
       escaped spelling of that tag, which a value may hold. *)
    let k, param_content, _ =
      read_until_stop text !i [ "<" ^ parameter_slash g ]
    in
    i := k;
    let pname, pflag, pval = parse_param (param_content ^ "<") in
    let pval = unescape_text ~close:(parameter_close g) pval in
    if List.mem_assoc pname !params then
      raise
        (Parse_error (Printf.sprintf "Duplicate parameter name: '%s'" pname));
    params := (pname, (pval, pflag)) :: !params;
    let m, content2, st2 =
      read_until_stop text !i [ parameter_start; invoke_end ]
    in
    i := m;
    st := st2;
    if content2 <> ">\n" then
      raise
        (Parse_error
           (Printf.sprintf
              "Parameter format error: expected '>\\n' but got '%s'" content2))
  done;
  (name, List.rev !params, !i)

(* ===================================================================== *)
(* Types                                                                  *)
(* ===================================================================== *)

type thinking_mode = Chat | Thinking
type reasoning_effort = High | Max
type task = Action | Query | Authority | Domain | Title | Read_url

let task_token = function
  | Action -> "<\xef\xbd\x9caction\xef\xbd\x9c>"
  | Query -> "<\xef\xbd\x9cquery\xef\xbd\x9c>"
  | Authority -> "<\xef\xbd\x9cauthority\xef\xbd\x9c>"
  | Domain -> "<\xef\xbd\x9cdomain\xef\xbd\x9c>"
  | Title -> "<\xef\xbd\x9ctitle\xef\xbd\x9c>"
  | Read_url -> "<\xef\xbd\x9cread_url\xef\xbd\x9c>"

type tool_call = { id : string option; name : string; arguments : string }

type content_block =
  | Text of string
  | Tool_result of { tool_use_id : string; content : Json.t }

type role =
  | System
  | Developer
  | User
  | Tool_role
  | Latest_reminder
  | Assistant

type message = {
  role : role;
  content : string;
  reasoning_content : string;
  tools : Json.t list;
  response_format : Json.t option;
  tool_calls : tool_call list;
  wo_eos : bool;
  task : task option;
  tool_call_id : string option;
  content_blocks : content_block list option;
}

type parsed_message = {
  content : string;
  reasoning_content : string;
  tool_calls : tool_call list;
}

let base role =
  {
    role;
    content = "";
    reasoning_content = "";
    tools = [];
    response_format = None;
    tool_calls = [];
    wo_eos = false;
    task = None;
    tool_call_id = None;
    content_blocks = None;
  }

let system ?(tools = []) ?response_format content =
  { (base System) with content; tools; response_format }

let developer ?(tools = []) ?response_format content =
  if content = "" then invalid_arg "Dsml.developer: content must be non-empty";
  { (base Developer) with content; tools; response_format }

let user ?task content = { (base User) with content; task }

let assistant ?(content = "") ?(reasoning_content = "") ?(tool_calls = [])
    ?(wo_eos = false) ?task () =
  { (base Assistant) with content; reasoning_content; tool_calls; wo_eos; task }

let tool ~id content = { (base Tool_role) with content; tool_call_id = Some id }
let latest_reminder content = { (base Latest_reminder) with content }
let tool_call ?id ~name ~arguments () = { id; name; arguments }

(* ===================================================================== *)
(* Tool arguments <-> DSML parameters (dynamic / generic)                 *)
(* ===================================================================== *)

let tools_from_openai (tools : Json.t list) : Json.t list =
  List.map
    (fun t ->
      match json_member "function" t with
      | Some f -> f
      | None -> invalid_arg "Dsml: tool object missing \"function\"")
    tools

(* JSON [arguments] -> DSML <parameter> tags.  String members become raw text
   ([string="true"]); other members carry their compact JSON ([string="false"]).
   A non-object collapses to a single "arguments" string parameter. *)
let encode_arguments_to_dsml g tc =
  let members =
    match Json.Value.of_string tc.arguments with
    | Ok (Json.Object (mems, _)) -> List.map (fun (nm, v) -> (fst nm, v)) mems
    | Ok _ | Error _ -> [ ("arguments", Json.Value.string tc.arguments) ]
  in
  String.concat "\n"
    (List.map
       (fun (k, v) ->
         let is_str, value =
           match v with
           | Json.String (s, _) -> (true, s)
           | other -> (false, Json.Value.to_string other)
         in
         Printf.sprintf "%s name=\"%s\" string=\"%s\">%s<%s>"
           (parameter_start g) k
           (if is_str then "true" else "false")
           (escape_text ~close:(parameter_close g) value)
           (parameter_slash g))
       members)

(* Decoded <parameter>s -> OpenAI [arguments] JSON object string. *)
let decode_dsml_to_arguments name (args : (string * (string * string)) list) =
  let member_of (k, (v, is_string)) =
    let value =
      if is_string = "true" then Json.Value.string v
      else
        match Json.Value.of_string v with
        | Ok j -> j
        | Error _ ->
            raise
              (Parse_error
                 (Printf.sprintf "Invalid JSON parameter value: '%s'" v))
    in
    Json.Value.member (Json.Value.name k) value
  in
  {
    id = None;
    name;
    arguments =
      Json.Value.to_string (Json.Value.object' (List.map member_of args));
  }

(* ===================================================================== *)
(* Codec: bidirectional combinators for the tool-call grammar            *)
(* ===================================================================== *)

module Codec = struct
  let valid_name name =
    name <> ""
    && String.for_all
         (function
           | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
           | _ -> false)
         name

  let check_name kind name =
    if not (valid_name name) then
      invalid_arg
        (Printf.sprintf
           "Dsml.Codec.%s: %S must contain only ASCII letters, digits, '_' or \
            '-'"
           kind name)

  (* A parameter value codec: raw text + the [string="..."] flag <-> ['a].
     [v_schema] is the JSON Schema fields ([("type", ...)], etc.) describing the
     value, used to advertise the parameter to the model. *)
  type 'a value = {
    v_dec : raw:string -> is_string:bool -> 'a;
    v_enc : 'a -> bool * string (* (is_string, raw text) *);
    v_schema : (string * Json.t) list;
  }

  let string =
    {
      v_dec = (fun ~raw ~is_string:_ -> raw);
      v_enc = (fun s -> (true, s));
      v_schema = [ ("type", Json.Value.string "string") ];
    }

  let json =
    {
      v_dec =
        (fun ~raw ~is_string ->
          if is_string then Json.Value.string raw
          else
            match Json.Value.of_string raw with
            | Ok j -> j
            | Error _ ->
                raise
                  (Parse_error (Printf.sprintf "Invalid JSON value: '%s'" raw)));
      v_enc =
        (fun j ->
          match j with
          | Json.String (s, _) -> (true, s)
          | other -> (false, Json.Value.to_string other));
      v_schema = [];
    }

  let bool =
    {
      v_dec =
        (fun ~raw ~is_string:_ ->
          match raw with
          | "true" -> true
          | "false" -> false
          | _ ->
              raise
                (Parse_error (Printf.sprintf "Invalid bool value: '%s'" raw)));
      v_enc = (fun b -> (false, if b then "true" else "false"));
      v_schema = [ ("type", Json.Value.string "boolean") ];
    }

  let int =
    {
      v_dec =
        (fun ~raw ~is_string:_ ->
          match int_of_string_opt raw with
          | Some i -> i
          | None ->
              raise (Parse_error (Printf.sprintf "Invalid int value: '%s'" raw)));
      v_enc = (fun i -> (false, string_of_int i));
      v_schema = [ ("type", Json.Value.string "integer") ];
    }

  let float =
    {
      v_dec =
        (fun ~raw ~is_string:_ ->
          match float_of_string_opt raw with
          | Some f when Float.is_finite f -> f
          | None ->
              raise
                (Parse_error (Printf.sprintf "Invalid float value: '%s'" raw))
          | Some _ ->
              raise
                (Parse_error (Printf.sprintf "Non-finite float value: '%s'" raw)));
      v_enc =
        (fun f ->
          if not (Float.is_finite f) then
            invalid_arg "Dsml.Codec.float: value must be finite";
          (false, Json.Value.to_string (Json.Value.number f)));
      v_schema = [ ("type", Json.Value.string "number") ];
    }

  let array ?minimum ?maximum item =
    Option.iter
      (fun bound ->
        if bound < 0 then invalid_arg "Dsml.Codec.array: negative bound")
      minimum;
    Option.iter
      (fun bound ->
        if bound < 0 then invalid_arg "Dsml.Codec.array: negative bound")
      maximum;
    (match (minimum, maximum) with
    | Some low, Some high when low > high ->
        invalid_arg "Dsml.Codec.array: minimum is greater than maximum"
    | _ -> ());
    let check_length error values =
      let length = List.length values in
      match minimum with
      | Some bound when length < bound -> error "array is shorter than minimum"
      | _ -> (
          match maximum with
          | Some bound when length > bound ->
              error "array is longer than maximum"
          | _ -> ())
    in
    let decode_item = function
      | Json.String (text, _) -> item.v_dec ~raw:text ~is_string:true
      | json -> item.v_dec ~raw:(Json.Value.to_string json) ~is_string:false
    in
    let encode_item value =
      match item.v_enc value with
      | true, text -> Json.Value.string text
      | false, text -> (
          match Json.Value.of_string text with
          | Ok json -> json
          | Error message -> invalid_arg ("Dsml.Codec.array: " ^ message))
    in
    let item_schema =
      Json.Value.object'
        (List.map
           (fun (name, json) -> Json.Value.member (Json.Value.name name) json)
           item.v_schema)
    in
    {
      v_dec =
        (fun ~raw ~is_string:_ ->
          match Json.Value.of_string raw with
          | Ok (Json.Array (items, _)) ->
              let values = List.map decode_item items in
              check_length (fun message -> raise (Parse_error message)) values;
              values
          | Ok _ -> raise (Parse_error "Expected an array")
          | Error _ -> raise (Parse_error "Invalid JSON array"));
      v_enc =
        (fun values ->
          check_length invalid_arg values;
          ( false,
            Json.Value.to_string (Json.Value.list (List.map encode_item values))
          ));
      v_schema =
        [ ("type", Json.Value.string "array"); ("items", item_schema) ]
        @ Option.fold ~none:[]
            ~some:(fun bound ->
              [ ("minItems", Json.Value.number (float_of_int bound)) ])
            minimum
        @ Option.fold ~none:[]
            ~some:(fun bound ->
              [ ("maxItems", Json.Value.number (float_of_int bound)) ])
            maximum;
    }

  let map_value ~dec ~enc v =
    {
      v_dec = (fun ~raw ~is_string -> dec (v.v_dec ~raw ~is_string));
      v_enc = (fun b -> v.v_enc (enc b));
      v_schema = v.v_schema;
    }

  (* A codec for one invocation.  Scanning (name + parameters) is done by the
     block reader; a codec only interprets and renders an invoke, so codecs can
     be dispatched on the tool name (see {!choice}). *)
  type 'a t = {
    name : string; (* fixed tool name, or "" *)
    schema : Json.t; (* JSON Schema for the parameters object *)
    decode_invoke : string -> (string * (string * string)) list -> 'a;
    encode_invoke : tags -> 'a -> string;
  }

  (* The "object" JSON Schema accepting any parameters. *)
  let any_schema =
    Json.Value.object'
      [
        Json.Value.member (Json.Value.name "type") (Json.Value.string "object");
      ]

  let render_param g name (is_string, raw) =
    Printf.sprintf "%s name=\"%s\" string=\"%s\">%s<%s>" (parameter_start g)
      name
      (if is_string then "true" else "false")
      (escape_text ~close:(parameter_close g) raw)
      (parameter_slash g)

  let render_invoke g name body =
    Printf.sprintf "%s name=\"%s\">\n%s\n%s>" (invoke_start g) name body
      (invoke_end g)

  module Object = struct
    type ('o, 'dec) map = {
      oname : string;
      odec : (string * (string * string)) list -> 'dec;
      oenc : ('o -> (string * Json.t) option) list; (* reversed *)
      oprops : (string * Json.t) list; (* (member name, schema), reversed *)
      oreq : string list; (* required member names, reversed *)
    }

    let map oname dec =
      check_name "Object.map" oname;
      { oname; odec = (fun _ -> dec); oenc = []; oprops = []; oreq = [] }

    let json_of_value value item =
      match value.v_enc item with
      | true, text -> Json.Value.string text
      | false, text -> (
          match Json.Value.of_string text with
          | Ok json -> json
          | Error message -> invalid_arg ("Dsml.Codec.Object: " ^ message))

    let params_of_members members =
      List.map
        (fun (name, json) ->
          match json with
          | Json.String (text, _) -> (fst name, (text, "true"))
          | json -> (fst name, (Json.Value.to_string json, "false")))
        members

    let members map value =
      List.filter_map (fun encode -> encode value) (List.rev map.oenc)

    let param ~enc ?description ?default name value m =
      check_name "Object.param" name;
      if List.mem_assoc name m.oprops then
        invalid_arg
          (Printf.sprintf "Dsml.Codec.Object.param: duplicate %S" name);
      Option.iter (fun default -> ignore (json_of_value value default)) default;
      let odec params =
        let f = m.odec params in
        match (List.assoc_opt name params, default) with
        | Some (raw, flag), _ -> f (value.v_dec ~raw ~is_string:(flag = "true"))
        | None, Some d -> f d
        | None, None ->
            raise (Parse_error (Printf.sprintf "Missing parameter: '%s'" name))
      in
      let enc1 o = Some (name, json_of_value value (enc o)) in
      let prop =
        let fields =
          value.v_schema
          @
          match description with
          | Some d -> [ ("description", Json.Value.string d) ]
          | None -> []
        in
        Json.Value.object'
          (List.map
             (fun (k, v) -> Json.Value.member (Json.Value.name k) v)
             fields)
      in
      {
        oname = m.oname;
        odec;
        oenc = enc1 :: m.oenc;
        oprops = (name, prop) :: m.oprops;
        oreq = (match default with Some _ -> m.oreq | None -> name :: m.oreq);
      }

    let optional ~enc ?description name value m =
      check_name "Object.optional" name;
      if List.mem_assoc name m.oprops then
        invalid_arg
          (Printf.sprintf "Dsml.Codec.Object.optional: duplicate %S" name);
      let odec params =
        let f = m.odec params in
        match List.assoc_opt name params with
        | Some (raw, flag) ->
            f (Some (value.v_dec ~raw ~is_string:(flag = "true")))
        | None -> f None
      in
      let enc1 o =
        Option.map (fun item -> (name, json_of_value value item)) (enc o)
      in
      let prop =
        let fields =
          value.v_schema
          @
          match description with
          | Some d -> [ ("description", Json.Value.string d) ]
          | None -> []
        in
        Json.Value.object'
          (List.map
             (fun (k, v) -> Json.Value.member (Json.Value.name k) v)
             fields)
      in
      {
        oname = m.oname;
        odec;
        oenc = enc1 :: m.oenc;
        oprops = (name, prop) :: m.oprops;
        oreq = m.oreq;
      }

    let seal m =
      let member k v = Json.Value.member (Json.Value.name k) v in
      {
        v_dec =
          (fun ~raw ~is_string:_ ->
            match Json.Value.of_string raw with
            | Ok (Json.Object (members, _)) ->
                m.odec (params_of_members members)
            | Ok _ -> raise (Parse_error "Expected an object")
            | Error _ -> raise (Parse_error "Invalid JSON object"));
        v_enc =
          (fun value ->
            let members =
              List.map (fun (name, json) -> member name json) (members m value)
            in
            (false, Json.Value.to_string (Json.Value.object' members)));
        v_schema =
          [
            ("type", Json.Value.string "object");
            ( "properties",
              Json.Value.object'
                (List.rev_map (fun (name, json) -> member name json) m.oprops)
            );
            ("required", Json.Value.list (List.rev_map Json.Value.string m.oreq));
          ];
      }
  end

  module Invoke = struct
    type ('o, 'dec) map = ('o, 'dec) Object.map

    let map = Object.map
    let param = Object.param
    let optional = Object.optional

    let seal map =
      let arguments = Object.seal map in
      let render_member g (name, json) =
        match json with
        | Json.String (text, _) -> render_param g name (true, text)
        | json -> render_param g name (false, Json.Value.to_string json)
      in
      {
        name = map.Object.oname;
        schema =
          Json.Value.object'
            (List.map
               (fun (name, json) ->
                 Json.Value.member (Json.Value.name name) json)
               arguments.v_schema);
        decode_invoke = (fun _name params -> map.Object.odec params);
        encode_invoke =
          (fun g value ->
            render_invoke g map.Object.oname
              (String.concat "\n"
                 (List.map (render_member g) (Object.members map value))));
      }
  end

  (* Any tool: parameters <-> JSON arguments. *)
  let dynamic =
    {
      name = "";
      schema = any_schema;
      decode_invoke = (fun name params -> decode_dsml_to_arguments name params);
      encode_invoke =
        (fun g tc -> render_invoke g tc.name (encode_arguments_to_dsml g tc));
    }

  let map ~dec ~enc c =
    {
      name = c.name;
      schema = c.schema;
      decode_invoke = (fun n p -> dec (c.decode_invoke n p));
      encode_invoke = (fun g b -> c.encode_invoke g (enc b));
    }

  let name c = c.name
  let schema c = c.schema

  (* Decode the JSON object [arguments] string (as carried by {!tool_call})
     through [codec], mirroring the [string="true|false"] convention: string
     members are raw text, every other member is its compact JSON. *)
  let decode_arguments codec arguments =
    let params_of members =
      List.map
        (fun (nm, v) ->
          match v with
          | Json.String (s, _) -> (fst nm, (s, "true"))
          | other -> (fst nm, (Json.Value.to_string other, "false")))
        members
    in
    match Json.Value.of_string arguments with
    | Ok (Json.Object (members, _)) -> (
        try Ok (codec.decode_invoke codec.name (params_of members))
        with Parse_error m -> Error m)
    | Ok _ -> Error "tool arguments are not a JSON object"
    | Error _ -> Error "tool arguments are not valid JSON"

  type 'b case = {
    cname : string;
    cdecode : string -> (string * (string * string)) list -> 'b;
    cencode : tags -> 'b -> string option;
  }

  let case ~inject ~project c =
    {
      cname = c.name;
      cdecode = (fun n p -> inject (c.decode_invoke n p));
      cencode = (fun g b -> Option.map (c.encode_invoke g) (project b));
    }

  let choice ?default cases =
    {
      name = "";
      schema = (match default with Some d -> d.schema | None -> any_schema);
      decode_invoke =
        (fun n p ->
          match List.find_opt (fun c -> c.cname = n) cases with
          | Some c -> c.cdecode n p
          | None -> (
              match default with
              | Some d -> d.decode_invoke n p
              | None ->
                  raise
                    (Parse_error (Printf.sprintf "No codec for tool: '%s'" n))));
      encode_invoke =
        (fun g b ->
          let rec go = function
            | c :: tl -> (
                match c.cencode g b with Some s -> s | None -> go tl)
            | [] -> (
                match default with
                | Some d -> d.encode_invoke g b
                | None -> raise (Parse_error "No codec to encode tool call"))
          in
          go cases);
    }

  (* Read the invocations of a tool-call block; [index] is positioned just after
     the [tool_calls_open] token (at its '>'). *)
  let decode_block ?(tags = v4_tags) codec text index =
    let tool_calls_end = tool_calls_end tags in
    let calls = ref [] and i = ref index and stop = ref false in
    while not !stop do
      let j, content, st =
        read_until_stop text !i [ invoke_start tags; tool_calls_end ]
      in
      i := j;
      if content <> ">\n" then
        raise
          (Parse_error
             (Printf.sprintf
                "Tool call format error: expected '>\\n' but got '%s'" content));
      match st with
      | Some s when s = tool_calls_end -> stop := true
      | None -> raise (Parse_error "Missing special token in tool calls")
      | Some _ ->
          let name, params, j = read_one_invoke tags text !i in
          i := j;
          calls := codec.decode_invoke name params :: !calls
    done;
    (List.rev !calls, !i)

  let encode_block ?(tags = v4_tags) codec calls =
    Printf.sprintf "%s>\n%s\n%s" (tool_calls_open tags)
      (String.concat "\n" (List.map (codec.encode_invoke tags) calls))
      (tool_calls_end tags)

  let decode codec text =
    let tool_calls_open = tool_calls_open v4_tags in
    match find_from text 0 tool_calls_open with
    | None -> Error "no <｜DSML｜tool_calls> block"
    | Some p -> (
        try
          Ok (fst (decode_block codec text (p + String.length tool_calls_open)))
        with Parse_error m -> Error m)

  let encode codec calls = encode_block codec calls

  let encode_arguments codec value =
    try
      match decode dynamic (encode codec [ value ]) with
      | Ok [ { arguments; _ } ] -> Ok arguments
      | Ok _ -> Error "encoded tool call did not contain one invocation"
      | Error message -> Error message
    with
    | Parse_error message -> Error message
    | Invalid_argument message -> Error message
end

(* ===================================================================== *)
(* Tool: OpenAI tool-schema construction                                 *)
(* ===================================================================== *)

module Tool = struct
  let v ~name ?(description = "") ?parameters () : Json.t =
    Codec.check_name "Tool.v" name;
    let fields =
      [ ("name", Json.Value.string name) ]
      @ (if description = "" then []
         else [ ("description", Json.Value.string description) ])
      @ match parameters with Some p -> [ ("parameters", p) ] | None -> []
    in
    let mem (k, v) = Json.Value.member (Json.Value.name k) v in
    Json.Value.object'
      [
        mem ("type", Json.Value.string "function");
        mem ("function", Json.Value.object' (List.map mem fields));
      ]
end

(* ===================================================================== *)
(* GLM tool-call grammar                                                  *)
(* ===================================================================== *)

(* A GLM call is one block per invocation, with untyped values:

     <tool_call>NAME<arg_key>K</arg_key><arg_value>V</arg_value>...</tool_call>

   The grammar carries no string="..." flag, so a decoded value's type is
   settled by reading it: a value that parses as JSON and is not a JSON string
   is taken as that JSON, and anything else is the raw text. A quoted string
   the model writes stays raw, quotes and all, since the quotes are then what
   it wrote. Encoding mirrors {!encode_arguments_to_dsml}: a string member is
   its raw text, any other member its compact JSON. *)
let glm_argument_value raw =
  match Json.Value.of_string raw with
  | Ok (Json.String _) | Error _ -> Json.Value.string raw
  | Ok j -> j

let glm_encode_tool_call tc =
  let members =
    match Json.Value.of_string tc.arguments with
    | Ok (Json.Object (mems, _)) -> List.map (fun (nm, v) -> (fst nm, v)) mems
    | Ok _ | Error _ -> [ ("arguments", Json.Value.string tc.arguments) ]
  in
  let arg (k, v) =
    let raw =
      match v with
      | Json.String (s, _) -> s
      | other -> Json.Value.to_string other
    in
    glm_arg_key_open
    ^ escape_text ~close:glm_arg_key_close k
    ^ glm_arg_key_close ^ glm_arg_value_open
    ^ escape_text ~close:glm_arg_value_close raw
    ^ glm_arg_value_close
  in
  glm_tool_call_open ^ tc.name
  ^ String.concat "" (List.map arg members)
  ^ glm_tool_call_close

(* Decode one whole block, [glm_tool_call_open] through
   [glm_tool_call_close]. Raises [Parse_error] on anything else, and the
   caller reports the block as the text it was. *)
let glm_decode_tool_call block =
  let open_len = String.length glm_tool_call_open in
  let close_len = String.length glm_tool_call_close in
  if
    (not (starts_with block ~prefix:glm_tool_call_open))
    || (not (ends_with block ~suffix:glm_tool_call_close))
    || String.length block < open_len + close_len
  then raise (Parse_error "Malformed GLM tool-call block");
  let inner =
    String.sub block open_len (String.length block - open_len - close_len)
  in
  let name_end =
    match find_from inner 0 glm_arg_key_open with
    | Some p -> p
    | None -> String.length inner
  in
  let name = String.trim (String.sub inner 0 name_end) in
  if name = "" || String.exists (fun c -> c <= ' ' || c = '<') name then
    raise (Parse_error (Printf.sprintf "Invalid tool name: '%s'" name));
  let args = ref [] and i = ref name_end in
  let expect tag =
    let l = String.length tag in
    if !i + l > String.length inner || String.sub inner !i l <> tag then
      raise (Parse_error (Printf.sprintf "Expected %s in GLM tool call" tag));
    i := !i + l
  in
  let until tag =
    match find_from inner !i tag with
    | None ->
        raise
          (Parse_error (Printf.sprintf "Unterminated %s in GLM tool call" tag))
    | Some p ->
        let s = String.sub inner !i (p - !i) in
        i := p + String.length tag;
        s
  in
  while !i < String.length inner do
    expect glm_arg_key_open;
    let k = unescape_text ~close:glm_arg_key_close (until glm_arg_key_close) in
    if k = "" then raise (Parse_error "Empty <arg_key> in GLM tool call");
    if List.mem_assoc k !args then
      raise (Parse_error (Printf.sprintf "Duplicate parameter name: '%s'" k));
    expect glm_arg_value_open;
    let v =
      unescape_text ~close:glm_arg_value_close (until glm_arg_value_close)
    in
    args := (k, v) :: !args
  done;
  let members =
    List.rev_map
      (fun (k, v) ->
        Json.Value.member (Json.Value.name k) (glm_argument_value v))
      !args
  in
  {
    id = None;
    name;
    arguments = Json.Value.to_string (Json.Value.object' members);
  }

(* The system-prompt paragraph advertising the tools, worded as upstream's
   agent words it, so the model meets the grammar it was trained on. *)
let glm_tools_template_head =
  "# Tools\n\n\
   You may call one or more functions to assist with the user query.\n\n\
   You are provided with function signatures within <tools></tools> XML tags:\n\
   <tools>\n"

let glm_tools_template_tail =
  "\n\
   </tools>\n\n\
   For a function call, output the function name and arguments within exactly \
   this XML format:\n\
   <tool_call>{function-name}<arg_key>{arg-key-1}</arg_key><arg_value>{arg-value-1}</arg_value><arg_key>{arg-key-2}</arg_key><arg_value>{arg-value-2}</arg_value>...</tool_call>\n\n\
   Tool calls are not allowed inside <think></think>; finish thinking before \
   emitting <tool_call>.\n\n\
   Only if an argument value itself contains the exact text </arg_value>, \
   write it as &lt;/arg_value> inside the value. Nothing else is escaped.\n"

let glm_render_tools (functions : Json.t list) =
  glm_tools_template_head
  ^ String.concat "\n" (List.map Json.Value.to_string functions)
  ^ glm_tools_template_tail

(* ===================================================================== *)
(* Qwen tool-call grammar                                                 *)
(* ===================================================================== *)

(* A Qwen call is one block per invocation, each value on lines of its own:

     <tool_call>
     <function=NAME>
     <parameter=K>
     V
     </parameter>
     </function>
     </tool_call>

   As in GLM, values are untyped and read by {!glm_argument_value}. A value
   runs to the first </parameter> after it and loses the newline on either
   side, so it may hold any other tag, <tool_call> and </tool_call> among
   them. *)
let qwen_encode_tool_call tc =
  let members =
    match Json.Value.of_string tc.arguments with
    | Ok (Json.Object (mems, _)) -> List.map (fun (nm, v) -> (fst nm, v)) mems
    | Ok _ | Error _ -> [ ("arguments", Json.Value.string tc.arguments) ]
  in
  let param (k, v) =
    let raw =
      match v with
      | Json.String (s, _) -> s
      | other -> Json.Value.to_string other
    in
    qwen_parameter_open ^ k ^ ">\n"
    ^ escape_text ~close:qwen_parameter_close raw
    ^ "\n" ^ qwen_parameter_close ^ "\n"
  in
  glm_tool_call_open ^ "\n" ^ qwen_function_open ^ tc.name ^ ">\n"
  ^ String.concat "" (List.map param members)
  ^ qwen_function_close ^ "\n" ^ glm_tool_call_close

let is_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

(* The index of the </tool_call> that closes the block opened at the start of
   [text], skipping any inside a parameter value, or None while the block is
   still open. *)
let qwen_block_close text =
  let rec scan i =
    let param = find_from text i qwen_parameter_open in
    let close = find_from text i glm_tool_call_close in
    match (param, close) with
    | Some p, Some c when c < p -> Some c
    | None, close -> close
    | Some p, _ -> (
        match find_from text p ">" with
        | None -> None
        | Some gt -> (
            match find_from text (gt + 1) qwen_parameter_close with
            | None -> None
            | Some q -> scan (q + String.length qwen_parameter_close)))
  in
  scan (String.length glm_tool_call_open)

(* Decode one whole block, [glm_tool_call_open] through [glm_tool_call_close].
   Raises [Parse_error] on anything else. *)
let qwen_decode_tool_call block =
  let open_len = String.length glm_tool_call_open in
  let close_len = String.length glm_tool_call_close in
  if
    (not (starts_with block ~prefix:glm_tool_call_open))
    || (not (ends_with block ~suffix:glm_tool_call_close))
    || String.length block < open_len + close_len
  then raise (Parse_error "Malformed Qwen tool-call block");
  let inner =
    String.sub block open_len (String.length block - open_len - close_len)
  in
  let n = String.length inner in
  let i = ref 0 in
  let skip_space () =
    while !i < n && is_space inner.[!i] do
      incr i
    done
  in
  let at tag =
    let l = String.length tag in
    !i + l <= n && String.sub inner !i l = tag
  in
  let expect tag what =
    if not (at tag) then
      raise (Parse_error (Printf.sprintf "Expected %s in Qwen tool call" what));
    i := !i + String.length tag
  in
  (* The name inside [<function=NAME>] or [<parameter=NAME>]. *)
  let tag_name what =
    match String.index_from_opt inner !i '>' with
    | None ->
        raise
          (Parse_error (Printf.sprintf "Unterminated %s in Qwen tool call" what))
    | Some gt ->
        let name = String.trim (String.sub inner !i (gt - !i)) in
        i := gt + 1;
        if name = "" || String.exists (fun c -> c <= ' ' || c = '<') name then
          raise
            (Parse_error
               (Printf.sprintf "Invalid %s name in Qwen tool call: '%s'" what
                  name));
        name
  in
  skip_space ();
  expect qwen_function_open "<function=NAME>";
  let name = tag_name "function" in
  let args = ref [] in
  let rec params () =
    skip_space ();
    if at qwen_function_close then begin
      i := !i + String.length qwen_function_close;
      skip_space ();
      if !i < n then
        raise
          (Parse_error "Unexpected text after </function> in Qwen tool call")
    end
    else begin
      expect qwen_parameter_open "<parameter=NAME> or </function>";
      let k = tag_name "parameter" in
      if List.mem_assoc k !args then
        raise (Parse_error (Printf.sprintf "Duplicate parameter name: '%s'" k));
      match find_from inner !i qwen_parameter_close with
      | None -> raise (Parse_error "Unterminated <parameter> in Qwen tool call")
      | Some close ->
          let v = String.sub inner !i (close - !i) in
          let v = if starts_with v ~prefix:"\n" then sub_from v 1 else v in
          let v =
            if ends_with v ~suffix:"\n" then String.sub v 0 (String.length v - 1)
            else v
          in
          args := (k, unescape_text ~close:qwen_parameter_close v) :: !args;
          i := close + String.length qwen_parameter_close;
          params ()
    end
  in
  params ();
  let members =
    List.rev_map
      (fun (k, v) ->
        Json.Value.member (Json.Value.name k) (glm_argument_value v))
      !args
  in
  {
    id = None;
    name;
    arguments = Json.Value.to_string (Json.Value.object' members);
  }

(* The official template's paragraph advertising the tools, followed by the
   rule upstream's agent adds about thinking. *)
let qwen_tools_template_tail =
  "\n\
   </tools>\n\n\
   If you choose to call a function ONLY reply in the following format with NO \
   suffix:\n\n\
   <tool_call>\n\
   <function=example_function_name>\n\
   <parameter=example_parameter_1>\n\
   value_1\n\
   </parameter>\n\
   <parameter=example_parameter_2>\n\
   This is the value for the second parameter\n\
   that can span\n\
   multiple lines\n\
   </parameter>\n\
   </function>\n\
   </tool_call>\n\n\
   <IMPORTANT>\n\
   Reminder:\n\
   - Function calls MUST follow the specified format: an inner \
   <function=...></function> block must be nested within \
   <tool_call></tool_call> XML tags\n\
   - Required parameters MUST be specified\n\
   - You may provide optional reasoning for your function call in natural \
   language BEFORE the function call, but NOT after\n\
   - If there is no function call available, answer the question like normal \
   with your current knowledge and do not tell the user about function calls\n\
   </IMPORTANT>\n\n\
   Tool calls are not allowed inside <think></think>; finish thinking before \
   emitting <tool_call>.\n\n\
   Only if a parameter value itself contains the exact text </parameter>, \
   write it as &lt;/parameter> inside the value. Nothing else is escaped.\n"

let qwen_render_tools (functions : Json.t list) =
  "# Tools\n\nYou have access to the following functions:\n\n<tools>"
  ^ String.concat ""
      (List.map
         (fun f ->
           "\n{\"type\": \"function\", \"function\": " ^ Json.Value.to_string f
           ^ "}")
         functions)
  ^ qwen_tools_template_tail

(* ===================================================================== *)
(* Stream: incremental completion decoding                               *)
(* ===================================================================== *)

module Stream = struct
  type event =
    | Reasoning of string
    | Content of string
    | Tool_call of tool_call
    | Tool_error of { message : string; generated : string }
    | Done

  type phase =
    | In_reasoning
    | In_content
    | In_tool_calls
    | After_tool_calls
    | Ended

  type t = {
    dialect : dialect;
    mutable phase : phase;
    mutable buf : string;
    mutable tc : string;
  }

  let create ?(dialect = Deepseek) = function
    | Thinking -> { dialect; phase = In_reasoning; buf = ""; tc = "" }
    | Chat -> { dialect; phase = In_content; buf = ""; tc = "" }

  (* What ends the content of a turn, and what opens a tool-call block. A GLM
     model ends its turns with role markers as well as its end token, and GLM
     and Qwen calls are one block each rather than one block of invokes. Qwen's
     stops are the two tokens the engine stops generation at. *)
  let content_stops = function
    | Deepseek -> [ tool_calls_start v4_tags; eos_token ]
    | Deepseek41 -> [ tool_calls_start v41_tags; eos_token ]
    | Qwen -> [ glm_tool_call_open; qwen_im_end_token; qwen_endoftext_token ]
    | Glm ->
        [
          glm_tool_call_open;
          glm_eos_token;
          glm_user_token;
          glm_observation_token;
          glm_assistant_token;
          glm_system_token;
        ]

  let call_opener = function
    | Deepseek -> tool_calls_open v4_tags
    | Deepseek41 -> tool_calls_open v41_tags
    | Glm | Qwen -> glm_tool_call_open

  let call_closer = function
    | Deepseek -> tool_calls_end v4_tags
    | Deepseek41 -> tool_calls_end v41_tags
    | Glm | Qwen -> glm_tool_call_close

  let opens_call dialect tok =
    match dialect with
    | Deepseek -> tok = tool_calls_start v4_tags
    | Deepseek41 -> tok = tool_calls_start v41_tags
    | Glm | Qwen -> tok = glm_tool_call_open

  (* Earliest complete needle in [buf], if any. *)
  let earliest buf needles =
    List.fold_left
      (fun acc nd ->
        match find_from buf 0 nd with
        | Some p -> (
            match acc with
            | Some (bp, _, _) when bp <= p -> acc
            | _ -> Some (p, nd, String.length nd))
        | None -> acc)
      None needles

  (* Length of the longest suffix of [buf] that is a proper prefix of a needle:
     bytes we must hold back as a possible split token. *)
  let held buf needles =
    let n = String.length buf in
    List.fold_left
      (fun acc nd ->
        let maxk = min n (String.length nd - 1) in
        let rec try_k k =
          if k <= acc then acc
          else if String.sub buf (n - k) k = String.sub nd 0 k then k
          else try_k (k - 1)
        in
        try_k maxk)
      0 needles

  let feed t chunk =
    t.buf <- t.buf ^ chunk;
    let evs = ref [] in
    let emit e = evs := e :: !evs in
    let emit_text mk s = if s <> "" then emit (mk s) in
    let again = ref true in
    while !again do
      match t.phase with
      | Ended -> again := false
      | In_reasoning -> (
          match earliest t.buf [ thinking_end_token ] with
          | Some (p, _, l) ->
              emit_text (fun s -> Reasoning s) (String.sub t.buf 0 p);
              t.buf <- sub_from t.buf (p + l);
              t.phase <- In_content
          | None ->
              let h = held t.buf [ thinking_end_token ] in
              let n = String.length t.buf in
              emit_text (fun s -> Reasoning s) (String.sub t.buf 0 (n - h));
              t.buf <- String.sub t.buf (n - h) h;
              again := false)
      | In_content -> (
          let stops = content_stops t.dialect in
          match earliest t.buf stops with
          | Some (p, tok, l) ->
              emit_text (fun s -> Content s) (String.sub t.buf 0 p);
              t.buf <- sub_from t.buf (p + l);
              if opens_call t.dialect tok then (
                t.tc <- call_opener t.dialect;
                t.phase <- In_tool_calls)
              else (
                emit Done;
                t.phase <- Ended)
          | None ->
              let h = held t.buf stops in
              let n = String.length t.buf in
              emit_text (fun s -> Content s) (String.sub t.buf 0 (n - h));
              t.buf <- String.sub t.buf (n - h) h;
              again := false)
      | In_tool_calls -> (
          (* The block and what follows it, once the block has closed. *)
          let closed =
            match t.dialect with
            | Qwen -> (
                (* A Qwen value may hold </tool_call>, so the closer is looked
                   for outside the values rather than taken at its first
                   occurrence. The scan starts from the opener each time, which
                   is cheap for a block no longer than one reply. *)
                let text = t.tc ^ t.buf in
                match qwen_block_close text with
                | Some p ->
                    let l = p + String.length glm_tool_call_close in
                    Some (String.sub text 0 l, sub_from text l)
                | None ->
                    t.tc <- text;
                    t.buf <- "";
                    None)
            | Deepseek | Deepseek41 | Glm -> (
                let closer = call_closer t.dialect in
                match earliest t.buf [ closer ] with
                | Some (p, _, l) ->
                    Some
                      ( t.tc ^ String.sub t.buf 0 p ^ closer,
                        sub_from t.buf (p + l) )
                | None ->
                    let h = held t.buf [ closer ] in
                    let n = String.length t.buf in
                    t.tc <- t.tc ^ String.sub t.buf 0 (n - h);
                    t.buf <- String.sub t.buf (n - h) h;
                    None)
          in
          match closed with
          | None -> again := false
          | Some (block, rest) -> (
              t.buf <- rest;
              t.tc <- "";
              (* A block the codec refuses is reported as the text it was, with
                 the special tokens it carries made inert, and never silenced.
                 The model wrote it, so the caller has to see it to know why no
                 call arrived.  Decoding then goes on as content, since a model
                 that writes a block this way commonly writes its answer after
                 it, and the discard to EOS that follows a well-formed block
                 would lose that answer.  Text reaching a caller by the ordinary
                 content path is not rewritten, so markup the model writes
                 mid-sentence, with no blank line before it, still arrives
                 whole. *)
              let refused message =
                emit
                  (Tool_error
                     {
                       message;
                       generated = neutralise_specials ~dialect:t.dialect block;
                     });
                t.phase <- In_content
              in
              match t.dialect with
              | Deepseek | Deepseek41 -> (
                  let tags =
                    if t.dialect = Deepseek41 then v41_tags else v4_tags
                  in
                  match
                    Codec.decode_block ~tags Codec.dynamic block
                      (String.length (tool_calls_open tags))
                  with
                  | calls, _ ->
                      List.iter (fun c -> emit (Tool_call c)) calls;
                      t.phase <- After_tool_calls
                  | exception Parse_error message -> refused message)
              | Glm | Qwen -> (
                  (* One block is one call, and the model may write another
                     block, or its answer, after it, so decoding stays in
                     content either way. *)
                  let decode =
                    if t.dialect = Glm then glm_decode_tool_call
                    else qwen_decode_tool_call
                  in
                  match decode block with
                  | call ->
                      emit (Tool_call call);
                      t.phase <- In_content
                  | exception Parse_error message -> refused message)))
      | After_tool_calls -> (
          match earliest t.buf [ eos_token ] with
          | Some (p, _, l) ->
              t.buf <- sub_from t.buf (p + l);
              emit Done;
              t.phase <- Ended
          | None ->
              let h = held t.buf [ eos_token ] in
              let n = String.length t.buf in
              t.buf <- String.sub t.buf (n - h) h;
              again := false)
    done;
    List.rev !evs

  (* Asked before [finish], which clears the phase. *)
  let in_tool_call t = t.phase = In_tool_calls

  let last_index s needle =
    let rec loop from found =
      match find_from s from needle with
      | None -> found
      | Some at -> loop (at + 1) (Some at)
    in
    loop 0 None

  let suffix_is_prefix ~needle s =
    let n = String.length s and m = String.length needle in
    let rec loop len =
      if len <= 1 then false
      else if String.sub s (n - len) len = String.sub needle 0 len then true
      else loop (len - 1)
    in
    loop (min n (m - 1))

  let sampling_mode t =
    match t.phase with
    | In_content ->
        if held t.buf (content_stops t.dialect) > 1 then `Greedy
        else `Configured
    | In_tool_calls ->
        let text = t.tc ^ t.buf in
        let value_open, value_close =
          match t.dialect with
          | Deepseek -> (parameter_start v4_tags, "<" ^ parameter_slash v4_tags)
          | Deepseek41 ->
              (parameter_start v41_tags, "<" ^ parameter_slash v41_tags)
          | Glm -> (glm_arg_value_open, glm_arg_value_close)
          | Qwen -> (qwen_parameter_open, qwen_parameter_close)
        in
        (* A GLM value opens with its tag, and the others once the tag that
           names the parameter has closed. *)
        let opened_value opened =
          match t.dialect with
          | Glm -> true
          | Deepseek | Deepseek41 | Qwen -> find_from text opened ">" <> None
        in
        let in_value =
          match (last_index text value_open, last_index text value_close) with
          | Some opened, Some closed when opened > closed -> opened_value opened
          | Some opened, None -> opened_value opened
          | _ -> false
        in
        if not in_value then `Greedy
        else if suffix_is_prefix ~needle:value_close text then `Greedy
        else `Configured
    | In_reasoning | After_tool_calls | Ended -> `Configured

  let finish t =
    let evs = ref [] in
    (match t.phase with
    | In_reasoning -> if t.buf <> "" then evs := [ Reasoning t.buf ]
    | In_content -> if t.buf <> "" then evs := [ Content t.buf ]
    (* A block left open when the stream ends is reported the same way.  It
       holds at least the token that opened it, so there is always text. *)
    | In_tool_calls ->
        evs :=
          [
            Tool_error
              {
                message = "incomplete tool call";
                generated = neutralise_specials ~dialect:t.dialect (t.tc ^ t.buf);
              };
          ]
    | _ -> ());
    let tail = if t.phase = Ended then [] else [ Done ] in
    t.buf <- "";
    t.tc <- "";
    t.phase <- Ended;
    !evs @ tail
end

(* ===================================================================== *)
(* Message rendering                                                      *)
(* ===================================================================== *)

let render_response_format rf =
  "## Response Format:\n\n\
   You MUST strictly adhere to the following schema to reply:\n"
  ^ Json.Value.to_string rf

let tools_template_head =
  "## Tools\n\n\
   You have access to a set of tools to help answer the user's question. You \
   can invoke tools by writing a \"<\xef\xbd\x9cDSML\xef\xbd\x9ctool_calls>\" \
   block like the following:\n\n\
   <\xef\xbd\x9cDSML\xef\xbd\x9ctool_calls>\n\
   <\xef\xbd\x9cDSML\xef\xbd\x9cinvoke name=\"$TOOL_NAME\">\n\
   <\xef\xbd\x9cDSML\xef\xbd\x9cparameter name=\"$PARAMETER_NAME\" \
   string=\"true|false\">$PARAMETER_VALUE</\xef\xbd\x9cDSML\xef\xbd\x9cparameter>\n\
   ...\n\
   </\xef\xbd\x9cDSML\xef\xbd\x9cinvoke>\n\
   <\xef\xbd\x9cDSML\xef\xbd\x9cinvoke name=\"$TOOL_NAME2\">\n\
   ...\n\
   </\xef\xbd\x9cDSML\xef\xbd\x9cinvoke>\n\
   </\xef\xbd\x9cDSML\xef\xbd\x9ctool_calls>\n\n\
   String parameters should be specified as is and set `string=\"true\"`. For \
   all other types (numbers, booleans, arrays, objects), pass the value in \
   JSON format and set `string=\"false\"`.\n\n\
   Only if a string value itself contains the exact closing tag \
   `</\xef\xbd\x9cDSML\xef\xbd\x9cparameter>`, write that tag as \
   `&lt;/\xef\xbd\x9cDSML\xef\xbd\x9cparameter>` inside the value, and write a \
   literal `&lt;/\xef\xbd\x9cDSML\xef\xbd\x9cparameter>` as \
   `&amp;lt;/\xef\xbd\x9cDSML\xef\xbd\x9cparameter>`. Nothing else is \
   escaped.\n\n\
   If thinking_mode is enabled (triggered by <think>), you MUST output your \
   complete reasoning inside <think>...</think> BEFORE any tool calls or final \
   response.\n\n\
   Otherwise, output directly after </think> with tool calls or final \
   response.\n\n\
   ### Available Tool Schemas\n\n"

let tools_template_tail =
  "\n\n\
   You MUST strictly follow the above defined tool name and parameter schemas \
   to invoke tool calls.\n"

let render_tools (functions : Json.t list) =
  tools_template_head
  ^ String.concat "\n" (List.map Json.Value.to_string functions)
  ^ tools_template_tail

(* V4.1's paragraph, in the wording of upstream's server. *)
let v41_tools_template_head =
  "## Tools\n\n\
   You can invoke tools using this format:\n\n\
   <\xef\xbd\x9cDSML\xef\xbd\x9c calls>\n\
   <\xef\xbd\x9cDSML\xef\xbd\x9c invoke name=\"$TOOL_NAME\">\n\
   <\xef\xbd\x9cDSML\xef\xbd\x9c parameter name=\"$PARAMETER_NAME\" \
   string=\"true|false\">$PARAMETER_VALUE</\xef\xbd\x9cDSML\xef\xbd\x9c \
   parameter>\n\
   </\xef\xbd\x9cDSML\xef\xbd\x9c invoke>\n\
   </\xef\xbd\x9cDSML\xef\xbd\x9c calls>\n\n\
   String values use string=\"true\"; all other values use JSON and \
   string=\"false\".\n\n\
   Only if a string value itself contains the exact closing tag \
   `</\xef\xbd\x9cDSML\xef\xbd\x9c parameter>`, write that tag as \
   `&lt;/\xef\xbd\x9cDSML\xef\xbd\x9c parameter>` inside the value. Nothing \
   else is escaped.\n\n\
   Finish reasoning with </think> before tool calls or a final response.\n\n\
   ### Available Tool Schemas\n\n"

let v41_tools_template_tail =
  "\n\n\
   You MUST strictly follow the above defined tool name and parameter schemas \
   to invoke tool calls. Use the exact parameter names from the schemas.\n"

let v41_render_tools (functions : Json.t list) =
  v41_tools_template_head
  ^ String.concat "\n" (List.map Json.Value.to_string functions)
  ^ v41_tools_template_tail

let tool_prompt ?(dialect = Deepseek) functions =
  match dialect with
  | Deepseek -> render_tools (tools_from_openai functions)
  | Deepseek41 -> v41_render_tools (tools_from_openai functions)
  | Glm -> glm_render_tools functions
  | Qwen -> qwen_render_tools (tools_from_openai functions)

let find_last_user_index msgs =
  let r = ref (-1) in
  Array.iteri
    (fun i m -> if m.role = User || m.role = Developer then r := i)
    msgs;
  !r

let render_tool_result_content = function
  | Json.String (s, _) -> s
  | Json.Array (items, _) ->
      String.concat "\n\n"
        (List.map
           (fun b ->
             match json_member "type" b with
             | Some (Json.String ("text", _)) -> (
                 match json_member "text" b with
                 | Some (Json.String (t, _)) -> t
                 | _ -> "")
             | Some (Json.String (ty, _)) ->
                 Printf.sprintf "[Unsupported %s]" ty
             | Some other ->
                 Printf.sprintf "[Unsupported %s]" (Json.Value.to_string other)
             | None -> "[Unsupported None]")
           items)
  | other -> Json.Value.to_string other

(* V4.1's effort line, which the engine writes as a system turn ahead of the
   conversation whenever thinking is on. *)
let v41_reasoning_effort = function
  | Some Max ->
      "Reasoning Effort: 100 (range 1-100, the higher the value, the more \
       thorough the reasoning)\n\n"
  | Some High | None ->
      "Reasoning Effort: 75 (range 1-100, the higher the value, the more \
       thorough the reasoning)\n\n"

let render_message ~v41 msgs index ~thinking_mode ~drop_thinking
    ~reasoning_effort =
  let n = Array.length msgs in
  let msg = msgs.(index) in
  let last_user_idx = find_last_user_index msgs in
  let tags = if v41 then v41_tags else v4_tags in
  let b = Buffer.create 256 in
  if index = 0 && thinking_mode = Thinking then
    begin if v41 then begin
      Buffer.add_string b deepseek41_system_token;
      Buffer.add_string b (v41_reasoning_effort reasoning_effort)
    end
    else if reasoning_effort = Some Max then
      Buffer.add_string b reasoning_effort_max
    end;
  (match msg.role with
  | System -> (
      (* V4.1 marks a system turn, and the effort line has already opened the
         first one when thinking is on. *)
      if v41 && not (index = 0 && thinking_mode = Thinking) then
        Buffer.add_string b deepseek41_system_token;
      Buffer.add_string b msg.content;
      if msg.tools <> [] then (
        Buffer.add_string b "\n\n";
        Buffer.add_string b
          ((if v41 then v41_render_tools else render_tools)
             (tools_from_openai msg.tools)));
      match msg.response_format with
      | Some rf -> Buffer.add_string b ("\n\n" ^ render_response_format rf)
      | None -> ())
  | Developer -> (
      Buffer.add_string b user_token;
      Buffer.add_string b msg.content;
      if msg.tools <> [] then (
        Buffer.add_string b "\n\n";
        Buffer.add_string b
          ((if v41 then v41_render_tools else render_tools)
             (tools_from_openai msg.tools)));
      match msg.response_format with
      | Some rf -> Buffer.add_string b ("\n\n" ^ render_response_format rf)
      | None -> ())
  | User -> (
      Buffer.add_string b user_token;
      match msg.content_blocks with
      | Some blocks ->
          Buffer.add_string b
            (String.concat "\n\n"
               (List.map
                  (function
                    | Text t -> t
                    | Tool_result { content; _ } ->
                        "<tool_result>"
                        ^ render_tool_result_content content
                        ^ "</tool_result>")
                  blocks))
      | None -> Buffer.add_string b msg.content)
  | Latest_reminder ->
      Buffer.add_string b latest_reminder_token;
      Buffer.add_string b msg.content
  | Tool_role ->
      raise
        (Failure
           "Dsml: tool messages must be merged into user messages before \
            rendering (use encode_messages)")
  | Assistant ->
      let tc_content =
        if msg.tool_calls <> [] then
          "\n\n" ^ Codec.encode_block ~tags Codec.dynamic msg.tool_calls
        else ""
      in
      let prev_has_task = index - 1 >= 0 && msgs.(index - 1).task <> None in
      let thinking_part =
        if thinking_mode = Thinking && not prev_has_task then
          if (not drop_thinking) || index > last_user_idx then
            msg.reasoning_content ^ thinking_end_token
          else ""
        else ""
      in
      Buffer.add_string b thinking_part;
      Buffer.add_string b msg.content;
      Buffer.add_string b tc_content;
      if not msg.wo_eos then Buffer.add_string b eos_token);
  (* Transition tokens: skipped if the next message is a non-assistant,
     non-latest_reminder turn (it emits its own prefix). *)
  let early_return =
    index + 1 < n
    &&
    let r = msgs.(index + 1).role in
    r <> Assistant && r <> Latest_reminder
  in
  if not early_return then
    begin match msg.task with
    | Some t ->
        if t <> Action then Buffer.add_string b (task_token t)
        else begin
          Buffer.add_string b assistant_token;
          Buffer.add_string b
            (if thinking_mode <> Thinking then thinking_end_token
             else thinking_start_token);
          Buffer.add_string b (task_token t)
        end
    | None -> (
        match msg.role with
        | User | Developer ->
            Buffer.add_string b assistant_token;
            if (not drop_thinking) && thinking_mode = Thinking then
              Buffer.add_string b thinking_start_token
            else if
              drop_thinking && thinking_mode = Thinking
              && index >= last_user_idx
            then Buffer.add_string b thinking_start_token
            else Buffer.add_string b thinking_end_token
        | _ -> ())
    end;
  Buffer.contents b

(* ===================================================================== *)
(* Preprocessing                                                          *)
(* ===================================================================== *)

let merge_tool_messages (messages : message list) : message list =
  let merged = ref [] in
  List.iter
    (fun msg ->
      match msg.role with
      | Tool_role -> (
          let block =
            Tool_result
              {
                tool_use_id =
                  (match msg.tool_call_id with Some s -> s | None -> "");
                content = Json.Value.string msg.content;
              }
          in
          match !merged with
          | prev :: rest when prev.role = User && prev.content_blocks <> None ->
              let blocks =
                match prev.content_blocks with Some b -> b | None -> []
              in
              merged :=
                { prev with content_blocks = Some (blocks @ [ block ]) } :: rest
          | _ ->
              merged :=
                { (base User) with content_blocks = Some [ block ] } :: !merged)
      | User -> (
          let block = Text msg.content in
          match !merged with
          | prev :: rest
            when prev.role = User
                 && prev.content_blocks <> None
                 && prev.task = None ->
              let blocks =
                match prev.content_blocks with Some b -> b | None -> []
              in
              merged :=
                { prev with content_blocks = Some (blocks @ [ block ]) } :: rest
          | _ ->
              merged :=
                {
                  (base User) with
                  content = msg.content;
                  content_blocks = Some [ block ];
                  task = msg.task;
                  wo_eos = msg.wo_eos;
                }
                :: !merged)
      | _ -> merged := msg :: !merged)
    messages;
  List.rev !merged

let sort_tool_results_by_call_order (messages : message list) : message list =
  let last_order : (string, int) Hashtbl.t = Hashtbl.create 8 in
  let rec go acc = function
    | [] -> List.rev acc
    | msg :: tl ->
        let msg =
          match msg.role with
          | Assistant when msg.tool_calls <> [] ->
              Hashtbl.reset last_order;
              List.iteri
                (fun idx tc ->
                  match tc.id with
                  | Some id when id <> "" -> Hashtbl.replace last_order id idx
                  | _ -> ())
                msg.tool_calls;
              msg
          | User -> (
              match msg.content_blocks with
              | Some blocks ->
                  let tool_blocks =
                    List.filter
                      (function Tool_result _ -> true | _ -> false)
                      blocks
                  in
                  if
                    List.length tool_blocks > 1 && Hashtbl.length last_order > 0
                  then begin
                    let key = function
                      | Tool_result { tool_use_id; _ } -> (
                          match Hashtbl.find_opt last_order tool_use_id with
                          | Some i -> i
                          | None -> 0)
                      | _ -> 0
                    in
                    let sorted =
                      List.stable_sort
                        (fun a b -> compare (key a) (key b))
                        tool_blocks
                    in
                    let arr = Array.of_list sorted in
                    let i = ref 0 in
                    let new_blocks =
                      List.map
                        (function
                          | Tool_result _ ->
                              let blk = arr.(!i) in
                              incr i;
                              blk
                          | other -> other)
                        blocks
                    in
                    { msg with content_blocks = Some new_blocks }
                  end
                  else msg
              | None -> msg)
          | _ -> msg
        in
        go (msg :: acc) tl
  in
  go [] messages

let drop_thinking_messages (messages : message list) : message list =
  let arr = Array.of_list messages in
  let last_user_idx = find_last_user_index arr in
  let keep = function
    | User | System | Tool_role | Latest_reminder -> true
    | _ -> false
  in
  let out = ref [] in
  Array.iteri
    (fun idx msg ->
      if keep msg.role || idx >= last_user_idx then out := msg :: !out
      else
        match msg.role with
        | Assistant -> out := { msg with reasoning_content = "" } :: !out
        | _ -> ())
    arr;
  List.rev !out

(* ===================================================================== *)
(* Encode                                                                 *)
(* ===================================================================== *)

let drop_n k lst =
  let rec go k = function
    | l when k <= 0 -> l
    | _ :: tl -> go (k - 1) tl
    | [] -> []
  in
  go k lst

let deepseek_encode_messages ~v41 ?(context = []) ?(drop_thinking = true)
    ?(add_default_bos_token = true) ?reasoning_effort thinking_mode messages =
  let messages = merge_tool_messages messages in
  let messages =
    drop_n (List.length context)
      (sort_tool_results_by_call_order (context @ messages))
  in
  let context =
    if context <> [] then
      sort_tool_results_by_call_order (merge_tool_messages context)
    else context
  in
  let full_messages = context @ messages in
  let b = Buffer.create 1024 in
  if add_default_bos_token && context = [] then Buffer.add_string b bos_token;
  let effective_drop =
    if List.exists (fun m -> m.tools <> []) full_messages then false
    else drop_thinking
  in
  let full_arr, num_to_render, context_len =
    if thinking_mode = Thinking && effective_drop then begin
      let fm = drop_thinking_messages full_messages in
      let num = List.length fm - List.length (drop_thinking_messages context) in
      (Array.of_list fm, num, List.length fm - num)
    end
    else (Array.of_list full_messages, List.length messages, List.length context)
  in
  for idx = 0 to num_to_render - 1 do
    Buffer.add_string b
      (render_message ~v41 full_arr (idx + context_len) ~thinking_mode
         ~drop_thinking:effective_drop ~reasoning_effort)
  done;
  Buffer.contents b

(* GLM rendering. The turn structure differs from DeepSeek's in three ways
   that matter here. A system turn has a marker of its own. A tool result is
   an observation turn of its own, wrapped in <tool_response>, rather than
   part of a user turn. And no end token closes an assistant turn: the next
   role marker is the boundary, which is why generation stops at role markers
   as well as at the end token. As in the DeepSeek renderer, the assistant
   marker and the opening think token are emitted by the turn before, so the
   generation prefix falls out of rendering the last message. *)
let glm_keep_reasoning ~thinking_mode ~drop_thinking ~last_user_idx index =
  thinking_mode = Thinking && ((not drop_thinking) || index > last_user_idx)

let glm_render_message msgs index ~thinking_mode ~drop_thinking
    ~reasoning_effort =
  let n = Array.length msgs in
  let msg = msgs.(index) in
  let last_user_idx = find_last_user_index msgs in
  let keep = glm_keep_reasoning ~thinking_mode ~drop_thinking ~last_user_idx in
  let b = Buffer.create 256 in
  (* The engine's own chat encoder puts the effort ahead of the system turn,
     in a system turn of its own, and thinking without an effort line is not a
     form it writes. *)
  if index = 0 && thinking_mode = Thinking then begin
    Buffer.add_string b glm_system_token;
    Buffer.add_string b
      (match reasoning_effort with
      | Some Max -> "Reasoning Effort: Max"
      | _ -> "Reasoning Effort: High")
  end;
  (match msg.role with
  | System | Developer -> (
      Buffer.add_string b glm_system_token;
      Buffer.add_string b msg.content;
      if msg.tools <> [] then (
        Buffer.add_string b "\n\n";
        Buffer.add_string b (glm_render_tools (tools_from_openai msg.tools)));
      match msg.response_format with
      | Some rf -> Buffer.add_string b ("\n\n" ^ render_response_format rf)
      | None -> ())
  | User ->
      Buffer.add_string b glm_user_token;
      Buffer.add_string b msg.content
  | Latest_reminder ->
      (* GLM has no reminder marker, so the turn is a user turn. *)
      Buffer.add_string b glm_user_token;
      Buffer.add_string b msg.content
  | Tool_role ->
      Buffer.add_string b glm_observation_token;
      Buffer.add_string b glm_tool_response_open;
      Buffer.add_string b msg.content;
      Buffer.add_string b glm_tool_response_close
  | Assistant ->
      (* The marker and the opening think token came from the turn before. *)
      if keep index then begin
        Buffer.add_string b msg.reasoning_content;
        Buffer.add_string b thinking_end_token
      end;
      Buffer.add_string b msg.content;
      List.iter
        (fun tc -> Buffer.add_string b (glm_encode_tool_call tc))
        msg.tool_calls);
  (* The transition into the assistant turn that follows, or into the one
     about to be generated when this is the last message. *)
  let opens_assistant = index + 1 >= n || msgs.(index + 1).role = Assistant in
  (match msg.role with
  | (User | Developer | Tool_role | Latest_reminder) when opens_assistant ->
      Buffer.add_string b glm_assistant_token;
      Buffer.add_string b thinking_start_token;
      if not (keep (index + 1)) then Buffer.add_string b thinking_end_token
  | _ -> ());
  Buffer.contents b

(* GLM tool results stay their own turns, so there is no merge and no
   reordering: the messages render in the order they are given. *)
let glm_encode_messages ?(context = []) ?(drop_thinking = true)
    ?(add_default_bos_token = true) ?reasoning_effort thinking_mode messages =
  let full_messages = context @ messages in
  let b = Buffer.create 1024 in
  if add_default_bos_token && context = [] then begin
    Buffer.add_string b glm_bos_token;
    Buffer.add_string b glm_sop_token
  end;
  let effective_drop =
    if List.exists (fun m -> m.tools <> []) full_messages then false
    else drop_thinking
  in
  let full_arr, num_to_render, context_len =
    if thinking_mode = Thinking && effective_drop then begin
      let fm = drop_thinking_messages full_messages in
      let num = List.length fm - List.length (drop_thinking_messages context) in
      (Array.of_list fm, num, List.length fm - num)
    end
    else (Array.of_list full_messages, List.length messages, List.length context)
  in
  for idx = 0 to num_to_render - 1 do
    Buffer.add_string b
      (glm_render_message full_arr (idx + context_len) ~thinking_mode
         ~drop_thinking:effective_drop ~reasoning_effort)
  done;
  Buffer.contents b

(* Qwen rendering, ChatML as the official template writes it. Every turn is
   opened by <|im_start|> and its role and closed by <|im_end|> and a newline.
   The effort instruction, the tools and the system prompt share the first
   system turn. Consecutive tool results share one user turn, each in its own
   <tool_response>. An assistant turn always carries a think span, left empty
   where its reasoning is dropped, which is the span the generation prefix
   writes when thinking is off. *)
let qwen_reasoning_xhigh =
  "Reasoning effort is set to xhigh. Please think carefully through the task, \
   validate key assumptions, consider plausible alternatives, and prioritize \
   correctness, consistency, and clarity in the final answer."

let qwen_open b role =
  Buffer.add_string b qwen_im_start_token;
  Buffer.add_string b role;
  Buffer.add_char b '\n'

let qwen_close b =
  Buffer.add_string b qwen_im_end_token;
  Buffer.add_char b '\n'

let qwen_render_message msgs index ~thinking_mode ~drop_thinking =
  let n = Array.length msgs in
  let msg = msgs.(index) in
  let last_user_idx = find_last_user_index msgs in
  let keep = glm_keep_reasoning ~thinking_mode ~drop_thinking ~last_user_idx in
  let effort = index = 0 && thinking_mode = Thinking in
  let is_tool i = i >= 0 && i < n && msgs.(i).role = Tool_role in
  let b = Buffer.create 256 in
  (match msg.role with
  | System | Developer ->
      let parts =
        (if effort then [ qwen_reasoning_xhigh ] else [])
        @ (if msg.tools <> [] then
             [ qwen_render_tools (tools_from_openai msg.tools) ]
           else [])
        @ (if msg.content <> "" then [ msg.content ] else [])
        @
        match msg.response_format with
        | Some rf -> [ render_response_format rf ]
        | None -> []
      in
      qwen_open b "system";
      Buffer.add_string b (String.concat "\n\n" parts);
      qwen_close b
  | User | Latest_reminder ->
      if effort then begin
        qwen_open b "system";
        Buffer.add_string b qwen_reasoning_xhigh;
        qwen_close b
      end;
      qwen_open b "user";
      Buffer.add_string b msg.content;
      qwen_close b
  | Tool_role ->
      if effort then begin
        qwen_open b "system";
        Buffer.add_string b qwen_reasoning_xhigh;
        qwen_close b
      end;
      if not (is_tool (index - 1)) then begin
        Buffer.add_string b qwen_im_start_token;
        Buffer.add_string b "user"
      end;
      Buffer.add_string b "\n";
      Buffer.add_string b glm_tool_response_open;
      Buffer.add_string b "\n";
      Buffer.add_string b msg.content;
      Buffer.add_string b "\n";
      Buffer.add_string b glm_tool_response_close;
      if not (is_tool (index + 1)) then qwen_close b
  | Assistant ->
      qwen_open b "assistant";
      Buffer.add_string b thinking_start_token;
      Buffer.add_string b "\n";
      if keep index then Buffer.add_string b msg.reasoning_content;
      Buffer.add_string b "\n";
      Buffer.add_string b thinking_end_token;
      Buffer.add_string b "\n\n";
      Buffer.add_string b msg.content;
      List.iteri
        (fun i tc ->
          if i > 0 then Buffer.add_string b "\n"
          else if msg.content <> "" then Buffer.add_string b "\n\n";
          Buffer.add_string b (qwen_encode_tool_call tc))
        msg.tool_calls;
      if not msg.wo_eos then qwen_close b);
  (* The generation prefix, when this is the last message and not the
     model's. *)
  if index + 1 >= n && msg.role <> Assistant then begin
    qwen_open b "assistant";
    Buffer.add_string b thinking_start_token;
    Buffer.add_string b "\n";
    if thinking_mode <> Thinking then begin
      Buffer.add_string b "\n";
      Buffer.add_string b thinking_end_token;
      Buffer.add_string b "\n\n"
    end
  end;
  Buffer.contents b

(* Qwen has no BOS token, so [add_default_bos_token] has nothing to add. *)
let qwen_encode_messages ?(context = []) ?(drop_thinking = true)
    ?add_default_bos_token:_ ?reasoning_effort:_ thinking_mode messages =
  let full_messages = context @ messages in
  let b = Buffer.create 1024 in
  let effective_drop =
    if List.exists (fun m -> m.tools <> []) full_messages then false
    else drop_thinking
  in
  let full_arr, num_to_render, context_len =
    if thinking_mode = Thinking && effective_drop then begin
      let fm = drop_thinking_messages full_messages in
      let num = List.length fm - List.length (drop_thinking_messages context) in
      (Array.of_list fm, num, List.length fm - num)
    end
    else (Array.of_list full_messages, List.length messages, List.length context)
  in
  for idx = 0 to num_to_render - 1 do
    Buffer.add_string b
      (qwen_render_message full_arr (idx + context_len) ~thinking_mode
         ~drop_thinking:effective_drop)
  done;
  Buffer.contents b

let encode_messages ?(dialect = Deepseek) ?context ?drop_thinking
    ?add_default_bos_token ?reasoning_effort thinking_mode messages =
  (match dialect with
  | Deepseek -> deepseek_encode_messages ~v41:false
  | Deepseek41 -> deepseek_encode_messages ~v41:true
  | Glm -> glm_encode_messages
  | Qwen -> qwen_encode_messages)
    ?context ?drop_thinking ?add_default_bos_token ?reasoning_effort
    thinking_mode messages

(* ===================================================================== *)
(* Parse                                                                  *)
(* ===================================================================== *)

let parse_message_from_completion_text thinking_mode text =
  let n = String.length text in
  let index = ref 0 and stop = ref None in
  let reasoning = ref "" and summary = ref "" and calls = ref [] in
  let tool_calls_start = tool_calls_start v4_tags in
  if thinking_mode = Thinking then begin
    let i, content, st =
      read_until_stop text !index [ thinking_end_token; tool_calls_start ]
    in
    index := i;
    reasoning := content;
    stop := st;
    if st <> Some thinking_end_token then
      raise (Parse_error "Invalid thinking format: missing </think>")
  end;
  let i, content, st =
    read_until_stop text !index [ eos_token; tool_calls_start ]
  in
  index := i;
  summary := content;
  stop := st;
  let is_tool_calling = st = Some tool_calls_start in
  if (not is_tool_calling) && st <> Some eos_token then
    raise (Parse_error "Invalid format: missing EOS token");
  if is_tool_calling then begin
    let tcs, j = Codec.decode_block Codec.dynamic text !index in
    index := j;
    calls := tcs;
    let k, tail, st3 = read_until_stop text !index [ eos_token ] in
    index := k;
    stop := st3;
    if tail <> "" then raise (Parse_error "Unexpected content after tool calls")
  end;
  if not (!index = n && (!stop = Some eos_token || !stop = None)) then
    raise (Parse_error "Unexpected content at end");
  List.iter
    (fun sp ->
      if contains !summary sp || contains !reasoning sp then
        raise (Parse_error "Unexpected special token in content"))
    [
      bos_token; eos_token; thinking_start_token; thinking_end_token; dsml_token;
    ];
  { content = !summary; reasoning_content = !reasoning; tool_calls = !calls }

(* ===================================================================== *)
(* bytesrw I/O boundary                                                   *)
(* ===================================================================== *)

let encode_messages_to_writer w ?dialect ?context ?drop_thinking
    ?add_default_bos_token ?reasoning_effort thinking_mode messages =
  let s =
    encode_messages ?dialect ?context ?drop_thinking ?add_default_bos_token
      ?reasoning_effort thinking_mode messages
  in
  Bytesrw.Bytes.Writer.write_string w s

let parse_message_from_reader thinking_mode r =
  parse_message_from_completion_text thinking_mode
    (Bytesrw.Bytes.Reader.to_string r)
