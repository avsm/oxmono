(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for the DSML encoder/parser.

   Oracles:
   - the two Quick Start examples documented in DeepSeek-V4's encoding README;
   - the canonical bash tool-call DSML string asserted by this repo's own C
     tests (ds4_server.c:test_tool_memory_max_ids_prunes_oldest). *)

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n" name
  end

let check_eq name ~expect ~got =
  if expect = got then Printf.printf "ok   - %s\n" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n  expect: %S\n  got:    %S\n" name expect got
  end

open Dsml

(* substring test (the library keeps its own internal version private) *)
let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec at i j =
    j >= m || (i + j < n && s.[i + j] = sub.[j] && at i (j + 1))
  in
  let rec loop i = i + m <= n && (at i 0 || loop (i + 1)) in
  m = 0 || loop 0

let dsml = dsml_token

type edit = { path : string; line : int }

let mk_edit path line = { path; line }

let edit_codec =
  let open Codec in
  Invoke.map "edit" mk_edit
  |> Invoke.param ~enc:(fun e -> e.path) "path" string
  |> Invoke.param ~enc:(fun e -> e.line) "line" int
  |> Invoke.seal

(* A typed dispatch over a heterogeneous block: known tools to records, the rest
   to the generic [tool_call]. *)
type call = An_edit of edit | Other of tool_call

let call_codec =
  Codec.choice
    ~default:
      (Codec.map
         ~dec:(fun tc -> Other tc)
         ~enc:(function Other tc -> tc | _ -> assert false)
         Codec.dynamic)
    [
      Codec.case
        ~inject:(fun e -> An_edit e)
        ~project:(function An_edit e -> Some e | _ -> None)
        edit_codec;
    ]

(* Canonical one-call DSML block (matches the C repo's a_dsml literal). *)
let bash_a_block =
  Printf.sprintf
    "\n\n\
     <%stool_calls>\n\
     <%sinvoke name=\"bash\">\n\
     <%sparameter name=\"command\" string=\"true\">a</%sparameter>\n\
     </%sinvoke>\n\
     </%stool_calls>"
    dsml dsml dsml dsml dsml dsml

(* A call to a tool that takes no parameters, as captured from DeepSeek-V4 and
   as the encoder renders it. The invoke body is empty, which leaves the blank
   line between the two invoke tags. *)
let project_block =
  Printf.sprintf
    "<%stool_calls>\n\
     <%sinvoke name=\"project\">\n\n\
     </%sinvoke>\n\
     </%stool_calls>"
    dsml dsml dsml dsml

(* Two blocks the decoder must refuse, written over [tok] so that a test can
   name both what is fed (the real special tokens) and what is expected back
   (the inert forms the stream neutralises them to). The first has a parameter
   tag that never closes, the second stops in the middle. [extra] goes at the
   end of the parameter value, for a block carrying another special token. *)
let bad_block tok extra =
  Printf.sprintf
    "<%stool_calls>\n\
     <%sinvoke name=\"f\">\n\
     <%sparameter name=\"x\" string=\"true\">v%s\n\
     </%sinvoke>\n\
     </%stool_calls>"
    tok tok tok extra tok tok

let open_block tok =
  Printf.sprintf "<%stool_calls>\n<%sinvoke name=\"project\">" tok tok

(* Every marker the library names. Text it hands back as content, and text it
   records into a conversation, must carry none of them. *)
let specials =
  [
    bos_token;
    eos_token;
    thinking_start_token;
    thinking_end_token;
    dsml;
    user_token;
    assistant_token;
    latest_reminder_token;
  ]

let no_special s = List.for_all (fun tk -> not (contains s tk)) specials

(* [chunks_of n s] is [s] cut into pieces of [n] bytes, so that a stream fed
   with them is split inside multi-byte tokens. *)
let chunks_of n s =
  let rec go i acc =
    if i >= String.length s then List.rev acc
    else
      let k = min n (String.length s - i) in
      go (i + k) (String.sub s i k :: acc)
  in
  go 0 []

let stream_events mode chunks =
  let s = Stream.create mode in
  let evs = List.concat_map (Stream.feed s) chunks in
  evs @ Stream.finish s

let stream_text evs =
  String.concat ""
    (List.filter_map
       (function
         | Stream.Content c -> Some c
         | Stream.Tool_error { generated; _ } -> Some generated
         | _ -> None)
       evs)

(* The suffix of [s] from the first occurrence of [sub]. *)
let from sub s =
  let n = String.length s and m = String.length sub in
  let rec find i =
    if i + m > n then ""
    else if String.sub s i m = sub then String.sub s i (n - i)
    else find (i + 1)
  in
  find 0

let stream_calls evs =
  List.filter_map (function Stream.Tool_call c -> Some c | _ -> None) evs

let stream_done evs =
  List.exists (function Stream.Done -> true | _ -> false) evs

let stream_error evs =
  List.exists (function Stream.Tool_error _ -> true | _ -> false) evs

let () =
  (* T1: README quick start, encoding. *)
  let got =
    encode_messages Thinking
      [ system "You are a helpful assistant."; user "What is 2+2?" ]
  in
  let expect =
    bos_token ^ "You are a helpful assistant." ^ user_token ^ "What is 2+2?"
    ^ assistant_token ^ thinking_start_token
  in
  check_eq "encode quick-start" ~expect ~got;

  (* T2: README quick start, parsing. *)
  let p =
    parse_message_from_completion_text Thinking
      ("Simple arithmetic." ^ thinking_end_token ^ "2 + 2 = 4." ^ eos_token)
  in
  check "parse quick-start reasoning"
    (p.reasoning_content = "Simple arithmetic.");
  check "parse quick-start content" (p.content = "2 + 2 = 4.");
  check "parse quick-start no tools" (p.tool_calls = []);

  (* T3: parse a single tool call out of a thinking completion. *)
  let completion =
    "need a tool" ^ thinking_end_token ^ "I will inspect." ^ bash_a_block
    ^ eos_token
  in
  let p = parse_message_from_completion_text Thinking completion in
  check "parse tool reasoning" (p.reasoning_content = "need a tool");
  check "parse tool content" (p.content = "I will inspect.");
  (match p.tool_calls with
  | [ tc ] ->
      check "parse tool name" (tc.name = "bash");
      check_eq "parse tool arguments" ~expect:{|{"command":"a"}|}
        ~got:tc.arguments
  | _ -> check "parse tool single call" false);

  (* T4: encode an assistant tool call -> canonical block matches the C repo. *)
  let got =
    encode_messages Thinking
      [
        user "do it";
        assistant
          ~tool_calls:
            [ tool_call ~name:"bash" ~arguments:{|{"command":"a"}|} () ]
          ();
      ]
  in
  check "encode tool block matches C repo vector" (contains got bash_a_block);

  (* T5: chat mode closes thinking immediately after the assistant prefix. *)
  let got = encode_messages Chat [ system "S"; user "U" ] in
  let expect =
    bos_token ^ "S" ^ user_token ^ "U" ^ assistant_token ^ thinking_end_token
  in
  check_eq "encode chat mode" ~expect ~got;

  (* T6: reasoning_effort Max prepends the maximum-reasoning prefix (after BOS,
     before the system message). *)
  let got =
    encode_messages ~reasoning_effort:Max Thinking [ system "S"; user "U" ]
  in
  check "reasoning-effort max prefix"
    (String.starts_with
       ~prefix:
         (bos_token
        ^ "Reasoning Effort: Absolute maximum with no shortcuts permitted.")
       got);

  (* T7: numeric (string="false") parameter round-trips through JSON. *)
  let completion =
    thinking_end_token ^ "ok"
    ^ Printf.sprintf
        "\n\n\
         <%stool_calls>\n\
         <%sinvoke name=\"f\">\n\
         <%sparameter name=\"count\" string=\"false\">5</%sparameter>\n\
         </%sinvoke>\n\
         </%stool_calls>"
        dsml dsml dsml dsml dsml dsml
    ^ eos_token
  in
  let p = parse_message_from_completion_text Thinking completion in
  (match p.tool_calls with
  | [ tc ] ->
      check_eq "numeric param arguments" ~expect:{|{"count":5}|}
        ~got:tc.arguments
  | _ -> check "numeric param single call" false);

  (* T8: mixed string + numeric params keep order and JSON formatting. *)
  let completion =
    thinking_end_token
    ^ Printf.sprintf
        "\n\n\
         <%stool_calls>\n\
         <%sinvoke name=\"run\">\n\
         <%sparameter name=\"cmd\" string=\"true\">ls</%sparameter>\n\
         <%sparameter name=\"count\" string=\"false\">5</%sparameter>\n\
         </%sinvoke>\n\
         </%stool_calls>"
        dsml dsml dsml dsml dsml dsml dsml dsml
    ^ eos_token
  in
  let p = parse_message_from_completion_text Thinking completion in
  (match p.tool_calls with
  | [ tc ] ->
      check_eq "mixed param arguments" ~expect:{|{"cmd":"ls","count":5}|}
        ~got:tc.arguments
  | _ -> check "mixed param single call" false);

  (* T9: JSON via jsont, with compact output and member order preserved. *)
  check_eq "json compact" ~expect:{|{"a":1,"b":"x"}|}
    ~got:
      (Json.Value.to_string (Json.Value.of_string_exn {|{ "a": 1, "b": "x" }|}));
  check_eq "json roundtrip" ~expect:{|{"a":1,"b":[1,2]}|}
    ~got:(Json.Value.to_string (Json.Value.of_string_exn {|{"a":1,"b":[1,2]}|}));

  (* T10: bytesrw I/O boundary agrees with the pure functions. *)
  let msgs = [ system "You are a helpful assistant."; user "What is 2+2?" ] in
  let buf = Buffer.create 256 in
  let w = Bytesrw.Bytes.Writer.of_buffer buf in
  encode_messages_to_writer w Thinking msgs;
  Bytesrw.Bytes.Writer.write_eod w;
  check_eq "bytesrw writer == encode_messages"
    ~expect:(encode_messages Thinking msgs)
    ~got:(Buffer.contents buf);
  let r =
    Bytesrw.Bytes.Reader.of_string
      ("Simple." ^ thinking_end_token ^ "4." ^ eos_token)
  in
  let p = parse_message_from_reader Thinking r in
  check "bytesrw reader parses"
    (p.reasoning_content = "Simple." && p.content = "4.");

  (* T11: typed invoke codec (combinator style) encodes and decodes. *)
  let block = Codec.encode edit_codec [ { path = "/tmp/x.c"; line = 42 } ] in
  (match Codec.decode edit_codec block with
  | Ok [ e ] ->
      check "typed codec roundtrip" (e.path = "/tmp/x.c" && e.line = 42)
  | _ -> check "typed codec roundtrip" false);

  (* T12: the same block decodes generically via the dynamic codec. *)
  (match Codec.decode Codec.dynamic block with
  | Ok [ tc ] ->
      check_eq "typed block via dynamic"
        ~expect:{|{"path":"/tmp/x.c","line":42}|} ~got:tc.arguments
  | _ -> check "typed block via dynamic" false);

  (* T13: choice codec dispatches a mixed block by tool name. *)
  let block =
    Codec.encode call_codec
      [
        An_edit { path = "/a"; line = 1 };
        Other (tool_call ~name:"bash" ~arguments:{|{"command":"ls"}|} ());
      ]
  in
  (match Codec.decode call_codec block with
  | Ok [ An_edit e; Other tc ] ->
      check "choice roundtrip" (e.path = "/a" && e.line = 1 && tc.name = "bash")
  | _ -> check "choice roundtrip" false);

  (* T14: streaming decode, fed byte-by-byte, matches the batch parser. *)
  let completion =
    "reason" ^ thinking_end_token ^ "hello " ^ bash_a_block ^ eos_token
  in
  let s = Stream.create Thinking in
  let evs = ref [] in
  String.iter
    (fun c -> evs := !evs @ Stream.feed s (String.make 1 c))
    completion;
  evs := !evs @ Stream.finish s;
  let collect f = List.filter_map f !evs in
  let reasoning =
    String.concat ""
      (collect (function Stream.Reasoning r -> Some r | _ -> None))
  in
  let content =
    String.concat ""
      (collect (function Stream.Content c -> Some c | _ -> None))
  in
  let calls = collect (function Stream.Tool_call c -> Some c | _ -> None) in
  let is_done =
    List.exists (function Stream.Done -> true | _ -> false) !evs
  in
  check "stream reasoning" (reasoning = "reason");
  check "stream content" (content = "hello ");
  check "stream tool call"
    (match calls with
    | [ tc ] -> tc.name = "bash" && tc.arguments = {|{"command":"a"}|}
    | _ -> false);
  check "stream done" is_done;

  (* T15: Tool.v builds the OpenAI tool object. *)
  let params =
    Json.Value.of_string_exn
      {|{"type":"object","properties":{"command":{"type":"string"}}}|}
  in
  check_eq "tool schema"
    ~expect:
      {|{"type":"function","function":{"name":"bash","description":"run a shell command","parameters":{"type":"object","properties":{"command":{"type":"string"}}}}}|}
    ~got:
      (Json.Value.to_string
         (Tool.v ~name:"bash" ~description:"run a shell command"
            ~parameters:params ()));

  (* T16: a sealed invoke codec derives its parameters JSON Schema. *)
  check_eq "codec schema"
    ~expect:
      {|{"type":"object","properties":{"path":{"type":"string"},"line":{"type":"integer"}},"required":["path","line"]}|}
    ~got:(Json.Value.to_string (Codec.schema edit_codec));

  (* T17: decode_arguments turns a JSON arguments object into the typed value. *)
  check "codec decode_arguments"
    (match
       Codec.decode_arguments edit_codec {|{"path":"/tmp/x.c","line":42}|}
     with
    | Ok e -> e.path = "/tmp/x.c" && e.line = 42
    | Error _ -> false);

  check "codec decode_arguments error"
    (match Codec.decode_arguments edit_codec {|{"path":"/tmp/x.c"}|} with
    | Error _ -> true
    | Ok _ -> false);

  (* T18: a parameter with a default is optional, in the schema and in a call
     that leaves it out. *)
  let defaulted =
    let open Codec in
    Invoke.map "edit" mk_edit
    |> Invoke.param ~enc:(fun e -> e.path) "path" string
    |> Invoke.param ~enc:(fun e -> e.line) ~default:1 "line" int
    |> Invoke.seal
  in
  check_eq "codec schema with a default"
    ~expect:
      {|{"type":"object","properties":{"path":{"type":"string"},"line":{"type":"integer"}},"required":["path"]}|}
    ~got:(Json.Value.to_string (Codec.schema defaulted));

  check "codec decode_arguments takes the default"
    (match Codec.decode_arguments defaulted {|{"path":"/tmp/x.c"}|} with
    | Ok e -> e.path = "/tmp/x.c" && e.line = 1
    | Error _ -> false);

  check "codec decode_arguments prefers what was sent"
    (match
       Codec.decode_arguments defaulted {|{"path":"/tmp/x.c","line":9}|}
     with
    | Ok e -> e.line = 9
    | Error _ -> false);

  let optional =
    let open Codec in
    Invoke.map "lookup" (fun path line -> (path, line))
    |> Invoke.param ~enc:fst "path" string
    |> Invoke.optional ~enc:snd "line" int
    |> Invoke.seal
  in
  check_eq "optional parameter is not required"
    ~expect:
      {|{"type":"object","properties":{"path":{"type":"string"},"line":{"type":"integer"}},"required":["path"]}|}
    ~got:(Json.Value.to_string (Codec.schema optional));
  check "optional parameter decodes absence"
    (Codec.decode_arguments optional {|{"path":"a"}|} = Ok ("a", None));
  check "optional parameter decodes presence"
    (Codec.decode_arguments optional {|{"path":"a","line":4}|}
    = Ok ("a", Some 4));
  check "optional parameter is omitted when encoding"
    (Codec.encode_arguments optional ("a", None) = Ok {|{"path":"a"}|});
  check "present optional parameter is encoded"
    (Codec.encode_arguments optional ("a", Some 4)
    = Ok {|{"path":"a","line":4}|});

  let invalid f =
    match f () with exception Invalid_argument _ -> true | _ -> false
  in
  check "codec refuses an invalid tool name"
    (invalid (fun () -> Codec.Invoke.map "bad name" ()));
  check "codec refuses a duplicate parameter"
    (invalid (fun () ->
         let open Codec in
         Invoke.map "duplicate" (fun a b -> (a, b))
         |> Invoke.param ~enc:fst "value" string
         |> Invoke.param ~enc:snd "value" string
         |> Invoke.seal));
  let float_codec =
    let open Codec in
    Invoke.map "number" Fun.id
    |> Invoke.param ~enc:Fun.id "value" float
    |> Invoke.seal
  in
  check "float codec refuses a non-finite value"
    (match Codec.decode_arguments float_codec {|{"value":"nan"}|} with
    | Error _ -> true
    | Ok _ -> false);
  let array_codec =
    let open Codec in
    Invoke.map "numbers" Fun.id
    |> Invoke.param ~enc:Fun.id "values" (array ~minimum:1 ~maximum:2 int)
    |> Invoke.seal
  in
  check "array codec decodes typed values"
    (Codec.decode_arguments array_codec {|{"values":[1,2]}|} = Ok [ 1; 2 ]);
  check "array codec checks its bounds"
    (match Codec.decode_arguments array_codec {|{"values":[]}|} with
    | Error _ -> true
    | Ok _ -> false);
  check_eq "array codec describes its items and bounds"
    ~expect:
      {|{"type":"object","properties":{"values":{"type":"array","items":{"type":"integer"},"minItems":1,"maxItems":2}},"required":["values"]}|}
    ~got:(Json.Value.to_string (Codec.schema array_codec));
  let point =
    let open Codec in
    Object.map "point" (fun x y -> (x, y))
    |> Object.param ~enc:fst "x" int
    |> Object.param ~enc:snd "y" int
    |> Object.seal
  in
  let points =
    let open Codec in
    Invoke.map "points" Fun.id
    |> Invoke.param ~enc:Fun.id "values" (array point)
    |> Invoke.seal
  in
  check "object codecs compose inside arrays"
    (Codec.decode_arguments points {|{"values":[{"x":1,"y":2}]}|}
    = Ok [ (1, 2) ]);
  check "object codecs encode inside arrays"
    (Codec.encode_arguments points [ (1, 2) ]
    = Ok {|{"values":[{"x":1,"y":2}]}|});
  let strings =
    let open Codec in
    Invoke.map "strings" Fun.id
    |> Invoke.param ~enc:Fun.id "values" (array string)
    |> Invoke.seal
  in
  let nested_close = "x</" ^ dsml_token ^ "parameter>y" in
  check "JSON-valued parameters escape a nested closing tag"
    (Codec.decode strings (Codec.encode strings [ [ nested_close ] ])
    = Ok [ [ nested_close ] ]);

  (* T19: a call with no parameters decodes, and the encoder renders exactly
     the form the model emits. *)
  check_eq "zero-parameter block encoding" ~expect:project_block
    ~got:
      (Codec.encode Codec.dynamic
         [ tool_call ~name:"project" ~arguments:"{}" () ]);

  (match Codec.decode Codec.dynamic project_block with
  | Ok [ tc ] ->
      check "zero-parameter call name" (tc.name = "project");
      check_eq "zero-parameter call arguments" ~expect:"{}" ~got:tc.arguments
  | _ -> check "zero-parameter call decodes" false);

  (* T20: the same block parses out of a whole completion. *)
  let p =
    parse_message_from_completion_text Chat
      ("ok" ^ "\n\n" ^ project_block ^ eos_token)
  in
  check "zero-parameter completion content" (p.content = "ok");
  check "zero-parameter completion call"
    (match p.tool_calls with
    | [ tc ] -> tc.name = "project" && tc.arguments = "{}"
    | _ -> false);

  (* T21: the same block streams, fed in chunks that split the DSML token. *)
  List.iter
    (fun n ->
      let evs =
        stream_events Chat (chunks_of n ("\n\n" ^ project_block ^ eos_token))
      in
      let name =
        Printf.sprintf "stream zero-parameter call in %d-byte chunks" n
      in
      check name
        (match stream_calls evs with
        | [ tc ] -> tc.name = "project" && tc.arguments = "{}"
        | _ -> false);
      check (name ^ ", done") (stream_done evs);
      check_eq (name ^ ", no stray content") ~expect:"" ~got:(stream_text evs))
    [ 1; 5; 7 ];

  (* T22: a block the codec refuses is reported as the text it was, with its
     markup token neutralised, and what the model writes after it is still
     decoded as content. *)
  let evs =
    stream_events Chat
      (chunks_of 5
         ("hi\n\n" ^ bad_block dsml "" ^ "trailing words" ^ eos_token))
  in
  check_eq "refused block surfaces as content"
    ~expect:("hi" ^ bad_block "|DSML|" "" ^ "trailing words")
    ~got:(stream_text evs);
  check "refused block reports an error" (stream_error evs);
  check "refused block carries no special token" (no_special (stream_text evs));
  check "refused block yields no call" (stream_calls evs = []);
  check "refused block still ends the turn" (stream_done evs);

  (* T23: a block still open at end of stream surfaces the same way. *)
  let evs =
    stream_events Chat [ "\n\n" ^ open_block dsml ^ thinking_end_token ]
  in
  check_eq "unterminated block surfaces as content"
    ~expect:(open_block "|DSML|" ^ "[/think]")
    ~got:(stream_text evs);
  check "unterminated block carries no special token"
    (no_special (stream_text evs));
  check "unterminated block still ends the turn" (stream_done evs);

  (* The caller that stopped the stream has to be able to tell that content
     apart from a reply, since one is a discarded call and the other is an
     answer. *)
  let s = Stream.create Chat in
  ignore (Stream.feed s ("\n\n" ^ open_block dsml));
  check "a block left open reads as a call in progress" (Stream.in_tool_call s);
  ignore (Stream.finish s);
  let s = Stream.create Chat in
  ignore (Stream.feed s "a plain reply");
  check "text does not read as a call in progress" (not (Stream.in_tool_call s));
  ignore (Stream.feed s ("\n\n" ^ bash_a_block));
  check "a block that closed does not read as a call in progress"
    (not (Stream.in_tool_call s));

  (* T24: a refused block carrying another of the tokenizer's special tokens
     surfaces with that one neutralised too. A raw [</think>] here would be a
     think token again in the next prompt, and would break the turn apart. *)
  let evs =
    stream_events Chat
      (chunks_of 5 ("\n\n" ^ bad_block dsml thinking_end_token ^ eos_token))
  in
  check_eq "special token inside a refused block is neutralised"
    ~expect:(bad_block "|DSML|" "[/think]")
    ~got:(stream_text evs);
  check "refused block with a think token carries no special token"
    (no_special (stream_text evs));

  (* T25: the tolerance for an empty invoke body also accepts a blank line
     before the first parameter, which the model writes from time to time. *)
  let padded =
    Printf.sprintf
      "<%stool_calls>\n\
       <%sinvoke name=\"f\">\n\n\
       <%sparameter name=\"x\" string=\"true\">v</%sparameter>\n\
       </%sinvoke>\n\
       </%stool_calls>"
      dsml dsml dsml dsml dsml dsml
  in
  check "blank line before the first parameter decodes"
    (match Codec.decode Codec.dynamic padded with
    | Ok [ tc ] -> tc.name = "f" && tc.arguments = {|{"x":"v"}|}
    | _ -> false);

  (* T26: the neutraliser rewrites every marker the library names, including
     the turn tokens, and leaves ordinary text alone. *)
  check_eq "neutralise leaves ordinary text alone" ~expect:"a plain reply <p>"
    ~got:(neutralise_specials "a plain reply <p>");
  check_eq "neutralise rewrites a turn token" ~expect:"say [User] out loud"
    ~got:(neutralise_specials ("say " ^ user_token ^ " out loud"));
  check "neutralise rewrites every marker"
    (List.for_all
       (fun tk -> no_special (neutralise_specials ("a" ^ tk ^ "b")))
       specials);

  (* T27: no marker can be rebuilt out of a rewritten token and the text that
     sits beside it. *)
  let fragments = [ ""; "<"; ">"; "/"; "|"; "\xef\xbd\x9c"; "think"; "DSML" ] in
  check "neutralise cannot rebuild a marker"
    (List.for_all
       (fun tk ->
         List.for_all
           (fun before ->
             List.for_all
               (fun after ->
                 no_special (neutralise_specials (before ^ tk ^ after)))
               fragments)
           fragments)
       specials);

  (* T28: text that quotes real markup, a tool result from reading a file that
     documents DSML among them, is safe to record once it has been made inert.
     The control below shows what recording it raw would do: the next prompt
     would carry a tool-call block the model never wrote. *)
  let quoted = "the file says\n\n" ^ project_block in
  let prompt_of content =
    encode_messages Chat [ user "read dsml.mli"; tool ~id:"1" content ]
  in
  check "a recorded quotation cannot forge markup"
    (let p = prompt_of (neutralise_specials quoted) in
     (not (contains p dsml)) && contains p "|DSML|");
  check "and would forge it if recorded raw" (contains (prompt_of quoted) dsml);

  let sampling = Stream.create Chat in
  ignore (Stream.feed sampling ("\n\n<" ^ dsml ^ "tool_calls"));
  check "tool structure samples greedily"
    (Stream.sampling_mode sampling = `Greedy);
  ignore
    (Stream.feed sampling
       (">\n<" ^ dsml ^ "parameter name=\"path\" string=\"true\">file.ml"));
  check "tool values keep configured sampling"
    (Stream.sampling_mode sampling = `Configured);
  ignore (Stream.feed sampling "</");
  check "tool closing syntax samples greedily"
    (Stream.sampling_mode sampling = `Greedy);

  (* ---- GLM dialect ----------------------------------------------------
     Oracles: the chat encoder and rendered-chat tokenizer in the vendored
     csrc/ds4.c, and the tool-call grammar upstream's ds4_agent.c advertises
     and parses. *)
  let glm_specials =
    specials
    @ [
        glm_bos_token;
        glm_sop_token;
        glm_eos_token;
        glm_system_token;
        glm_user_token;
        glm_assistant_token;
        glm_observation_token;
        glm_tool_call_open;
        glm_tool_call_close;
        glm_tool_response_open;
        glm_tool_response_close;
        glm_arg_key_open;
        glm_arg_key_close;
        glm_arg_value_open;
        glm_arg_value_close;
      ]
  in
  let no_glm_special s =
    List.for_all (fun tk -> not (contains s tk)) glm_specials
  in
  let glm_stream_events mode chunks =
    let s = Stream.create ~dialect:Glm mode in
    let evs = List.concat_map (Stream.feed s) chunks in
    evs @ Stream.finish s
  in
  let stream_reasoning evs =
    String.concat ""
      (List.filter_map
         (function Stream.Reasoning r -> Some r | _ -> None)
         evs)
  in

  (* G1: thinking encode, mirroring the engine's own chat encoder: bos
     sequence, the effort line as a system turn of its own, the system and
     user turns, and the generation prefix opening thinking. *)
  let got =
    encode_messages ~dialect:Glm Thinking
      [ system "You are helpful."; user "What is 2+2?" ]
  in
  let expect =
    glm_bos_token ^ glm_sop_token ^ glm_system_token ^ "Reasoning Effort: High"
    ^ glm_system_token ^ "You are helpful." ^ glm_user_token ^ "What is 2+2?"
    ^ glm_assistant_token ^ thinking_start_token
  in
  check_eq "glm encode thinking quick-start" ~expect ~got;

  (* G2: chat mode writes no effort line and closes thinking in the prefix. *)
  let got = encode_messages ~dialect:Glm Chat [ system "S"; user "U" ] in
  let expect =
    glm_bos_token ^ glm_sop_token ^ glm_system_token ^ "S" ^ glm_user_token
    ^ "U" ^ glm_assistant_token ^ thinking_start_token ^ thinking_end_token
  in
  check_eq "glm encode chat mode" ~expect ~got;

  (* G3: tools render into the system turn in upstream's wording, schemas
     inside <tools> and the grammar spelled out. *)
  let tools =
    [
      Tool.v ~name:"read" ~description:"Read a file."
        ~parameters:(Json.Value.of_string_exn {|{"type":"object"}|})
        ();
    ]
  in
  let got = encode_messages ~dialect:Glm Chat [ system ~tools "S"; user "U" ] in
  check "glm tools block advertises the grammar"
    (contains got "<tools>"
    && contains got {|"name":"read"|}
    && contains got (glm_tool_call_open ^ "{function-name}"));

  (* G4: an assistant tool call renders as one block per invocation, string
     members raw and other members as their compact JSON. *)
  let got =
    encode_messages ~dialect:Glm Thinking
      [
        user "do it";
        assistant
          ~tool_calls:
            [
              tool_call ~name:"read" ~arguments:{|{"path":"/tmp/x","line":42}|}
                ();
            ]
          ();
      ]
  in
  check "glm encode tool block"
    (contains got
       (glm_tool_call_open ^ "read" ^ glm_arg_key_open ^ "path"
      ^ glm_arg_key_close ^ glm_arg_value_open ^ "/tmp/x" ^ glm_arg_value_close
      ^ glm_arg_key_open ^ "line" ^ glm_arg_key_close ^ glm_arg_value_open
      ^ "42" ^ glm_arg_value_close ^ glm_tool_call_close));

  (* G5: a tool result is an observation turn wrapped in <tool_response>, and
     an assistant turn is closed by the next role marker rather than an end
     token. *)
  let got =
    encode_messages ~dialect:Glm Thinking
      [ user "u"; assistant ~content:"c" (); tool ~id:"1" "out" ]
  in
  let expect =
    glm_bos_token ^ glm_sop_token ^ glm_system_token ^ "Reasoning Effort: High"
    ^ glm_user_token ^ "u" ^ glm_assistant_token ^ thinking_start_token
    ^ thinking_end_token ^ "c" ^ glm_observation_token ^ glm_tool_response_open
    ^ "out" ^ glm_tool_response_close ^ glm_assistant_token
    ^ thinking_start_token
  in
  check_eq "glm encode observation turn" ~expect ~got;

  (* G6: reasoning before the last user turn is dropped, and the turn that
     lost it opens with an empty think span, as the engine writes one. *)
  let got =
    encode_messages ~dialect:Glm Thinking
      [ user "a"; assistant ~reasoning_content:"r1" ~content:"c1" (); user "b" ]
  in
  check "glm drops earlier reasoning"
    ((not (contains got "r1"))
    && contains got
         (glm_assistant_token ^ thinking_start_token ^ thinking_end_token ^ "c1")
    );

  (* G7: stream decode of a plain thinking reply, split inside markers. *)
  let evs =
    glm_stream_events Thinking
      (chunks_of 3 ("thoughts" ^ thinking_end_token ^ "The answer."))
  in
  check_eq "glm stream reasoning" ~expect:"thoughts" ~got:(stream_reasoning evs);
  check_eq "glm stream content" ~expect:"The answer." ~got:(stream_text evs);
  check "glm stream ends" (stream_done evs);

  (* G8: one call, keys and values typed by reading them: a path stays a
     string, a number becomes a JSON number, member order kept. *)
  let block =
    glm_tool_call_open ^ "read" ^ glm_arg_key_open ^ "path" ^ glm_arg_key_close
    ^ glm_arg_value_open ^ "/tmp/x" ^ glm_arg_value_close ^ glm_arg_key_open
    ^ "line" ^ glm_arg_key_close ^ glm_arg_value_open ^ "42"
    ^ glm_arg_value_close ^ glm_tool_call_close
  in
  let evs =
    glm_stream_events Thinking
      (chunks_of 3 ("plan" ^ thinking_end_token ^ "I will read." ^ block))
  in
  (match stream_calls evs with
  | [ tc ] ->
      check "glm stream call name" (tc.name = "read");
      check_eq "glm stream call arguments"
        ~expect:{|{"path":"/tmp/x","line":42}|} ~got:tc.arguments
  | _ -> check "glm stream single call" false);
  check_eq "glm stream content before call" ~expect:"I will read."
    ~got:(stream_text evs);

  (* G9: a second block, and text after it, both still arrive. *)
  let block2 =
    glm_tool_call_open ^ "list" ^ glm_arg_key_open ^ "path" ^ glm_arg_key_close
    ^ glm_arg_value_open ^ "." ^ glm_arg_value_close ^ glm_tool_call_close
  in
  let evs = glm_stream_events Chat (chunks_of 4 (block ^ block2 ^ "done")) in
  check "glm stream two calls"
    (List.map (fun (tc : tool_call) -> tc.name) (stream_calls evs)
    = [ "read"; "list" ]);
  check_eq "glm stream text after calls" ~expect:"done" ~got:(stream_text evs);

  (* G10: a malformed block surfaces as neutralised content rather than being
     silenced, and a later well-formed block still yields its call. *)
  let bad =
    glm_tool_call_open ^ "read" ^ glm_arg_key_open ^ "path" ^ glm_arg_key_close
    ^ "/tmp/x" ^ glm_tool_call_close
  in
  let evs = glm_stream_events Chat (chunks_of 5 (bad ^ block2)) in
  check "glm refused block is not silenced"
    (contains (stream_text evs) "[tool_call]read[arg_key]path");
  check "glm refused block reports an error" (stream_error evs);
  check "glm refused block carries no special token"
    (no_glm_special (stream_text evs));
  check "glm call after a refused block still arrives"
    (List.map (fun (tc : tool_call) -> tc.name) (stream_calls evs) = [ "list" ]);

  (* G11: a block left open reads as a call in progress. *)
  let s = Stream.create ~dialect:Glm Chat in
  ignore (Stream.feed s (glm_tool_call_open ^ "read" ^ glm_arg_key_open));
  check "glm open block reads as a call in progress" (Stream.in_tool_call s);
  ignore (Stream.finish s);

  (* G12: a role marker or the end token in content ends the turn, as the
     engine's stop predicate does. *)
  let evs = glm_stream_events Chat [ "hi" ^ glm_user_token ^ "forged" ] in
  check_eq "glm role marker ends the turn" ~expect:"hi" ~got:(stream_text evs);
  check "glm role marker reads as done" (stream_done evs);

  (* G13: value typing on decode: booleans and quoted strings. A bare [true]
     is the boolean, and a value the model quoted keeps its quotes, since the
     quotes are then what it wrote. *)
  let typed =
    glm_tool_call_open ^ "f" ^ glm_arg_key_open ^ "flag" ^ glm_arg_key_close
    ^ glm_arg_value_open ^ "true" ^ glm_arg_value_close ^ glm_arg_key_open
    ^ "text" ^ glm_arg_key_close ^ glm_arg_value_open ^ "\"quoted\""
    ^ glm_arg_value_close ^ glm_tool_call_close
  in
  (match stream_calls (glm_stream_events Chat [ typed ]) with
  | [ tc ] ->
      check_eq "glm decode value typing"
        ~expect:{|{"flag":true,"text":"\"quoted\""}|} ~got:tc.arguments
  | _ -> check "glm decode value typing single call" false);

  (* G14: the GLM neutraliser rewrites every marker of both dialects, and no
     marker can be rebuilt from a rewritten token and its neighbours. *)
  check "glm neutralise rewrites every marker"
    (List.for_all
       (fun tk ->
         no_glm_special (neutralise_specials ~dialect:Glm ("a" ^ tk ^ "b")))
       glm_specials);
  let fragments = [ ""; "<"; ">"; "/"; "|"; "think"; "tool_call"; "arg_key" ] in
  check "glm neutralise cannot rebuild a marker"
    (List.for_all
       (fun tk ->
         List.for_all
           (fun before ->
             List.for_all
               (fun after ->
                 no_glm_special
                   (neutralise_specials ~dialect:Glm (before ^ tk ^ after)))
               fragments)
           fragments)
       glm_specials);

  (* G15: a recorded tool result that quotes GLM markup cannot forge an
     observation turn or a call, once made inert. *)
  let quoted = "the file says " ^ glm_tool_response_close ^ block2 in
  let prompt_of content =
    encode_messages ~dialect:Glm Chat [ user "read x"; tool ~id:"1" content ]
  in
  check "glm recorded quotation cannot forge markup"
    (let p = prompt_of (neutralise_specials ~dialect:Glm quoted) in
     contains p "[/tool_response]"
     && contains p "[tool_call]"
     && not (contains p (glm_tool_response_close ^ block2)));

  (* ---- DeepSeek V4.1 ---- *)
  let v41_calls_open = "<" ^ dsml ^ " calls>" in
  let v41_calls_close = "</" ^ dsml ^ " calls>" in
  let v41_stream_events mode chunks =
    let s = Stream.create ~dialect:Deepseek41 mode in
    let evs = List.concat_map (Stream.feed s) chunks in
    evs @ Stream.finish s
  in

  (* V1: thinking encode, mirroring the engine: the effort line opens the
     first system turn and the system prompt follows it with no second marker. *)
  let got =
    encode_messages ~dialect:Deepseek41 Thinking
      [ system "You are helpful."; user "What is 2+2?" ]
  in
  let expect =
    bos_token ^ deepseek41_system_token
    ^ "Reasoning Effort: 75 (range 1-100, the higher the value, the more \
       thorough the reasoning)\n\n" ^ "You are helpful." ^ user_token
    ^ "What is 2+2?" ^ assistant_token ^ thinking_start_token
  in
  check_eq "v41 encode thinking quick-start" ~expect ~got;

  (* V2: chat mode marks the system turn itself and writes no effort. *)
  let got = encode_messages ~dialect:Deepseek41 Chat [ system "S"; user "U" ] in
  let expect =
    bos_token ^ deepseek41_system_token ^ "S" ^ user_token ^ "U"
    ^ assistant_token ^ thinking_end_token
  in
  check_eq "v41 encode chat mode" ~expect ~got;

  (* V3: tool calls render with the V4.1 tag names. *)
  let got =
    encode_messages ~dialect:Deepseek41 Chat
      [
        user "do it";
        assistant
          ~tool_calls:
            [ tool_call ~name:"read" ~arguments:{|{"path":"/tmp/x","n":2}|} () ]
          ();
      ]
  in
  check_eq "v41 encode tool block"
    ~expect:
      (v41_calls_open ^ "\n<" ^ dsml ^ " invoke name=\"read\">\n<" ^ dsml
     ^ " parameter name=\"path\" string=\"true\">/tmp/x</" ^ dsml
     ^ " parameter>\n<" ^ dsml ^ " parameter name=\"n\" string=\"false\">2</"
     ^ dsml ^ " parameter>\n</" ^ dsml ^ " invoke>\n" ^ v41_calls_close
     ^ eos_token)
    ~got:(from v41_calls_open got);
  check "v41 tool prompt uses the V4.1 tags"
    (let p = tool_prompt ~dialect:Deepseek41 [ Tool.v ~name:"read" () ] in
     contains p v41_calls_open && not (contains p (dsml ^ "tool_calls")));

  (* V4: a block the V4.1 model writes decodes, split anywhere. *)
  let block =
    v41_calls_open ^ "\n<" ^ dsml ^ " invoke name=\"read\">\n<" ^ dsml
    ^ " parameter name=\"path\" string=\"true\">/tmp/x</" ^ dsml
    ^ " parameter>\n</" ^ dsml ^ " invoke>\n" ^ v41_calls_close
  in
  let evs =
    v41_stream_events Thinking
      (chunks_of 3 ("plan" ^ thinking_end_token ^ "Reading.\n\n" ^ block))
  in
  (match stream_calls evs with
  | [ tc ] ->
      check_eq "v41 stream call" ~expect:{|{"path":"/tmp/x"}|} ~got:tc.arguments
  | _ -> check "v41 stream single call" false);
  check_eq "v41 stream content before call" ~expect:"Reading."
    ~got:(stream_text evs);

  (* V5: V4's spelling is not V4.1's grammar, so it stays text. *)
  let v4_block =
    "\n\n<" ^ dsml ^ "tool_calls>\n<" ^ dsml ^ "invoke name=\"read\">\n</"
    ^ dsml ^ "invoke>\n</" ^ dsml ^ "tool_calls>"
  in
  let evs = v41_stream_events Chat [ v4_block ] in
  check "v41 stream refuses the V4 spelling as a call" (stream_calls evs = []);

  (* V6: the V4.1 neutraliser also rewrites the system marker. *)
  check_eq "v41 neutralise system marker" ~expect:"a[System]b"
    ~got:
      (neutralise_specials ~dialect:Deepseek41
         ("a" ^ deepseek41_system_token ^ "b"));

  (* ---- Qwen ---- *)
  let qwen_stream_events mode chunks =
    let s = Stream.create ~dialect:Qwen mode in
    let evs = List.concat_map (Stream.feed s) chunks in
    evs @ Stream.finish s
  in
  let qwen_specials =
    [
      qwen_im_start_token;
      qwen_im_end_token;
      qwen_endoftext_token;
      glm_tool_call_open;
      glm_tool_call_close;
      glm_tool_response_open;
      glm_tool_response_close;
      thinking_start_token;
      thinking_end_token;
      dsml_token;
      user_token;
    ]
  in
  let no_qwen_special s =
    List.for_all (fun tk -> not (contains s tk)) qwen_specials
  in
  let xhigh =
    "Reasoning effort is set to xhigh. Please think carefully through the \
     task, validate key assumptions, consider plausible alternatives, and \
     prioritize correctness, consistency, and clarity in the final answer."
  in

  (* Q1: thinking encode, the effort and the system prompt in one system turn
     and the generation prefix opening thinking, as upstream's server writes
     it. *)
  let got =
    encode_messages ~dialect:Qwen Thinking
      [ system "You are helpful."; user "What is 2+2?" ]
  in
  check_eq "qwen encode thinking quick-start"
    ~expect:
      ("<|im_start|>system\n" ^ xhigh
     ^ "\n\n\
        You are helpful.<|im_end|>\n\
        <|im_start|>user\n\
        What is 2+2?<|im_end|>\n\
        <|im_start|>assistant\n\
        <think>\n")
    ~got;

  (* Q2: chat mode writes no effort and an empty think span. *)
  let got = encode_messages ~dialect:Qwen Chat [ system "S"; user "U" ] in
  check_eq "qwen encode chat mode"
    ~expect:
      "<|im_start|>system\n\
       S<|im_end|>\n\
       <|im_start|>user\n\
       U<|im_end|>\n\
       <|im_start|>assistant\n\
       <think>\n\n\
       </think>\n\n"
    ~got;

  (* Q3: the tools are listed one wrapped function per line. *)
  let p =
    tool_prompt ~dialect:Qwen
      [
        Tool.v ~name:"read"
          ~parameters:(Json.Value.of_string_exn {|{"type":"object"}|})
          ();
      ]
  in
  check "qwen tool prompt lists wrapped functions"
    (contains p
       "<tools>\n{\"type\": \"function\", \"function\": {\"name\":\"read\""
    && contains p "<function=example_function_name>");

  (* Q4: an assistant call, and two results sharing one user turn. *)
  let got =
    encode_messages ~dialect:Qwen Chat
      [
        user "run";
        assistant ~content:"ok"
          ~tool_calls:
            [
              tool_call ~name:"bash"
                ~arguments:{|{"command":"ls","timeout":10}|} ();
            ]
          ();
        tool ~id:"1" "a";
        tool ~id:"2" "b";
      ]
  in
  check_eq "qwen encode call and results"
    ~expect:
      "<|im_start|>user\n\
       run<|im_end|>\n\
       <|im_start|>assistant\n\
       <think>\n\n\
       </think>\n\n\
       ok\n\n\
       <tool_call>\n\
       <function=bash>\n\
       <parameter=command>\n\
       ls\n\
       </parameter>\n\
       <parameter=timeout>\n\
       10\n\
       </parameter>\n\
       </function>\n\
       </tool_call><|im_end|>\n\
       <|im_start|>user\n\
       <tool_response>\n\
       a\n\
       </tool_response>\n\
       <tool_response>\n\
       b\n\
       </tool_response><|im_end|>\n\
       <|im_start|>assistant\n\
       <think>\n\n\
       </think>\n\n"
    ~got;

  (* Q5: a call decodes with a multi-line value, a number typed as JSON, and
     a value holding </tool_call>, which must not close the block. *)
  let value = "let x = 1\n(* </tool_call> *)\n" in
  let block =
    "<tool_call>\n\
     <function=write>\n\
     <parameter=path>\n\
     a.ml\n\
     </parameter>\n\
     <parameter=content>\n" ^ value
    ^ "\n\
       </parameter>\n\
       <parameter=mode>\n\
       420\n\
       </parameter>\n\
       </function>\n\
       </tool_call>"
  in
  let evs =
    qwen_stream_events Thinking
      (chunks_of 3 ("plan\n" ^ thinking_end_token ^ "\n\nWriting.\n" ^ block))
  in
  (match stream_calls evs with
  | [ tc ] ->
      check "qwen stream call name" (tc.name = "write");
      check_eq "qwen stream call arguments"
        ~expect:
          {|{"path":"a.ml","content":"let x = 1\n(* </tool_call> *)\n","mode":420}|}
        ~got:tc.arguments
  | _ -> check "qwen stream single call" false);
  check "qwen stream no error" (not (stream_error evs));
  check_eq "qwen round trip" ~expect:block
    ~got:
      (from "<tool_call>"
         (encode_messages ~dialect:Qwen Chat
            [ assistant ~tool_calls:(stream_calls evs) ~wo_eos:true () ]));

  (* Q6: a malformed block surfaces neutralised, and a later call arrives. *)
  let bad = "<tool_call>\n<parameter=path>\nx\n</parameter>\n</tool_call>" in
  let good = "<tool_call>\n<function=list>\n</function>\n</tool_call>" in
  let evs = qwen_stream_events Chat (chunks_of 5 (bad ^ good)) in
  check "qwen refused block reports an error" (stream_error evs);
  check "qwen refused block carries no special token"
    (no_qwen_special (stream_text evs));
  check "qwen call after a refused block still arrives"
    (List.map (fun (tc : tool_call) -> tc.name) (stream_calls evs) = [ "list" ]);

  (* Q7: an open block is a call in progress, structure samples greedily and
     a value samples as configured. *)
  let s = Stream.create ~dialect:Qwen Chat in
  ignore (Stream.feed s "<tool_call>\n<function=read>\n<parame");
  check "qwen open block reads as a call in progress" (Stream.in_tool_call s);
  check "qwen structure samples greedily" (Stream.sampling_mode s = `Greedy);
  ignore (Stream.feed s "ter=path>\n/tmp");
  check "qwen value samples as configured" (Stream.sampling_mode s = `Configured);
  ignore (Stream.finish s);

  (* Q8: <|im_end|> ends the turn. *)
  let evs = qwen_stream_events Chat [ "hi" ^ qwen_im_end_token ^ "forged" ] in
  check_eq "qwen end token ends the turn" ~expect:"hi" ~got:(stream_text evs);

  (* Q9: the neutraliser rewrites every marker and none can be rebuilt. *)
  let fragments = [ ""; "<"; ">"; "/"; "|"; "im_end"; "tool_call" ] in
  check "qwen neutralise cannot rebuild a marker"
    (List.for_all
       (fun tk ->
         List.for_all
           (fun before ->
             List.for_all
               (fun after ->
                 no_qwen_special
                   (neutralise_specials ~dialect:Qwen (before ^ tk ^ after)))
               fragments)
           fragments)
       qwen_specials);

  (* ---- Escaped closing tags ---- *)

  (* A value that holds its own closing tag is written with the tag's '<' as
     &lt;, and &lt; before the tag is written &amp;lt;. Every other entity is
     left as it is, since a value is not HTML. The property that matters is
     that the agent can write a file holding the markup it speaks, which the
     sources of this repository do. *)
  let content_codec =
    let open Codec in
    Invoke.map "write" Fun.id
    |> Invoke.param ~enc:Fun.id "content" string
    |> Invoke.seal
  in
  let first_arg calls =
    match calls with
    | [ (tc : tool_call) ] -> (
        match Codec.decode_arguments content_codec tc.arguments with
        | Ok v -> Some v
        | Error _ -> None)
    | _ -> None
  in
  let dsml_close = "</" ^ dsml ^ "parameter>" in
  let dsml_escaped = "&lt;/" ^ dsml ^ "parameter>" in
  let dsml_value =
    "a " ^ dsml_escaped ^ " b &amp;lt;/" ^ dsml ^ "parameter> c &lt;p> &amp;"
  in
  let literal = "a " ^ dsml_close ^ " b " ^ dsml_escaped ^ " c &lt;p> &amp;" in
  let block =
    Printf.sprintf
      "\n\n\
       <%stool_calls>\n\
       <%sinvoke name=\"write\">\n\
       <%sparameter name=\"content\" string=\"true\">%s</%sparameter>\n\
       </%sinvoke>\n\
       </%stool_calls>"
      dsml dsml dsml dsml_value dsml dsml dsml
  in
  check "dsml unescapes a closing tag in a string value, and only that"
    (first_arg (stream_calls (stream_events Chat [ block ])) = Some literal);
  check "dsml unescapes a closing tag fed a few bytes at a time"
    (List.for_all
       (fun n ->
         first_arg (stream_calls (stream_events Chat (chunks_of n block)))
         = Some literal)
       [ 1; 2; 3; 5; 7 ]);
  check "the typed encoder escapes a closing tag in a string value"
    (contains (Codec.encode content_codec [ literal ]) dsml_value);
  check "the typed encoder's block decodes to the value it was given"
    (Codec.decode content_codec (Codec.encode content_codec [ literal ])
    = Ok [ literal ]);
  let rendered =
    match Codec.decode Codec.dynamic block with
    | Ok calls ->
        encode_messages Chat [ user "x"; assistant ~tool_calls:calls () ]
    | Error e -> e
  in
  check "dsml escapes a closing tag when it renders a string value"
    (contains rendered dsml_value);

  let glm_value = "x &lt;/arg_value> y &amp;lt;/arg_value> z" in
  let glm_block =
    glm_tool_call_open ^ "write" ^ glm_arg_key_open ^ "content"
    ^ glm_arg_key_close ^ glm_arg_value_open ^ glm_value ^ glm_arg_value_close
    ^ glm_tool_call_close
  in
  check "glm unescapes a closing tag fed a few bytes at a time"
    (List.for_all
       (fun n ->
         first_arg
           (stream_calls (glm_stream_events Chat (chunks_of n glm_block)))
         = Some "x </arg_value> y &lt;/arg_value> z")
       [ 1; 2; 3; 5; 7 ]);
  let evs = glm_stream_events Chat [ glm_block ] in
  check "glm unescapes a closing tag in a value"
    (first_arg (stream_calls evs) = Some "x </arg_value> y &lt;/arg_value> z");

  let qwen_value = "x &lt;/parameter> y &amp;lt;/parameter> z" in
  let qwen_block =
    "<tool_call>\n<function=write>\n<parameter=content>\n" ^ qwen_value
    ^ "\n</parameter>\n</function>\n</tool_call>"
  in
  check "qwen unescapes a closing tag fed a few bytes at a time"
    (List.for_all
       (fun n ->
         first_arg
           (stream_calls (qwen_stream_events Chat (chunks_of n qwen_block)))
         = Some "x </parameter> y &lt;/parameter> z")
       [ 1; 2; 3; 5; 7 ]);
  let evs = qwen_stream_events Chat [ qwen_block ] in
  check "qwen unescapes a closing tag in a value"
    (first_arg (stream_calls evs) = Some "x </parameter> y &lt;/parameter> z");

  if !failures = 0 then Printf.printf "\nAll tests passed.\n"
  else begin
    Printf.printf "\n%d test(s) failed.\n" !failures;
    exit 1
  end
