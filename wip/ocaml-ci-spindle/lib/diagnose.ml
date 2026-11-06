(* SPDX-License-Identifier: ISC *)

type t = {
  fetch : Fetch_httpz.t;
  clock : Eio.Time.Mono.ty Eio.Resource.t;
  url : string;
  model : string;
  key : string option;
  timeout : float;
}

type failure = { workflow : string; error : string option; log : string }

let default_model = "Qwen/Qwen3.8-27B-FP8"

(* Plain http is for a model on the spindle's own machine or network, and a
   key, which would cross it in the clear, is sent only over https or to a
   loopback address. *)
let check_url ~key url =
  let after prefix =
    if String.starts_with ~prefix url then
      Some (String.sub url (String.length prefix)
              (String.length url - String.length prefix))
    else None
  in
  match (after "https://", after "http://") with
  | Some _, _ -> ()
  | None, Some rest ->
      let authority =
        match String.index_opt rest '/' with
        | Some i -> String.sub rest 0 i
        | None -> rest
      in
      let host =
        match String.rindex_opt authority ':' with
        | Some i when not (String.ends_with ~suffix:"]" authority) ->
            String.sub authority 0 i
        | _ -> authority
      in
      let loopback =
        List.mem (String.lowercase_ascii host)
          [ "localhost"; "127.0.0.1"; "[::1]" ]
      in
      if key <> None && not loopback then
        invalid_arg
          "Diagnose.v: an API key is sent only over https or to a loopback \
           address"
  | None, None -> invalid_arg "Diagnose.v: the URL must be http or https"

let v ?key ?(model = default_model) ?(timeout = 600.) ~clock fetch url =
  check_url ~key url;
  { fetch; clock :> Eio.Time.Mono.ty Eio.Resource.t; url; model; key; timeout }

let model t = t.model
let url t = t.url

(* Bounds on what is sent, so that a huge log cannot make a huge request. *)
let max_log = 24 * 1024
let max_source = 32 * 1024
let max_sources = 6

(* A repository may hold any number of root files, so they are bounded too,
   as is the whole prompt, which goes to a paid API. *)
let max_root = 8
let max_prompt = 256 * 1024

let tail n s =
  if String.length s <= n then s
  else "...\n" ^ String.sub s (String.length s - n) n

let contains s sub =
  let n = String.length sub in
  let rec at i =
    i + n <= String.length s && (String.sub s i n = sub || at (i + 1))
  in
  at 0

(* The failed step is the last one, so the log is kept from its start where it
   fits. *)
let relevant log =
  let lines = String.split_on_char '\n' log in
  let rec last_step acc = function
    | [] -> acc
    | l :: rest when String.starts_with ~prefix:"## " l -> last_step [ l ] rest
    | l :: rest -> last_step (l :: acc) rest
  in
  let step = String.concat "\n" (List.rev (last_step [] lines)) in
  tail max_log (if String.length step > 200 then step else log)

(* Paths the compiler or dune names, such as [File "src/a.ml", line 3]. *)
let mentioned log =
  let found = ref [] in
  let prefix = "File \"" in
  let n = String.length prefix in
  let rec scan i =
    match String.index_from_opt log i 'F' with
    | None -> ()
    | Some j ->
        (if j + n <= String.length log && String.sub log j n = prefix then
           match String.index_from_opt log (j + n) '"' with
           | Some k ->
               let path = String.sub log (j + n) (k - j - n) in
               if
                 path <> ""
                 && (not (List.mem path !found))
                 && (not (String.starts_with ~prefix:"/" path))
                 && not (contains path "..")
               then found := path :: !found
           | None -> ());
        scan (j + 1)
  in
  scan 0;
  List.rev !found

let system_prompt =
  "You are an expert OCaml engineer diagnosing failed continuous integration \
   builds. Each build installs a project's opam dependencies and runs `dune \
   build @install @check @runtest`. Explain the cause of each failure \
   concisely as Markdown bullet points, grouping builds that fail for the same \
   reason. Where the fix is small and you are confident of it, give it as a \
   unified diff against the files shown. Do not invent files you have not \
   seen. If the failure is in the CI infrastructure rather than the project, \
   say so."

let prompt ~(files : Spindle.Executor.files) ~passed failures =
  let b = Buffer.create 65536 in
  let add fmt = Printf.bprintf b fmt in
  List.iter
    (fun f ->
      add "# Failed build: %s\n" f.workflow;
      Option.iter (add "Error: %s\n") f.error;
      add "\n```\n%s\n```\n\n" (relevant f.log))
    failures;
  if passed <> [] then
    add "These builds passed: %s.\n\n" (String.concat ", " passed);
  let root =
    files.list ""
    |> List.filter (fun f ->
        Filename.check_suffix f ".opam" || f = "dune-project" || f = "dune")
  in
  let mentioned = List.concat_map (fun f -> mentioned f.log) failures in
  let root =
    List.sort_uniq String.compare root |> List.filteri (fun i _ -> i < max_root)
  in
  let sources =
    root
    @ (List.filter (fun f -> not (List.mem f root)) mentioned
      |> List.filteri (fun i _ -> i < max_sources))
  in
  List.iter
    (fun path ->
      match files.read path with
      | Some text
        when String.length text <= max_source
             && Buffer.length b + String.length text < max_prompt ->
          add "# File %s\n\n```\n%s\n```\n\n" path text
      | _ -> ())
    sources;
  Buffer.contents b

let request t ~prompt =
  let open Jsont.Json in
  let message role content =
    object'
      [ mem (name "role") (string role); mem (name "content") (string content) ]
  in
  object'
    [
      mem (name "model") (string t.model);
      mem (name "temperature") (number 0.2);
      mem (name "max_tokens") (int 8192);
      mem (name "messages")
        (list [ message "system" system_prompt; message "user" prompt ]);
    ]

let member k = function
  | Jsont.Object (mems, _) ->
      List.find_map
        (fun ((n, _), v) -> if String.equal n k then Some v else None)
        mems
  | _ -> None

(* Reasoning models may think aloud first, which is not the answer. *)
let strip_thinking s =
  let close = "</think>" in
  let n = String.length close in
  let rec last i found =
    match String.index_from_opt s i '<' with
    | None -> found
    | Some j ->
        if j + n <= String.length s && String.sub s j n = close then
          last (j + 1) (Some (j + n))
        else last (j + 1) found
  in
  match last 0 None with
  | Some k -> String.trim (String.sub s k (String.length s - k))
  | None -> String.trim s

let answer body =
  match Jsont_bytesrw.decode_string Jsont.json body with
  | Error e -> Error ("unreadable response: " ^ e)
  | Ok json -> (
      match member "choices" json with
      | Some (Jsont.Array (choice :: _, _)) -> (
          match Option.bind (member "message" choice) (member "content") with
          | Some (Jsont.String (s, _)) -> Ok (strip_thinking s)
          | _ -> Error "the response has no message")
      | _ -> (
          match Option.bind (member "error" json) (member "message") with
          | Some (Jsont.String (s, _)) -> Error s
          | _ -> Error "the response has no choices"))

let ask t ~prompt =
  let body =
    match Jsont_bytesrw.encode_string Jsont.json (request t ~prompt) with
    | Ok s -> s
    | Error e -> failwith e
  in
  let headers =
    let open Fetch.Header in
    match t.key with
    | Some key ->
        [
          raw "Content-Type" "application/json";
          raw "Authorization" ("Bearer " ^ key);
        ]
    | None -> [ raw "Content-Type" "application/json" ]
  in
  let endpoint =
    (if String.ends_with ~suffix:"/" t.url then t.url else t.url ^ "/")
    ^ "chat/completions"
  in
  match
    Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds t.clock t.timeout)
      (fun () ->
        Fetch.with_response ~headers ~body:(Fetch.String body) ~redirects:0
          t.fetch `POST endpoint (fun r ->
            ( Fetch.status r,
              Eio.Buf_read.(
                parse_exn ~max_size:(4 * 1024 * 1024) take_all (Fetch.body r))
            )))
  with
  | 200, body -> answer body
  | code, body -> (
      match answer body with
      | Error why -> Error (Printf.sprintf "HTTP %d: %s" code why)
      | Ok _ -> Error (Printf.sprintf "HTTP %d" code))
  | exception Eio.Time.Timeout -> Error "the model did not answer in time"
  | exception (Eio.Cancel.Cancelled _ as e) -> raise e
  | exception (Eio.Io _ as e) -> Error (Printexc.to_string e)
