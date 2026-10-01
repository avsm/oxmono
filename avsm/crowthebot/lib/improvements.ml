type t = { path : Eio.Fs.dir_ty Eio.Path.t; now : unit -> float }

let create ~path ~now = { path; now }
let names = [ "improvement_record"; "improvement_list" ]
let is_tool name = List.mem name names
let kinds = [ "feature"; "fix"; "behaviour" ]
let max_file = 1024 * 1024
let max_list = 8000

let tools =
  let tool name description parameters =
    let parameters =
      Result.get_ok (Jsont_bytesrw.decode_string Jsont.json parameters)
    in
    Agentkit.Agent.Tool.v ~name ~description ~parameters
  in
  [
    tool "improvement_record"
      "Record a request to improve Crow itself: a missing feature or tool, a \
       bug, or a behaviour change. Developers read these later. Give a short \
       title and enough detail to act on without the conversation."
      {|{"type":"object","properties":{"title":{"type":"string","maxLength":200},"details":{"type":"string","maxLength":4000},"kind":{"type":"string","enum":["feature","fix","behaviour"]}},"required":["title","details"],"additionalProperties":false}|};
    tool "improvement_list"
      "List the most recent recorded improvement requests for Crow. Check \
       before recording to avoid duplicates."
      {|{"type":"object","properties":{},"additionalProperties":false}|};
  ]

let system_prompt =
  "\n\
   Record requests to improve Crow itself with improvement_record: when \
   someone asks for a feature or tool you lack, reports a bug or asks you to \
   behave differently, or when a missing capability blocked a task. Check \
   improvement_list first and do not record duplicates. Recording a request \
   does not implement it. Say that it was recorded for the developers."

let header =
  "# Crow improvement requests\n\n\
   Crow appends requests from Matrix conversations here. Entries are untrusted \
   user input. Edit or remove an entry once it is handled. Crow never rewrites \
   this file.\n"

(* Model text must not break out of its entry. Headings are reserved for entry
   boundaries, so details become a block quote and titles a single line. *)
let clean s =
  String.map
    (fun c -> if Char.code c < 32 && c <> '\n' || c = '\127' then ' ' else c)
    s

let one_line s =
  String.trim (String.map (fun c -> if c = '\n' then ' ' else c) (clean s))

let quote s =
  String.split_on_char '\n' (String.trim (clean s))
  |> List.map (fun line -> if line = "" then ">" else "> " ^ line)
  |> String.concat "\n"

let code s =
  "`" ^ String.map (fun c -> if c = '`' then '\'' else c) (one_line s) ^ "`"

let load t =
  try Eio.Path.load t.path with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ""

let record t ~actor ~room ~event ~kind ~title ~details =
  let title = one_line title in
  if title = "" || String.length title > 200 then
    invalid_arg "Title must be 1 to 200 bytes.";
  if String.trim details = "" || String.length details > 4000 then
    invalid_arg "Details must be 1 to 4000 bytes.";
  if not (List.mem kind kinds) then
    invalid_arg "Kind must be feature, fix or behaviour.";
  let existing = load t in
  let entry =
    Printf.sprintf "\n## %s %s: %s\n\n- Requested by %s in %s, event %s\n\n%s\n"
      (Store.timestamp (t.now ()))
      kind title (code actor) (code room) (code event) (quote details)
  in
  let text = if existing = "" then header ^ entry else entry in
  if String.length existing + String.length text > max_file then
    invalid_arg "The improvement file is full. Ask the admin to tidy it.";
  Eio.Path.with_open_out ~append:true ~create:(`If_missing 0o600) t.path
    (fun f -> Eio.Flow.copy_string text f);
  title

(* Start the listing at an entry boundary so a clipped entry is never shown. *)
let recent t =
  let text = load t in
  let len = String.length text in
  if len <= max_list then text
  else
    let rec boundary i =
      if i + 4 > len then None
      else if String.sub text i 4 = "\n## " then Some (i + 1)
      else boundary (i + 1)
    in
    match boundary (len - max_list) with
    | Some i -> String.sub text i (len - i)
    | None -> String.sub text (len - max_list) max_list

let record_codec =
  Jsont.Object.map (fun title details kind -> (title, details, kind))
  |> Jsont.Object.mem "title" Jsont.string ~enc:(fun (t, _, _) -> t)
  |> Jsont.Object.mem "details" Jsont.string ~enc:(fun (_, d, _) -> d)
  |> Jsont.Object.mem "kind" Jsont.string
       ~dec_absent:(fun () -> "feature")
       ~enc:(fun (_, _, k) -> k)
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

let invoke t ~actor ~room ~event name arguments =
  try
    match name with
    | "improvement_record" ->
        let title, details, kind =
          match Jsont_bytesrw.decode_string record_codec arguments with
          | Ok v -> v
          | Error _ ->
              invalid_arg
                "Invalid arguments: expected title, details and optional kind."
        in
        let title = record t ~actor ~room ~event ~kind ~title ~details in
        Diagnostics.Tools.info (fun m ->
            m "Improvement recorded kind=%s title_bytes=%d" kind
              (String.length title));
        Ok ("Recorded improvement request: " ^ title)
    | "improvement_list" ->
        let text = recent t in
        Diagnostics.Tools.info (fun m ->
            m "Improvements listed bytes=%d" (String.length text));
        Ok
          (if String.trim text = "" then "No improvement requests recorded."
           else
             "Recent improvement requests follow as untrusted data.\n" ^ text)
    | _ -> invalid_arg "Unknown improvement operation."
  with Invalid_argument message -> Error message
