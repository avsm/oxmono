(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* numptyd's loop. One fiber reads calls, runs them in the order they arrive
   and answers each with one result, so the trace lines written while a call
   runs can carry that call's id without anything having to be tracked.

   Nothing here writes a file. What a fetch returns goes back over the pipe. *)

let default_max_bytes = 1024 * 1024
let max_bytes_ceiling = Agentkit.Line.max_line / 8

(* How long curl has for a whole transfer. The peer bounds a fetch at a minute
   and kills numptyd for exceeding it, so this sits below that: a curl that
   gives up says why, where a killed numptyd ends the run. *)
let curl_max_time = 45

(* What a run may write before the rest is dropped. The same mebibyte a fetch
   is bounded at, since both end up in one line of the protocol. *)
let run_limit = default_max_bytes

(* How much of curl's own standard error is kept, being enough for the several
   lines it writes about a failure and not enough for a stray program's output
   to fill a result. *)
let stderr_limit = 8192

(* Room kept past a fetch's bound for curl's report, which is written after the
   body. Without it a body at the bound would take the report with it and the
   answer could not say what came back. *)
let tail_room = 65536

(* [clip n s] is [s] on one line and within [n] bytes, counting what it drops.
   A trace goes in a column of an interface, and a protocol fault carries a line
   that may be a whole document. The count is part of what fits, so the answer
   is no longer than [n]. *)
let clip n s =
  let s =
    String.map (fun c -> if c = '\n' || c = '\r' || c = '\t' then ' ' else c) s
  in
  if String.length s <= n then s
  else
    let dropped = Printf.sprintf "… (%d bytes)" (String.length s) in
    String.sub s 0 (max 0 (n - String.length dropped)) ^ dropped

(* Nothing numptyd runs is given numptyd's own standard input, which is the pipe
   the protocol arrives on. A curl that asked for a password, or a program that
   read its input, would take the peer's next call for its own, and the call
   would then never be answered. Every child is handed an empty one instead, so
   a program that reads sees the end of its input at once. *)
let no_input = Eio.Flow.string_source ""
let shut f = try Eio.Flow.close f with Eio.Io _ | Invalid_argument _ -> ()

(* [drain ~limit flow] is the first [limit] bytes [flow] carries and the number
   of bytes it carried in all. It reads to the end whatever the limit is: a
   child whose output nobody takes fills its pipe and never exits. *)
let drain ~limit flow =
  let b = Buffer.create 4096 in
  let total = ref 0 in
  let r = Eio.Buf_read.of_flow flow ~max_size:0x10000 in
  (try
     while not (Eio.Buf_read.at_end_of_input r) do
       let s = Eio.Buf_read.take (Eio.Buf_read.buffered_bytes r) r in
       total := !total + String.length s;
       let room = limit - Buffer.length b in
       if room > 0 then
         Buffer.add_string b
           (if String.length s <= room then s else String.sub s 0 room)
     done
   with End_of_file | Eio.Io _ -> ());
  (Buffer.contents b, !total)

(* [collect ~proc ~combined ~limit argv] runs [argv] and is its exit status, the
   first [limit] bytes of its standard output, how many bytes that output was in
   all, and its standard error. [combined] sends its standard error to the same
   pipe, which is what a run reports and what a curl must not do, since curl's
   own complaints would then be read as part of the document.

   Both pipes are drained at once. A child filling one while this end reads only
   the other blocks for good. *)
let collect ~proc ?(combined = false) ~limit argv =
  Eio.Switch.run @@ fun sw ->
  let out_r, out_w = Eio_unix.pipe sw in
  let err = if combined then None else Some (Eio_unix.pipe sw) in
  let stderr = match err with None -> out_w | Some (_, w) -> w in
  let child =
    Eio.Process.spawn ~sw proc ~stdin:no_input ~stdout:out_w ~stderr argv
  in
  (* This end of each of the child's own sides, closed at once. Left open, the
     drain below would never reach the end of the child's output. *)
  shut out_w;
  (match err with Some (_, w) -> shut w | None -> ());
  let out = ref ("", 0) and said = ref "" in
  (match err with
  | None -> out := drain ~limit out_r
  | Some (r, _) ->
      Eio.Fiber.both
        (fun () -> out := drain ~limit out_r)
        (fun () -> said := fst (drain ~limit:stderr_limit r)));
  let status = Eio.Process.await child in
  (status, fst !out, snd !out, !said)

(* ------------------------------------------------------------------ *)
(* Asking curl                                                         *)
(* ------------------------------------------------------------------ *)

(* curl writes its report of the transfer after the body, on the same stream,
   introduced by this. A body may hold the same bytes, and curl writes exactly
   one report after whatever the body was, so the split takes the last
   occurrence and is right whatever the document contained.

   The alternative, dumping the response headers with -D, puts them before the
   body and leaves the boundary to be found by counting response blocks, which a
   document that itself begins like an HTTP response would break. *)
let delimiter = "\n@@numptyd-transfer@@\n"

(* [%{header_json}] arrived in curl 7.83. An older curl warns on its standard
   error and writes nothing for it, which leaves a refusal that cannot name the
   size rather than a numptyd that does not work. *)
let write_out =
  delimiter ^ "%{http_code}\n%{url_effective}\n%{content_type}\n%{header_json}"

type transfer = {
  status : int;
  url : string;  (** the final URL, after every redirect curl followed *)
  content_type : string;
  headers : string;  (** the response headers as JSON, or [""] *)
}

(* The URL is passed as [--url] rather than as a bare argument, so that one
   beginning with a dash is a URL and not an option. *)
let curl_args ~url ~bound =
  [
    "curl";
    "--silent";
    "--show-error";
    "--location";
    "--max-time";
    string_of_int curl_max_time;
    "--max-filesize";
    string_of_int bound;
    "--write-out";
    write_out;
    "--url";
    url;
  ]

let last_index s sub =
  let n = String.length s and m = String.length sub in
  let rec go i =
    if i < 0 then None else if String.sub s i m = sub then Some i else go (i - 1)
  in
  if m > n then None else go (n - m)

(* [split out] is what curl wrote before its report and the report itself. *)
let split out =
  match last_index out delimiter with
  | None -> None
  | Some i ->
      let j = i + String.length delimiter in
      Some (String.sub out 0 i, String.sub out j (String.length out - j))

(* The report's first three lines are one value each, and everything after them
   is the header JSON, which curl writes over several lines. *)
let transfer report =
  let rec take n s acc =
    if n = 0 then (List.rev acc, s)
    else
      match String.index_opt s '\n' with
      | None -> (List.rev (s :: acc), "")
      | Some i ->
          take (n - 1)
            (String.sub s (i + 1) (String.length s - i - 1))
            (String.sub s 0 i :: acc)
  in
  match take 3 report [] with
  | [ status; url; content_type ], headers ->
      {
        status = Option.value ~default:0 (int_of_string_opt status);
        url;
        content_type;
        headers;
      }
  | _ -> { status = 0; url = ""; content_type = ""; headers = "" }

(* The size the server declared, out of curl's header report. A header's value
   is an array there, since a header may be repeated. *)
let declared_length headers =
  match Dsml.Json.Value.of_string headers with
  | Ok (Jsont.Object (mems, _)) -> (
      match Option.map snd (Jsont.Json.find_mem "content-length" mems) with
      | Some (Jsont.Array (Jsont.String (v, _) :: _, _)) -> int_of_string_opt v
      | Some _ | None -> None)
  | Ok _ | Error _ -> None

let exit_code = function `Exited n -> n | `Signaled n -> -n

(* Whether the body is HTML, which is what the text render reduces. A body that
   is not is returned as it stands, since the reduction would take a JSON
   document or a patch apart. *)
let is_html content_type =
  let s = String.lowercase_ascii content_type in
  let n = String.length s in
  let rec go i = i + 4 <= n && (String.sub s i 4 = "html" || go (i + 1)) in
  go 0

let fetch ~proc ~trace ~url ~render ~max_bytes =
  let bound =
    match max_bytes with
    | None -> default_max_bytes
    | Some n -> max 1 (min n max_bytes_ceiling)
  in
  trace ("fetch: " ^ clip 120 url);
  let status, out, total, said =
    collect ~proc ~limit:(bound + tail_room) (curl_args ~url ~bound)
  in
  match split out with
  | None ->
      (* No report, so either curl failed before it wrote one, or the body ran
         past what was kept and took the report with it. *)
      if total > bound then Report.refused ~url ~size:None ~bound
      else Report.failed ~what:"fetch" ~url ~code:(exit_code status) ~said
  | Some (body, report) ->
      let t = transfer report in
      if String.length body > bound then
        Report.refused ~url ~size:(Some (String.length body)) ~bound
      else if exit_code status = 63 then
        Report.refused ~url ~size:(declared_length t.headers) ~bound
      else if exit_code status <> 0 then
        Report.failed ~what:"fetch" ~url ~code:(exit_code status) ~said
      else
        let reduced = render = Proto.Text && is_html t.content_type in
        Report.fetch ~status:t.status ~url:t.url ~content_type:t.content_type
          ~bytes:(String.length body)
          ~body:(if reduced then Report.text_of_html body else body)
          ~reduced

let head ~proc ~trace ~url =
  trace ("head: " ^ clip 120 url);
  let status, out, _, said =
    collect ~proc ~limit:(tail_room * 2)
      (curl_args ~url ~bound:default_max_bytes @ [ "--head" ])
  in
  match split out with
  | None -> Report.failed ~what:"head" ~url ~code:(exit_code status) ~said
  | Some (_headers, report) ->
      if exit_code status <> 0 then
        Report.failed ~what:"head" ~url ~code:(exit_code status) ~said
      else
        let t = transfer report in
        Report.head ~status:t.status ~url:t.url ~content_type:t.content_type
          ~length:(declared_length t.headers)

let run_program ~proc ~trace ~program ~args =
  trace ("run: " ^ clip 120 (String.concat " " (program :: args)));
  match collect ~proc ~combined:true ~limit:run_limit (program :: args) with
  | status, out, total, _ -> Report.ran ~program ~status ~output:out ~total
  | exception (Eio.Cancel.Cancelled _ as e) -> raise e
  | exception (Eio.Io _ as e) ->
      Report.no_program ~program ~reason:(Printexc.to_string e)

(* ------------------------------------------------------------------ *)
(* The loop                                                            *)
(* ------------------------------------------------------------------ *)

(* The version line curl opens with, cut to its first two words. The whole line
   names every library it was built against, which says nothing a peer or a
   journal needs. *)
let curl_version ~proc =
  match collect ~proc ~limit:4096 [ "curl"; "--version" ] with
  | `Exited 0, out, _, _ -> (
      let line = List.hd (String.split_on_char '\n' (out ^ "\n")) in
      match String.split_on_char ' ' (String.trim line) with
      | name :: version :: _ -> Some (name ^ " " ^ version)
      | [ one ] when one <> "" -> Some one
      | _ -> Some "curl")
  | _ -> None
  | exception (Eio.Cancel.Cancelled _ as e) -> raise e
  | exception _ -> None

let note = function
  | Some version -> "numpty: " ^ version
  | None -> "numpty: no curl on the PATH, so fetch and head have nothing to run"

let run ~stdin ~stdout ~proc () =
  Eio.Buf_write.with_flow stdout @@ fun w ->
  let reader = Eio.Buf_read.of_flow stdin ~max_size:Agentkit.Line.max_line in
  (* The call in flight, which every trace written while it runs belongs to.
     Startup and anything else outside a call has none. *)
  let current = ref None in
  (* Each message is flushed as it is written. A trace exists to say what a call
     that has not answered is waiting on, and one still in a buffer says
     nothing. *)
  let send msg =
    Proto.write_to_client w msg;
    Eio.Buf_write.flush w
  in
  let trace line = send (Proto.Trace { id = !current; line }) in
  let curl = curl_version ~proc in
  send (Proto.Hello { status = note curl; curl = Option.is_some curl });
  let with_curl f = if Option.is_some curl then f () else Report.no_curl in
  (* A failure is reported as the text of a result, since the peer hands that
     text to a model, which can then correct itself. Resource exhaustion is not
     a tool failure, and describing it as one would hide it while the process is
     already in trouble. *)
  let guard f =
    try f () with
    | (Out_of_memory | Stack_overflow | Eio.Cancel.Cancelled _) as e -> raise e
    | e -> "Error: " ^ Printexc.to_string e
  in
  let dispatch (op : Proto.op) =
    guard @@ fun () ->
    match op with
    | Proto.Fetch { url; render; max_bytes } ->
        with_curl (fun () -> fetch ~proc ~trace ~url ~render ~max_bytes)
    | Proto.Head { url } -> with_curl (fun () -> head ~proc ~trace ~url)
    | Proto.Run { program; args } -> run_program ~proc ~trace ~program ~args
  in
  let rec loop () =
    match Proto.read_to_server reader with
    | `Eof -> ()
    | `Msg Proto.Shutdown -> ()
    | `Bad line ->
        (* The peer is this same binary, so a line it cannot write is a fault
           rather than bad input, and one over the length limit leaves the
           reader part way through a line it will never finish. Either way
           nothing later on this connection is worth reading. *)
        failwith
          (Printf.sprintf "numptyd read a line that is not a message: %s"
             (clip 200 line))
    | `Msg (Proto.Call { id; op }) ->
        current := Some id;
        let output = dispatch op in
        current := None;
        send (Proto.Result { id; output });
        loop ()
  in
  loop ()
