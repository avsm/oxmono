(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
module C = Dooit.Common
open Termanil_model

let key s = C.digest (String.concat "\000" [ s.service; s.account; s.id ])
let path root s = Filename.concat root (key s ^ ".md")
let receipt p = p ^ ".receipt.json"

let locked root f =
  C.mkdir root;
  let p = Filename.concat root ".lock" in
  if C.exists p then C.regular p;
  let fd =
    Unix.openfile p [ Unix.O_CREAT; Unix.O_RDWR; Unix.O_CLOEXEC ] 0o600
  in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      (try Unix.lockf fd Unix.F_TLOCK 0
       with Unix.Unix_error _ -> C.fail "Reply store is busy");
      f ())

let split raw =
  if not (String.starts_with ~prefix:"---\n" raw) then
    C.fail "Reply needs YAML frontmatter";
  let rec close i =
    match String.index_from_opt raw i '\n' with
    | None -> C.fail "Unclosed reply frontmatter"
    | Some j when String.sub raw i (j - i) = "---" -> (i, j + 1)
    | Some j -> close (j + 1)
  in
  let stop, body = close 4 in
  ( C.yaml (String.sub raw 4 (stop - 4)),
    String.sub raw 0 body,
    String.sub raw body (String.length raw - body) )

let read p =
  let raw = C.read ~limit:(1024 * 1024) p in
  let meta, _, body = split raw in
  if C.field "schema" meta <> "termanil-reply/v1" then
    C.fail "Unsupported reply schema";
  let source =
    {
      service = C.field "service" meta;
      account = C.field "account" meta;
      id = C.field "email" meta;
    }
  in
  if Filename.basename p <> key source ^ ".md" then
    C.fail "Reply source does not match filename";
  let revision = C.digest raw in
  let r = if C.exists (receipt p) then C.load_json (receipt p) else C.obj [] in
  let old_state =
    Option.value (Option.map C.string (C.find "state" r)) ~default:"draft"
  in
  let state =
    if old_state = "uncertain" then old_state
    else if C.find "revision" r = Some (C.str revision) then old_state
    else "draft"
  in
  {
    source;
    thread_id = C.field "thread" meta;
    subject = C.field "subject" meta;
    recipients = List.map C.string (C.items "to" meta);
    body;
    revision;
    state;
    path = p;
  }

let list root =
  if not (C.exists root) then ([], [])
  else
    Array.to_list (Sys.readdir root)
    |> List.sort String.compare
    |> List.fold_left
         (fun (ds, warnings) name ->
           if not (Filename.check_suffix name ".md") then (ds, warnings)
           else
             try (read (Filename.concat root name) :: ds, warnings) with
             | C.Error s -> (ds, (name ^ ": " ^ s) :: warnings)
             | _ -> (ds, (name ^ ": cannot read reply") :: warnings))
         ([], [])

let record ?remote ?submission (d : draft) state =
  C.save_json (receipt d.path)
    (C.obj
       ([ ("revision", C.str d.revision); ("state", C.str state) ]
       @ Option.to_list (Option.map (fun s -> ("email_id", C.str s)) remote)
       @ Option.to_list
           (Option.map (fun s -> ("submission_id", C.str s)) submission)))

let check (d : draft) =
  let now = read d.path in
  if now.revision <> d.revision then
    C.fail "Reply changed on disk. Refresh and review it again.";
  now

let save root ~source ~thread_id ~subject ~recipients ~body ~expected =
  locked root (fun () ->
      if String.length body > 512 * 1024 then C.fail "Reply exceeds 512 KiB";
      let p = path root source in
      let before = C.read_opt p in
      if Option.map C.digest before <> expected then
        C.fail "Reply changed on disk. Your editor buffer was retained.";
      if C.exists p && (read p).state = "uncertain" then
        C.fail "Resolve the uncertain send before changing this reply";
      let header =
        match before with
        | Some raw ->
            let _, header, _ = split raw in
            header
        | None ->
            "---\n"
            ^ C.json_string ~pretty:true
                (C.obj
                   [
                     ("schema", C.str "termanil-reply/v1");
                     ("service", C.str source.service);
                     ("account", C.str source.account);
                     ("email", C.str source.id);
                     ("thread", C.str thread_id);
                     ("subject", C.str subject);
                     ("to", C.arr (List.map C.str recipients));
                   ])
            ^ "\n---\n"
      in
      let raw = header ^ body in
      let unchanged () =
        if C.read_opt p <> before then C.fail "Reply changed during save"
      in
      (match before with
      | None -> C.create_file p raw
      | Some _ -> C.atomic_write ~check:unchanged p raw);
      read p)

let owned root (d : draft) =
  if d.path <> path root d.source then
    C.fail "Reply does not belong to this store"

let queue root d ready =
  locked root (fun () ->
      owned root d;
      let d = check d in
      if d.state <> "draft" && d.state <> "ready" then
        C.fail "Only unsent replies can be queued";
      if ready && String.trim d.body = "" then
        C.fail "Fill in the reply before queueing it";
      record d (if ready then "ready" else "draft");
      read d.path)

let send root drafts ~prepare =
  locked root (fun () ->
      let keys =
        List.map
          (fun (d : draft) ->
            owned root d;
            key d.source)
          drafts
      in
      if List.length keys <> List.length (List.sort_uniq String.compare keys)
      then C.fail "Duplicate reply in batch";
      let drafts =
        List.map
          (fun d ->
            let now = check d in
            if now.state <> "draft" && now.state <> "ready" then
              C.fail "Reply already sent or outcome uncertain";
            now)
          drafts
      in
      (* Preflight the whole batch before any submission. *)
      let plans = List.map (fun d -> (d, prepare d)) drafts in
      let rec run results lines = function
        | [] -> (List.rev results, List.rev lines)
        | (d, submit) :: rest -> (
            ignore (check d);
            record d "uncertain";
            match
              submit ~before_submit:(fun remote -> record d ~remote "uncertain")
            with
            | submission ->
                record d ~submission "sent";
                run (read d.path :: results)
                  (("Sent: " ^ d.subject) :: lines)
                  rest
            | exception _ ->
                ( List.rev (read d.path :: results),
                  List.rev
                    (("Uncertain send: " ^ d.subject
                    ^ ". Batch stopped. Check server submissions before \
                       retrying.")
                    :: lines) ))
      in
      run [] [] plans)

let verify root ~lookup =
  locked root (fun () ->
      let drafts, warnings = list root in
      let lines = ref warnings in
      let drafts =
        List.map
          (fun (d : draft) ->
            if d.state <> "uncertain" then d
            else
              let r = C.load_json (receipt d.path) in
              let remote = Option.map C.string (C.find "email_id" r) in
              let accepted =
                Option.bind remote (fun id -> lookup { d.source with id })
              in
              match accepted with
              | None ->
                  lines :=
                    ("Still uncertain: " ^ d.subject
                   ^ ". No acceptance found. No retry was made.")
                    :: !lines;
                  d
              | Some submission ->
                  (* Retain the revision actually submitted, even if the file was edited. *)
                  C.save_json (receipt d.path)
                    (C.obj
                       (("state", C.str "sent")
                       :: ("submission_id", C.str submission)
                       :: List.remove_assoc "state"
                            (List.remove_assoc "submission_id" (C.assoc r))));
                  lines := ("Verified accepted: " ^ d.subject) :: !lines;
                  read d.path)
          drafts
      in
      ( drafts,
        if !lines = [] then [ "No uncertain submissions" ] else List.rev !lines
      ))
