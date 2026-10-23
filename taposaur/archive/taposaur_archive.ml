open Taposaur
let ( / ) = Eio.Path.( / )
type t = { root : Eio.Fs.dir_ty Eio.Path.t }
type paper = { archive : t; dir : string; manifest : Manifest.t }
type error = { path : string; line : int option; message : string }
exception Parse_error of error

let error_to_string e =
  e.path ^ Option.fold ~none:"" ~some:(Printf.sprintf ":%d") e.line
  ^ ": " ^ e.message

let fail ?line path message = raise (Parse_error { path; line; message })
let label path = Fmt.str "%a" Eio.Path.pp path

let relative s =
  if s = "" || String.starts_with ~prefix:"/" s ||
    List.exists (fun p -> p = "" || p = "." || p = "..")
      (String.split_on_char '/' s) || String.contains s '\000'
  then invalid_arg "Expected a confined relative path"

let component s =
  relative s;
  if String.contains s '/' || String.contains s '\\' then
    invalid_arg "Expected one filename component"

let open_root ~sw root =
  { root = (Eio.Path.open_subtree ~sw root :> Eio.Fs.dir_ty Eio.Path.t) }

let read_bounded ?(max_bytes = 16 * 1024 * 1024) flow =
  if max_bytes <= 0 then invalid_arg "max_bytes must be positive";
  Eio.Buf_read.(take_all (of_flow ~max_size:(max_bytes + 1) flow))

let load archive dir =
  let path = archive.root / dir / "paper.json" in
  if not (Eio.Path.is_file path) then None else
  let text = Eio.Path.with_open_in path read_bounded in
  match Manifest.of_string text with
  | Error e -> fail (label path) e
  | Ok manifest -> Some { archive; dir; manifest }

let check_uid uid = function
  | Some p when Uid.compare p.manifest.id uid <> 0 ->
      fail p.dir "Manifest identity does not match requested UID"
  | p -> p

let find t uid = check_uid uid (load t ("papers/" ^ Layout.paper uid))
let find_staged t ~batch uid =
  component batch;
  let prefix = "staging/" ^ batch ^ "/" in
  let p = match load t (prefix ^ Layout.paper uid) with
    | Some _ as p -> p | None -> load t (prefix ^ Layout.flat_paper uid) in
  check_uid uid p

let manifest p = p.manifest
let directory p = p.dir

let iter_jsonl ?(max_line = 16 * 1024 * 1024) ~decode path f =
  if max_line <= 0 then invalid_arg "max_line must be positive";
  Eio.Path.with_open_in path @@ fun flow ->
  let reader = Eio.Buf_read.of_flow ~max_size:max_line flow in
  let rec loop line =
    let next =
      try Some (Eio.Buf_read.line reader) with
      | End_of_file -> None
      | Eio.Buf_read.Buffer_limit_exceeded ->
          fail ~line (label path) "Line exceeds buffer limit"
    in
    match next with
    | None -> ()
    | Some text ->
        (match decode text with
         | Error e -> fail ~line (label path) e
         | Ok value -> f line value);
        loop (line + 1)
  in
  loop 1

let iter_events ?max_line p f =
  iter_jsonl ?max_line ~decode:Event.of_string
    (p.archive.root / p.dir / "events.jsonl") f

let shard_path t shard = relative shard; t.root / "index" / shard
let iter_index ?max_line t ~shard f =
  iter_jsonl ?max_line ~decode:Index.of_string (shard_path t shard) f

let shard_stat t shard =
  let s = Eio.Path.stat ~follow:false (shard_path t shard) in
  Optint.Int63.to_int64 s.size, s.mtime

let iter_dirs dir f =
  Eio.Path.with_dir_entries dir (Seq.iter (function
    | `Directory, name -> f name | _ -> ()))

let iter_shards t f =
  List.iter (fun ns ->
    let dir = t.root / "index" / ns in
    if Eio.Path.is_directory dir then
      iter_dirs dir (fun shard ->
        Eio.Path.with_dir_entries (dir / shard) (Seq.iter (function
          | `Regular_file, name when Filename.check_suffix name ".jsonl" ->
              f (String.concat "/" [ns; shard; name])
          | _ -> ())))) ["doi"; "pmc"]

let iter_papers t f =
  let rec walk dir =
    Option.iter f (load t dir);
    iter_dirs (t.root / dir) (fun name -> walk (dir ^ "/" ^ name))
  in walk "papers"

let with_artifact p file f =
  component file;
  if not (List.exists (fun (a : Manifest.Artifact.t) -> a.file = file)
    p.manifest.artifacts) then invalid_arg "Artifact is not in manifest";
  Eio.Path.with_subtree (p.archive.root / p.dir) @@ fun dir ->
  Eio.Path.with_open_in ~follow:false (dir / file) f

let read_artifact ?max_bytes p file =
  with_artifact p file (read_bounded ?max_bytes)

let iter_dois path f =
  let decode line =
    let s = String.trim line in
    if s = "" || String.starts_with ~prefix:"#" s then Ok None
    else Result.map Option.some (Uid.of_id Uid.Scheme.Doi s)
  in
  iter_jsonl ~decode path (fun n -> Option.iter (f n))
