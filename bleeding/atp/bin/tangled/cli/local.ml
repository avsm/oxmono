(* SPDX-License-Identifier: ISC *)
(* The Git working tree a command runs in. Commands default to the Tangled
   repository it is a clone of, and to its branch and commits. *)

(* [git args] is the trimmed output of [git args], or [None] if it fails,
   as it does outside a working tree. *)
let git args =
  let command =
    String.concat " " (List.map Filename.quote ("git" :: args)) ^ " 2>/dev/null"
  in
  let channel = Unix.open_process_in command in
  let output = In_channel.input_all channel in
  match Unix.close_process_in channel with
  | Unix.WEXITED 0 -> Some (String.trim output)
  | _ -> None

let strip_suffix ~suffix s =
  if String.ends_with ~suffix s then
    String.sub s 0 (String.length s - String.length suffix)
  else s

(* [spec_of_url url] is the repository a remote URL names, in a form
   {!Tangled.Api.resolve_repo} reads. [tangled repo clone] names it by its
   DID, as in [https://knot.example/did:plc:abc]. Older clones name an owner
   and a repository, as in [git@tangled.org:alice.example/demo]. *)
let spec_of_url url =
  let url = strip_suffix ~suffix:".git" (strip_suffix ~suffix:"/" url) in
  let path =
    match String.index_opt url ':' with
    | Some i when String.starts_with ~prefix:"git@" url ->
        String.sub url (i + 1) (String.length url - i - 1)
    | _ -> (
        match String.index_from_opt url (String.length "https://") '/' with
        | Some i when String.contains url ':' ->
            String.sub url (i + 1) (String.length url - i - 1)
        | _ -> "")
  in
  let parts = List.filter (( <> ) "") (String.split_on_char '/' path) in
  match List.rev parts with
  | did :: _ when String.starts_with ~prefix:"did:" did -> Some did
  | [ name; owner ] ->
      let owner =
        if String.starts_with ~prefix:"@" owner then
          String.sub owner 1 (String.length owner - 1)
        else owner
      in
      if String.contains owner '.' || String.starts_with ~prefix:"did:" owner
      then Some (owner ^ "/" ^ name)
      else None
  | _ -> None

(* [remotes ()] is the clone's remotes, [origin] first. *)
let remotes () =
  match git [ "remote" ] with
  | None | Some "" -> []
  | Some names ->
      let names = String.split_on_char '\n' names in
      let origin, others = List.partition (( = ) "origin") names in
      origin @ others

let repo () =
  List.find_map
    (fun remote ->
      Option.bind (git [ "remote"; "get-url"; remote ]) spec_of_url)
    (remotes ())

let branch () =
  match git [ "symbolic-ref"; "--quiet"; "--short"; "HEAD" ] with
  | Some "" -> None
  | b -> b

let head () = git [ "rev-parse"; "--verify"; "--quiet"; "HEAD^{commit}" ]
let has_commit sha = git [ "cat-file"; "-e"; sha ^ "^{commit}" ] <> None
let merge_base a b = git [ "merge-base"; a; b ]

let rev_parse rev =
  git [ "rev-parse"; "--verify"; "--quiet"; rev ^ "^{commit}" ]

(* [run args] runs [git args] with its output on the terminal, and is [true]
   iff it succeeds. *)
let run args =
  Sys.command (String.concat " " (List.map Filename.quote ("git" :: args))) = 0
