(* SPDX-License-Identifier: ISC *)

type t = { opam_files : (string * string) list }

(* opam's own rule for package names, which keeps them safe in shell commands
   and OBuilder specs. *)
let valid_name n =
  n <> ""
  && n.[0] <> '-'
  && String.exists (function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false) n
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '+' -> true
         | _ -> false)
       n

let find (files : Spindle.Executor.files) =
  let opam =
    files.list ""
    |> List.filter (fun f ->
        Filename.check_suffix f ".opam"
        && valid_name (Filename.chop_suffix f ".opam"))
    |> List.sort String.compare
    |> List.filter_map (fun f ->
        Option.map (fun text -> (f, text)) (files.read f))
  in
  if opam = [] then None else Some { opam_files = opam }

let packages t =
  List.map
    (fun (f, text) -> (Filename.chop_suffix f ".opam", text))
    t.opam_files

type remote = { url : string; host : string; branch : string option }

let max_remotes = 4

let safe_url_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '.' | '_' | '~' | '/' | ':'
  | '%' | '@' | '+' ->
      true
  | _ -> false

(* Git's own rules for a ref name (git check-ref-format), restricted to
   characters a shell and an opam URL read literally. *)
let safe_branch b =
  let component c =
    c <> ""
    && c.[0] <> '.'
    && (not (String.ends_with ~suffix:".lock" c))
    && String.for_all
         (function
           | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '.' | '_' -> true
           | _ -> false)
         c
  in
  let rec no_dots i =
    i + 1 >= String.length b
    || ((b.[i] <> '.' || b.[i + 1] <> '.') && no_dots (i + 1))
  in
  b <> ""
  && String.length b <= 200
  && b.[0] <> '-'
  && (not (String.ends_with ~suffix:"." b))
  && List.for_all component (String.split_on_char '/' b)
  && no_dots 0

(* A host is a name of letters, digits and hyphens whose last label has a
   letter, so that no spelling of an address passes, and is not one of the
   names that only a local network resolves. Where it resolves is checked
   again when it is fetched. *)
let public_name h =
  let labels = String.split_on_char '.' h in
  let label l =
    l <> ""
    && String.length l <= 63
    && l.[0] <> '-'
    && l.[String.length l - 1] <> '-'
    && String.for_all
         (function 'a' .. 'z' | '0' .. '9' | '-' -> true | _ -> false)
         l
  in
  List.length labels >= 2
  && List.for_all label labels
  && String.exists
       (function 'a' .. 'z' -> true | _ -> false)
       (List.nth labels (List.length labels - 1))
  && not
       (List.exists
          (fun s -> h = s || String.ends_with ~suffix:("." ^ s) h)
          [ "localhost"; "local"; "internal"; "lan"; "home.arpa"; "test"; "invalid" ])

let remote_of_string s =
  let s =
    if String.starts_with ~prefix:"git+" s then
      String.sub s 4 (String.length s - 4)
    else s
  in
  let url, branch =
    match String.index_opt s '#' with
    | Some i ->
        let n = String.length s - i - 1 in
        (String.sub s 0 i, Some (String.sub s (i + 1) n))
    | None -> (s, None)
  in
  let prefix = "https://" in
  if not (String.starts_with ~prefix url) then
    Error "only https remotes are used"
  else if String.length url > 300 || not (String.for_all safe_url_char url) then
    Error "the address has characters a remote may not have"
  else
    let rest = String.sub url 8 (String.length url - 8) in
    let authority =
      match String.index_opt rest '/' with
      | Some i -> String.sub rest 0 i
      | None -> rest
    in
    let host = String.lowercase_ascii authority in
    if String.contains authority '@' then
      Error "a remote may not carry credentials"
    else if String.contains authority ':' then
      Error "a remote is reached on the https port, without naming one"
    else if not (public_name host) then Error "the host must be a public name"
    else
      match branch with
      | Some b when not (safe_branch b) ->
          Error "the branch name is not one git allows"
      | Some "" | None -> Ok { url; host; branch = None }
      | Some b -> Ok { url; host; branch = Some b }

let field_strings (v : OpamParserTypes.FullPos.value) =
  match v.pelem with
  | String s -> [ s ]
  | List { pelem = l; _ } ->
      List.filter_map
        (fun (v : OpamParserTypes.FullPos.value) ->
          match v.pelem with String s -> Some s | _ -> None)
        l
  | _ -> []

let remotes t =
  let entries =
    List.concat_map
      (fun (_, text) ->
        match OpamFile.OPAM.read_from_string text with
        | opam -> (
            match
              OpamStd.String.Map.find_opt "x-opam-repositories"
                (OpamFile.OPAM.extensions opam)
            with
            | Some v -> field_strings v
            | None -> [])
        | exception _ -> [])
      t.opam_files
  in
  let accepted, ignored =
    List.fold_left
      (fun (accepted, ignored) s ->
        match remote_of_string s with
        | Error why -> (accepted, (s, why) :: ignored)
        | Ok r when List.mem r accepted -> (accepted, ignored)
        | Ok _ when List.length accepted >= max_remotes ->
            let why =
              Printf.sprintf "at most %d remotes are used" max_remotes
            in
            (accepted, (s, why) :: ignored)
        | Ok r -> (r :: accepted, ignored))
      ([], []) entries
  in
  (List.rev accepted, List.rev ignored)
