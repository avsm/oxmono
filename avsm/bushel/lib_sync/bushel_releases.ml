(*---------------------------------------------------------------------------
  Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind

let token_of_env = function Some "" | None -> None | t -> t
let token () = token_of_env (Sys.getenv_opt "GITHUB_TOKEN")

let nonblank = function
  | Some s when String.trim s <> "" -> Some (String.trim s)
  | _ -> None

let name_of repo =
  match String.rindex_opt repo '/' with
  | Some i -> String.sub repo (i + 1) (String.length repo - i - 1)
  | None -> repo

(* A release title that is only the version says nothing the line does not. *)
let useful_title (c : Bushel_forge.candidate) =
  match nonblank c.title with
  | Some t when t <> c.version && t <> c.tag -> Some t
  | _ -> None

let build (c : Bushel_forge.candidate) ~registries ~description ~summary =
  let summary =
    match nonblank summary with
    | Some s -> s
    | None -> (
      match
        Option.map Bushel_registries.summary_of_description
          (nonblank description)
      with
      | Some s when s <> "" -> s
      | _ -> (
        match useful_title c with
        | Some t -> t
        | None -> name_of c.repo ^ " " ^ c.version))
  in
  {
    Bushel.Release.version = c.version;
    tag = (if c.tag = c.version then None else Some c.tag);
    date = c.date;
    summary;
    url = c.url;
    registries;
  }

let reconcile ~existing ~summary_given (fresh : Bushel.Release.release) =
  match existing with
  | None -> fresh
  | Some (old : Bushel.Release.release) ->
    let kept =
      if summary_given then fresh else { fresh with summary = old.summary }
    in
    Bushel.Release.add_registries
      { kept with registries = old.registries }
      fresh.registries

let refusal ~github_user ~force (c : Bushel_forge.candidate) =
  match c.author with
  | None -> None
  | Some _ when force -> None
  | Some author -> (
    match github_user with
    | None ->
      Some
        "set github_user in the [releases] section of the config, or use \
         --force"
    | Some user
      when String.lowercase_ascii user = String.lowercase_ascii author ->
      None
    | Some _ ->
      Some
        (Printf.sprintf "%s %s was published by %s, not you. Use --force."
           c.repo c.tag author))

let refresh ~cutoff ~lookup ts =
  let attached = ref [] and failed = ref [] in
  let refresh_release (t : Bushel.Release.t) (r : Bushel.Release.release) =
    if r.date < cutoff then r
    else
      match lookup t r with
      | Error e ->
        failed := (t.repo, r.version, e) :: !failed;
        r
      | Ok found ->
        let r' = Bushel.Release.add_registries r found in
        List.iter
          (fun (g : Bushel.Release.registry) ->
            if
              not
                (List.exists
                   (fun (h : Bushel.Release.registry) -> h.name = g.name)
                   r.registries)
            then attached := (t.repo, r.version, g.name) :: !attached)
          r'.registries;
        r'
  in
  let updated =
    List.map
      (fun (t : Bushel.Release.t) ->
        { t with releases = List.map (refresh_release t) t.releases })
      ts
  in
  (updated, List.rev !attached, List.rev !failed)

let github_get ~http ~token url =
  match token with
  | Some t ->
    Bushel_http.get_with_header ~http ~header:("Authorization: Bearer " ^ t) url
  | None -> Bushel_http.get ~http url

let github_release ~http ~token ~repo ~tag =
  let* body =
    github_get ~http ~token
      (Printf.sprintf "https://api.github.com/repos/%s/releases/tags/%s" repo
         (Bushel.Release.encode_segment tag))
  in
  Bushel_forge.github_release ~repo body

(* Releases come 100 to a page. The walk ends at an empty page, at a page with
   nothing new, which covers a server that ignores the page number, and after
   [github_pages] pages. *)
let github_pages = 10

let github_releases ~http ~token ~repo =
  let rec pages n acc =
    let* body =
      github_get ~http ~token
        (Printf.sprintf
           "https://api.github.com/repos/%s/releases?per_page=100&page=%d" repo
           n)
    in
    let* found = Bushel_forge.github_releases ~repo body in
    let fresh =
      List.filter
        (fun (c : Bushel_forge.candidate) ->
          let same (a : Bushel_forge.candidate) = a.tag = c.tag in
          not (List.exists same acc))
        found
    in
    if fresh = [] || n >= github_pages then Ok acc
    else pages (n + 1) (acc @ fresh)
  in
  pages 1 []

let github_events ~http ~token ~user =
  let* body =
    github_get ~http ~token
      (Printf.sprintf
         "https://api.github.com/users/%s/events/public?per_page=100" user)
  in
  Ok (Bushel_forge.github_events body)

let handle_of repo =
  match String.rindex_opt repo '/' with
  | Some i -> String.sub repo 0 i
  | None -> repo

let tangled_artifacts ~sw ~env ~http ~repo =
  try
    let* did = Xrpc.Identity.did_of_handle http (handle_of repo) in
    let* pds = Xrpc.Identity.pds_of_did http did in
    let api = Tangled.Api.create ~sw ~env ~app_name:"bushel" ~pds ~http () in
    let artifacts = Tangled.Api.list_artifacts api ~did ~repo:(name_of repo) in
    let candidates =
      List.filter_map
        (fun (_, (a : Tangled.Api.Lex.Repo.Artifact.main)) ->
          Bushel_forge.tangled_candidate ~repo ~name:a.name
            ~created_at:a.created_at)
        artifacts
    in
    Ok (Bushel_forge.one_per_version candidates)
  with ex -> Error (Printexc.to_string ex)
