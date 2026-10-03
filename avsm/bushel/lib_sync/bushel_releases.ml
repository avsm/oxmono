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

let github_get ~http ~token url =
  match token with
  | Some t ->
    Bushel_http.get_with_header ~http ~header:("Authorization: Bearer " ^ t) url
  | None -> Bushel_http.get ~http url

let github_release ~http ~token ~repo ~tag =
  let* body =
    github_get ~http ~token
      (Printf.sprintf "https://api.github.com/repos/%s/releases/tags/%s" repo
         tag)
  in
  Bushel_forge.github_release ~repo body

let github_releases ~http ~token ~repo =
  let* body =
    github_get ~http ~token
      (Printf.sprintf "https://api.github.com/repos/%s/releases?per_page=100"
         repo)
  in
  Bushel_forge.github_releases ~repo body

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
    (* Several artifacts of one version are one release. *)
    Ok
      (List.fold_left
         (fun acc (c : Bushel_forge.candidate) ->
           let same (a : Bushel_forge.candidate) = a.version = c.version in
           if List.exists same acc then acc else acc @ [ c ])
         [] candidates)
  with ex -> Error (Printexc.to_string ex)
