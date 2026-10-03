(*---------------------------------------------------------------------------
  Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type candidate = {
  repo : string;
  forge : Bushel.Release.forge;
  tag : string;
  version : string;
  date : Ptime.date;
  title : string option;
  url : string;
  author : string option;
  prerelease : bool;
}

let member name = function
  | Jsont.Object (ms, _) -> (
    match Jsont.Json.find_mem name ms with Some (_, v) -> Some v | None -> None)
  | _ -> None

let path names v =
  List.fold_left (fun acc n -> Option.bind acc (member n)) (Some v) names

let str = function Some (Jsont.String (s, _)) -> Some s | _ -> None
let bool = function Some (Jsont.Bool (b, _)) -> b | _ -> false
let items = function Some (Jsont.Array (l, _)) -> l | _ -> []

(* The first ten characters of an RFC 3339 time are the calendar date the
   forge reports, in the zone it reports it. *)
let date_of s =
  Bushel.Types.date_of_string ~kind:"date"
    (String.sub s 0 (min 10 (String.length s)))
  |> Result.to_option

let parse json =
  Jsont_bytesrw.decode_string Jsont.json json
  |> Result.map_error (fun e -> "invalid JSON: " ^ e)

let is_digit c = c >= '0' && c <= '9'

let version_of_tag tag =
  if String.length tag > 1 && tag.[0] = 'v' && is_digit tag.[1] then
    String.sub tag 1 (String.length tag - 1)
  else tag

let github_candidate ~repo v =
  match
    ( str (member "tag_name" v),
      Option.bind (str (member "published_at" v)) date_of )
  with
  | Some tag, Some date when not (bool (member "draft" v)) ->
    Some
      {
        repo;
        forge = Bushel.Release.Github;
        tag;
        version = version_of_tag tag;
        date;
        title = (match str (member "name" v) with Some "" -> None | t -> t);
        url =
          Option.value (str (member "html_url" v))
            ~default:
              (Printf.sprintf "https://github.com/%s/releases/tag/%s" repo tag);
        author = str (path [ "author"; "login" ] v);
        prerelease = bool (member "prerelease" v);
      }
  | _ -> None

let github_releases ~repo json =
  Result.map
    (fun v -> List.filter_map (github_candidate ~repo) (items (Some v)))
    (parse json)

let github_release ~repo json =
  match parse json with
  | Error e -> Error e
  | Ok v -> (
    match github_candidate ~repo v with
    | Some c -> Ok c
    | None -> Error "not a published release")

let github_events json =
  match parse json with
  | Error _ -> []
  | Ok v ->
    List.filter_map
      (fun e ->
        if
          str (member "type" e) = Some "ReleaseEvent"
          && str (path [ "payload"; "action" ] e) = Some "published"
        then
          match
            (str (path [ "repo"; "name" ] e),
             str (path [ "payload"; "release"; "tag_name" ] e))
          with
          | Some repo, Some tag -> Some (repo, tag)
          | _ -> None
        else None)
      (items (Some v))

let archive_suffixes = [ ".tbz"; ".tar.bz2"; ".tar.gz"; ".tgz"; ".zip" ]

let strip_prefix ~prefix s =
  if String.starts_with ~prefix s then
    let n = String.length prefix in
    Some (String.sub s n (String.length s - n))
  else None

let strip_suffix s =
  List.find_map
    (fun suffix ->
      if String.ends_with ~suffix s then
        Some (String.sub s 0 (String.length s - String.length suffix))
      else None)
    archive_suffixes

let tangled_version ~repo_name name =
  match strip_prefix ~prefix:(repo_name ^ "-") name with
  | None -> None
  | Some rest -> (
    match strip_suffix rest with Some "" | None -> None | Some v -> Some v)

let last_segment repo =
  match String.rindex_opt repo '/' with
  | Some i -> String.sub repo (i + 1) (String.length repo - i - 1)
  | None -> repo

let tangled_artifacts ~repo json =
  let repo_name = last_segment repo in
  match parse json with
  | Error e -> Error e
  | Ok v ->
    let cands =
      List.filter_map
        (fun r ->
          let value = member "value" r in
          match
            ( Option.bind value (fun v -> str (member "name" v)),
              Option.bind value (fun v ->
                  Option.bind (str (member "createdAt" v)) date_of) )
          with
          | Some name, Some date -> (
            match tangled_version ~repo_name name with
            | None -> None
            | Some version ->
              Some
                {
                  repo;
                  forge = Bushel.Release.Tangled;
                  tag = version;
                  version;
                  date;
                  title = Some name;
                  url = "https://tangled.org/" ^ repo;
                  author = None;
                  prerelease = false;
                })
          | _ -> None)
        (items (member "records" v))
    in
    (* Several artifacts of one version are one release. *)
    Ok
      (List.fold_left
         (fun acc c ->
           if List.exists (fun a -> a.version = c.version) acc then acc
           else acc @ [ c ])
         [] cands)

let unregistered ~author ~registered candidates =
  let known repo version =
    List.exists
      (fun (t : Bushel.Release.t) ->
        t.repo = repo
        && List.exists (fun r -> r.Bushel.Release.version = version) t.releases)
      registered
  in
  List.filter
    (fun c ->
      (not c.prerelease)
      && (match c.author with None -> true | Some a -> a = author)
      && not (known c.repo c.version))
    candidates
