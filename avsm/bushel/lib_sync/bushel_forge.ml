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

let tangled_candidate ~repo ~name ~created_at =
  match (Tangled.Api.artifact_version name, date_of created_at) with
  | Some version, Some date ->
    Some
      {
        repo;
        forge = Bushel.Release.Tangled;
        tag = version;
        version;
        date;
        title = None;
        url = "https://tangled.org/" ^ repo;
        author = None;
        prerelease = false;
      }
  | _ -> None

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
