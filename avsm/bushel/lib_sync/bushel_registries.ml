(*---------------------------------------------------------------------------
  Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let repository_url forge repo =
  match forge with
  | Bushel.Release.Github -> "https://github.com/" ^ repo
  | Bushel.Release.Tangled -> "git+https://tangled.org/" ^ repo

let squash s =
  String.split_on_char ' '
    (String.map (function '\n' | '\r' | '\t' -> ' ' | c -> c) s)
  |> List.filter (fun w -> w <> "")
  |> String.concat " "

(* The first full stop that ends a word, so that 1.5 is not a sentence end. *)
let first_sentence s =
  let n = String.length s in
  let rec go i =
    if i >= n then s
    else if s.[i] = '.' && (i + 1 = n || s.[i + 1] = ' ') then
      String.sub s 0 (i + 1)
    else go (i + 1)
  in
  go 0

(* [cut n s] is [s] cut to [n] characters without splitting a UTF-8 sequence. *)
let cut n s =
  let step i = i + Uchar.utf_decode_length (String.get_utf_8_uchar s i) in
  let rec go i count =
    if i >= String.length s then s
    else if count = n then String.sub s 0 i
    else go (step i) (count + 1)
  in
  go 0 0

let characters s =
  let rec go i count =
    if i >= String.length s then count
    else
      go (i + Uchar.utf_decode_length (String.get_utf_8_uchar s i)) (count + 1)
  in
  go 0 0

let summary_of_description d =
  let s = first_sentence (squash d) in
  if characters s <= 120 then s else cut 117 s ^ "..."

let attach ?prefer ~allowed ~packages ~carries () =
  List.filter_map
    (fun registry ->
      let here =
        List.filter_map
          (fun (r, p) -> if r = registry then Some p else None)
          packages
      in
      let package =
        let wanted = Option.value prefer ~default:[] in
        match List.find_opt (fun p -> List.mem p here) wanted with
        | Some p -> Some p
        | None -> ( match here with [] -> None | p :: _ -> Some p)
      in
      match package with
      | None -> None
      | Some package -> (
        match carries ~registry ~package with
        | None -> None
        | Some url -> Some { Bushel.Release.name = registry; package; url }))
    allowed

let strip_prefix prefix s =
  if String.starts_with ~prefix s then
    let n = String.length prefix in
    Some (String.sub s n (String.length s - n))
  else None

let last_segment repo =
  match String.rindex_opt repo '/' with
  | Some i -> String.sub repo (i + 1) (String.length repo - i - 1)
  | None -> repo

let preferred_packages repo =
  let name = last_segment repo in
  match strip_prefix "ocaml-" name with
  | Some short when short <> "" -> [ name; short ]
  | _ -> [ name ]

let pick_description ?(prefer = []) ~allowed
    ~(attached : Bushel.Release.registry list) ~found () =
  let described registry package =
    List.find_map
      (fun (r, p, d) -> if r = registry && p = package then d else None)
      found
  in
  match
    List.find_map
      (fun (g : Bushel.Release.registry) -> described g.name g.package)
      attached
  with
  | Some _ as d -> d
  | None ->
    List.find_map
      (fun registry ->
        let here =
          List.filter_map
            (fun (r, p, d) ->
              if r = registry then Option.map (fun d -> (p, d)) d else None)
            found
        in
        match List.find_opt (fun (p, _) -> List.mem p prefer) here with
        | Some (_, d) -> Some d
        | None -> ( match here with (_, d) :: _ -> Some d | [] -> None))
      allowed

let lookup eco ~allowed ~forge ~repo ~version =
  let repository_url = repository_url forge repo in
  match
    Ecosystems.PackageWithRegistry.lookup_package ~repository_url eco ()
  with
  | exception ex -> Error (Printexc.to_string ex)
  | found -> (
    let module P = Ecosystems.PackageWithRegistry.T in
    let packages =
      List.map
        (fun p -> (Ecosystems.Registry.T.name (P.registry p), P.name p))
        found
    in
    let carries ~registry ~package =
      match
        Ecosystems.VersionWithDependencies.get_registry_package_version
          ~registry_name:registry ~package_name:package ~version_number:version
          eco ()
      with
      | v -> Ecosystems.VersionWithDependencies.T.registry_url v
      | exception Openapi.Runtime.Api_error { status = 404; _ } -> None
    in
    match
      attach ~prefer:(preferred_packages repo) ~allowed ~packages ~carries ()
    with
    | exception ex -> Error (Printexc.to_string ex)
    | registries ->
      let description =
        pick_description ~prefer:(preferred_packages repo) ~allowed
          ~attached:registries
          ~found:
            (List.map
               (fun p ->
                 (Ecosystems.Registry.T.name (P.registry p), P.name p,
                  P.description p))
               found)
          ()
      in
      Ok (registries, description))
