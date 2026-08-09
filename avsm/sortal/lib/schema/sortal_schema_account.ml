(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Platform = Sortal_schema_platform
module Smap = Stdlib.Map.Make (String)

type app = Bluesky | Tangled | Standard_site

type atproto = { handle : string; did : string option; apps : app list }

type t =
  | Simple of Platform.simple * string
  | Federated of Platform.federated * string * string
  | Atproto of atproto

let app_to_string = function
  | Bluesky -> "bluesky"
  | Tangled -> "tangled"
  | Standard_site -> "standard-site"

let app_of_string = function
  | "bluesky" -> Some Bluesky
  | "tangled" -> Some Tangled
  | "standard-site" -> Some Standard_site
  | _ -> None

let platform = function
  | Simple (p, _) -> Platform.Simple p
  | Federated (p, _, _) -> Platform.Federated p
  | Atproto _ -> Platform.Atproto

let handle = function
  | Simple (_, h) -> h
  | Federated (_, user, host) -> user ^ "@" ^ host
  | Atproto a -> a.handle

let app_url a app =
  match app with
  | Bluesky -> "https://bsky.app/profile/" ^ a.handle
  | Tangled -> "https://tangled.org/@" ^ a.handle
  | Standard_site -> "https://" ^ a.handle

let url = function
  | Simple (p, h) -> Platform.simple_url p h
  | Federated (p, user, host) -> Platform.federated_url p ~user ~host
  | Atproto a ->
      let app = match a.apps with app :: _ -> app | [] -> Bluesky in
      app_url a app

let check = function
  | Simple (p, h) -> Platform.check_simple p h
  | Federated (p, user, host) -> Platform.check_federated p ~user ~host
  | Atproto a -> Platform.check_atproto_handle a.handle

(* Split on the last '@' so a Zulip display name, which may itself contain
   an '@', still yields the right host. *)
let split_user_host s =
  match String.rindex_opt s '@' with
  | None -> None
  | Some i ->
      let user = String.sub s 0 i in
      let host = String.sub s (i + 1) (String.length s - i - 1) in
      if user = "" || host = "" then None else Some (user, host)

(* The value of one mapping member, before its key is known. *)
type raw = Scalar of string | Seq of string list | Obj of atproto

let app_json =
  let dec meta s =
    match app_of_string s with
    | Some a -> a
    | None -> Jsont.Error.msgf meta "App: unknown AT Protocol app: %S" s
  in
  Jsont.Base.string (Jsont.Base.map ~kind:"App" ~dec ~enc:app_to_string ())

let atproto_json =
  let open Jsont.Object in
  map ~kind:"Atproto" (fun handle did apps -> { handle; did; apps })
  |> mem "handle" Jsont.string ~enc:(fun a -> a.handle)
  |> opt_mem "did" Jsont.string ~enc:(fun a -> a.did)
  |> mem "apps" (Jsont.list app_json) ~dec_absent:[] ~enc:(fun a -> a.apps)
  |> error_unknown
  (* Unlike the top-level mapping, which also errors on an unrecognised
     member, this closed sub-object has no [version] field of its own to
     gate additions. Adding a member here later means older readers reject
     files that use it, which is the price of catching a typo such as
     ["dad"] for ["did"] now. *)
  |> finish

(* Each of the three shapes a member's value may take gets its own [raw t],
   selected by [enc] below, rather than a single map with [assert false]
   branches. *)
let scalar_json =
  Jsont.map ~kind:"Scalar"
    ~dec:(fun s -> Scalar s)
    ~enc:(function Scalar s -> s | _ -> invalid_arg "scalar_json")
    Jsont.string

let seq_json =
  Jsont.map ~kind:"Seq"
    ~dec:(fun l -> Seq l)
    ~enc:(function Seq l -> l | _ -> invalid_arg "seq_json")
    (Jsont.list Jsont.string)

let obj_json =
  Jsont.map ~kind:"Obj"
    ~dec:(fun a -> Obj a)
    ~enc:(function Obj a -> a | _ -> invalid_arg "obj_json")
    atproto_json

let raw_json =
  Jsont.any ~kind:"AccountValue" ~dec_string:scalar_json ~dec_array:seq_json
    ~dec_object:obj_json
    ~enc:(function Scalar _ -> scalar_json | Seq _ -> seq_json | Obj _ -> obj_json)
    ()

(* [of_key_value] runs inside the [Jsont.map ~dec] closure of [json_t], which
   never receives a [meta], so every message here names the offending
   platform key by hand to stay locatable without one. *)
let of_key_value key raw =
  match Platform.of_key key with
  | None -> Jsont.Error.msgf Jsont.Meta.none "unknown platform: %S" key
  | Some Platform.Atproto -> (
      match raw with
      | Obj a -> [ Atproto a ]
      | Scalar h -> [ Atproto { handle = h; did = None; apps = [] } ]
      | Seq _ ->
          Jsont.Error.msgf Jsont.Meta.none
            "%s: atproto takes one handle, not a sequence" key)
  | Some (Platform.Simple p) -> (
      let one h = Simple (p, h) in
      match raw with
      | Scalar h -> [ one h ]
      | Seq hs -> List.map one hs
      | Obj _ ->
          Jsont.Error.msgf Jsont.Meta.none "%s takes a handle, not an object"
            key)
  | Some (Platform.Federated p) ->
      let one h =
        match split_user_host h with
        | Some (user, host) -> Federated (p, user, host)
        | None ->
            Jsont.Error.msgf Jsont.Meta.none
              "%s needs a user@host handle, got %S" key h
      in
      (match raw with
       | Scalar h -> [ one h ]
       | Seq hs -> List.map one hs
       | Obj _ ->
           Jsont.Error.msgf Jsont.Meta.none "%s takes a handle, not an object"
             key)

let to_key_value accounts =
  (* Group by platform key, preserving declaration order within a platform. *)
  let add m a =
    let k = Platform.key (platform a) in
    let existing = Option.value ~default:[] (Smap.find_opt k m) in
    Smap.add k (existing @ [ a ]) m
  in
  let grouped = List.fold_left add Smap.empty accounts in
  Smap.map
    (fun group ->
      match group with
      (* An atproto identity with neither a resolved DID nor any app
         narrows back to the bare handle it would decode from, mirroring
         the rest of the schema's rule of omitting what is empty. *)
      | [ Atproto { handle = h; did = None; apps = [] } ] -> Scalar h
      | [ Atproto a ] -> Obj a
      | [ one ] -> Scalar (handle one)
      | many -> Seq (List.map handle many))
    grouped

let json_t =
  Jsont.map ~kind:"Accounts"
    ~dec:(fun m -> Smap.fold (fun k v acc -> acc @ of_key_value k v) m [])
    ~enc:to_key_value
    (Jsont.Object.as_string_map raw_json)
