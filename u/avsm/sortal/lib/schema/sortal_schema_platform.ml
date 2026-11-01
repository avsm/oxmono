(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type simple =
  | Github | Gitlab | Codeberg
  | Orcid | Scholar
  | Twitter | LinkedIn | Threads | Instagram | Flickr

type federated =
  | Mastodon | Pixelfed | PeerTube
  | Matrix | Zulip | Discourse

type id = Simple of simple | Federated of federated | Atproto

(* [all_simple] and [all_federated] are maintained by hand, so this match
   exists to fail the build when a constructor is added without being
   listed. Extend both together. *)
let _remember_to_extend_all_simple = function
  | Github | Gitlab | Codeberg | Orcid | Scholar
  | Twitter | LinkedIn | Threads | Instagram | Flickr -> ()

let _remember_to_extend_all_federated = function
  | Mastodon | Pixelfed | PeerTube | Matrix | Zulip | Discourse -> ()

let all_simple =
  [ Github; Gitlab; Codeberg; Orcid; Scholar;
    Twitter; LinkedIn; Threads; Instagram; Flickr ]

let all_federated =
  [ Mastodon; Pixelfed; PeerTube; Matrix; Zulip; Discourse ]

let all =
  List.map (fun p -> Simple p) all_simple
  @ List.map (fun p -> Federated p) all_federated
  @ [ Atproto ]

let simple_key = function
  | Github -> "github" | Gitlab -> "gitlab" | Codeberg -> "codeberg"
  | Orcid -> "orcid" | Scholar -> "scholar"
  | Twitter -> "twitter" | LinkedIn -> "linkedin" | Threads -> "threads"
  | Instagram -> "instagram" | Flickr -> "flickr"

let federated_key = function
  | Mastodon -> "mastodon" | Pixelfed -> "pixelfed" | PeerTube -> "peertube"
  | Matrix -> "matrix" | Zulip -> "zulip" | Discourse -> "discourse"

let key = function
  | Simple p -> simple_key p
  | Federated p -> federated_key p
  | Atproto -> "atproto"

let of_key s = List.find_opt (fun id -> String.equal (key id) s) all

let simple_url p handle =
  match p with
  | Github -> "https://github.com/" ^ handle
  | Gitlab -> "https://gitlab.com/" ^ handle
  | Codeberg -> "https://codeberg.org/" ^ handle
  | Orcid -> "https://orcid.org/" ^ handle
  | Scholar -> "https://scholar.google.com/citations?user=" ^ handle
  | Twitter -> "https://twitter.com/" ^ handle
  | LinkedIn -> "https://www.linkedin.com/in/" ^ handle
  | Threads -> "https://www.threads.com/@" ^ handle
  | Instagram -> "https://www.instagram.com/" ^ handle
  | Flickr -> "https://www.flickr.com/photos/" ^ handle

let federated_url p ~user ~host =
  match p with
  | Mastodon | Pixelfed -> Printf.sprintf "https://%s/@%s" host user
  (* PeerTube distinguishes an [/a/] account URL from a [/c/] channel URL.
     The handle recorded here names a channel, not an account: the store's
     only PeerTube entry is https://crank.recoil.org/c/anil/videos, which
     this derivation reproduces exactly. *)
  | PeerTube -> Printf.sprintf "https://%s/c/%s/videos" host user
  | Matrix -> Printf.sprintf "https://matrix.to/#/@%s:%s" user host
  | Discourse -> Printf.sprintf "https://%s/u/%s" host user
  (* A Zulip account is recorded by display name, so no user URL exists. *)
  | Zulip -> "https://" ^ host

let is_ascii_alnum c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

let no_spaces label s =
  if s = "" then Error (label ^ " is empty")
  else if String.exists (fun c -> c = ' ' || c = '\t') s then
    Error (label ^ " contains whitespace")
  else Ok ()

(* ISO 7064 MOD 11-2, as ORCID specifies for its final check digit. Returns
   the expected check character rather than a bool, so a caller comparing
   against the supplied digit sees which character was expected on failure. *)
let orcid_check_digit digits =
  let total =
    String.fold_left
      (fun acc c -> (acc + (Char.code c - Char.code '0')) * 2)
      0 digits
  in
  let remainder = total mod 11 in
  let result = (12 - remainder) mod 11 in
  if result = 10 then 'X' else Char.chr (result + Char.code '0')

let check_orcid s =
  let bare = String.concat "" (String.split_on_char '-' s) in
  if String.length bare <> 16 then
    Error "an ORCID is 16 characters in four hyphenated groups"
  else
    let body = String.sub bare 0 15 and check = bare.[15] in
    if not (String.for_all (fun c -> c >= '0' && c <= '9') body) then
      Error "an ORCID's first 15 characters are digits"
    else if orcid_check_digit body <> check then
      Error "ORCID checksum does not match"
    else Ok ()

let check_simple p handle =
  match p with
  | Orcid -> check_orcid handle
  | Github | Gitlab | Codeberg | Scholar
  | Twitter | LinkedIn | Threads | Instagram | Flickr ->
      no_spaces "handle" handle

let check_federated p ~user ~host =
  match p with
  (* Zulip records a display name, which may contain spaces. *)
  | Zulip -> if user = "" then Error "user is empty" else no_spaces "host" host
  | Mastodon | Pixelfed | PeerTube | Matrix | Discourse ->
      (match no_spaces "user" user with
       | Error _ as e -> e
       | Ok () -> no_spaces "host" host)

let check_atproto_handle h =
  let segment_ok s =
    s <> "" && String.length s <= 63
    && String.for_all (fun c -> is_ascii_alnum c || c = '-') s
    && s.[0] <> '-'
    && s.[String.length s - 1] <> '-'
  in
  let segments = String.split_on_char '.' h in
  if h = "" then Error "handle is empty"
  else if String.length h > 253 then Error "handle exceeds 253 characters"
  else if not (String.for_all (fun c -> Char.code c < 128) h) then
    Error "handle is not ASCII"
  else if List.length segments < 2 then
    Error "handle needs two or more dot-separated segments"
  else if not (List.for_all segment_ok segments) then
    Error "a handle segment is empty, too long, or badly hyphenated"
  else
    let tld = List.nth segments (List.length segments - 1) in
    if tld.[0] >= '0' && tld.[0] <= '9' then
      Error "the final segment must not start with a digit"
    else Ok ()
