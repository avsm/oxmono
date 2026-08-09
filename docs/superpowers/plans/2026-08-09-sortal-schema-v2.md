# Sortal Schema V2 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the V1 contact schema with a model built on a single `account` notion, and migrate the 460 live contacts to it.

**Architecture:** New schema modules are built alongside V1 so the tree stays green. A pure `V1.t -> V2.t` migration is written and pinned against a golden copy of the live database. Only then does `Sortal_schema.Contact` flip to V2, in one commit that also updates the store, CLI, web pages and the three consumer packages. Probes are a separate, later plan.

**Tech Stack:** OCaml 5.2.0+ox, dune 3.21, `jsont` for codecs, `yamlt` for YAML, `eio` for I/O, `cmdliner` for the CLI, `ptime` for dates.

## Global Constraints

- Before every commit, `dune build` must be clean repo-wide and `dune runtest avsm/sortal` must be clean. No red commits.
- `dune build @fmt` is **suspended** for this plan: `ocamlformat` is not installed in the `5.2.0+ox` switch, so it fails on untouched files too. Run it once, scoped to `avsm/sortal`, if the switch ever gains it. Match the surrounding formatting by hand meanwhile.
- Repo-wide `dune runtest` fails at `bleeding/atp/atp/test/dune:6` and `bleeding/fetch/main/tests/main.md:1`. Both predate this work and are out of scope. Do not attempt to fix them, and do not let them gate a commit.
- Never run `dune build @fmt --auto-promote` unscoped. It reformats `dune` files across the whole repository.
- One commit per self-contained change, one-line imperative message, no trailers or sign-off.
- Keep a mechanical change, such as a reformat, out of the commit that changes behaviour.
- Prose in `.mli` files follows `CLAUDE.md`: document a value as `[foo x y] is ...`, name its arguments, no em-dashes, do not join two clauses with a semicolon.
- Every new module gets an `.mli`.
- **Decode errors must carry position.** When mapping a JSON string to a variant or a parsed value, use `Jsont.Base.string (Jsont.Base.map ~kind ~dec ~enc ())`, whose `dec` has type `meta -> string -> 'a` and so can forward a real `meta` into `Jsont.Error.msgf meta ...`. The precedent is `avsm/sortal/lib/schema/sortal_schema_temporal.ml:120-129`. Do NOT use the top-level `Jsont.map` for this: its `dec` is `'a -> 'b` and never receives a `meta`, so `Jsont.Meta.none` is the only thing you can pass and the error loses its position. Inside a `Jsont.Object.map` constructor closure no `meta` exists either, so `Jsont.Meta.none` is acceptable there, but the message must then name the field it concerns so it stays locatable.
- A parser exposed as `string -> 'a option` must reject what its `.mli` says it rejects. `int_of_string_opt` accepts `0x10`, `0o17`, `2_006` and `+2001`, so it is not by itself a decimal-digit check.
- Tests are plain executables using `assert` and `print_endline`, matching `avsm/sortal/test/test_schema.ml`. Do not introduce alcotest.
- Copyright header on every new file, copied verbatim from an existing file in the same directory.
- The design spec is `docs/superpowers/specs/2026-08-09-sortal-schema-v2-design.md`. It is authoritative where this plan is silent.

## File Structure

Created in `avsm/sortal/lib/schema/`:

| file | responsibility |
|---|---|
| `sortal_schema_date.ml{,i}` | ISO 8601 date parse and format over `Ptime.date`. Replaces `sortal_schema_temporal`. |
| `sortal_schema_platform.ml{,i}` | The closed platform vocabulary, its per-platform spec table, and URL derivation. |
| `sortal_schema_account.ml{,i}` | The `account` type and its YAML mapping codec. |
| `sortal_schema_contact_v2.ml{,i}` | The V2 contact record, accessors and codec. |
| `sortal_schema_migrate.ml{,i}` | Pure `V1.t -> V2.t`. |

Retained: `sortal_schema_contact_v1.ml{,i}` and `sortal_schema_temporal.ml{,i}`, read-only, used only by the migration. Deleted in Task 12.

Modified: `sortal_schema.ml{,i}`, `lib/core/sortal_store.ml{,i}`, `lib/core/sortal_cmd.ml`, `bin/sortal_cli.ml`, `lib/web/pages.ml`, and the consumer packages listed in Task 9.

---

### Task 1: Date module

**Files:**
- Create: `avsm/sortal/lib/schema/sortal_schema_date.ml`, `avsm/sortal/lib/schema/sortal_schema_date.mli`
- Modify: `avsm/sortal/test/test_schema.ml`

**Interfaces:**
- Consumes: nothing.
- Produces: `Sortal_schema_date.t = Ptime.date`, `val parse : string -> t option`, `val to_string : t -> string`, `val json_t : t Jsont.t`.

- [ ] **Step 1: Write the failing test**

Add to `avsm/sortal/test/test_schema.ml`, and add a call to it in the `main` at the bottom of that file:

```ocaml
let test_date () =
  let p = Sortal_schema.Date.parse in
  assert (p "2001" = Some (2001, 1, 1));
  assert (p "2001-03" = Some (2001, 3, 1));
  assert (p "2001-03-15" = Some (2001, 3, 15));
  assert (p "" = None);
  assert (p "not-a-date" = None);
  assert (p "2001-13-01" = None);
  assert (p "2001-02-30" = None);
  assert (Sortal_schema.Date.to_string (2001, 3, 15) = "2001-03-15");
  print_endline "✓ Date parsing works"
```

- [ ] **Step 2: Run test to verify it fails**

Run: `dune build @avsm/sortal/runtest`
Expected: FAIL, `Unbound module Sortal_schema.Date`.

- [ ] **Step 3: Write the interface**

Create `avsm/sortal/lib/schema/sortal_schema_date.mli` with the copyright header from `sortal_schema_temporal.mli`, then:

```ocaml
(** ISO 8601 calendar dates.

    Dates are used only to bound an affiliation. No other field in the
    schema carries a date. *)

type t = Ptime.date
(** A date as a [(year, month, day)] triple. *)

val parse : string -> t option
(** [parse s] is the date [s] denotes, or [None] if [s] is not an ISO 8601
    date. A year alone and a year and month are accepted, and are completed
    with the first day of the implied period, so ["2001"] and ["2001-01"]
    are both [(2001, 1, 1)]. A date that names a day outside its month, such
    as ["2001-02-30"], is rejected. *)

val to_string : t -> string
(** [to_string d] is [d] as an ISO 8601 date, always in [YYYY-MM-DD] form. *)

val compare : t -> t -> int
(** [compare a b] orders dates chronologically. *)

val json_t : t Jsont.t
(** [json_t] maps a date to and from its {!to_string} form. Decoding a string
    that {!parse} rejects is a decoding error. *)
```

- [ ] **Step 4: Write the implementation**

Create `avsm/sortal/lib/schema/sortal_schema_date.ml` with the same copyright header, then:

```ocaml
type t = Ptime.date

let is_valid (y, m, d) =
  m >= 1 && m <= 12 && d >= 1
  && Ptime.of_date (y, m, d) <> None

let parse s =
  let int_of s = int_of_string_opt (String.trim s) in
  let candidate =
    match String.split_on_char '-' (String.trim s) with
    | [ y ] -> (match int_of y with Some y -> Some (y, 1, 1) | None -> None)
    | [ y; m ] ->
        (match (int_of y, int_of m) with
         | Some y, Some m -> Some (y, m, 1)
         | _ -> None)
    | [ y; m; d ] ->
        (match (int_of y, int_of m, int_of d) with
         | Some y, Some m, Some d -> Some (y, m, d)
         | _ -> None)
    | _ -> None
  in
  match candidate with
  | Some date when is_valid date -> Some date
  | _ -> None

let to_string (y, m, d) = Printf.sprintf "%04d-%02d-%02d" y m d

let compare = Stdlib.compare

let json_t =
  let dec meta s =
    match parse s with
    | Some d -> d
    | None -> Jsont.Error.msgf meta "Date: not an ISO 8601 date: %S" s
  in
  Jsont.Base.string (Jsont.Base.map ~kind:"Date" ~dec ~enc:to_string ())
```

- [ ] **Step 5: Run the tests**

Run: `dune build @avsm/sortal/runtest 2>&1 | tail -20`
Expected: the date test prints and passes. If `Jsont.Error.msgf` does not typecheck, check its arity against `/Users/avsm/.opam/5.2.0+ox/lib/jsont/jsont.mli` and adjust. Do not silently fall back to `failwith`, because a decode error must carry position information.

- [ ] **Step 6: Format and commit**

```bash
dune build @fmt --auto-promote
dune build && dune runtest
git add avsm/sortal/lib/schema/sortal_schema_date.ml* avsm/sortal/test/test_schema.ml
git commit -m "Add date module for schema V2"
```

---

### Task 2: Platform vocabulary

**Files:**
- Create: `avsm/sortal/lib/schema/sortal_schema_platform.ml`, `avsm/sortal/lib/schema/sortal_schema_platform.mli`
- Modify: `avsm/sortal/test/test_schema.ml`

**Interfaces:**
- Consumes: nothing.
- Produces: `Sortal_schema_platform.simple`, `.federated`, `.id`, `val key : id -> string`, `val of_key : string -> id option`, `val simple_url : simple -> string -> string`, `val federated_url : federated -> user:string -> host:string -> string`, `val check_simple : simple -> string -> (unit, string) result`, `val all : id list`.

This task defines the vocabulary and URL derivation only. The `probe` field of the spec record is deferred to the probes plan, so `spec` is not introduced yet. Adding it later is one field on one record.

- [ ] **Step 1: Write the failing test**

Add to `avsm/sortal/test/test_schema.ml` and call it from `main`:

```ocaml
let test_platform () =
  let module P = Sortal_schema.Platform in
  assert (P.of_key "github" = Some (P.Simple P.Github));
  assert (P.of_key "mastodon" = Some (P.Federated P.Mastodon));
  assert (P.of_key "atproto" = Some P.Atproto);
  assert (P.of_key "githb" = None);
  assert (P.key (P.Simple P.Github) = "github");
  assert (P.key P.Atproto = "atproto");
  (* every platform round-trips through its key *)
  List.iter (fun id -> assert (P.of_key (P.key id) = Some id)) P.all;
  (* keys are unique *)
  let keys = List.map P.key P.all in
  assert (List.length (List.sort_uniq String.compare keys) = List.length keys);
  assert (P.simple_url P.Github "avsm" = "https://github.com/avsm");
  assert (P.simple_url P.Orcid "0000-0001-8954-2428"
          = "https://orcid.org/0000-0001-8954-2428");
  assert (P.federated_url P.Mastodon ~user:"avsm" ~host:"amok.recoil.org"
          = "https://amok.recoil.org/@avsm");
  assert (P.federated_url P.Matrix ~user:"avsm" ~host:"recoil.org"
          = "https://matrix.to/#/@avsm:recoil.org");
  assert (P.federated_url P.Discourse ~user:"avsm" ~host:"discuss.ocaml.org"
          = "https://discuss.ocaml.org/u/avsm");
  (* Zulip cannot derive a user URL, only a host one *)
  assert (P.federated_url P.Zulip ~user:"Anil Madhavapeddy" ~host:"eeg.zulipchat.com"
          = "https://eeg.zulipchat.com");
  (* ORCID checksum, ISO 7064 MOD 11-2 *)
  assert (P.check_simple P.Orcid "0000-0001-8954-2428" = Ok ());
  assert (Result.is_error (P.check_simple P.Orcid "0000-0001-8954-2427"));
  assert (Result.is_error (P.check_simple P.Orcid "nonsense"));
  assert (P.check_simple P.Github "avsm" = Ok ());
  assert (Result.is_error (P.check_simple P.Github "not a handle"));
  print_endline "✓ Platform vocabulary works"
```

- [ ] **Step 2: Run test to verify it fails**

Run: `dune build @avsm/sortal/runtest`
Expected: FAIL, `Unbound module Sortal_schema.Platform`.

- [ ] **Step 3: Write the interface**

Create `avsm/sortal/lib/schema/sortal_schema_platform.mli` with the copyright header, then:

```ocaml
(** The platform vocabulary.

    A platform is a service a contact holds an account on. The vocabulary is
    closed, so an unrecognised platform is a decoding error rather than a
    silently accepted typo. Adding a platform means adding a constructor and
    the row the compiler then demands. *)

type simple =
  | Github | Gitlab | Codeberg
  | Orcid | Scholar
  | Twitter | LinkedIn | Threads | Instagram | Flickr
(** Platforms whose URL derives from a bare handle. *)

type federated =
  | Mastodon | Pixelfed | PeerTube
  | Matrix | Zulip | Discourse
(** Platforms federated across instances, where an account is a user at a
    host. *)

type id = Simple of simple | Federated of federated | Atproto
(** [id] names any platform, and is what a lookup takes. *)

val all : id list
(** [all] is every platform, in the order they are declared. *)

val key : id -> string
(** [key id] is the YAML mapping key for [id]. Keys are unique across
    platforms. *)

val of_key : string -> id option
(** [of_key s] is the platform [s] names, or [None] if [s] is not a platform
    key. *)

val simple_url : simple -> string -> string
(** [simple_url p handle] is the canonical URL of [handle] on [p]. *)

val federated_url : federated -> user:string -> host:string -> string
(** [federated_url p ~user ~host] is the canonical URL of [user] at [host] on
    [p]. Zulip is the exception: a Zulip account is recorded by display name
    rather than by handle, so no user URL can be derived and the host URL is
    returned instead. *)

val check_simple : simple -> string -> (unit, string) result
(** [check_simple p handle] is [Ok ()] if [handle] is syntactically a [p]
    handle, or [Error why] naming the problem. The check is local and makes
    no network request. *)

val check_federated : federated -> user:string -> host:string ->
  (unit, string) result
(** [check_federated p ~user ~host] is [Ok ()] if [user] at [host] is
    syntactically a [p] account, or [Error why]. *)

val check_atproto_handle : string -> (unit, string) result
(** [check_atproto_handle h] is [Ok ()] if [h] satisfies the AT Protocol
    handle syntax, or [Error why]. A handle is ASCII, at most 253 characters,
    and has two or more dot-separated segments of 1 to 63 characters drawn
    from letters, digits and hyphens, where no segment starts or ends with a
    hyphen and the final segment does not start with a digit. *)
```

- [ ] **Step 4: Write the implementation**

Create `avsm/sortal/lib/schema/sortal_schema_platform.ml` with the copyright header, then:

```ocaml
type simple =
  | Github | Gitlab | Codeberg
  | Orcid | Scholar
  | Twitter | LinkedIn | Threads | Instagram | Flickr

type federated =
  | Mastodon | Pixelfed | PeerTube
  | Matrix | Zulip | Discourse

type id = Simple of simple | Federated of federated | Atproto

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

(* ISO 7064 MOD 11-2, as ORCID specifies for its final check digit. *)
let orcid_checksum_ok digits =
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
    else if orcid_checksum_ok body <> check then
      Error "ORCID checksum does not match"
    else Ok ()

let check_simple p handle =
  match p with
  | Orcid -> check_orcid handle
  | _ -> no_spaces "handle" handle

let check_federated p ~user ~host =
  match p with
  (* Zulip records a display name, which may contain spaces. *)
  | Zulip -> if user = "" then Error "user is empty" else no_spaces "host" host
  | _ ->
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
```

- [ ] **Step 5: Run the tests**

Run: `dune build @avsm/sortal/runtest 2>&1 | tail -20`
Expected: PASS. If the ORCID assertion fails, verify `orcid_checksum_ok` against the known-good `0000-0001-8954-2428` by hand before changing the test, because the test value is taken from live data and is correct.

- [ ] **Step 6: Format and commit**

```bash
dune build @fmt --auto-promote
dune build && dune runtest
git add avsm/sortal/lib/schema/sortal_schema_platform.ml* avsm/sortal/test/test_schema.ml
git commit -m "Add closed platform vocabulary for schema V2"
```

---

### Task 3: Account type and codec

**Files:**
- Create: `avsm/sortal/lib/schema/sortal_schema_account.ml`, `avsm/sortal/lib/schema/sortal_schema_account.mli`
- Modify: `avsm/sortal/test/test_schema.ml`

**Interfaces:**
- Consumes: `Sortal_schema_platform` from Task 2.
- Produces: `Sortal_schema_account.app`, `.atproto`, `.t`, `val platform : t -> Platform.id`, `val handle : t -> string`, `val url : t -> string`, `val json_t : t list Jsont.t`.

Note that `json_t` maps the whole `accounts` mapping, not a single account, because the mapping key carries the platform.

- [ ] **Step 1: Write the failing test**

Add to `avsm/sortal/test/test_schema.ml` and call it from `main`:

```ocaml
let decode_accounts s =
  Jsont_bytesrw.decode_string Sortal_schema.Account.json_t s

let encode_accounts a =
  Jsont_bytesrw.encode_string Sortal_schema.Account.json_t a

let test_account_codec () =
  let module A = Sortal_schema.Account in
  let module P = Sortal_schema.Platform in
  (* scalar form *)
  (match decode_accounts {|{"github":"avsm"}|} with
   | Ok [ a ] ->
       assert (A.platform a = P.Simple P.Github);
       assert (A.handle a = "avsm");
       assert (A.url a = "https://github.com/avsm")
   | Ok _ -> assert false
   | Error e -> failwith e);
  (* federated form splits on the last @ *)
  (match decode_accounts {|{"mastodon":"avsm@amok.recoil.org"}|} with
   | Ok [ A.Federated (P.Mastodon, user, host) ] ->
       assert (user = "avsm");
       assert (host = "amok.recoil.org")
   | Ok _ -> assert false
   | Error e -> failwith e);
  (* sequence form *)
  (match decode_accounts {|{"github":["avsm","avsm-work"]}|} with
   | Ok [ a; b ] -> assert (A.handle a = "avsm"); assert (A.handle b = "avsm-work")
   | Ok _ -> assert false
   | Error e -> failwith e);
  (* atproto object form *)
  (match decode_accounts
           {|{"atproto":{"handle":"anil.recoil.org","did":"did:plc:x","apps":["bluesky","tangled"]}}|}
   with
   | Ok [ A.Atproto a ] ->
       assert (a.A.handle = "anil.recoil.org");
       assert (a.A.did = Some "did:plc:x");
       assert (a.A.apps = [ A.Bluesky; A.Tangled ])
   | Ok _ -> assert false
   | Error e -> failwith e);
  (* an unknown platform key is a decode error, not a silent accept *)
  assert (Result.is_error (decode_accounts {|{"githb":"avsm"}|}));
  (* a federated platform given a bare handle is a decode error *)
  assert (Result.is_error (decode_accounts {|{"mastodon":"avsm"}|}));
  (* an unknown app is a decode error *)
  assert (Result.is_error
            (decode_accounts {|{"atproto":{"handle":"a.b","apps":["nope"]}}|}));
  (* round trip *)
  let src =
    {|{"github":"avsm","mastodon":"avsm@amok.recoil.org","atproto":{"handle":"anil.recoil.org","apps":["bluesky"]}}|}
  in
  (match decode_accounts src with
   | Ok accounts ->
       (match encode_accounts accounts with
        | Ok out ->
            (match decode_accounts out with
             | Ok again -> assert (again = accounts)
             | Error e -> failwith e)
        | Error e -> failwith e)
   | Error e -> failwith e);
  print_endline "✓ Account codec works"
```

- [ ] **Step 2: Run test to verify it fails**

Run: `dune build @avsm/sortal/runtest`
Expected: FAIL, `Unbound module Sortal_schema.Account`.

- [ ] **Step 3: Write the interface**

Create `avsm/sortal/lib/schema/sortal_schema_account.mli` with the copyright header, then:

```ocaml
(** Accounts a contact holds on a platform.

    An account is a handle on a platform. Its URL is derived from the two and
    is never stored, so a platform, a handle and a URL cannot disagree. *)

module Platform = Sortal_schema_platform

type app = Bluesky | Tangled | Standard_site
(** A front end onto a single AT Protocol identity. *)

type atproto = {
  handle : string;      (** the AT Protocol handle, such as ["anil.recoil.org"] *)
  did : string option;  (** the resolved DID, [None] until a probe fills it *)
  apps : app list;      (** the front ends this identity is reachable through *)
}
(** An AT Protocol identity. One handle names one person, and [apps] lists
    the services that identity is usable through. *)

type t =
  | Simple of Platform.simple * string
      (** [Simple (p, handle)] is [handle] on [p]. *)
  | Federated of Platform.federated * string * string
      (** [Federated (p, user, host)] is [user] at [host] on [p]. *)
  | Atproto of atproto

val platform : t -> Platform.id
(** [platform a] is the platform [a] is held on. *)

val handle : t -> string
(** [handle a] is [a]'s handle. For a federated account this is the
    [user@host] form, and for an AT Protocol account it is the bare handle. *)

val url : t -> string
(** [url a] is [a]'s canonical URL, derived from its platform and handle. For
    an AT Protocol account this is the URL of its first app, or the Bluesky
    URL if it lists none. *)

val app_url : atproto -> app -> string
(** [app_url a app] is the URL of [a]'s identity on [app]. *)

val app_to_string : app -> string
(** [app_to_string app] is [app]'s name as it appears in YAML. *)

val app_of_string : string -> app option
(** [app_of_string s] is the app [s] names, or [None]. *)

val check : t -> (unit, string) result
(** [check a] is [Ok ()] if [a]'s handle is syntactically valid for its
    platform, or [Error why]. The check is local. *)

val json_t : t list Jsont.t
(** [json_t] maps the whole [accounts] mapping, because the mapping key
    carries the platform.

    A member's value is a string naming one handle, an array of strings
    naming several, or, for [atproto] alone, an object. A member whose name
    is not a platform key is a decoding error. *)
```

- [ ] **Step 4: Write the implementation**

Create `avsm/sortal/lib/schema/sortal_schema_account.ml` with the copyright header, then:

```ocaml
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
type raw =
  | Scalar of string
  | Seq of string list
  | Obj of atproto

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
  |> finish

let raw_json =
  Jsont.any ~kind:"AccountValue"
    ~dec_string:(Jsont.map ~dec:(fun s -> Scalar s) Jsont.string)
    ~dec_array:
      (Jsont.map ~dec:(fun l -> Seq l) (Jsont.list Jsont.string))
    ~dec_object:(Jsont.map ~dec:(fun a -> Obj a) atproto_json)
    ~enc:(function
      | Scalar _ -> Jsont.map ~enc:(function Scalar s -> s | _ -> assert false)
                      Jsont.string
      | Seq _ -> Jsont.map ~enc:(function Seq l -> l | _ -> assert false)
                   (Jsont.list Jsont.string)
      | Obj _ -> Jsont.map ~enc:(function Obj a -> a | _ -> assert false)
                   atproto_json)
    ()

let of_key_value key raw =
  let err fmt = Jsont.Error.msgf Jsont.Meta.none fmt in
  match Platform.of_key key with
  | None -> err "unknown platform: %S" key
  | Some Platform.Atproto -> (
      match raw with
      | Obj a -> [ Atproto a ]
      | Scalar h -> [ Atproto { handle = h; did = None; apps = [] } ]
      | Seq _ -> err "atproto takes one handle, not a sequence")
  | Some (Platform.Simple p) -> (
      let one h = Simple (p, h) in
      match raw with
      | Scalar h -> [ one h ]
      | Seq hs -> List.map one hs
      | Obj _ -> err "%s takes a handle, not an object" key)
  | Some (Platform.Federated p) ->
      let one h =
        match split_user_host h with
        | Some (user, host) -> Federated (p, user, host)
        | None -> err "%s needs a user@host handle, got %S" key h
      in
      (match raw with
       | Scalar h -> [ one h ]
       | Seq hs -> List.map one hs
       | Obj _ -> err "%s takes a handle, not an object" key)

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
      | [ Atproto a ] -> Obj a
      | [ one ] -> Scalar (handle one)
      | many -> Seq (List.map handle many))
    grouped

let json_t =
  Jsont.map ~kind:"Accounts"
    ~dec:(fun m ->
      Smap.fold (fun k v acc -> acc @ of_key_value k v) m [])
    ~enc:to_key_value
    (Jsont.Object.as_string_map raw_json)
```

- [ ] **Step 5: Run the tests**

Run: `dune build @avsm/sortal/runtest 2>&1 | tail -30`

Expected: PASS. Two things commonly need adjusting against the real `jsont` interface at `/Users/avsm/.opam/5.2.0+ox/lib/jsont/jsont.mli`:

- `Jsont.Object.mem` may spell its absent-value argument differently from `~dec_absent`. Check the signature at line 969 and adjust the `apps` member.
- `Jsont.any`'s `~enc` returns the type to encode with. If the `assert false` branches read badly, replace `raw` with three separate encoders selected by `enc`.

Do not weaken the "unknown platform key is an error" assertion to make the build pass. That behaviour is the point of the closed vocabulary.

- [ ] **Step 6: Format and commit**

```bash
dune build @fmt --auto-promote
dune build && dune runtest
git add avsm/sortal/lib/schema/sortal_schema_account.ml* avsm/sortal/test/test_schema.ml
git commit -m "Add account type and mapping codec for schema V2"
```

---

### Task 4: V2 contact record

**Files:**
- Create: `avsm/sortal/lib/schema/sortal_schema_contact_v2.ml`, `avsm/sortal/lib/schema/sortal_schema_contact_v2.mli`
- Modify: `avsm/sortal/test/test_schema.ml`

**Interfaces:**
- Consumes: `Sortal_schema_date` (Task 1), `Sortal_schema_platform` (Task 2), `Sortal_schema_account` (Task 3), the existing `Sortal_schema_feed`.
- Produces: `Sortal_schema_contact_v2.t`, `.kind`, `.link`, `.affiliation`, `val make`, the accessors listed in the interface below, and `val json_t : t Jsont.t`.

- [ ] **Step 1: Write the failing test**

Add to `avsm/sortal/test/test_schema.ml` and call it from `main`:

```ocaml
let test_contact_v2 () =
  let module C = Sortal_schema.V2.Contact in
  let module A = Sortal_schema.Account in
  let module P = Sortal_schema.Platform in
  let c =
    C.make ~handle:"avsm" ~names:[ "Anil Madhavapeddy" ]
      ~emails:[ "anil@recoil.org"; "avsm2@cam.ac.uk" ]
      ~accounts:
        [ A.Simple (P.Github, "avsm");
          A.Federated (P.Mastodon, "avsm", "amok.recoil.org");
          A.Atproto { A.handle = "anil.recoil.org"; did = None;
                      apps = [ A.Bluesky ] } ]
      ~links:[ { C.url = "https://anil.recoil.org"; label = None } ]
      ()
  in
  assert (C.handle c = "avsm");
  assert (C.name c = "Anil Madhavapeddy");
  assert (C.handle_on c (P.Simple P.Github) = Some "avsm");
  assert (C.handle_on c (P.Simple P.Twitter) = None);
  assert (C.url_on c (P.Simple P.Github) = Some "https://github.com/avsm");
  assert (C.atproto_handle c = Some "anil.recoil.org");
  assert (C.best_url c = Some "https://anil.recoil.org");
  (* a contact with no links falls back to an account URL *)
  let bare = C.make ~handle:"x" ~names:[ "X" ]
      ~accounts:[ A.Simple (P.Github, "x") ] () in
  assert (C.best_url bare = Some "https://github.com/x");
  (* round trip *)
  (match Jsont_bytesrw.encode_string C.json_t c with
   | Ok json ->
       (match Jsont_bytesrw.decode_string C.json_t json with
        | Ok d -> assert (d = c)
        | Error e -> failwith e)
   | Error e -> failwith e);
  (* empty collections are omitted, and an unlabelled link is a bare string *)
  let minimal = C.make ~handle:"m" ~names:[ "M" ]
      ~links:[ { C.url = "https://m.example"; label = None } ] () in
  (match Jsont_bytesrw.encode_string C.json_t minimal with
   | Ok json ->
       assert (not (contains json "\"emails\""));
       assert (not (contains json "\"accounts\""));
       assert (not (contains json "\"affiliations\""));
       assert (contains json "\"https://m.example\"");
       assert (not (contains json "\"label\""))
   | Error e -> failwith e);
  (* the version member must be 2, and 1 must be rejected *)
  assert (Result.is_error
            (Jsont_bytesrw.decode_string C.json_t
               {|{"version":1,"kind":"person","handle":"a","names":["A"]}|}));
  print_endline "✓ V2 contact works"
```

The test uses a `contains` helper, because `test_schema.ml` depends only on `sortal.schema` and `jsont` and so has no substring function available. Add it near the top of `test_schema.ml`:

```ocaml
let contains haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec go i = i + n <= h && (String.sub haystack i n = needle || go (i + 1)) in
  n = 0 || go 0
```

- [ ] **Step 2: Run test to verify it fails**

Run: `dune build @avsm/sortal/runtest`
Expected: FAIL, `Unbound module Sortal_schema.V2`.

- [ ] **Step 3: Write the interface**

Create `avsm/sortal/lib/schema/sortal_schema_contact_v2.mli` with the copyright header, then:

```ocaml
(** Contact schema V2.

    A contact is an identity, the accounts it is reachable through, and the
    context around it. V2 replaces V1's [services], [urls], [orcid] and
    [atproto] fields, which recorded the same fact four ways, with a single
    {!Sortal_schema_account.t} list. *)

module Account = Sortal_schema_account
module Platform = Sortal_schema_platform
module Date = Sortal_schema_date
module Feed = Sortal_schema_feed

val version : int
(** [version] is the schema version this module reads and writes, [2]. *)

type kind = Person | Organization

type link = {
  url : string;
  label : string option;  (** a human description, rarely needed *)
}
(** A web page that is not an account. *)

type affiliation = {
  org : string;
  department : string option;
  title : string option;
  url : string option;
  address : string option;
  from : Date.t option;   (** inclusive *)
  until : Date.t option;  (** exclusive *)
}
(** An employment or academic affiliation. This is the only part of the
    schema that carries a date. *)

type t

val make :
  handle:string ->
  names:string list ->
  ?kind:kind ->
  ?emails:string list ->
  ?accounts:Account.t list ->
  ?links:link list ->
  ?affiliations:affiliation list ->
  ?photo:string ->
  ?feeds:Feed.t list ->
  ?vcard:(string * string) list ->
  unit -> t
(** [make ~handle ~names ()] is a contact. [kind] defaults to [Person] and
    every list defaults to empty. [names] must not be empty, and its first
    entry is the primary name. *)

(** {1 Accessors} *)

val kind : t -> kind
val handle : t -> string
val names : t -> string list

val name : t -> string
(** [name t] is [t]'s primary name, the first of {!names}. *)

val emails : t -> string list
(** [emails t] is [t]'s addresses, preferred first. *)

val accounts : t -> Account.t list
val links : t -> link list
val affiliations : t -> affiliation list
val photo : t -> string option
val feeds : t -> Feed.t list
val vcard : t -> (string * string) list

(** {1 Account queries} *)

val accounts_on : t -> Platform.id -> Account.t list
(** [accounts_on t p] is every account [t] holds on [p], in file order. *)

val account_on : t -> Platform.id -> Account.t option
(** [account_on t p] is [t]'s first account on [p]. *)

val handle_on : t -> Platform.id -> string option
(** [handle_on t p] is the handle of [t]'s first account on [p]. *)

val url_on : t -> Platform.id -> string option
(** [url_on t p] is the URL of [t]'s first account on [p]. *)

val atproto : t -> Account.atproto option
(** [atproto t] is [t]'s AT Protocol identity. *)

val atproto_handle : t -> string option
val atproto_did : t -> string option

val set_atproto_did : t -> string -> t
(** [set_atproto_did t did] is [t] with its AT Protocol DID set. It is [t]
    unchanged if [t] has no AT Protocol account. *)

val best_url : t -> string option
(** [best_url t] is the URL a reader should follow to find [t]. It is the
    first link if there is one, and otherwise the URL of the first account. *)

val current_affiliation : t -> affiliation option
(** [current_affiliation t] is [t]'s first affiliation with no [until] date. *)

(** {1 Modification} *)

val add_feed : t -> Feed.t -> t
val remove_feed : t -> string -> t
(** [remove_feed t url] is [t] without any feed whose URL is [url]. *)

val check : t -> (unit, string) result
(** [check t] is [Ok ()] if [t]'s names are non-empty and every account
    passes {!Sortal_schema_account.check}, or [Error why]. *)

(** {1 Comparison, display and encoding} *)

val compare : t -> t -> int
val pp : Format.formatter -> t -> unit

val json_t : t Jsont.t
(** [json_t] maps a V2 contact. The [version] member is always encoded and
    must equal [2] on decoding, so a V1 file is rejected rather than
    misread. Empty collections and absent options are omitted on encoding. A
    link with no label encodes as a bare string. *)
```

- [ ] **Step 4: Write the implementation**

Create `avsm/sortal/lib/schema/sortal_schema_contact_v2.ml` with the copyright header. Follow the codec style of `sortal_schema_contact_v1.ml:417-545`, which uses `Jsont.Object.map ~kind ... |> mem ... |> opt_mem ... |> finish`.

```ocaml
module Account = Sortal_schema_account
module Platform = Sortal_schema_platform
module Date = Sortal_schema_date
module Feed = Sortal_schema_feed

let version = 2

type kind = Person | Organization

type link = { url : string; label : string option }

type affiliation = {
  org : string;
  department : string option;
  title : string option;
  url : string option;
  address : string option;
  from : Date.t option;
  until : Date.t option;
}

type t = {
  kind : kind;
  handle : string;
  names : string list;
  emails : string list;
  accounts : Account.t list;
  links : link list;
  affiliations : affiliation list;
  photo : string option;
  feeds : Feed.t list;
  vcard : (string * string) list;
}

let make ~handle ~names ?(kind = Person) ?(emails = []) ?(accounts = [])
    ?(links = []) ?(affiliations = []) ?photo ?(feeds = []) ?(vcard = []) () =
  { kind; handle; names; emails; accounts; links; affiliations; photo;
    feeds; vcard }

let kind t = t.kind
let handle t = t.handle
let names t = t.names
let name t = match t.names with n :: _ -> n | [] -> t.handle
let emails t = t.emails
let accounts t = t.accounts
let links t = t.links
let affiliations t = t.affiliations
let photo t = t.photo
let feeds t = t.feeds
let vcard t = t.vcard

let accounts_on t p =
  List.filter (fun a -> Account.platform a = p) t.accounts

let account_on t p = List.nth_opt (accounts_on t p) 0
let handle_on t p = Option.map Account.handle (account_on t p)
let url_on t p = Option.map Account.url (account_on t p)

let atproto t =
  List.find_map
    (function Account.Atproto a -> Some a | _ -> None)
    t.accounts

let atproto_handle t = Option.map (fun (a : Account.atproto) -> a.handle) (atproto t)
let atproto_did t = Option.bind (atproto t) (fun (a : Account.atproto) -> a.did)

let set_atproto_did t did =
  let replace = function
    | Account.Atproto a -> Account.Atproto { a with did = Some did }
    | other -> other
  in
  { t with accounts = List.map replace t.accounts }

let best_url t =
  match t.links with
  | { url; _ } :: _ -> Some url
  | [] -> (match t.accounts with a :: _ -> Some (Account.url a) | [] -> None)

let current_affiliation t = List.find_opt (fun a -> a.until = None) t.affiliations

let add_feed t feed = { t with feeds = t.feeds @ [ feed ] }

let remove_feed t url =
  { t with feeds = List.filter (fun f -> Feed.url f <> url) t.feeds }

let check t =
  if t.names = [] then Error "a contact needs at least one name"
  else
    List.fold_left
      (fun acc a -> match acc with Error _ -> acc | Ok () -> Account.check a)
      (Ok ()) t.accounts

let compare a b = String.compare a.handle b.handle

let kind_to_string = function Person -> "person" | Organization -> "organization"

let kind_of_string = function
  | "person" -> Some Person
  | "organization" -> Some Organization
  | _ -> None

let kind_json =
  let dec meta s =
    match kind_of_string s with
    | Some k -> k
    | None -> Jsont.Error.msgf meta "Kind: unknown contact kind: %S" s
  in
  Jsont.Base.string (Jsont.Base.map ~kind:"Kind" ~dec ~enc:kind_to_string ())

(* A link with no label encodes as a bare string, which is what 318 of the
   319 links in the live database are. *)
let link_json =
  let obj =
    let open Jsont.Object in
    map ~kind:"Link" (fun url label -> { url; label })
    |> mem "url" Jsont.string ~enc:(fun l -> l.url)
    |> opt_mem "label" Jsont.string ~enc:(fun l -> l.label)
    |> finish
  in
  Jsont.any ~kind:"Link"
    ~dec_string:
      (Jsont.map ~dec:(fun url -> { url; label = None }) Jsont.string)
    ~dec_object:obj
    ~enc:(fun l ->
      match l.label with
      | None -> Jsont.map ~enc:(fun l -> l.url) Jsont.string
      | Some _ -> obj)
    ()

let affiliation_json =
  let open Jsont.Object in
  map ~kind:"Affiliation"
    (fun org department title url address from until ->
      { org; department; title; url; address; from; until })
  |> mem "org" Jsont.string ~enc:(fun a -> a.org)
  |> opt_mem "department" Jsont.string ~enc:(fun a -> a.department)
  |> opt_mem "title" Jsont.string ~enc:(fun a -> a.title)
  |> opt_mem "url" Jsont.string ~enc:(fun a -> a.url)
  |> opt_mem "address" Jsont.string ~enc:(fun a -> a.address)
  |> opt_mem "from" Date.json_t ~enc:(fun a -> a.from)
  |> opt_mem "until" Date.json_t ~enc:(fun a -> a.until)
  |> finish

let vcard_json =
  Jsont.map ~kind:"VCard"
    ~dec:(fun m -> Stdlib.Map.Make(String).bindings m)
    ~enc:(fun l ->
      List.fold_left
        (fun m (k, v) -> Stdlib.Map.Make(String).add k v m)
        (Stdlib.Map.Make(String).empty) l)
    (Jsont.Object.as_string_map Jsont.string)

(* Omit a collection that is empty rather than writing it as null, which is
   what V1 does in 173 of the 460 live files. *)
let list_mem name codec get =
  Jsont.Object.mem name codec ~dec_absent:[] ~enc:get
    ~enc_omit:(fun v -> v = [])

let json_t =
  let open Jsont.Object in
  map ~kind:"ContactV2"
    (fun v kind handle names emails accounts links affiliations photo feeds
         vcard ->
      if v <> version then
        Jsont.Error.msgf Jsont.Meta.none "expected schema version %d, got %d"
          version v;
      { kind; handle; names; emails; accounts; links; affiliations; photo;
        feeds; vcard })
  |> mem "version" Jsont.int ~enc:(fun _ -> version)
  |> mem "kind" kind_json ~enc:(fun c -> c.kind)
  |> mem "handle" Jsont.string ~enc:(fun c -> c.handle)
  |> mem "names" (Jsont.list Jsont.string) ~enc:(fun c -> c.names)
  |> list_mem "emails" (Jsont.list Jsont.string) (fun c -> c.emails)
  |> list_mem "accounts" Account.json_t (fun c -> c.accounts)
  |> list_mem "links" (Jsont.list link_json) (fun c -> c.links)
  |> list_mem "affiliations" (Jsont.list affiliation_json)
       (fun c -> c.affiliations)
  |> opt_mem "photo" Jsont.string ~enc:(fun c -> c.photo)
  |> list_mem "feeds" (Jsont.list Feed.json_t) (fun c -> c.feeds)
  |> list_mem "vcard" vcard_json (fun c -> c.vcard)
  |> finish

let pp ppf t =
  Fmt.pf ppf "@[<v>%a (%a)@,%a@]"
    Fmt.(styled `Bold string) (name t)
    Fmt.(styled `Faint string) t.handle
    Fmt.(list ~sep:cut string)
    (List.map (fun a -> Platform.key (Account.platform a) ^ ": " ^ Account.handle a)
       t.accounts)
```

- [ ] **Step 5: Run the tests**

Run: `dune build @avsm/sortal/runtest 2>&1 | tail -30`

Expected: PASS. Likely adjustments against the real `jsont` interface:

- `Jsont.Object.mem`'s optional arguments for a default and for omitting on encode may be named differently from `~dec_absent` and `~enc_omit`. Read the signature at `jsont.mli:969` and fix `list_mem` once, since every list member goes through it.
- `vcard_json` uses `Stdlib.Map.Make(String)` inline three times, which will not share a type. Bind `module Smap = Stdlib.Map.Make (String)` at the top of the file and use `Smap` instead.

- [ ] **Step 6: Format and commit**

```bash
dune build @fmt --auto-promote
dune build && dune runtest
git add avsm/sortal/lib/schema/sortal_schema_contact_v2.ml* avsm/sortal/test/test_schema.ml
git commit -m "Add V2 contact record"
```

---

### Task 5: Wire V2 into the schema library

**Files:**
- Modify: `avsm/sortal/lib/schema/sortal_schema.ml`, `avsm/sortal/lib/schema/sortal_schema.mli`

**Interfaces:**
- Consumes: Tasks 1 to 4.
- Produces: `Sortal_schema.V2`, `Sortal_schema.Account`, `Sortal_schema.Platform`, `Sortal_schema.Date`. `Sortal_schema.Contact` still aliases V1 after this task and flips in Task 8.

- [ ] **Step 1: Update the interface**

Replace the body of `avsm/sortal/lib/schema/sortal_schema.mli` below its existing header comment with:

```ocaml
(** {1 Schema Version 1}

    V1 is retained so that {!Sortal_schema_migrate} can read existing files.
    It is removed once every store has been migrated. *)

module V1 : sig
  module Temporal = Sortal_schema_temporal
  module Feed = Sortal_schema_feed
  module Contact = Sortal_schema_contact_v1
end

(** {1 Schema Version 2} *)

module V2 : sig
  module Date = Sortal_schema_date
  module Platform = Sortal_schema_platform
  module Account = Sortal_schema_account
  module Feed = Sortal_schema_feed
  module Contact = Sortal_schema_contact_v2
end

(** {1 Current version aliases}

    These point at the current stable version. They move to V2 when the
    store is migrated. *)

module Date = V2.Date
module Platform = V2.Platform
module Account = V2.Account
module Feed = V2.Feed
module Temporal = V1.Temporal
module Contact = V1.Contact
```

- [ ] **Step 2: Update the implementation**

Mirror the same structure in `avsm/sortal/lib/schema/sortal_schema.ml`.

- [ ] **Step 3: Verify the build and tests**

Run: `dune build && dune runtest 2>&1 | tail -20`
Expected: clean. Nothing outside the schema library changes, because `Contact` and `Temporal` still alias V1.

- [ ] **Step 4: Format and commit**

```bash
dune build @fmt --auto-promote
git add avsm/sortal/lib/schema/sortal_schema.ml*
git commit -m "Expose schema V2 alongside V1"
```

---

### Task 6: V1 to V2 migration

**Files:**
- Create: `avsm/sortal/lib/schema/sortal_schema_migrate.ml`, `avsm/sortal/lib/schema/sortal_schema_migrate.mli`
- Modify: `avsm/sortal/lib/schema/sortal_schema.ml`, `avsm/sortal/lib/schema/sortal_schema.mli`, `avsm/sortal/test/test_schema.ml`

**Interfaces:**
- Consumes: `Sortal_schema.V1.Contact`, `Sortal_schema.V2.Contact`, `Sortal_schema.Account`, `Sortal_schema.Platform`.
- Produces: `Sortal_schema_migrate.v1_to_v2 : V1.Contact.t -> (V2.Contact.t, string) result`, and `val account_host_platform : string -> Platform.id option`.

The migration is a pure function so it can be tested without I/O. It returns `Error` rather than dropping a field it does not recognise.

- [ ] **Step 1: Write the failing test**

Add to `avsm/sortal/test/test_schema.ml` and call it from `main`:

```ocaml
let test_migrate () =
  let module V1 = Sortal_schema.V1.Contact in
  let module V2 = Sortal_schema.V2.Contact in
  let module A = Sortal_schema.Account in
  let module P = Sortal_schema.Platform in
  let migrate c =
    match Sortal_schema.Migrate.v1_to_v2 c with
    | Ok v2 -> v2
    | Error e -> failwith e
  in
  (* a github service loses its url, kind and primary *)
  let c = V1.make ~handle:"a" ~names:[ "A" ]
      ~services:[ V1.make_service ~kind:V1.Github ~handle:"avsm"
                    ~primary:false "https://github.com/avsm" ] () in
  assert (V2.handle_on (migrate c) (P.Simple P.Github) = Some "avsm");

  (* a service with no handle derives one from the url tail *)
  let c = V1.make ~handle:"b" ~names:[ "B" ]
      ~services:[ V1.make_service ~kind:V1.Github "https://github.com/mdales" ] () in
  assert (V2.handle_on (migrate c) (P.Simple P.Github) = Some "mdales");

  (* a service whose url is really a bare handle, as mdales.yaml has *)
  let c = V1.make ~handle:"c" ~names:[ "C" ]
      ~services:[ V1.make_service ~kind:V1.Github "mdales" ] () in
  assert (V2.handle_on (migrate c) (P.Simple P.Github) = Some "mdales");

  (* both mastodon spellings converge *)
  let c = V1.make ~handle:"d" ~names:[ "D" ]
      ~services:[ V1.make_service ~kind:(V1.ActivityPub V1.Mastodon)
                    ~handle:"avsm@amok.recoil.org" "https://amok.recoil.org/@avsm" ] () in
  assert (V2.handle_on (migrate c) (P.Federated P.Mastodon)
          = Some "avsm@amok.recoil.org");

  (* photo splits by host *)
  let c = V1.make ~handle:"e" ~names:[ "E" ]
      ~services:[ V1.make_service ~kind:V1.Photo ~handle:"avsm"
                    "https://www.instagram.com/avsm";
                  V1.make_service ~kind:V1.Photo ~handle:"avsm"
                    "https://www.flickr.com/photos/avsm" ] () in
  let m = migrate c in
  assert (V2.handle_on m (P.Simple P.Instagram) = Some "avsm");
  assert (V2.handle_on m (P.Simple P.Flickr) = Some "avsm");

  (* the orcid field becomes an account *)
  let c = V1.make ~handle:"f" ~names:[ "F" ] ~orcid:"0000-0001-8954-2428" () in
  assert (V2.handle_on (migrate c) (P.Simple P.Orcid)
          = Some "0000-0001-8954-2428");

  (* a linkedin url is promoted out of urls *)
  let c = V1.make ~handle:"g" ~names:[ "G" ]
      ~urls:[ V1.url_of_string "https://www.linkedin.com/in/anilmadhavapeddy" ] () in
  let m = migrate c in
  assert (V2.handle_on m (P.Simple P.LinkedIn) = Some "anilmadhavapeddy");
  assert (V2.links m = []);

  (* a github.io page is a personal site, not a github account *)
  let c = V1.make ~handle:"h" ~names:[ "H" ]
      ~urls:[ V1.url_of_string "https://ancazugo.github.io/" ] () in
  let m = migrate c in
  assert (V2.account_on m (P.Simple P.Github) = None);
  assert (List.length (V2.links m) = 1);

  (* emails lose their type and keep their order *)
  let c = V1.make ~handle:"i" ~names:[ "I" ]
      ~emails:[ V1.make_email ~type_:V1.Personal "anil@recoil.org";
                V1.make_email ~type_:V1.Work "avsm2@cam.ac.uk" ] () in
  assert (V2.emails (migrate c) = [ "anil@recoil.org"; "avsm2@cam.ac.uk" ]);

  (* an unknown custom service kind is an error, not a silent drop *)
  let c = V1.make ~handle:"j" ~names:[ "J" ]
      ~services:[ V1.make_service ~kind:(V1.Custom "myspace")
                    "https://myspace.com/j" ] () in
  assert (Result.is_error (Sortal_schema.Migrate.v1_to_v2 c));
  print_endline "✓ V1 to V2 migration works"
```

- [ ] **Step 2: Run test to verify it fails**

Run: `dune build @avsm/sortal/runtest`
Expected: FAIL, `Unbound module Sortal_schema.Migrate`.

- [ ] **Step 3: Write the interface**

Create `avsm/sortal/lib/schema/sortal_schema_migrate.mli` with the copyright header, then:

```ocaml
(** Migration from schema V1 to V2.

    The migration is pure so that it can be tested against a copy of a live
    store without touching it. It fails rather than dropping a field it does
    not recognise, so a store is never silently truncated. *)

val v1_to_v2 :
  Sortal_schema_contact_v1.t ->
  (Sortal_schema_contact_v2.t, string) result
(** [v1_to_v2 c] is [c] in the V2 schema, or [Error why] naming the first
    field that has no V2 equivalent.

    A [services] entry becomes an account, losing its [url], [label] and
    [primary]. Its handle is taken from the entry, or derived from the URL
    tail, or is the URL itself when the URL is a bare handle. A [urls] entry
    whose host names a platform becomes an account, and every other entry
    becomes a link. The [orcid] field and the [atproto] block become
    accounts, [organizations] becomes affiliations, and [icon] and
    [thumbnail] collapse into [photo]. *)

val platform_of_host : string -> Sortal_schema_platform.id option
(** [platform_of_host h] is the platform served at host [h], or [None] if
    [h] is an ordinary web host. A [github.io] host is a personal site and
    is not GitHub. *)
```

- [ ] **Step 4: Write the implementation**

Create `avsm/sortal/lib/schema/sortal_schema_migrate.ml` with the copyright header. The `Result` plumbing uses `let*`, matching `ocaml-dev:result` conventions used elsewhere in the repo.

```ocaml
module V1 = Sortal_schema_contact_v1
module V2 = Sortal_schema_contact_v2
module A = Sortal_schema_account
module P = Sortal_schema_platform
module D = Sortal_schema_date

let ( let* ) = Result.bind

let strip_www h =
  match String.starts_with ~prefix:"www." h with
  | true -> String.sub h 4 (String.length h - 4)
  | false -> h

let host_of_url url =
  match String.index_opt url ':' with
  | None -> None
  | Some _ ->
      let after_scheme =
        match String.split_on_char '/' url with
        | _scheme :: "" :: host :: _ -> Some host
        | _ -> None
      in
      Option.map strip_www after_scheme

let platform_of_host h =
  (* A github.io host is a personal page, not a GitHub account. *)
  if String.ends_with ~suffix:"github.io" h then None
  else
    match h with
    | "github.com" -> Some (P.Simple P.Github)
    | "gitlab.com" -> Some (P.Simple P.Gitlab)
    | "codeberg.org" -> Some (P.Simple P.Codeberg)
    | "orcid.org" -> Some (P.Simple P.Orcid)
    | "scholar.google.com" -> Some (P.Simple P.Scholar)
    | "twitter.com" | "x.com" -> Some (P.Simple P.Twitter)
    | "linkedin.com" | "uk.linkedin.com" -> Some (P.Simple P.LinkedIn)
    | "threads.com" | "threads.net" -> Some (P.Simple P.Threads)
    | "instagram.com" -> Some (P.Simple P.Instagram)
    | "flickr.com" -> Some (P.Simple P.Flickr)
    | _ -> None

(* The last non-empty path segment, which is the handle for every platform
   whose URL is a bare path. Scholar is the exception and is handled by its
   caller. *)
let url_tail url =
  let segments =
    String.split_on_char '/' url |> List.filter (fun s -> s <> "")
  in
  match List.rev segments with tail :: _ -> Some tail | [] -> None

let scholar_id url =
  match String.index_opt url '=' with
  | None -> None
  | Some i -> Some (String.sub url (i + 1) (String.length url - i - 1))

let handle_of_simple p url =
  match p with
  | P.Scholar -> scholar_id url
  | _ -> url_tail url

let account_of_simple p handle = Ok (A.Simple (p, handle))

let account_of_federated p handle =
  match String.rindex_opt handle '@' with
  | Some i when i > 0 && i < String.length handle - 1 ->
      let user = String.sub handle 0 i in
      let host = String.sub handle (i + 1) (String.length handle - i - 1) in
      Ok (A.Federated (p, user, host))
  | _ ->
      Error
        (Printf.sprintf "%s account %S is not in user@host form"
           (P.key (P.Federated p)) handle)

(* A service's handle, preferring the recorded one, then the URL tail, then
   the URL itself when it is a bare handle rather than a URL. *)
let service_handle (s : V1.service) =
  match s.handle with
  | Some h -> Some h
  | None ->
      if String.starts_with ~prefix:"http" s.url then url_tail s.url
      else Some s.url

let account_of_service (s : V1.service) =
  let need_handle () =
    match service_handle s with
    | Some h -> Ok h
    | None -> Error (Printf.sprintf "service %S has no usable handle" s.url)
  in
  match s.kind with
  | None -> Error (Printf.sprintf "service %S has no kind" s.url)
  | Some V1.Github -> let* h = need_handle () in account_of_simple P.Github h
  | Some V1.Twitter -> let* h = need_handle () in account_of_simple P.Twitter h
  | Some V1.LinkedIn -> let* h = need_handle () in account_of_simple P.LinkedIn h
  | Some V1.Git -> (
      match Option.bind (host_of_url s.url) platform_of_host with
      | Some (P.Simple p) -> let* h = need_handle () in account_of_simple p h
      | _ -> Error (Printf.sprintf "git service %S has no known host" s.url))
  | Some V1.Photo -> (
      match Option.bind (host_of_url s.url) platform_of_host with
      | Some (P.Simple p) -> let* h = need_handle () in account_of_simple p h
      | _ -> Error (Printf.sprintf "photo service %S has no known host" s.url))
  | Some (V1.ActivityPub V1.Mastodon) ->
      let* h = need_handle () in account_of_federated P.Mastodon h
  | Some (V1.ActivityPub V1.Pixelfed) ->
      let* h = need_handle () in account_of_federated P.Pixelfed h
  | Some (V1.ActivityPub V1.PeerTube) ->
      let* h = need_handle () in account_of_federated P.PeerTube h
  | Some (V1.ActivityPub (V1.Other_activitypub v)) ->
      Error (Printf.sprintf "unknown ActivityPub variant %S" v)
  | Some (V1.Custom "matrix") ->
      let* h = need_handle () in
      let h = if String.contains h '@' then h
              else match host_of_url s.url with
                   | Some host -> h ^ "@" ^ host
                   | None -> h in
      account_of_federated P.Matrix h
  | Some (V1.Custom "zulip") ->
      let* h = need_handle () in
      let h = if String.contains h '@' then h
              else match host_of_url s.url with
                   | Some host -> h ^ "@" ^ host
                   | None -> h in
      account_of_federated P.Zulip h
  | Some (V1.Custom "discourse") ->
      let* h = need_handle () in
      let h = if String.contains h '@' then h
              else match host_of_url s.url with
                   | Some host -> h ^ "@" ^ host
                   | None -> h in
      account_of_federated P.Discourse h
  | Some (V1.Custom "threads") ->
      let* h = need_handle () in account_of_simple P.Threads h
  | Some (V1.Custom "bluesky") ->
      let* h = need_handle () in
      Ok (A.Atproto { A.handle = h; did = None; apps = [ A.Bluesky ] })
  | Some (V1.Custom other) ->
      Error (Printf.sprintf "unknown service kind %S" other)

let app_of_v1 = function
  | V1.ATBluesky -> Ok A.Bluesky
  | V1.ATTangled -> Ok A.Tangled
  | V1.ATCustom "standard-site" -> Ok A.Standard_site
  | V1.ATCustom other ->
      Error (Printf.sprintf "unknown AT Protocol app %S" other)

let account_of_atproto (a : V1.atproto) =
  let* apps =
    List.fold_left
      (fun acc (s : V1.atproto_service) ->
        let* acc = acc in
        let* app = app_of_v1 s.atp_type in
        Ok (acc @ [ app ]))
      (Ok []) a.atp_services
  in
  Ok (A.Atproto { A.handle = a.atp_handle; did = a.atp_did; apps })

let affiliation_of_org (o : V1.organization) =
  let date d = Option.bind d (fun d -> Some d) in
  {
    V2.org = o.name;
    department = o.department;
    title = o.title;
    url = o.url;
    address = o.address;
    from = date (Option.bind o.range (fun (r : Sortal_schema_temporal.range) -> r.from));
    until = date (Option.bind o.range (fun (r : Sortal_schema_temporal.range) -> r.until));
  }

let v1_to_v2 (c : V1.t) =
  let collect f xs =
    List.fold_left
      (fun acc x ->
        let* acc = acc in
        let* y = f x in
        Ok (acc @ [ y ]))
      (Ok []) xs
  in
  let* service_accounts = collect account_of_service (V1.services c) in
  let* atproto_accounts =
    match V1.atproto c with
    | None -> Ok []
    | Some a -> let* acc = account_of_atproto a in Ok [ acc ]
  in
  let orcid_accounts =
    match V1.orcid c with
    | None -> []
    | Some o -> [ A.Simple (P.Orcid, o) ]
  in
  (* Split urls into promoted accounts and plain links. *)
  let promoted, plain =
    List.partition_map
      (fun (u : V1.url_entry) ->
        match Option.bind (host_of_url u.url) platform_of_host with
        | Some (P.Simple p) -> (
            match handle_of_simple p u.url with
            | Some h -> Either.Left (A.Simple (p, h))
            | None -> Either.Right u)
        | _ -> Either.Right u)
      (V1.urls c)
  in
  let links =
    List.map (fun (u : V1.url_entry) -> { V2.url = u.url; label = u.label }) plain
  in
  let accounts =
    service_accounts @ orcid_accounts @ atproto_accounts @ promoted
  in
  (* Drop an account whose platform is already present, keeping the first,
     so an ORCID recorded both as a field and as a URL yields one account. *)
  let accounts =
    List.fold_left
      (fun acc a ->
        let seen = List.exists (fun b -> A.platform b = A.platform a
                                         && A.handle b = A.handle a) acc in
        if seen then acc else acc @ [ a ])
      [] accounts
  in
  let kind = match V1.kind c with
    | V1.Person -> V2.Person
    | V1.Organization -> V2.Organization
  in
  let photo =
    match V1.thumbnail c with Some t -> Some t | None -> V1.icon c
  in
  Ok
    (V2.make ~handle:(V1.handle c) ~names:(V1.names c) ~kind
       ~emails:(List.map (fun (e : V1.email) -> e.address) (V1.emails c))
       ~accounts ~links
       ~affiliations:(List.map affiliation_of_org (V1.organizations c))
       ?photo
       ~feeds:(Option.value ~default:[] (V1.feeds c))
       ())
```

- [ ] **Step 5: Expose the module**

Add `module Migrate = Sortal_schema_migrate` to both `sortal_schema.ml` and `sortal_schema.mli`, after the `V2` module.

- [ ] **Step 6: Run the tests**

Run: `dune build @avsm/sortal/runtest 2>&1 | tail -40`

Expected: PASS. Two likely problems:

- `Either.Left` and `List.partition_map` need OCaml 4.12 or later, which this switch has. If `Either` is not in scope, use `List.partition` over a predicate and map each side separately.
- `V1.icon` is at `sortal_schema_contact_v1.mli:228` and `sortal_schema_contact_v1.ml:142`, so it needs no work.

- [ ] **Step 7: Format and commit**

```bash
dune build @fmt --auto-promote
dune build && dune runtest
git add avsm/sortal/lib/schema/sortal_schema_migrate.ml* avsm/sortal/lib/schema/sortal_schema.ml*
git add avsm/sortal/test/test_schema.ml
git commit -m "Add V1 to V2 contact migration"
```

---

### Task 7: Golden test against the live database

**Files:**
- Create: `avsm/sortal/test/test_migrate_golden.ml`
- Modify: `avsm/sortal/test/dune`

**Interfaces:**
- Consumes: `Sortal_schema.Migrate.v1_to_v2`, `Sortal_schema.V1.Contact.json_t`, `Sortal_schema.V2.Contact.json_t`.
- Produces: nothing consumed by later tasks. This is the gate that makes Task 8 safe.

The test reads the live store in place. Nothing is copied into the repository, because the store holds 460 real people's names, emails and affiliations and committing them would put that in the git history permanently. The cost of this choice is that the test cannot gate CI or a fresh clone, so it must skip cleanly when the store is absent rather than fail.

- [ ] **Step 1: Confirm the store is readable**

```bash
ls ~/.local/share/sortal/*.yaml | wc -l
```

Expected: 460. The test locates the store from `SORTAL_DATA_DIR` if set, and otherwise from `$HOME/.local/share/sortal`, matching how `Sortal_store` resolves it.

- [ ] **Step 2: Write the test**

Create `avsm/sortal/test/test_migrate_golden.ml` with the copyright header:

```ocaml
(** Every contact in the live store must migrate, and the result must
    re-encode and decode unchanged. This is the gate for switching the store
    to V2.

    The store is read in place rather than copied into the repository,
    because it holds real personal data. That means this test cannot run in
    CI or in a fresh clone, so an absent store is a skip and not a
    failure. *)

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let store_dir () =
  match Sys.getenv_opt "SORTAL_DATA_DIR" with
  | Some d -> d
  | None -> Filename.concat (Sys.getenv "HOME") ".local/share/sortal"

let () =
  let dir = store_dir () in
  if not (Sys.file_exists dir && Sys.is_directory dir) then begin
    Printf.printf "- no store at %s, skipping migration check\n" dir;
    exit 0
  end;
  let files =
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".yaml")
    |> List.sort String.compare
  in
  if files = [] then begin
    Printf.printf "- store at %s holds no contacts, skipping\n" dir;
    exit 0
  end;
  let failures = ref [] in
  List.iter
    (fun f ->
      let path = Filename.concat dir f in
      let yaml = read_file path in
      let reader = Bytesrw.Bytes.Reader.of_string yaml in
      match Yamlt.decode Sortal_schema.V1.Contact.json_t reader with
      | Error e -> failures := (f, "V1 decode: " ^ e) :: !failures
      | Ok v1 -> (
          match Sortal_schema.Migrate.v1_to_v2 v1 with
          | Error e -> failures := (f, "migrate: " ^ e) :: !failures
          | Ok v2 -> (
              match Jsont_bytesrw.encode_string Sortal_schema.V2.Contact.json_t v2 with
              | Error e -> failures := (f, "V2 encode: " ^ e) :: !failures
              | Ok json -> (
                  match
                    Jsont_bytesrw.decode_string Sortal_schema.V2.Contact.json_t json
                  with
                  | Error e -> failures := (f, "V2 decode: " ^ e) :: !failures
                  | Ok again ->
                      if again <> v2 then
                        failures := (f, "round trip differs") :: !failures))))
    files;
  match !failures with
  | [] -> Printf.printf "✓ %d contacts migrate cleanly\n" (List.length files)
  | fs ->
      List.iter (fun (f, why) -> Printf.eprintf "%s: %s\n" f why) fs;
      Printf.eprintf "%d of %d contacts failed\n" (List.length fs)
        (List.length files);
      exit 1
```

- [ ] **Step 3: Wire it into dune**

Append to `avsm/sortal/test/dune`:

```
(test
 (name test_migrate_golden)
 (deps (universe))
 (libraries sortal.schema yamlt jsont jsont.bytesrw bytesrw))
```

`(deps (universe))` is required. Without it dune caches the result and the test stops re-running when the store changes, which is exactly when it matters.

- [ ] **Step 4: Run it**

Run: `dune build @avsm/sortal/runtest 2>&1 | tail -40`

Expected: `✓ 460 contacts migrate cleanly`. A skip message means the store was not found, which is a failure of this step even though the test exits 0. If contacts fail, fix `Sortal_schema_migrate`, not the test. Every failure is a real case the migration does not yet handle. Do not add a catch-all that turns an unknown field into a link, because that is exactly the silent truncation the spec forbids.

- [ ] **Step 5: Inspect the output by eye**

Write a scratch binary or use the existing CLI to print the migrated form of `avsm.yaml` and compare it against the worked example in the design spec. The account keys, the atproto block and the two affiliations should match.

- [ ] **Step 6: Commit**

```bash
dune build @fmt --auto-promote
dune build && dune runtest
git add avsm/sortal/test/test_migrate_golden.ml avsm/sortal/test/dune
git commit -m "Pin V1 to V2 migration against the live contact store"
```

---

### Task 8: Switch the store and CLI to V2

**Files:**
- Modify: `avsm/sortal/lib/schema/sortal_schema.ml`, `avsm/sortal/lib/schema/sortal_schema.mli`
- Modify: `avsm/sortal/lib/core/sortal_store.ml`, `avsm/sortal/lib/core/sortal_store.mli`
- Modify: `avsm/sortal/lib/core/sortal_cmd.ml`
- Modify: `avsm/sortal/bin/sortal_cli.ml`
- Modify: `avsm/sortal/lib/web/pages.ml`, `avsm/sortal/lib/web/sortal_web.ml`
- Modify: `avsm/sortal/test/test_sortal.ml`, `avsm/sortal/test/test_web.ml`

**Interfaces:**
- Consumes: everything from Tasks 1 to 7.
- Produces: `Sortal_schema.Contact = V2.Contact`, and a store whose `save` and `lookup` read and write V2.

This task is deliberately large. The repository requires every commit to build and test cleanly, and `Sortal_schema.Contact` cannot flip without every user of it flipping in the same commit. Work through the files in the order given and only commit at the end.

- [ ] **Step 1: Flip the aliases**

In both `sortal_schema.ml` and `sortal_schema.mli`, change:

```ocaml
module Contact = V1.Contact
```

to:

```ocaml
module Contact = V2.Contact
```

Leave `module Temporal = V1.Temporal` in place for now. It is removed in Task 12.

- [ ] **Step 2: Build and capture the full error list**

```bash
dune build 2>&1 | grep -E "^File" | sort -u > /tmp/v2-errors.txt
wc -l /tmp/v2-errors.txt
cat /tmp/v2-errors.txt
```

This is the authoritative worklist for the rest of this task. Work down it.

- [ ] **Step 3: Update the store**

In `avsm/sortal/lib/core/sortal_store.mli`, delete these values, which have no V2 meaning because nothing carries a date any more except an affiliation:

- `find_by_email_at`
- `list_at`
- `module Temporal = Sortal_schema.Temporal`

Change `find_by_org` to drop its `?from` and `?until` arguments:

```ocaml
val find_by_org : t -> org:string -> Contact.t list
(** [find_by_org t ~org] is every contact with an affiliation whose
    organisation name contains [org], compared case-insensitively, sorted by
    handle. *)
```

Replace the per-field mutators with two general ones, since V2 has a single account notion:

```ocaml
val set_account : t -> string -> Contact.Account.t -> (unit, string) result
(** [set_account t handle account] adds [account] to the contact named
    [handle], replacing any existing account on the same platform. It is
    [Error why] if no such contact exists or if [account] fails its syntax
    check. *)

val unset_account : t -> string -> Contact.Platform.id -> (unit, string) result
(** [unset_account t handle platform] removes every account [handle] holds on
    [platform]. It is [Error why] if no such contact exists. *)
```

Delete `add_email`, `remove_email`, `add_service`, `remove_service`, `add_organization`, `remove_organization`, `add_url`, `remove_url` from both the `.mli` and the `.ml`, and implement `set_account` and `unset_account` in terms of the existing `update_contact`.

- [ ] **Step 4: Update the CLI**

In `avsm/sortal/lib/core/sortal_cmd.ml`, replace the eight `add_*_info` and `remove_*_info` command descriptions at lines 543 to 552 with two:

```ocaml
let set_info =
  Cmd.info "set" ~doc:"Set a contact's account on a platform"
let unset_info =
  Cmd.info "unset" ~doc:"Remove a contact's account on a platform"
```

In `avsm/sortal/bin/sortal_cli.ml`, replace the corresponding sub-command terms at lines 288 to 443 with `set` and `unset`. The `set` term takes three positional arguments, a handle, a platform key and a value:

```ocaml
let set_cmd =
  let handle = Arg.(required & pos 0 (some string) None
                    & info [] ~docv:"HANDLE" ~doc:"The contact to modify.") in
  let platform = Arg.(required & pos 1 (some string) None
                      & info [] ~docv:"PLATFORM"
                          ~doc:"The platform key, such as $(b,github).") in
  let value = Arg.(required & pos 2 (some string) None
                   & info [] ~docv:"HANDLE"
                       ~doc:"The handle on that platform.") in
  ...
```

When `Sortal_schema.Platform.of_key` returns `None`, exit non-zero with a message naming every known key:

```ocaml
Fmt.epr "unknown platform %S@.known platforms: %s@." key
  (String.concat ", "
     (List.map Sortal_schema.Platform.key Sortal_schema.Platform.all));
exit 1
```

- [ ] **Step 5: Update the web pages**

Work through the errors in `avsm/sortal/lib/web/pages.ml` and `sortal_web.ml`. The mapping is mechanical:

| V1 | V2 |
|---|---|
| `Contact.urls c` | `Contact.links c` |
| `Contact.current_url c` | `Contact.best_url c` |
| `Contact.services c` and `current_services c` | `Contact.accounts c` |
| `Contact.services_of_kind c Photo` | `List.concat_map (Contact.accounts_on c) [Simple Instagram; Simple Flickr]` |
| `Contact.github_handle c` | `Contact.handle_on c (Simple Github)` |
| `Contact.twitter_handle c` | `Contact.handle_on c (Simple Twitter)` |
| `Contact.bluesky_handle c` | `Contact.atproto_handle c` |
| `Contact.linkedin c` | `Contact.account_on c (Simple LinkedIn)` |
| `Contact.mastodon c` | `Contact.account_on c (Federated Mastodon)` |
| `Contact.orcid c` | `Contact.handle_on c (Simple Orcid)` |
| `Contact.current_organization c` | `Contact.current_affiliation c` |
| `Contact.current_organizations c` | `Contact.affiliations c` |
| `(svc : Contact.service).url` | `Account.url account` |
| `(e : Contact.email).address` | the string itself, since `emails` is now `string list` |
| `Contact.feeds c` returning an option | `Contact.feeds c` returning a list, so drop the `Option.value ~default:[]` |
| `Contact.thumbnail c` | `Contact.photo c` |

- [ ] **Step 6: Update the sortal tests**

`test_sortal.ml` and `test_web.ml` construct V1 contacts. Convert their fixtures to `Sortal_schema.Contact.make` in its V2 form. Keep every existing assertion that still has meaning, and delete only those that tested temporal queries.

- [ ] **Step 7: Verify**

```bash
dune build 2>&1 | tail -20
dune runtest 2>&1 | tail -30
```

Expected: both clean, including the golden migration test.

- [ ] **Step 8: Format and commit**

```bash
dune build @fmt --auto-promote
dune build && dune runtest
git add avsm/sortal
git commit -m "Switch sortal store and CLI to schema V2"
```

---

### Task 9: Update the consumer packages

**Files:**
- Modify: `avsm/arod/lib_component/sidebar.ml`, `layout.ml`, `links.ml`, `network.ml`, `common.ml`, `markdown_export.ml`
- Modify: `avsm/arod/lib/arod_jsonld.ml`, `arod_md.ml`, `arod_ctx.ml`
- Modify: `avsm/arod/lib_handlers/arod_handlers.ml`, `avsm/arod/bin/main.ml`
- Modify: `avsm/bushel/lib/bushel_entry.ml`
- Modify: `avsm/tessabot/lib/tessabot_config.ml`, `avsm/tessabot/bin/main.ml`

**Interfaces:**
- Consumes: the V2 `Contact` interface from Task 4 and the mapping table in Task 8 Step 5.
- Produces: nothing. This is the last step that makes the tree build.

Task 8 leaves these packages broken, so in practice Tasks 8 and 9 are one commit. Keep them as separate tasks for review, but stage the whole tree and commit once at the end of Task 9 if the tree cannot be green in between. If it can, commit separately.

- [ ] **Step 1: Get the worklist**

```bash
dune build 2>&1 | grep -E "^File" | sort -u
```

The known call sites, from a survey of the tree, are roughly 45 across these files. `avsm/arod/lib_component/sidebar.ml` holds about 20 of them and is by far the largest.

- [ ] **Step 2: Update tessabot and bushel first**

These are trivial and confirm the mapping works before the large file. `bushel_entry.ml:179` uses `Contact.names`, which is unchanged. `tessabot_config.ml:33` and `tessabot/bin/main.ml:31` use `Contact.feeds`, which is now a list rather than an option, so remove the `Option.value ~default:[]` wrapper.

- [ ] **Step 3: Update arod's smaller files**

Work through `common.ml:128`, `network.ml:464`, `markdown_export.ml:347`, `arod_ctx.ml:167`, `arod_handlers.ml:731` and `bin/main.ml:229`, which are all `Contact.feeds` or `Contact.kind`. Then `links.ml:114-115`, which becomes:

```ocaml
List.iter (fun (l : Contact.link) -> add_url c l.url) (Contact.links c);
List.iter (fun a -> add_url c (Contact.Account.url a)) (Contact.accounts c)
```

- [ ] **Step 4: Update arod_jsonld.ml and arod_md.ml**

`arod_jsonld.ml:100` becomes:

```ocaml
Contact.url_on author (Simple LinkedIn);
```

`arod_jsonld.ml:102` and `:123` use `Contact.orcid`, which becomes `Contact.handle_on author (Simple Orcid)`. Note that `:102` builds `"https://orcid.org/" ^ o`, which is now `Contact.url_on author (Simple Orcid)` directly.

`arod_jsonld.ml:111-112` uses `services_of_kind author Photo`, which becomes the Instagram and Flickr concatenation from the Task 8 mapping table.

- [ ] **Step 5: Update layout.ml and sidebar.ml**

These are the two files that render a contact's presence, so they carry the most sites. Convert each `(svc : Contact.service)` binding to an `Account.t`, and each `svc.url` to `Account.url svc`. `sidebar.ml:1086-1089` filters emails by validity, which no longer exists, so it becomes a plain `List.map` over `Contact.emails`.

- [ ] **Step 6: Verify the whole tree**

```bash
dune build 2>&1 | tail -20
dune runtest 2>&1 | tail -30
```

Expected: both clean across every package.

- [ ] **Step 7: Format and commit**

```bash
dune build @fmt --auto-promote
dune build && dune runtest
git add avsm
git commit -m "Update arod, bushel and tessabot for schema V2"
```

---

### Task 10: The migrate command

**Files:**
- Modify: `avsm/sortal/lib/core/sortal_store.ml`, `avsm/sortal/lib/core/sortal_store.mli`
- Modify: `avsm/sortal/bin/sortal_cli.ml`

**Interfaces:**
- Consumes: `Sortal_schema.Migrate.v1_to_v2`, the V2 store from Task 8.
- Produces: `Sortal_store.migrate : t -> dry_run:bool -> (int * (string * string) list)`, and a `sortal migrate` sub-command.

- [ ] **Step 1: Add the store function to the interface**

In `avsm/sortal/lib/core/sortal_store.mli`:

```ocaml
val migrate : t -> dry_run:bool -> int * (string * string) list
(** [migrate t ~dry_run] rewrites every V1 file in [t] into V2, and is the
    number rewritten paired with the handle and reason of each file that
    could not be. A file that is already V2 is left alone and is not
    counted. When [dry_run] is true nothing is written.

    The store is git versioned, so a rewrite is recoverable. Run
    [sortal git commit] first if there are uncommitted changes. *)
```

- [ ] **Step 2: Implement it**

In `avsm/sortal/lib/core/sortal_store.ml`, following the directory scan at `sortal_store.ml:259`:

```ocaml
let migrate t ~dry_run =
  let migrated = ref 0 and failures = ref [] in
  let entries = Eio.Path.read_dir t.data_dir in
  List.iter
    (fun entry ->
      if Filename.check_suffix entry ".yaml" then begin
        let handle = Filename.chop_suffix entry ".yaml" in
        let path = Eio.Path.(t.data_dir / entry) in
        let yaml = Eio.Path.load path in
        let reader () = Bytesrw.Bytes.Reader.of_string yaml in
        match Yamlt.decode Sortal_schema.V2.Contact.json_t (reader ()) with
        | Ok _ -> ()  (* already V2 *)
        | Error _ -> (
            match Yamlt.decode Sortal_schema.V1.Contact.json_t (reader ()) with
            | Error e -> failures := (handle, "V1 decode: " ^ e) :: !failures
            | Ok v1 -> (
                match Sortal_schema.Migrate.v1_to_v2 v1 with
                | Error e -> failures := (handle, e) :: !failures
                | Ok v2 ->
                    if not dry_run then save t v2;
                    incr migrated))
      end)
    entries;
  (!migrated, List.rev !failures)
```

- [ ] **Step 3: Add the CLI sub-command**

In `avsm/sortal/bin/sortal_cli.ml`, alongside the existing `sync` sub-command:

```ocaml
let migrate_cmd =
  let dry_run =
    Arg.(value & flag
         & info [ "dry-run"; "n" ]
             ~doc:"Report what would change without writing anything.")
  in
  let run () dry_run =
    let store = store_of_env () in
    let migrated, failures = Sortal.Store.migrate store ~dry_run in
    List.iter (fun (h, why) -> Fmt.epr "%s: %s@." h why) failures;
    Fmt.pr "%d contacts %s@." migrated
      (if dry_run then "would be migrated" else "migrated");
    if failures <> [] then begin
      Fmt.epr "%d contacts failed, nothing was written for them@."
        (List.length failures);
      exit 1
    end
  in
  let doc = "Rewrite V1 contact files into the V2 schema" in
  let man =
    [ `S Manpage.s_description;
      `P "Rewrites every V1 file in the store into the V2 schema. A file \
          already in V2 is left alone. The store is git versioned, so \
          commit any outstanding changes first and review the resulting \
          diff before committing it.";
      `P "A contact that cannot be migrated is reported and left \
          untouched, and the command exits non-zero." ]
  in
  Cmd.v (Cmd.info "migrate" ~doc ~man)
    Term.(const run $ common_term $ dry_run)
```

Adjust `store_of_env` and `common_term` to match how the neighbouring `sync` sub-command in the same file obtains its store, rather than inventing new plumbing.

- [ ] **Step 4: Verify against a copy, never the live store**

```bash
rm -rf /tmp/sortal-migrate-test
cp -r ~/.local/share/sortal /tmp/sortal-migrate-test
SORTAL_DATA_DIR=/tmp/sortal-migrate-test dune exec avsm/sortal/bin/sortal_cli.exe -- migrate --dry-run
```

Expected: `460 contacts would be migrated`, no failures.

Then run it for real against the copy and inspect:

```bash
SORTAL_DATA_DIR=/tmp/sortal-migrate-test dune exec avsm/sortal/bin/sortal_cli.exe -- migrate
diff <(cat ~/.local/share/sortal/avsm.yaml) /tmp/sortal-migrate-test/avsm.yaml
wc -l /tmp/sortal-migrate-test/*.yaml | tail -1
```

Expected: the total is close to 3655 lines, against 4412 before. Read the new `avsm.yaml` in full and check it against the worked example in the design spec.

- [ ] **Step 5: Format and commit**

```bash
dune build @fmt --auto-promote
dune build && dune runtest
git add avsm/sortal/lib/core/sortal_store.ml* avsm/sortal/bin/sortal_cli.ml
git commit -m "Add sortal migrate command"
```

- [ ] **Step 6: Migrate the real store**

This step changes the owner's data. Confirm with them before running it.

```bash
cd ~/.local/share/sortal && git status --short   # must be clean
dune exec avsm/sortal/bin/sortal_cli.exe -- migrate
cd ~/.local/share/sortal && git diff --stat | tail -3
```

Review the diff before committing it in the store's own repository.

---

### Task 11: Revise the specification drafts

**Files:**
- Create: `avsm/sortal/spec/draft-madhavapeddy-sortal-01.txt`
- Modify: `avsm/sortal/spec/draft-madhavapeddy-sortal-impl-00.txt`

**Interfaces:**
- Consumes: the implemented V2 schema.
- Produces: documentation only.

`draft-madhavapeddy-sortal-00.txt` normatively describes V1 and is now wrong.

- [ ] **Step 1: Copy the draft and bump its version**

```bash
cp avsm/sortal/spec/draft-madhavapeddy-sortal-00.txt \
   avsm/sortal/spec/draft-madhavapeddy-sortal-01.txt
```

Update the header, the expiry date and every self-reference from `-00` to `-01`.

- [ ] **Step 2: Rewrite the affected sections**

| section | change |
|---|---|
| 2, Temporal Range | Reduce to a date. Delete range semantics, validity testing and range overlap. |
| 4, Email Object | An email is a string. Delete the type and note members. |
| 5, Organisation Object | Rename to Affiliation Object. Rename `name` to `org`. Replace `range` with `from` and `until`. |
| 6, URL Entry Object | Rename to Link Object. Delete `range`. State that an unlabelled link serialises as a bare string. |
| 7, Service Object | Replace with an Account section describing the platform-keyed mapping, the three value forms, and that an unknown key is an error. |
| 7.1, Service Kind | Replace with the platform vocabulary from `Sortal_schema_platform`. |
| 7.2, ActivityPub Variants | Delete. Mastodon, Pixelfed and PeerTube are now federated platforms. |
| 8, AT Protocol Object | Keep, but describe it as an account value with `handle`, `did` and `apps`. |
| 10.3, Encoding Rules | State that empty collections and absent options are omitted. |
| 11, Schema Versioning | Record that V2 supersedes V1 and that a V1 file is rejected rather than misread. |
| 12, Examples | Replace all three with the V2 forms from the design spec. |
| Appendix A, JSON Schema | Regenerate for V2. |
| Appendix B, ABNF for Service Kind Strings | Replace with the platform key list. |
| Appendix C, Comparison with vCard | Update to cite RFC 9554 `SOCIALPROFILE`, `SERVICE-TYPE` and `USERNAME`. |

- [ ] **Step 3: Check the companion draft**

Read `draft-madhavapeddy-sortal-impl-00.txt` and correct any V1 reference. `draft-madhavapeddy-sortal-vcard-00.txt` needs no change, because its `X-VALID-FROM`, `X-VALID-UNTIL`, `X-FEED` and `X-ATPROTO` definitions are what V2 maps onto.

- [ ] **Step 4: Commit**

```bash
git add avsm/sortal/spec
git commit -m "Revise sortal data model draft for schema V2"
```

---

### Task 12: Remove V1 and record the change

**Files:**
- Delete: `avsm/sortal/lib/schema/sortal_schema_contact_v1.ml{,i}`, `sortal_schema_temporal.ml{,i}`, `sortal_schema_migrate.ml{,i}`
- Modify: `avsm/sortal/lib/schema/sortal_schema.ml{,i}`, `avsm/sortal/test/dune`
- Delete: `avsm/sortal/test/test_migrate_golden.ml`
- Modify: `avsm/sortal/README.md`
- Create or modify: `avsm/sortal/CHANGES.md`

Do this only after Task 10 Step 6 has migrated the real store and the owner has committed the result. Until then V1 must stay readable.

- [ ] **Step 1: Confirm no store still needs V1**

Ask the owner to confirm every sortal store they use has been migrated. There is no way to check this from the repository.

- [ ] **Step 2: Delete the modules and their wiring**

Remove the `V1` module from `sortal_schema.ml` and `.mli`, along with `module Temporal = V1.Temporal` and `module Migrate = Sortal_schema_migrate`. Also remove `module Temporal = Sortal_schema.Temporal` from `avsm/sortal/lib/core/sortal.ml:6`, which re-exports it to every consumer. Delete the files and the golden test stanza from `test/dune`.

- [ ] **Step 3: Update the README**

`avsm/sortal/README.md` documents the V1 field list under "Metadata Fields" and shows V1 examples under "Usage Example". Replace both with the V2 model, and replace the `add-email` and `add-service` CLI examples with `set` and `unset`.

- [ ] **Step 4: Write the changelog**

Create `avsm/sortal/CHANGES.md` if it does not exist, using the exact format of `bleeding/xdge/CHANGES.md`, which is a version heading underlined with dashes and one bullet per user-visible change attributed to its author:

```markdown
v2.0.0 (dev)
------------

- Replace the V1 contact schema with V2, which records every online presence
  as an account on a platform rather than as one of `services`, `urls`,
  `orcid` or `atproto` (@avsm)
- Derive an account's URL from its platform and handle, so a platform, a
  handle and a URL can no longer disagree (@avsm)
- Reject an unknown platform key when decoding, rather than accepting it
  (@avsm)
- Drop temporal validity from emails, links and accounts, keeping it only on
  affiliations (@avsm)
- Drop the work and personal labels from emails (@avsm)
- Replace `add-email`, `add-service`, `add-url`, `add-org`, `add-atproto` and
  their `remove-` counterparts with `sortal set` and `sortal unset` (@avsm)
- Add `sortal migrate`, which rewrites a V1 store into V2 (@avsm)
```

- [ ] **Step 5: Verify and commit**

```bash
dune build && dune runtest && dune build @fmt
git add -A avsm/sortal
git commit -m "Remove schema V1 after migration"
```

---

## Follow-on work, not in this plan

Per-platform probes are specified in the design document under "Probes" and are a separate plan. They add a `probe` field to the platform spec record, a `Sortal_probe` module over `Fetch.read_only`, a verdict cache under `XDG_CACHE_HOME`, and a `sortal check` command. They build on `bleeding/webfinger` and `bleeding/atp`, and they depend on nothing in this plan beyond `Sortal_schema_platform` and `Sortal_schema_account`.
