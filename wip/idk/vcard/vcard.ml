(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Text = Vcard_text
module Param = Vcard_param
module Value_type = Vcard_value_type
module Registry = Vcard_registry
module Date = Vcard_date
module Property = Vcard_property
module N = Vcard_n
module Adr = Vcard_adr
module Jscomps = Vcard_jscomps
open Vcard_result

type t = { version : string; properties : Property.t list }

let v ?(version = "4.0") properties = { version; properties }
let version t = t.version
let properties t = t.properties

let equal a b =
  String.equal a.version b.version
  && List.equal Property.equal a.properties b.properties

let strip_cr l =
  let n = String.length l in
  if n > 0 && l.[n - 1] = '\r' then String.sub l 0 (n - 1) else l

(* RFC 6350 Section 3.2: a CRLF followed by a space or a tab is removed. *)
let unfold s =
  let physical = List.map strip_cr (String.split_on_char '\n' s) in
  let b = Buffer.create 128 in
  (* A blank line is a logical line of its own, so that a caller counting the
     result matches the physical lines it came from. *)
  let flush acc = Buffer.contents b :: acc in
  let rec go acc started = function
    | [] -> List.rev (if started then Buffer.contents b :: acc else acc)
    | l :: rest when l <> "" && (l.[0] = ' ' || l.[0] = '\t') ->
        Buffer.add_substring b l 1 (String.length l - 1);
        go acc true rest
    | l :: rest ->
        let acc = if started then flush acc else acc in
        Buffer.clear b;
        Buffer.add_string b l;
        go acc true rest
  in
  go [] false physical

let is_continuation c = Char.code c land 0xC0 = 0x80

(* RFC 6350 Section 3.2: fold at 75 octets, keeping multi-octet characters
   contiguous. *)
let fold line =
  let limit = 75 in
  let n = String.length line in
  if n <= limit then line
  else
    let b = Buffer.create (n + (n / limit * 3)) in
    let rec go start first =
      let avail = if first then limit else limit - 1 in
      if n - start <= avail then
        Buffer.add_string b (String.sub line start (n - start))
      else
        let cut = ref (start + avail) in
        while !cut > start + 1 && is_continuation line.[!cut] do
          decr cut
        done;
        Buffer.add_string b (String.sub line start (!cut - start));
        Buffer.add_string b "\r\n ";
        go !cut false
    in
    go 0 true;
    Buffer.contents b

(* RFC 6350 Sections 6.1.1 and 6.1.2: BEGIN and END take no parameter and
   their value is "VCARD", case insensitively. *)
let delimiter p =
  match Property.name p with
  | ("BEGIN" | "END") as name
    when String.equal (String.uppercase_ascii (Property.value p)) "VCARD" ->
      if Property.params p = [] then Ok (Some name)
      else error "%s:VCARD takes no parameter" name
  | _ -> Ok None

let of_string s =
  let lines = unfold s in
  let rec go cards current i = function
    | [] -> (
        match current with
        | None -> Ok (List.rev cards)
        | Some (_, _, start) ->
            error "line %d: the vCard has no END:VCARD" start)
    | line :: rest when String.trim line = "" -> go cards current (i + 1) rest
    | line :: rest -> (
        let* p =
          match Property.of_string line with
          | Ok p -> Ok p
          | Error msg -> error "line %d: %s" i msg
        in
        let* delimiter =
          match delimiter p with
          | Ok d -> Ok d
          | Error msg -> error "line %d: %s" i msg
        in
        match (current, delimiter) with
        | None, Some "BEGIN" -> go cards (Some (None, [], i)) (i + 1) rest
        | None, _ -> error "line %d: expected BEGIN:VCARD" i
        | Some (None, _, start), None
          when String.equal (Property.name p) "VERSION" ->
            go cards (Some (Some (Property.value p), [], start)) (i + 1) rest
        | Some (None, _, _), _ ->
            error "line %d: VERSION must follow BEGIN:VCARD" i
        | Some (Some version, props, _), Some "END" ->
            go
              ({ version; properties = List.rev props } :: cards)
              None (i + 1) rest
        | Some _, Some _ -> error "line %d: BEGIN:VCARD inside a vCard" i
        | Some (Some version, props, start), None ->
            if String.equal (Property.name p) "VERSION" then
              error "line %d: a second VERSION property" i
            else go cards (Some (Some version, p :: props, start)) (i + 1) rest)
  in
  go [] None 1 lines

let one_of_string s =
  match of_string s with
  | Ok [ t ] -> Ok t
  | Ok [] -> error "no vCard"
  | Ok cards -> error "%d vCards where one was expected" (List.length cards)
  | Error _ as e -> e

let to_string t =
  let b = Buffer.create 256 in
  let line l =
    Buffer.add_string b (fold l);
    Buffer.add_string b "\r\n"
  in
  line "BEGIN:VCARD";
  line ("VERSION:" ^ t.version);
  List.iter (fun p -> line (Property.to_string p)) t.properties;
  line "END:VCARD";
  Buffer.contents b

let pp ppf t = Format.pp_print_string ppf (to_string t)

let find_all t name =
  let name = String.uppercase_ascii name in
  List.filter (fun p -> String.equal (Property.name p) name) t.properties

let find t name = match find_all t name with [] -> None | p :: _ -> Some p

let group t g =
  let same x =
    String.equal (String.uppercase_ascii x) (String.uppercase_ascii g)
  in
  List.filter
    (fun p -> match Property.group p with Some x -> same x | None -> false)
    t.properties

let is_prop_id s =
  let n = String.length s in
  n >= 1 && n <= 255
  && String.for_all
       (fun c ->
         (c >= 'A' && c <= 'Z')
         || (c >= 'a' && c <= 'z')
         || (c >= '0' && c <= '9')
         || c = '-' || c = '_')
       s

(* RFC 6350 Section 5.3: pref-param = "PREF=" (1*2DIGIT / "100"), and the
   value is between 1 and 100. *)
let is_pref s =
  let n = String.length s in
  String.for_all (fun c -> c >= '0' && c <= '9') s
  && ((n >= 1 && n <= 2 && int_of_string s >= 1) || String.equal s "100")

(* RFC 6350 Section 5.5: pid-value = 1*DIGIT ["." 1*DIGIT], and PID must not
   appear on a property a card holds at most once. *)
let is_pid s =
  let digits d = d <> "" && String.for_all (fun c -> c >= '0' && c <= '9') d in
  match String.split_on_char '.' s with
  | [ a ] -> digits a
  | [ a; b ] -> digits a && digits b
  | _ -> false

let validate_property p =
  let name = Property.name p in
  let* () =
    check
      (not (List.mem name [ "BEGIN"; "END"; "VERSION" ]))
      "%s: a property may not be named %s" name name
  in
  let* () =
    match Property.find_first p "PREF" with
    | None -> Ok ()
    | Some s ->
        check (is_pref s) "%s: PREF %S is not one or two digits or 100" name s
  in
  let* () =
    match Property.pids p with
    | [] -> Ok ()
    | pids ->
        let* () =
          check (List.for_all is_pid pids)
            "%s: a PID is digits with an optional dot and digits" name
        in
        check
          (match Registry.cardinality name with
          | Registry.One | Registry.At_most_one -> false
          | _ -> true)
          "%s: PID may not appear on a property a card holds at most once" name
  in
  match Property.prop_id p with
  | None -> Ok ()
  | Some id ->
      check (is_prop_id id)
        "%s: PROP-ID %S is not 1 to 255 alphanumerics, hyphens and underscores"
        name id

(* RFC 6350 Section 5.4: instances sharing an ALTID count as one toward the
   cardinality. *)
let count t name =
  let ps = find_all t name in
  let without = List.filter (fun p -> Property.altid p = None) ps in
  let altids =
    List.sort_uniq String.compare (List.filter_map Property.altid ps)
  in
  List.length without + List.length altids

let validate t =
  let* () =
    check (String.equal t.version "4.0") "VERSION: %S is not 4.0" t.version
  in
  let rec each = function
    | [] -> Ok ()
    | p :: ps ->
        let* () = validate_property p in
        each ps
  in
  let* () = each t.properties in
  let rec cardinality = function
    | [] -> Ok ()
    | (e : Registry.entry) :: es ->
        let* () =
          match e.cardinality with
          | One_or_more ->
              check
                (count t e.name >= 1)
                "%s: at least one instance is required" e.name
          | At_most_one ->
              check
                (count t e.name <= 1)
                "%s: at most one instance may be present" e.name
          | One | Many -> Ok ()
        in
        cardinality es
  in
  let* () =
    cardinality
      (List.filter
         (fun (e : Registry.entry) ->
           not (List.mem e.name [ "BEGIN"; "END"; "VERSION" ]))
         Registry.all)
  in
  let kind =
    match find t "KIND" with
    | Some p -> String.lowercase_ascii (Property.text p)
    | None -> "individual"
  in
  let* () =
    check
      (find_all t "MEMBER" = [] || String.equal kind "group")
      "MEMBER: is present but KIND is %S, not \"group\"" kind
  in
  Ok t
