(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type test = [ `Anyof | `Allof ]
type match_type = [ `Equals | `Contains | `Starts_with | `Ends_with ]

type text_match = {
  text : string;
  match_type : match_type;
  collation : string option;
  negate : bool;
}

type param_filter = {
  param : string;
  param_test : [ `Defined | `Not_defined | `Match of text_match ];
}

type prop_filter = {
  prop : string;
  prop_condition :
    [ `Defined
    | `Not_defined
    | `Matches of test * text_match list * param_filter list ];
}

type t = { test : test; props : prop_filter list }

let text_match ?(match_type = `Contains) ?collation ?(negate = false) text =
  { text; match_type; collation; negate }

(* A prop-filter with neither a text-match nor a param-filter tests only that
   the property is there, RFC 6352 Section 10.5.1. *)
let prop_condition test matches params =
  match (matches, params) with
  | [], [] -> `Defined
  | _ -> `Matches (test, matches, params)

let prop ?(test = `Anyof) ?(params = []) name matches =
  { prop = name; prop_condition = prop_condition test matches params }

let prop_not_defined name = { prop = name; prop_condition = `Not_defined }

let param name m =
  {
    param = name;
    param_test = (match m with Some tm -> `Match tm | None -> `Defined);
  }

let param_not_defined name = { param = name; param_test = `Not_defined }
let v ?(test = `Anyof) props = { test; props }
let all = v []

let equal_text_match (a : text_match) (b : text_match) =
  String.equal a.text b.text
  && a.match_type = b.match_type
  && Option.equal String.equal a.collation b.collation
  && Bool.equal a.negate b.negate

let equal_param_filter (a : param_filter) (b : param_filter) =
  String.equal a.param b.param
  &&
  match (a.param_test, b.param_test) with
  | `Defined, `Defined | `Not_defined, `Not_defined -> true
  | `Match x, `Match y -> equal_text_match x y
  | (`Defined | `Not_defined | `Match _), _ -> false

let equal_prop_filter (a : prop_filter) (b : prop_filter) =
  String.equal a.prop b.prop
  &&
  match (a.prop_condition, b.prop_condition) with
  | `Defined, `Defined | `Not_defined, `Not_defined -> true
  | `Matches (t0, m0, p0), `Matches (t1, m1, p1) ->
      t0 = t1
      && List.equal equal_text_match m0 m1
      && List.equal equal_param_filter p0 p1
  | (`Defined | `Not_defined | `Matches _), _ -> false

let equal a b = a.test = b.test && List.equal equal_prop_filter a.props b.props
let cname local = Httpz_dav.carddav local
let attr_name = ("", "name")
let attr_test = ("", "test")
let attr_collation = ("", "collation")
let attr_negate = ("", "negate-condition")
let attr_match_type = ("", "match-type")
let ( let* ) = Result.bind

let rec all_ok = function
  | [] -> Ok []
  | x :: xs ->
      let* x = x in
      let* xs = all_ok xs in
      Ok (x :: xs)

let string_of_test = function `Anyof -> "anyof" | `Allof -> "allof"

let test_of_string = function
  | "anyof" -> Ok `Anyof
  | "allof" -> Ok `Allof
  | s -> Error (Printf.sprintf "%S is not a filter test" s)

let string_of_match_type = function
  | `Equals -> "equals"
  | `Contains -> "contains"
  | `Starts_with -> "starts-with"
  | `Ends_with -> "ends-with"

let match_type_of_string = function
  | "equals" -> Ok `Equals
  | "contains" -> Ok `Contains
  | "starts-with" -> Ok `Starts_with
  | "ends-with" -> Ok `Ends_with
  | s -> Error (Printf.sprintf "%S is not a match type" s)

let bool_of_yes_no = function
  | None -> Ok false
  | Some "yes" -> Ok true
  | Some "no" -> Ok false
  | Some s -> Error (Printf.sprintf "%S is not yes or no" s)

let test_of_xml ~default x =
  match Httpz_dav.attr attr_test x with
  | None -> Ok default
  | Some s -> test_of_string s

let text_match_to_xml m =
  let attrs =
    (match m.collation with
      | None | Some "i;unicode-casemap" -> []
      | Some c -> [ (attr_collation, c) ])
    @ (if m.negate then [ (attr_negate, "yes") ] else [])
    @
    match m.match_type with
    | `Contains -> []
    | mt -> [ (attr_match_type, string_of_match_type mt) ]
  in
  Httpz_dav.element ~attrs (cname "text-match") [ Httpz_dav.Text m.text ]

let text_match_of_xml x =
  let* negate = bool_of_yes_no (Httpz_dav.attr attr_negate x) in
  let* match_type =
    match Httpz_dav.attr attr_match_type x with
    | None -> Ok `Contains
    | Some s -> match_type_of_string s
  in
  Ok
    {
      text = Httpz_dav.content x;
      match_type;
      collation = Httpz_dav.attr attr_collation x;
      negate;
    }

let param_filter_to_xml pf =
  let child =
    match pf.param_test with
    | `Defined -> []
    | `Not_defined -> [ Httpz_dav.empty (cname "is-not-defined") ]
    | `Match tm -> [ text_match_to_xml tm ]
  in
  Httpz_dav.el ~attrs:[ (attr_name, pf.param) ] (cname "param-filter") child

let param_filter_of_xml x =
  match Httpz_dav.attr attr_name x with
  | None -> Error "a param-filter has no name"
  | Some param -> (
      match Httpz_dav.find (cname "is-not-defined") x with
      | Some _ -> Ok { param; param_test = `Not_defined }
      | None -> (
          match Httpz_dav.find (cname "text-match") x with
          | None -> Ok { param; param_test = `Defined }
          | Some tm ->
              let* tm = text_match_of_xml tm in
              Ok { param; param_test = `Match tm }))

let prop_filter_to_xml pf =
  let test =
    match pf.prop_condition with
    | `Matches (test, _, _) when test <> `Anyof ->
        [ (attr_test, string_of_test test) ]
    | `Defined | `Not_defined | `Matches _ -> []
  in
  let attrs = (attr_name, pf.prop) :: test in
  let children =
    match pf.prop_condition with
    | `Defined -> []
    | `Not_defined -> [ Httpz_dav.empty (cname "is-not-defined") ]
    | `Matches (_, matches, params) ->
        List.map text_match_to_xml matches @ List.map param_filter_to_xml params
  in
  Httpz_dav.el ~attrs (cname "prop-filter") children

let prop_filter_of_xml x =
  match Httpz_dav.attr attr_name x with
  | None -> Error "a prop-filter has no name"
  | Some prop -> (
      let* test = test_of_xml ~default:`Anyof x in
      match Httpz_dav.find (cname "is-not-defined") x with
      | Some _ -> Ok { prop; prop_condition = `Not_defined }
      | None ->
          let* matches =
            all_ok
              (List.map text_match_of_xml
                 (Httpz_dav.children (cname "text-match") x))
          in
          let* params =
            all_ok
              (List.map param_filter_of_xml
                 (Httpz_dav.children (cname "param-filter") x))
          in
          Ok { prop; prop_condition = prop_condition test matches params })

let to_xml t =
  let attrs =
    if t.test = `Anyof then [] else [ (attr_test, string_of_test t.test) ]
  in
  Httpz_dav.el ~attrs (cname "filter") (List.map prop_filter_to_xml t.props)

let of_xml x =
  if not (Httpz_dav.is (cname "filter") x) then
    Error "the document is not a CARDDAV:filter"
  else
    let* test = test_of_xml ~default:`Anyof x in
    let* props =
      all_ok
        (List.map prop_filter_of_xml
           (Httpz_dav.children (cname "prop-filter") x))
    in
    Ok { test; props }

(* RFC 6352 Section 10.5.1: a "name" without a group prefix matches a
   property with any group prefix or none, and a "name" with a group
   prefix matches only that group's property of that name. *)
let split_group name =
  match String.index_opt name '.' with
  | None -> (None, name)
  | Some i ->
      ( Some (String.sub name 0 i),
        String.sub name (i + 1) (String.length name - i - 1) )

let matching_props card name =
  let group, pname = split_group name in
  let pname = String.uppercase_ascii pname in
  List.filter
    (fun p ->
      Vcard.Property.name p = pname
      &&
      match group with
      | None -> true
      | Some g -> (
          match Vcard.Property.group p with
          | Some pg -> String.uppercase_ascii pg = String.uppercase_ascii g
          | None -> false))
    (Vcard.properties card)

let normalize collation s =
  match collation with
  | Some "i;octet" -> s
  | Some "i;ascii-casemap" | Some "i;unicode-casemap" | None ->
      String.lowercase_ascii s
  | Some _ -> s

let is_substring ~needle haystack =
  let nl = String.length needle and hl = String.length haystack in
  let rec at i j = j >= nl || (haystack.[i + j] = needle.[j] && at i (j + 1)) in
  let rec go i = i + nl <= hl && (at i 0 || go (i + 1)) in
  nl = 0 || go 0

let starts_with ~prefix s =
  let pl = String.length prefix in
  String.length s >= pl && String.sub s 0 pl = prefix

let ends_with ~suffix s =
  let sl = String.length suffix and l = String.length s in
  l >= sl && String.sub s (l - sl) sl = suffix

let text_matches tm value =
  let a = normalize tm.collation value and b = normalize tm.collation tm.text in
  let m =
    match tm.match_type with
    | `Equals -> a = b
    | `Contains -> is_substring ~needle:b a
    | `Starts_with -> starts_with ~prefix:b a
    | `Ends_with -> ends_with ~suffix:b a
  in
  if tm.negate then not m else m

(* RFC 6350 Section 5 writes the TYPE, PID and SORT-AS lists as one quoted
   value, as in TYPE="voice,home", so a text-match tests each value of the
   list rather than the text they share. *)
let param_values p name =
  match Vcard.Property.find_values p name with
  | None -> None
  | Some values -> (
      match String.uppercase_ascii name with
      | "TYPE" | "PID" | "SORT-AS" ->
          Some (List.concat_map (String.split_on_char ',') values)
      | _ -> Some values)

let param_condition_matches p pf =
  match pf.param_test with
  | `Defined -> Vcard.Property.find_values p pf.param <> None
  | `Not_defined -> Vcard.Property.find_values p pf.param = None
  | `Match tm -> (
      match param_values p pf.param with
      | None -> false
      | Some values ->
          (* Section 10.5.4 negates the condition, so a parameter holding
             several values matches a negated text-match only when none of
             them matches. *)
          let any =
            List.exists (text_matches { tm with negate = false }) values
          in
          if tm.negate then not any else any)

let combine test f l =
  match test with `Anyof -> List.exists f l | `Allof -> List.for_all f l

let prop_filter_matches card pf =
  let props = matching_props card pf.prop in
  match pf.prop_condition with
  | `Defined -> props <> []
  | `Not_defined -> props = []
  | `Matches (test, matches, params) ->
      let text p tm = text_matches tm (Vcard.Property.text p) in
      List.exists
        (fun p ->
          match test with
          | `Anyof ->
              List.exists (text p) matches
              || List.exists (param_condition_matches p) params
          | `Allof ->
              List.for_all (text p) matches
              && List.for_all (param_condition_matches p) params)
        props

let matches t card =
  match t.props with
  | [] -> true
  | props -> combine t.test (prop_filter_matches card) props
