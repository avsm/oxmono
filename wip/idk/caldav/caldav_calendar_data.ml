(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Date = Ical.Date

let name = Httpz_dav.caldav "calendar-data"
let comp_elt = Httpz_dav.caldav "comp"
let allprop_name = Httpz_dav.caldav "allprop"
let allcomp_name = Httpz_dav.caldav "allcomp"
let prop_name = Httpz_dav.caldav "prop"
let expand_name = Httpz_dav.caldav "expand"
let limit_recurrence_set_name = Httpz_dav.caldav "limit-recurrence-set"
let limit_freebusy_set_name = Httpz_dav.caldav "limit-freebusy-set"
let attr_content_type = ("", "content-type")
let attr_version = ("", "version")
let attr_novalue = ("", "novalue")
let attr_name = ("", "name")
let attr_start = ("", "start")
let attr_end = ("", "end")
let ( let* ) = Result.bind

type comp = {
  comp_name : string;
  props : [ `All | `Props of (string * bool) list ];
  comps : [ `All | `Comps of comp list ];
}

type t = {
  content_type : string option;
  version : string option;
  comp : comp option;
  expand : Caldav_filter.time_range option;
  limit_recurrence_set : Caldav_filter.time_range option;
  limit_freebusy_set : Caldav_filter.time_range option;
}

let v ?content_type ?version ?comp ?expand ?limit_recurrence_set
    ?limit_freebusy_set () =
  (* RFC 4791 Section 9.6 admits one of CALDAV:expand and
     CALDAV:limit-recurrence-set, so a request cannot carry both. *)
  if expand <> None && limit_recurrence_set <> None then
    invalid_arg
      "Caldav_calendar_data.v: expand and limit_recurrence_set are exclusive";
  {
    content_type;
    version;
    comp;
    expand;
    limit_recurrence_set;
    limit_freebusy_set;
  }

let comp ?props ?comps name =
  {
    comp_name = name;
    props = (match props with None -> `All | Some ps -> `Props ps);
    comps = (match comps with None -> `All | Some cs -> `Comps cs);
  }

let rec all_ok = function
  | [] -> Ok []
  | x :: xs ->
      let* x = x in
      let* xs = all_ok xs in
      Ok (x :: xs)

let rec comp_to_xml c =
  let props_children =
    match c.props with
    | `All -> [ Httpz_dav.empty allprop_name ]
    | `Props ps ->
        List.map
          (fun (n, novalue) ->
            let attrs =
              (attr_name, n)
              :: (if novalue then [ (attr_novalue, "yes") ] else [])
            in
            Httpz_dav.el ~attrs prop_name [])
          ps
  in
  let comps_children =
    match c.comps with
    | `All -> [ Httpz_dav.empty allcomp_name ]
    | `Comps cs -> List.map comp_to_xml cs
  in
  Httpz_dav.el
    ~attrs:[ (attr_name, c.comp_name) ]
    comp_elt
    (props_children @ comps_children)

let prop_of_xml p =
  match Httpz_dav.attr attr_name p with
  | None -> Error "a prop has no name"
  | Some n ->
      Ok
        ( n,
          match Httpz_dav.attr attr_novalue p with
          | Some "yes" -> true
          | _ -> false )

let rec comp_of_xml x =
  match Httpz_dav.attr attr_name x with
  | None -> Error "a comp has no name"
  | Some comp_name ->
      let* props =
        match Httpz_dav.find allprop_name x with
        | Some _ -> Ok `All
        | None ->
            let* ps =
              all_ok (List.map prop_of_xml (Httpz_dav.children prop_name x))
            in
            Ok (`Props ps)
      in
      let* comps =
        match Httpz_dav.find allcomp_name x with
        | Some _ -> Ok `All
        | None ->
            let* cs =
              all_ok (List.map comp_of_xml (Httpz_dav.children comp_elt x))
            in
            Ok (`Comps cs)
      in
      Ok { comp_name; props; comps }

let attr_date_time n x =
  match Httpz_dav.attr n x with
  | None -> Ok None
  | Some s -> (
      match Date.date_time_of_string s with
      (* RFC 4791 Sections 9.6.5 and 9.9 require a date with UTC time. *)
      | Ok dt when not dt.Date.time.Date.utc ->
          Error (Printf.sprintf "%S is not a date-time in UTC" s)
      | Ok dt -> Ok (Some dt)
      | Error e -> Error e)

let range_attrs (tr : Caldav_filter.time_range) =
  (match tr.start with
    | None -> []
    | Some s -> [ (attr_start, Date.date_time_to_string s) ])
  @
  match tr.finish with
  | None -> []
  | Some f -> [ (attr_end, Date.date_time_to_string f) ]

let required_range tag x =
  let* start = attr_date_time attr_start x in
  let* finish = attr_date_time attr_end x in
  match (start, finish) with
  | Some s, Some f -> Ok Caldav_filter.{ start = Some s; finish = Some f }
  | _ -> Error (Printf.sprintf "a CALDAV:%s needs a start and an end" tag)

let equal_props a b =
  match (a, b) with
  | `All, `All -> true
  | `Props x, `Props y ->
      List.equal
        (fun (n0, v0) (n1, v1) -> String.equal n0 n1 && Bool.equal v0 v1)
        x y
  | (`All | `Props _), _ -> false

let rec equal_comp (a : comp) (b : comp) =
  String.equal a.comp_name b.comp_name
  && equal_props a.props b.props
  &&
  match (a.comps, b.comps) with
  | `All, `All -> true
  | `Comps x, `Comps y -> List.equal equal_comp x y
  | (`All | `Comps _), _ -> false

let equal (a : t) (b : t) =
  let range = Option.equal Caldav_filter.equal_time_range in
  Option.equal String.equal a.content_type b.content_type
  && Option.equal String.equal a.version b.version
  && Option.equal equal_comp a.comp b.comp
  && range a.expand b.expand
  && range a.limit_recurrence_set b.limit_recurrence_set
  && range a.limit_freebusy_set b.limit_freebusy_set

let to_xml t =
  let attrs =
    (match t.content_type with
      | None | Some "text/calendar" -> []
      | Some c -> [ (attr_content_type, c) ])
    @
    match t.version with
    | None | Some "2.0" -> []
    | Some v -> [ (attr_version, v) ]
  in
  let children =
    Option.to_list (Option.map comp_to_xml t.comp)
    @ (match t.expand with
      | Some tr -> [ Httpz_dav.el ~attrs:(range_attrs tr) expand_name [] ]
      | None -> (
          match t.limit_recurrence_set with
          | Some tr ->
              [
                Httpz_dav.el ~attrs:(range_attrs tr) limit_recurrence_set_name
                  [];
              ]
          | None -> []))
    @
    match t.limit_freebusy_set with
    | Some tr ->
        [ Httpz_dav.el ~attrs:(range_attrs tr) limit_freebusy_set_name [] ]
    | None -> []
  in
  Httpz_dav.el ~attrs name children

let of_xml x =
  if not (Httpz_dav.is name x) then
    Error "the document is not a CALDAV:calendar-data"
  else
    let content_type = Httpz_dav.attr attr_content_type x in
    let version = Httpz_dav.attr attr_version x in
    let* comp =
      match Httpz_dav.find comp_elt x with
      | None -> Ok None
      | Some c ->
          let* c = comp_of_xml c in
          Ok (Some c)
    in
    let* expand =
      match Httpz_dav.find expand_name x with
      | None -> Ok None
      | Some e ->
          let* tr = required_range "expand" e in
          Ok (Some tr)
    in
    let* limit_recurrence_set =
      match Httpz_dav.find limit_recurrence_set_name x with
      | None -> Ok None
      | Some e ->
          let* tr = required_range "limit-recurrence-set" e in
          Ok (Some tr)
    in
    let* limit_freebusy_set =
      match Httpz_dav.find limit_freebusy_set_name x with
      | None -> Ok None
      | Some e ->
          let* tr = required_range "limit-freebusy-set" e in
          Ok (Some tr)
    in
    Ok
      {
        content_type;
        version;
        comp;
        expand;
        limit_recurrence_set;
        limit_freebusy_set;
      }

(* RFC 4791 Section 9.6: an XML parser normalises CRLF to LF, so a bare line
   feed in a response's calendar-data stands for a carriage return and line
   feed. A server that emitted the carriage return anyway leaves the pair
   alone, so each line feed is judged on its own rather than the whole value on
   whether any pair survived. *)
let restore_crlf s =
  let b = Buffer.create (String.length s + 16) in
  String.iteri
    (fun i c ->
      if c = '\n' && not (i > 0 && s.[i - 1] = '\r') then
        Buffer.add_string b "\r\n"
      else Buffer.add_char b c)
    s;
  Buffer.contents b

let data p =
  if not (Httpz_dav.is name p) then None
  else Some (restore_crlf (Httpz_dav.content p))

let content_type_of p =
  ( Option.value ~default:"text/calendar" (Httpz_dav.attr attr_content_type p),
    Option.value ~default:"2.0" (Httpz_dav.attr attr_version p) )
