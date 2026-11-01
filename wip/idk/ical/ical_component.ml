(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind

type t = {
  name : string;
  properties : Ical_property.t list;
  components : t list;
}

let v ?(properties = []) ?(components = []) name =
  { name = String.uppercase_ascii name; properties; components }

let find c name =
  let name = String.uppercase_ascii name in
  List.find_opt (fun p -> String.equal (Ical_property.name p) name) c.properties

let find_all c name =
  let name = String.uppercase_ascii name in
  List.filter (fun p -> String.equal (Ical_property.name p) name) c.properties

let text c name = Option.map Ical_property.text (find c name)
let uid c = text c "UID"

let children c name =
  let name = String.uppercase_ascii name in
  List.filter (fun x -> String.equal x.name name) c.components

let add c p = { c with properties = c.properties @ [ p ] }

let replace c p =
  let name = Ical_property.name p in
  {
    c with
    properties =
      List.filter
        (fun x -> not (String.equal (Ical_property.name x) name))
        c.properties
      @ [ p ];
  }

let remove c name =
  let name = String.uppercase_ascii name in
  {
    c with
    properties =
      List.filter
        (fun x -> not (String.equal (Ical_property.name x) name))
        c.properties;
  }

let dtstart c =
  match find c "DTSTART" with
  | None -> Ok None
  | Some p -> Result.map Option.some (Ical_property.date_time p)

let next_day d =
  let dt = Ical_date.start_of_day d in
  let moved = Ical_duration.add dt { Ical_duration.zero with days = 1 } in
  moved.Ical_date.date

(* RFC 4791 Section 9.9: DTEND if present, else DUE, else DTSTART moved by
   DURATION, else the day after a DATE DTSTART, else DTSTART itself. *)
let dtend c =
  let some r = Result.map Option.some r in
  match find c "DTEND" with
  | Some p -> some (Ical_property.date_time p)
  | None -> (
      match find c "DUE" with
      | Some p -> some (Ical_property.date_time p)
      | None -> (
          match find c "DTSTART" with
          | None -> Ok None
          | Some start_p ->
              some
                (let* start = Ical_property.date_time start_p in
                 match find c "DURATION" with
                 | Some dp ->
                     let* d = Ical_property.duration dp in
                     let base =
                       match start with
                       | Ical_date.Date d0 -> Ical_date.start_of_day d0
                       | Ical_date.Date_time dt0 -> dt0
                     in
                     Ok (Ical_date.Date_time (Ical_duration.add base d))
                 | None -> (
                     match start with
                     | Ical_date.Date d0 -> Ok (Ical_date.Date (next_day d0))
                     | Ical_date.Date_time _ -> Ok start))))

let rec equal a b =
  String.equal a.name b.name
  && List.equal Ical_property.equal a.properties b.properties
  && List.equal equal a.components b.components

let rec pp ppf c =
  Format.pp_print_string ppf ("BEGIN:" ^ c.name ^ "\r\n");
  List.iter
    (fun p -> Format.pp_print_string ppf (Vcard.Property.to_string p ^ "\r\n"))
    c.properties;
  List.iter (pp ppf) c.components;
  Format.pp_print_string ppf ("END:" ^ c.name ^ "\r\n")
