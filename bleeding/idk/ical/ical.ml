(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Value_type = Ical_value_type
module Date = Ical_date
module Duration = Ical_duration
module Period = Ical_period
module Recur = Ical_recur
module Property = Ical_property
module Registry = Ical_registry
module Component = Ical_component
module Param = Vcard.Param

let with_components (t : Component.t) components = { t with components }
let error fmt = Format.kasprintf (fun s -> Error s) fmt
let ( let* ) = Result.bind

type t = Component.t

let v ?(prodid = "-//idk//ical//EN") ?(properties = []) components =
  let properties =
    Property.v "VERSION" "2.0" :: Property.v "PRODID" prodid :: properties
  in
  Component.v ~properties ~components "VCALENDAR"

let properties (t : t) = t.Component.properties
let components (t : t) = t.Component.components
let prodid t = Option.map Property.text (Component.find t "PRODID")
let version t = Option.map Property.text (Component.find t "VERSION")
let events t = Component.children t "VEVENT"
let todos t = Component.children t "VTODO"
let journals t = Component.children t "VJOURNAL"
let free_busy t = Component.children t "VFREEBUSY"
let timezones t = Component.children t "VTIMEZONE"

let uid t =
  let others =
    List.filter
      (fun c -> not (String.equal c.Component.name "VTIMEZONE"))
      (components t)
  in
  match others with
  | [] -> None
  | first :: rest -> (
      match Component.uid first with
      | None -> None
      | Some u ->
          if
            List.for_all
              (fun c ->
                match Component.uid c with
                | Some u' -> String.equal u u'
                | None -> false)
              rest
          then Some u
          else None)

let find_timezone t tzid =
  List.find_opt
    (fun c ->
      match Component.find c "TZID" with
      | Some p -> String.equal (Property.text p) tzid
      | None -> false)
    (timezones t)

let equal = Component.equal

let required_ok c =
  let rec go = function
    | [] -> Ok ()
    | name :: rest -> (
        match Component.find c name with
        | None -> error "%s: %s is required" c.Component.name name
        | Some _ -> go rest)
  in
  go (Registry.required c.Component.name)

let cardinality_ok c =
  let names =
    List.sort_uniq String.compare
      (List.map Property.name c.Component.properties)
  in
  let rec go = function
    | [] -> Ok ()
    | name :: rest -> (
        match Registry.cardinality ~component:c.Component.name name with
        | None | Some Registry.Many -> go rest
        | Some (Registry.One | Registry.At_most_one) ->
            if List.length (Component.find_all c name) > 1 then
              error "%s: %s may appear at most once" c.Component.name name
            else go rest)
  in
  go names

(* A typed read that fails names the component and property it came from, as
   every other check in [validate] does. *)
let in_prop c name r =
  Result.map_error
    (fun m -> Printf.sprintf "%s: %s: %s" c.Component.name name m)
    r

let date_time_ok c name =
  match Component.find c name with
  | None -> Ok ()
  | Some p ->
      in_prop c name
        (Result.map (fun (_ : Ical_date.t) -> ()) (Property.date_time p))

let duration_ok c name =
  match Component.find c name with
  | None -> Ok ()
  | Some p ->
      in_prop c name
        (Result.map (fun (_ : Ical_duration.t) -> ()) (Property.duration p))

let utc_offset_ok c name =
  match Component.find c name with
  | None -> Ok ()
  | Some p ->
      in_prop c name (Result.map (fun (_ : int) -> ()) (Property.utc_offset p))

let date_times_ok c name =
  let rec go = function
    | [] -> Ok ()
    | p :: rest ->
        let* () =
          in_prop c name
            (if Value_type.equal (Property.value_type p) Value_type.Period then
               Result.map
                 (fun (_ : Ical_period.t list) -> ())
                 (Property.periods p)
             else
               Result.map
                 (fun (_ : Ical_date.t list) -> ())
                 (Property.date_times p))
        in
        go rest
  in
  go (Component.find_all c name)

let freebusy_ok c =
  let rec go = function
    | [] -> Ok ()
    | p :: rest ->
        let* () =
          in_prop c "FREEBUSY"
            (Result.map
               (fun (_ : Ical_period.t list) -> ())
               (Property.periods p))
        in
        go rest
  in
  go (Component.find_all c "FREEBUSY")

let rrule_ok c =
  match Component.find c "RRULE" with
  | None -> Ok ()
  | Some p ->
      in_prop c "RRULE"
        (let* r = Property.recur p in
         Result.map (fun (_ : Ical_recur.t) -> ()) (Recur.validate r))

let trigger_ok c =
  match Component.find c "TRIGGER" with
  | None -> Ok ()
  | Some p ->
      in_prop c "TRIGGER"
        (if Value_type.equal (Property.value_type p) Value_type.Date_time then
           Result.map (fun (_ : Ical_date.t) -> ()) (Property.date_time p)
         else Result.map (fun (_ : Ical_duration.t) -> ()) (Property.duration p))

let typed_values_ok c =
  let* () = date_time_ok c "DTSTART" in
  let* () = date_time_ok c "DTEND" in
  let* () = date_time_ok c "DUE" in
  let* () = date_time_ok c "DTSTAMP" in
  let* () = date_time_ok c "CREATED" in
  let* () = date_time_ok c "LAST-MODIFIED" in
  let* () = date_time_ok c "COMPLETED" in
  let* () = date_time_ok c "RECURRENCE-ID" in
  let* () = date_times_ok c "EXDATE" in
  let* () = date_times_ok c "RDATE" in
  let* () = duration_ok c "DURATION" in
  let* () = rrule_ok c in
  let* () = utc_offset_ok c "TZOFFSETFROM" in
  let* () = utc_offset_ok c "TZOFFSETTO" in
  let* () = freebusy_ok c in
  trigger_ok c

let extra_ok c =
  match c.Component.name with
  | "VEVENT" ->
      if
        Option.is_some (Component.find c "DTEND")
        && Option.is_some (Component.find c "DURATION")
      then error "VEVENT: DTEND and DURATION may not both be present"
      else Ok ()
  | "VTODO" ->
      if
        Option.is_some (Component.find c "DUE")
        && Option.is_some (Component.find c "DURATION")
      then error "VTODO: DUE and DURATION may not both be present"
      else Ok ()
  | "VTIMEZONE" ->
      if
        List.is_empty (Component.children c "STANDARD")
        && List.is_empty (Component.children c "DAYLIGHT")
      then error "VTIMEZONE: at least one STANDARD or DAYLIGHT is required"
      else Ok ()
  | _ -> Ok ()

let rec validate_component c =
  let* () = required_ok c in
  let* () = cardinality_ok c in
  let* () = typed_values_ok c in
  let* () = extra_ok c in
  let rec each = function
    | [] -> Ok ()
    | child :: rest ->
        let* () = validate_component child in
        each rest
  in
  each c.Component.components

let validate t =
  let* () =
    match version t with
    | Some "2.0" -> Ok ()
    | _ -> error "VERSION must be 2.0"
  in
  let* () =
    if List.is_empty (components t) then
      error "at least one component is required"
    else Ok ()
  in
  let* () = validate_component t in
  Ok t

let of_string s =
  let lines = Vcard.unfold s in
  let rec go top stack i = function
    | [] -> (
        match stack with
        | [] -> Ok (List.rev top)
        | (name, _, _, line) :: _ ->
            error "line %d: %s has no matching END:%s" line name name)
    | line :: rest when String.trim line = "" -> go top stack (i + 1) rest
    | line :: rest -> (
        let* p =
          match Vcard.Property.of_string line with
          | Ok p -> Ok p
          | Error msg -> error "line %d: %s" i msg
        in
        let* () =
          match Vcard.Property.group p with
          | None -> Ok ()
          | Some g ->
              error "line %d: %s.%s takes no group" i g (Vcard.Property.name p)
        in
        let pname = Vcard.Property.name p in
        if String.equal pname "BEGIN" then
          let cname = String.uppercase_ascii (Vcard.Property.value p) in
          match stack with
          | [] when not (String.equal cname "VCALENDAR") ->
              error "line %d: expected BEGIN:VCALENDAR" i
          | _ -> go top ((cname, [], [], i) :: stack) (i + 1) rest
        else if String.equal pname "END" then
          let cname = String.uppercase_ascii (Vcard.Property.value p) in
          match stack with
          | [] -> error "line %d: END:%s without a matching BEGIN" i cname
          | (open_name, props, comps, _) :: rest_stack -> (
              if not (String.equal open_name cname) then
                error "line %d: END:%s does not match BEGIN:%s" i cname
                  open_name
              else
                let comp =
                  Component.v ~properties:(List.rev props)
                    ~components:(List.rev comps) open_name
                in
                match rest_stack with
                | [] -> go (comp :: top) [] (i + 1) rest
                | (pname2, pprops, pcomps, pline) :: outer ->
                    go top
                      ((pname2, pprops, comp :: pcomps, pline) :: outer)
                      (i + 1) rest)
        else
          match stack with
          | [] ->
              error "line %d: a property may not appear outside a component" i
          | (name, props, comps, line0) :: rest_stack ->
              go top
                ((name, p :: props, comps, line0) :: rest_stack)
                (i + 1) rest)
  in
  go [] [] 1 lines

let one_of_string s =
  match of_string s with
  | Ok [ t ] -> Ok t
  | Ok [] -> error "no VCALENDAR"
  | Ok ts -> error "%d VCALENDARs where one was expected" (List.length ts)
  | Error _ as e -> e

let to_string t =
  let b = Buffer.create 512 in
  let line s =
    Buffer.add_string b (Vcard.fold s);
    Buffer.add_string b "\r\n"
  in
  let rec go c =
    line ("BEGIN:" ^ c.Component.name);
    List.iter
      (fun p -> line (Vcard.Property.to_string p))
      c.Component.properties;
    List.iter go c.Component.components;
    line ("END:" ^ c.Component.name)
  in
  go t;
  Buffer.contents b

let pp ppf t = Format.pp_print_string ppf (to_string t)
