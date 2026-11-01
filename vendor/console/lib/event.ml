(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type kind = [ `Start | `Progress | `Label | `Output | `End | `Other of string ]
type count_unit = [ `Items | `Bytes | `Other of string ]
type level = [ `App | `Error | `Warning | `Info | `Debug | `Other of string ]
type status = [ `Succeeded | `Failed | `Cancelled | `Other of string ]
type task = int * string list

type t = {
  time : float;
  kind : kind;
  id : int option;
  task : string list option;
  parent : int option;
  count : int64 option;
  total : int64 option;
  unit : count_unit option;
  status : status option;
  elapsed : float option;
  text : string option;
  message : string option;
  level : level option;
}

(* Every number is written with 15 significant digits, the most that a
   float holds exactly: then a time rounded to the millisecond is written
   as short as it can be, 1.5 or 0, and an integer exactly below 10^15.
   An integer from 10^15 is a string of decimal digits, as jsont writes an
   integer beyond 2^53-1; both forms decode. *)
let number_format : Jsont.number_format = "%.15g"

let natural_error s =
  Jsont.Error.msgf Jsont.Meta.none "%s is not a natural number" s

let natural ~to_int64 ~of_int64 =
  let of_float f =
    if Float.is_integer f && f >= 0. && f < 0x1p63 then
      match of_int64 (Int64.of_float f) with
      | Some v -> v
      | None -> natural_error (Printf.sprintf "%.17g" f)
    else natural_error (Printf.sprintf "%.17g" f)
  and of_digits s =
    match Int64.of_string_opt s with
    | Some n when String.for_all Char.Ascii.is_digit s -> (
        match of_int64 n with Some v -> v | None -> natural_error s)
    | _ -> natural_error s
  in
  let number =
    Jsont.map Jsont.number ~dec:of_float ~enc:(fun v ->
        Int64.to_float (to_int64 v))
  and digits =
    Jsont.map Jsont.string ~dec:of_digits ~enc:(fun v ->
        Int64.to_string (to_int64 v))
  in
  let exact v = Int64.compare (to_int64 v) 1_000_000_000_000_000L < 0 in
  Jsont.any ~kind:"natural number" ~dec_number:number ~dec_string:digits
    ~enc:(fun v -> if exact v then number else digits)
    ()

let int =
  natural ~to_int64:Int64.of_int ~of_int64:(fun n ->
      if Int64.compare n (Int64.of_int max_int) <= 0 then Some (Int64.to_int n)
      else None)

let int64 = natural ~to_int64:Fun.id ~of_int64:Option.some

(* Seconds rounded to the millisecond in [-10^12, 10^12]: at most 15
   significant digits, so that the number written reads back as it was. *)
let max_seconds = 1e12

let to_seconds x =
  if not (Float.is_finite x) then 0.
  else
    let x = Float.min max_seconds (Float.max (-.max_seconds) x) in
    (Float.round (x *. 1000.) /. 1000.) +. 0.

let seconds = Jsont.map Jsont.number ~dec:to_seconds ~enc:to_seconds

(* A C1 control, U+0080 to U+009F, acts as an escape code on a terminal
   that reads 8-bit controls; jsont escapes C0 controls and DEL. *)
let is_c1 u =
  let c = Uchar.to_int u in
  c >= 0x80 && c <= 0x9f

(* [s] as valid UTF-8 of characters that [keep]: each byte that is not part
   of a character and each character [keep] refuses is U+FFFD. *)
let replaced keep s =
  let rec is_clean i =
    i >= String.length s
    ||
    let d = String.get_utf_8_uchar s i in
    Uchar.utf_decode_is_valid d
    && keep (Uchar.utf_decode_uchar d)
    && is_clean (i + Uchar.utf_decode_length d)
  in
  if is_clean 0 then s
  else
    let buf = Buffer.create (String.length s + 8) in
    let rec go i =
      if i < String.length s then (
        let d = String.get_utf_8_uchar s i in
        let u = Uchar.utf_decode_uchar d in
        Buffer.add_utf_8_uchar buf
          (if Uchar.utf_decode_is_valid d && keep u then u else Uchar.rep);
        go (i + Uchar.utf_decode_length d))
    in
    go 0;
    Buffer.contents buf

let clean = replaced (fun u -> not (is_c1 u))
let utf_8 = Jsont.map Jsont.string ~dec:Fun.id ~enc:clean

(* An open enumeration: the known values, then any other string. *)
type 'a enum = {
  of_string : string -> 'a;
  to_string : 'a -> string;
  other : 'a -> 'a;
}

let open_enum known =
  let of_string s =
    match List.assoc_opt s known with Some v -> v | None -> `Other s
  in
  let to_string v =
    match v with
    | `Other s -> s
    | v -> fst (List.find (fun (_, v') -> v' = v) known)
  in
  let other = function
    | `Other s -> (
        match List.assoc_opt s known with
        | Some v -> v
        | None -> `Other (clean s))
    | v -> v
  in
  { of_string; to_string; other }

let kinds () : kind enum =
  open_enum
    [
      ("start", `Start);
      ("progress", `Progress);
      ("label", `Label);
      ("output", `Output);
      ("end", `End);
    ]

let count_units () : count_unit enum =
  open_enum [ ("items", `Items); ("bytes", `Bytes) ]

let levels () : level enum =
  open_enum
    [
      ("app", `App);
      ("error", `Error);
      ("warning", `Warning);
      ("info", `Info);
      ("debug", `Debug);
    ]

let statuses () : status enum =
  open_enum
    [
      ("succeeded", `Succeeded); ("failed", `Failed); ("cancelled", `Cancelled);
    ]

let natural what n =
  if n < 0 then
    Printf.ksprintf invalid_arg "Console.Task.Event: negative %s" what

let natural64 what n =
  if Int64.compare n 0L < 0 then
    Printf.ksprintf invalid_arg "Console.Task.Event: negative %s" what

let kept s = Task_text.capped (clean s)

let event ?task ~time kind =
  Option.iter (fun (id, _) -> natural "id" id) task;
  {
    time = to_seconds time;
    kind;
    id = Option.map fst task;
    task = Option.map (fun (_, path) -> List.map kept path) task;
    parent = None;
    count = None;
    total = None;
    unit = None;
    status = None;
    elapsed = None;
    text = None;
    message = None;
    level = None;
  }

let start ?parent ~time task =
  Option.iter (natural "parent") parent;
  { (event ~task ~time `Start) with parent }

let progress ~time ~count ~total ~unit task =
  natural64 "count" count;
  natural64 "total" total;
  {
    (event ~task ~time `Progress) with
    count = Some count;
    total = Some total;
    unit = Some ((count_units ()).other unit);
  }

let label ~time task text =
  { (event ~task ~time `Label) with text = Some (kept text) }

let output ?task ?level ~time text =
  {
    (event ?task ~time `Output) with
    text = Some (clean text);
    level = Option.map (levels ()).other level;
  }

let ended status ?task ?message ~time ~elapsed () =
  {
    (event ?task ~time `End) with
    status = Some ((statuses ()).other status);
    elapsed = Some (to_seconds elapsed);
    message = Option.map kept message;
  }

let succeeded = ended `Succeeded
let failed = ended `Failed
let cancelled = ended `Cancelled

let other ?task ~time name =
  match (kinds ()).other (`Other name) with
  | `Other _ as kind -> event ?task ~time kind
  | _ ->
      Printf.ksprintf invalid_arg "Console.Task.Event.other: %S is listed" name

let time e = e.time
let kind e = e.kind

let task e =
  match (e.id, e.task) with Some id, Some t -> Some (id, t) | _ -> None

let parent e = e.parent
let count e = e.count
let total e = e.total
let unit e = e.unit
let status e = e.status
let elapsed e = e.elapsed
let text e = e.text
let message e = e.message
let level e = e.level

(* A member of an event of another name may have another type: it reads
   as [Mistyped], which an event of a listed name refuses. *)
type 'a member = Typed of 'a | Mistyped

let member c =
  let dec =
    Jsont.map Jsont.json ~dec:(fun j ->
        match Jsont.Json.decode c j with Ok v -> Typed v | Error _ -> Mistyped)
  and enc =
    Jsont.map c ~enc:(function
      | Typed v -> v
      | Mistyped -> invalid_arg "Console.Task.Event: mistyped member")
  in
  Jsont.any ~dec_null:dec ~dec_bool:dec ~dec_number:dec ~dec_string:dec
    ~dec_array:dec ~dec_object:dec
    ~enc:(fun _ -> enc)
    ()

(* The object as it is written: every member, in the order of the line,
   each absent when the event has none. *)
type members = {
  time : float;
  event : string;
  id : int member option;
  task : string list member option;
  parent : int member option;
  count : int64 member option;
  total : int64 member option;
  unit : string member option;
  status : string member option;
  elapsed : float member option;
  text : string member option;
  message : string member option;
  level : string member option;
}

let typed v = Option.map (fun v -> Typed v) v

let to_members (e : t) =
  let status = Option.map (statuses ()).to_string e.status in
  {
    time = e.time;
    event = (kinds ()).to_string e.kind;
    id = typed e.id;
    task = typed e.task;
    parent = typed e.parent;
    count = typed e.count;
    total = typed e.total;
    unit = typed (Option.map (count_units ()).to_string e.unit);
    status = typed status;
    elapsed = typed e.elapsed;
    text = typed e.text;
    message = typed e.message;
    level = typed (Option.map (levels ()).to_string e.level);
  }

let of_members (m : members) =
  let fail fmt = Jsont.Error.msgf Jsont.Meta.none fmt in
  let opt name = function
    | None -> None
    | Some (Typed v) -> Some v
    | Some Mistyped -> fail "%s event with a %s of another type" m.event name
  in
  let get name v =
    match opt name v with
    | Some v -> v
    | None -> fail "%s event without %s" m.event name
  in
  let time = m.time in
  let task () = (get "id" m.id, get "task" m.task) in
  let some_task what =
    match (opt "id" m.id, opt "task" m.task) with
    | Some id, Some task -> Some (id, task)
    | None, (None | Some []) -> None
    | Some _, None -> fail "%s event with an id and no task" what
    | None, Some _ -> fail "%s event with a task and no id" what
  in
  let parent () = opt "parent" m.parent in
  match (kinds ()).of_string m.event with
  | `Start -> start ?parent:(parent ()) ~time (task ())
  | `Progress ->
      let count = get "count" m.count and total = get "total" m.total in
      let unit = (count_units ()).of_string (get "unit" m.unit) in
      progress ~time ~count ~total ~unit (task ())
  | `Label -> label ~time (task ()) (get "text" m.text)
  | `Output ->
      let level = Option.map (levels ()).of_string (opt "level" m.level) in
      output ?task:(some_task "output") ?level ~time (get "text" m.text)
  | `End ->
      let status = (statuses ()).of_string (get "status" m.status) in
      ended status ?task:(some_task "end") ?message:(opt "message" m.message)
        ~time ~elapsed:(get "elapsed" m.elapsed) ()
  | `Other name ->
      let known = function Some (Typed v) -> Some v | _ -> None in
      let task =
        match (known m.id, known m.task) with
        | Some id, Some t -> Some (id, t)
        | _ -> None
      in
      other ?task ~time name

let jsont =
  let open Jsont.Object in
  map ~kind:"console progress event"
    (fun
      time
      event
      id
      task
      parent
      count
      total
      unit
      status
      elapsed
      text
      message
      level
    ->
      {
        time;
        event;
        id;
        task;
        parent;
        count;
        total;
        unit;
        status;
        elapsed;
        text;
        message;
        level;
      })
  |> mem "time" seconds ~enc:(fun (m : members) -> m.time)
  |> mem "event" utf_8 ~enc:(fun m -> m.event)
  |> opt_mem "id" (member int) ~enc:(fun m -> m.id)
  |> opt_mem "task" (member (Jsont.list utf_8)) ~enc:(fun m -> m.task)
  |> opt_mem "parent" (member int) ~enc:(fun m -> m.parent)
  |> opt_mem "count" (member int64) ~enc:(fun m -> m.count)
  |> opt_mem "total" (member int64) ~enc:(fun m -> m.total)
  |> opt_mem "unit" (member utf_8) ~enc:(fun m -> m.unit)
  |> opt_mem "status" (member utf_8) ~enc:(fun m -> m.status)
  |> opt_mem "elapsed" (member seconds) ~enc:(fun m -> m.elapsed)
  |> opt_mem "text" (member utf_8) ~enc:(fun m -> m.text)
  |> opt_mem "message" (member utf_8) ~enc:(fun m -> m.message)
  |> opt_mem "level" (member utf_8) ~enc:(fun m -> m.level)
  |> skip_unknown |> finish
  |> Jsont.map ~kind:"console progress event" ~dec:of_members ~enc:to_members

let to_string e =
  match Jsont_bytesrw.encode_string ~number_format jsont e with
  | Ok s -> s
  | Error e -> invalid_arg e

let of_string s =
  match Jsont_bytesrw.decode_string jsont s with
  | Ok e -> Ok e
  | Error e -> Error (`Msg (Console_text.strip e))

let pp ppf e = Format.pp_print_string ppf (to_string e)
