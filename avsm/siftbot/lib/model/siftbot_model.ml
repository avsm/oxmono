open Sexplib.Std

let sexp_of_result f g = function
  | Ok x -> Sexplib.Sexp.List [ Atom "Ok"; f x ]
  | Error x -> Sexplib.Sexp.List [ Atom "Error"; g x ]

let result_of_sexp f g = function
  | Sexplib.Sexp.List [ Atom "Ok"; x ] -> Ok (f x)
  | Sexplib.Sexp.List [ Atom "Error"; x ] -> Error (g x)
  | _ -> invalid_arg "Invalid result"

type source = { service : string; account : string; id : string }
[@@deriving sexp]

type dimension = Area | Triage | Delivery | Attention [@@deriving sexp]
type origin = Model | Rule | Admin | Server [@@deriving sexp]

type message = {
  source : source;
  thread : string;
  date : string;
  sender : string;
  recipients : string list;
  subject : string;
  original : string;
  text : string;
  headers : (string * string) list;
  message_ids : string list;
  reply_ids : string list;
  truncated : bool;
  outside_interval : bool;
  revision : string;
}
[@@deriving sexp]

type area = {
  id : string;
  name : string;
  definition : string;
  exclusions : string;
  parent : string option;
  status : string;
  revision : int;
}
[@@deriving sexp]

type assignment = {
  target : source;
  conversation : bool;
  dimension : dimension;
  value : string;
  origin : origin;
  version : string;
  source_revision : string;
  explanation : string;
  evidence : source list;
  score : float option;
}
[@@deriving sexp]

type snapshot = {
  revision : int;
  messages : message list;
  areas : area list;
  assignments : assignment list;
  jobs : string list;
  discussion : string;
}
[@@deriving sexp]

type edit =
  | Approve of string
  | Rename of string * string
  | Archive of string
  | Merge of string * string
  | Define of string * string * string
  | Assign of assignment
  | Undo
[@@deriving sexp]

type request =
  | Inspect of string
  | Discuss of string * source option
  | Edit of int * edit * string * string
[@@deriving sexp]

type response = (snapshot, string) result [@@deriving sexp]

let empty =
  {
    revision = 0;
    messages = [];
    areas = [];
    assignments = [];
    jobs = [];
    discussion = "";
  }

let key s = Sexplib.Sexp.to_string_mach (sexp_of_source s)

let dimension = function
  | Area -> "area"
  | Triage -> "triage"
  | Delivery -> "delivery"
  | Attention -> "attention"

let valid_value dim value =
  match dim with
  | Area -> value <> ""
  | Triage -> List.mem value [ "relevant"; "social"; "spam"; "uncertain" ]
  | Delivery ->
      List.mem value
        [ "direct"; "mailing-list"; "broadcast"; "automated"; "uncertain" ]
  | Attention ->
      List.mem value
        [ "needs-reply"; "waiting"; "deadline"; "fyi"; "uncertain"; "resolved" ]

let sanitize s =
  let b = Buffer.create (String.length s) in
  Uutf.String.fold_utf_8
    (fun () _ -> function
      | `Malformed _ -> Buffer.add_char b '?'
      | `Uchar u ->
          let n = Uchar.to_int u in
          if n = 10 then Buffer.add_char b '\n'
          else if n = 9 then Buffer.add_char b ' '
          else if
            n = 0x061c || n = 0x200e || n = 0x200f || n < 32
            || (n >= 127 && n <= 159)
            || (n >= 0x202a && n <= 0x202e)
            || (n >= 0x2066 && n <= 0x2069)
          then Buffer.add_char b '?'
          else Uutf.Buffer.add_utf_8 b u)
    () s;
  Buffer.contents b

let source_jsont =
  let make service account id = { service; account; id } in
  Jsont.Object.map ~kind:"Source" make
  |> Jsont.Object.mem "service" Jsont.string ~enc:(fun x -> x.service)
  |> Jsont.Object.mem "account" Jsont.string ~enc:(fun x -> x.account)
  |> Jsont.Object.mem "id" Jsont.string ~enc:(fun (x : source) -> x.id)
  |> Jsont.Object.finish

let pair_jsont =
  Jsont.t2 Jsont.string
    ~dec:(fun a b -> (a, b))
    ~enc:(fun (a, b) i -> if i = 0 then a else b)

let message_jsont =
  let make source thread date sender recipients subject original text headers
      message_ids reply_ids truncated outside_interval revision =
    {
      source;
      thread;
      date;
      sender;
      recipients;
      subject;
      original;
      text;
      headers;
      message_ids;
      reply_ids;
      truncated;
      outside_interval;
      revision;
    }
  in
  Jsont.Object.map ~kind:"Message" make
  |> Jsont.Object.mem "source" source_jsont ~enc:(fun x -> x.source)
  |> Jsont.Object.mem "thread" Jsont.string ~enc:(fun x -> x.thread)
  |> Jsont.Object.mem "date" Jsont.string ~enc:(fun x -> x.date)
  |> Jsont.Object.mem "sender" Jsont.string ~enc:(fun x -> x.sender)
  |> Jsont.Object.mem "recipients" (Jsont.list Jsont.string) ~enc:(fun x ->
         x.recipients)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun x -> x.subject)
  |> Jsont.Object.mem "original" Jsont.string ~enc:(fun x -> x.original)
  |> Jsont.Object.mem "text" Jsont.string ~enc:(fun x -> x.text)
  |> Jsont.Object.mem "headers" (Jsont.list pair_jsont) ~enc:(fun x ->
         x.headers)
  |> Jsont.Object.mem "messageIds" (Jsont.list Jsont.string) ~enc:(fun x ->
         x.message_ids)
  |> Jsont.Object.mem "replyIds" (Jsont.list Jsont.string) ~enc:(fun x ->
         x.reply_ids)
  |> Jsont.Object.mem "truncated" Jsont.bool ~enc:(fun x -> x.truncated)
  |> Jsont.Object.mem "outsideInterval" Jsont.bool ~enc:(fun x ->
         x.outside_interval)
  |> Jsont.Object.mem "revision" Jsont.string ~enc:(fun (x : message) ->
         x.revision)
  |> Jsont.Object.finish

type ui = {
  snapshot : snapshot;
  selected : source option;
  scroll : int;
  pending : int option;
  serial : int;
  error : string option;
}
[@@deriving sexp]

let initial =
  {
    snapshot = empty;
    selected = None;
    scroll = 0;
    pending = None;
    serial = 0;
    error = None;
  }

let begin_request t =
  if t.pending <> None then None
  else
    let serial = t.serial + 1 in
    Some ({ t with pending = Some serial; serial; error = None }, serial)

let receive t id result =
  if t.pending <> Some id then t
  else
    match result with
    | Error e -> { t with pending = None; error = Some e }
    | Ok snapshot when snapshot.revision < t.snapshot.revision ->
        { t with pending = None }
    | Ok snapshot ->
        let selected =
          match t.selected with
          | Some s when List.exists (fun m -> m.source = s) snapshot.messages ->
              Some s
          | _ ->
              Option.map (fun m -> m.source) (List.nth_opt snapshot.messages 0)
        in
        { t with snapshot; selected; pending = None; error = None }
