open Syndic_common.XML
open Syndic_common.Util
module XML = Syndic_xml
module Error = Syndic_error

type error' =
  [ `Line of string
  | `Column of string
  | `Text of string
  | `Element of string
  | `Parent of string
  | `Value of string ]

type error
type warning
type 'a kind = Error | Warning

let error = Error
let warning = Warning

type 'a t =
  { kind: 'a kind  (** Error or warning. *)
  ; line: int
        (** Within the source code of the validated document, refers to the
            line where the error was detected. *)
  ; column: int
        (** Within the source code of the validated document, refers to the
            line where the column was detected. *)
  ; text: string  (** The actual error message. *)
  ; element: string
        (** Element in the feed where the message was triggered. *)
  ; parent: string  (** In the feed, parent of the element. *)
  ; value: string
        (** If applicable the value of the element, attribute or content which
            triggered the message. *) }

let feed_url = Uriz.of_string_exn "http://validator.w3.org/feed/check.cgi"

(* [Uriz.with_query] takes the query as encoded text, so the parameters are
   joined here.  [`Query_value] encodes ['&'], ['='] and ['+'], which is what
   keeps a value from being read as a further parameter. *)
let query_of_params params =
  let param (k, v) =
    Uriz.pct_encode ~component:`Query_value k
    ^ "="
    ^ Uriz.pct_encode ~component:`Query_value v
  in
  String.concat "&" (List.map param params)

let url d =
  let q = [("output", "soap12")] in
  let q =
    match d with
    | `Data data -> ("rawdata", data) :: q
    | `Uri uri -> [("url", Uriz.to_string uri)]
  in
  Uriz.with_query feed_url (This (query_of_params q))

let make_error ~kind ~pos:_ (l : [< error'] list) =
  let line =
    match find (function `Line _ -> true | _ -> false) l with
    | Some (`Line line) -> ( try int_of_string line with _ -> 0 )
    | _ -> 0
  in
  let column =
    match find (function `Column _ -> true | _ -> false) l with
    | Some (`Column column) -> ( try int_of_string column with _ -> 0 )
    | _ -> 0
  in
  let text =
    match find (function `Text _ -> true | _ -> false) l with
    | Some (`Text text) -> text
    | _ -> ""
  in
  let element =
    match find (function `Element _ -> true | _ -> false) l with
    | Some (`Element element) -> element
    | _ -> ""
  in
  let parent =
    match find (function `Parent _ -> true | _ -> false) l with
    | Some (`Parent parent) -> parent
    | _ -> ""
  in
  let value =
    match find (function `Value _ -> true | _ -> false) l with
    | Some (`Value value) -> value
    | _ -> ""
  in
  ({kind; line; column; text; element; parent; value} : _ t)

let error_data_producer =
  [ ("line", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `Line a))
  ; ("column", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `Column a))
  ; ("text", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `Text a))
  ; ("element", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `Element a))
  ; ("parent", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `Parent a))
  ; ("value", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `Value a)) ]

let error_of_xml ~kind =
  generate_catcher ~data_producer:error_data_producer (make_error ~kind)

let make_errorlist ~pos:_ (l : _ t list) = l

let errorlist_of_xml =
  let data_producer = [("error", error_of_xml ~kind:Error)] in
  generate_catcher ~data_producer ~xmlbase:None make_errorlist

let warninglist_of_xml =
  let data_producer = [("warning", error_of_xml ~kind:Warning)] in
  generate_catcher ~data_producer ~xmlbase:None make_errorlist

let find_errorlist l =
  recursive_find
    (function XML.Node (_, t, _) -> tag_is t "errorlist" | _ -> false)
    l

let find_warninglist l =
  recursive_find
    (function XML.Node (_, t, _) -> tag_is t "warninglist" | _ -> false)
    l

let to_error {line; column; text; _} = ((line, column), text)

let parse input =
  let _, xml = XML.of_xmlm input in
  let err =
    match find_errorlist xml with
    | Some (XML.Node (p, t, d)) -> errorlist_of_xml (p, t, d)
    | _ -> []
  in
  let warn =
    match find_warninglist xml with
    | Some (XML.Node (p, t, d)) -> warninglist_of_xml (p, t, d)
    | _ -> []
  in
  (err, warn)
