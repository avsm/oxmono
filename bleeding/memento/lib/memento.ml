(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
module Datetime = struct
  type t = Ptime.t
  let of_http (s : string @ local) =
    if String.length s > 32767 then Error "HTTP date is too long"
    else
      let module I = Stdlib_stable.Int16_u in
      let module F = Stdlib_upstream_compatible.Float_u in
      match Httpz.Date.parse (Bytes.of_string s)
        (Httpz.Span.make ~off:(I.of_int 0) ~len:(I.of_int (String.length s))) with
      | #(Httpz.Date.Invalid, _) -> Error "Invalid HTTP date"
      | #(Httpz.Date.Valid, seconds) ->
          (match Ptime.of_float_s (F.to_float seconds) with
          | Some t -> Ok t | None -> Error "HTTP date is out of range")
  let to_http t =
    Httpz.Date.format (Stdlib_upstream_compatible.Float_u.of_float
      (Ptime.to_float_s (Ptime.truncate ~frac_s:0 t)))
  let of_json s = match Ptime.of_rfc3339 ~strict:true s with
    | Ok (t, _, _) -> Ok t | Error _ -> Error "Invalid RFC 3339 datetime"
  let to_json t =
    let s = Ptime.to_rfc3339 ~frac_s:12 ~tz_offset_s:0 t in
    let i = ref (String.length s - 2) in
    while s.[!i] = '0' do decr i done;
    if s.[!i] = '.' then decr i;
    String.sub s 0 (!i + 1) ^ "Z"
  let jsont = Jsont.of_of_string ~kind:"Memento datetime"
      ~enc:to_json of_json
end

let uri_jsont =
  Jsont.of_of_string ~kind:"absolute URI" ~enc:Fun.id (fun s ->
    match Httpz_uri.of_string s with
    | Null -> Error "Invalid URI"
    | This uri -> match Httpz_uri.scheme uri with
      | Null -> Error "Expected absolute URI"
      | This _ -> Ok s)


type capture = { uri : string; datetime : Datetime.t }
let capture_jsont =
  let open Jsont.Object in
  map (fun uri datetime -> { uri; datetime })
  |> mem "uri" uri_jsont ~enc:(fun c -> c.uri)
  |> mem "datetime" Datetime.jsont ~enc:(fun c -> c.datetime)
  |> finish

module Timemap = struct
  type reference = {
    uri : string; from : Datetime.t option; until : Datetime.t option;
    memento_compliant : bool option; archive_id : string option;
  }
  type formats = { json_format : string option; link_format : string option }
  type mementos = {
    list : capture list; first : capture option; last : capture option;
    closest : capture option;
  }
  type pages = { prev : reference option; next : reference option }
  type t = {
    original_uri : string; timegate_uri : string option;
    timemap_uri : formats option; mementos : mementos option;
    pages : pages option; timemap_index : reference list option;
  }
  let invalid s = Jsont.Error.msg Jsont.Meta.none s
  let compliant = Jsont.of_of_string ~enc:(fun b -> if b then "yes" else "no")
      (function "yes" -> Ok true | "no" -> Ok false
      | _ -> Error "Expected yes or no")
  let validate_interval from until = match from, until with
    | Some a, Some b when Ptime.is_later a ~than:b ->
        invalid "TimeMap interval is reversed"
    | _ -> ()
  let reference_jsont =
    let open Jsont.Object in
    map (fun uri from until memento_compliant archive_id ->
      { uri; from; until; memento_compliant; archive_id })
    |> mem "uri" uri_jsont ~enc:(fun (r : reference) -> r.uri)
    |> opt_mem "from" Datetime.jsont ~enc:(fun r -> r.from)
    |> opt_mem "until" Datetime.jsont ~enc:(fun r -> r.until)
    |> opt_mem "memento_compliant" compliant ~enc:(fun r -> r.memento_compliant)
    |> opt_mem "archive_id" Jsont.string ~enc:(fun r -> r.archive_id)
    |> finish
    |> Jsont.iter
       ~dec:(fun r -> validate_interval r.from r.until)
       ~enc:(fun r -> validate_interval r.from r.until)
  let formats_jsont =
    let open Jsont.Object in
    map (fun json_format link_format -> { json_format; link_format })
    |> opt_mem "json_format" uri_jsont ~enc:(fun f -> f.json_format)
    |> opt_mem "link_format" uri_jsont ~enc:(fun f -> f.link_format)
    |> finish
  let mementos_jsont =
    let open Jsont.Object in
    map (fun list first last closest -> { list; first; last; closest })
    |> mem "list" (Jsont.list capture_jsont) ~enc:(fun m -> m.list)
    |> opt_mem "first" capture_jsont ~enc:(fun m -> m.first)
    |> opt_mem "last" capture_jsont ~enc:(fun m -> m.last)
    |> opt_mem "closest" capture_jsont ~enc:(fun m -> m.closest)
    |> finish
  let pages_jsont =
    let open Jsont.Object in
    map (fun prev next -> { prev; next })
    |> mem "prev" (Jsont.option reference_jsont) ~enc:(fun p -> p.prev)
       ~dec_absent:(fun () -> None) ~enc_omit:Option.is_none
    |> mem "next" (Jsont.option reference_jsont) ~enc:(fun p -> p.next)
       ~dec_absent:(fun () -> None) ~enc_omit:Option.is_none
    |> finish
  let validate t =
    (match t.mementos, t.timemap_index with
    | Some _, None -> ()
    | None, Some _ when t.pages = None -> ()
    | _ -> invalid "Expected mementos or timemap_index, exclusively")
  let jsont =
    let open Jsont.Object in
    map (fun original_uri timegate_uri timemap_uri mementos pages timemap_index ->
      { original_uri; timegate_uri; timemap_uri; mementos; pages; timemap_index })
    |> mem "original_uri" uri_jsont ~enc:(fun t -> t.original_uri)
    |> opt_mem "timegate_uri" uri_jsont ~enc:(fun t -> t.timegate_uri)
    |> opt_mem "timemap_uri" formats_jsont ~enc:(fun t -> t.timemap_uri)
    |> opt_mem "mementos" mementos_jsont ~enc:(fun t -> t.mementos)
    |> opt_mem "pages" pages_jsont ~enc:(fun t -> t.pages)
    |> opt_mem "timemap_index" (Jsont.list reference_jsont)
         ~enc:(fun t -> t.timemap_index)
    |> finish |> Jsont.iter ~dec:validate ~enc:validate
  let captures t = match t.mementos with None -> [] | Some m -> m.list
  let references t =
    Option.value ~default:[] t.timemap_index @
      (match t.pages with None -> [] | Some p ->
        List.filter_map Fun.id [p.prev; p.next])
end

module Link = struct
  type t = Fetch.Header.link
  let has_rel = Fetch.Header.link_has_rel
  let find = Fetch.Header.link_rel
  let decode s = match Fetch.Header.decode_links s with
    | Some ls -> Ok ls | None -> Error "Invalid Link syntax"
  let encode ls = Fetch.Header.encode_links ls
  let original uri = Fetch.Header.link ~rel:"original" uri
  let timegate uri = Fetch.Header.link ~rel:"timegate" uri
  let memento ?(rels = []) c =
    Fetch.Header.link ~rel:(String.concat " " ("memento" :: rels))
      ~params:["datetime", Datetime.to_http c.datetime] c.uri
  let timemap ?from ?until ~media_type uri =
    let params = List.filter_map Fun.id
      [Option.map (fun t -> "from", Datetime.to_http t) from;
       Option.map (fun t -> "until", Datetime.to_http t) until] in
    Fetch.Header.link ~rel:"timemap" ~media_type ~params uri
  let captures ls =
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | l :: rest when not (has_rel "memento" l) -> loop acc rest
      | l :: rest ->
          let dates = List.filter (fun (n, _) ->
            String.lowercase_ascii n = "datetime") l.params in
          (match dates with
          | [(_, s)] -> (match Datetime.of_http s with
            | Ok datetime -> loop ({ uri = l.target; datetime } :: acc) rest
            | Error e -> Error e)
          | _ -> Error "Memento link requires exactly one datetime")
    in loop [] ls
  let validate_timemap links =
    if List.length (List.filter (has_rel "original") links) <> 1 then
      Error "TimeMap requires exactly one original link"
    else Result.map (fun _ -> ()) (captures links)
  let timemap_media = Fetch.Media.of_strings "application/link-format"
    ~encode:(fun links -> match validate_timemap links with
      | Error e -> invalid_arg e | Ok () -> encode links)
    ~decode:(fun body -> match decode body with
      | Error e -> Error e
      | Ok links -> Result.map (fun () -> links) (validate_timemap links))

end

let nearest datetime captures =
  let distance c = Ptime.Span.abs (Ptime.diff c.datetime datetime) in
  List.fold_left (fun best c -> match best with
    | None -> Some c
    | Some b when Ptime.Span.compare (distance c) (distance b) < 0 ||
        (Ptime.Span.compare (distance c) (distance b) = 0 && Ptime.is_earlier c.datetime ~than:b.datetime)
        -> Some c
    | _ -> best) None captures

module Headers = struct
  let accept_datetime = Fetch.Header.v "Accept-Datetime"
      ~encode:Datetime.to_http ~decode:(fun s -> Result.to_option (Datetime.of_http s))
  let memento_datetime = Fetch.Header.v "Memento-Datetime"
      ~encode:Datetime.to_http ~decode:(fun s -> Result.to_option (Datetime.of_http s))
  type metadata = {
    links : Link.t list; datetime : Datetime.t option;
    is_timegate : bool; is_memento : bool; do_not_negotiate : bool;
  }
  let read headers =
    let links = Fetch.Header.get Fetch.Header.links headers in
    let raw_links = Http.Header.get_multi headers "link" in
    let raw_date = Http.Header.get_multi headers "memento-datetime" in
    let datetime = Fetch.Header.get memento_datetime headers in
    if raw_links <> [] && links = None then Error "Malformed Link header"
    else if raw_date <> [] && datetime = None then
      Error "Malformed or repeated Memento-Datetime"
    else
      let links = Option.value ~default:[] links in
      let vary = Http.Header.get_multi headers "vary" |> String.concat "," in
      let is_timegate = String.split_on_char ',' vary |> List.exists
          (fun s -> String.lowercase_ascii (String.trim s) = "accept-datetime") in
      let is_memento = datetime <> None && Link.find "original" links <> None in
      let do_not_negotiate = List.exists (fun l -> Link.has_rel "type" l &&
          List.mem l.Fetch.Header.target
            ["http://mementoweb.org/terms/donotnegotiate";
             "https://mementoweb.org/terms/donotnegotiate";
             "/terms/donotnegotiate"]) links in
      if (is_timegate || datetime <> None) &&
          List.length (List.filter (Link.has_rel "original") links) > 1 then
        Error "Expected exactly one original link"
      else Ok { links; datetime; is_timegate; is_memento; do_not_negotiate }
  let memento ~original (c : capture) =
    ["Memento-Datetime", Datetime.to_http c.datetime;
     "Link", Link.encode [Link.original original]]
  let timegate ~original (c : capture) =
    ["Location", c.uri; "Vary", "accept-datetime";
     "Link", Link.encode [Link.original original; Link.memento c]]
end
