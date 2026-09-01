type Media.detail += Jsont of Jsont.Error.t

(* Jsont may style its output and quote attacker-controlled JSON values. The
   shared media boundary removes every terminal control class, not merely SGR
   colour sequences. *)
let sanitize = Media.sanitize_diagnostic

let loc_of_error (_, meta, _) =
  let loc = Jsont.Meta.textloc meta in
  if Jsont.Textloc.is_none loc then None
  else
    let first_byte = Jsont.Textloc.first_byte loc in
    let last_byte = Jsont.Textloc.last_byte loc in
    Some
      (Media.Loc.v ~first_byte ~last_byte
         ~first_line:(Jsont.Textloc.first_line loc)
         ~last_line:(Jsont.Textloc.last_line loc))

let malformed error =
  Media.malformed ?loc:(loc_of_error error) ~detail:(Jsont error)
    (sanitize (Jsont.Error.to_string error))

let default_max_depth = 128

exception Nesting_too_deep of int

(* Jsont's decoder is recursive in the JSON nesting depth and exposes no
   structural hook. Jsonm owns the lexical pass; this code counts only its
   container events, then Jsont remains the definitive typed decoder. *)
let depth_limited_reader ~max_depth reader =
  let decoder = Jsonm.decoder `Manual in
  let depth = ref 0 in
  let active = ref true in
  let rec drain () =
    match Jsonm.decode decoder with
    | `Lexeme (`As | `Os) ->
        incr depth;
        if !depth > max_depth then raise (Nesting_too_deep max_depth);
        drain ()
    | `Lexeme (`Ae | `Oe) ->
        decr depth;
        drain ()
    | `Lexeme _ -> drain ()
    | `Await -> ()
    | `End -> active := false
    | `Error _ -> active := false
  in
  let parse slice =
    if !active then begin
      Jsonm.Manual.src decoder (Bytesrw.Bytes.Slice.bytes slice)
        (Bytesrw.Bytes.Slice.first slice)
        (Bytesrw.Bytes.Slice.length slice);
      drain ()
    end
  in
  Bytesrw.Bytes.Reader.tap parse reader

let check_max_depth caller max_depth =
  if max_depth < 0 then
    invalid_arg ("Httpz.Json." ^ caller ^ ": max_depth must be non-negative")

let nesting_error max_depth =
  Jsont.Error.make_msg Jsont.Error.Context.empty Jsont.Meta.none
    (Printf.sprintf "JSON nesting deeper than %d" max_depth)

let decode' ?layout ?locs ?file ?(max_depth = default_max_depth) t reader =
  check_max_depth "decode'" max_depth;
  try
    Jsont_bytesrw.decode' ?layout ?locs ?file t
      (depth_limited_reader ~max_depth reader)
  with Nesting_too_deep max_depth -> Error (nesting_error max_depth)

let decode_string' ?layout ?locs ?file ?max_depth t source =
  decode' ?layout ?locs ?file ?max_depth t
    (Bytesrw.Bytes.Reader.of_string source)

let v ?(media = "application/json") ?(accept = [ "application/*+json" ])
    ?format ?(locs = true) ?(max_depth = default_max_depth) t =
  check_max_depth "v" max_depth;
  let decode_reader reader =
    Result.map_error malformed (decode' ~locs ~max_depth t reader)
  in
  Media.v_reader ~accept media
    ~encode:(fun value writer ->
      match Jsont_bytesrw.encode ?format t value ~eod:false writer with
      | Ok () -> ()
      | Error error -> invalid_arg ("Httpz.Json: " ^ sanitize error))
    (* Jsonm's incremental decoder is pure per call but predates portable-mode
       annotations, like the adjacent Cmarkit adapter boundary. *)
    ~decode:(Obj.magic_portable decode_reader)

let json = v Jsont.json

let lines ?(media = "application/jsonl")
    ?(accept =
      [ "application/x-ndjson";
        "application/jsonlines";
        "application/x-jsonlines" ])
    ?max_depth t =
  Media.lines ~accept media (v ?max_depth t)
