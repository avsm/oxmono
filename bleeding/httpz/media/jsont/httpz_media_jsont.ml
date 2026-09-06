module Media = Httpz_media

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

let default_max_depth = Httpz_jsont.default_max_depth
let decode' = Httpz_jsont.decode'
let decode_string' = Httpz_jsont.decode_string'

let check_max_depth caller max_depth =
  if max_depth < 0 then
    invalid_arg
      ("Httpz_media_jsont." ^ caller ^ ": max_depth must be non-negative")

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
      | Error error -> invalid_arg ("Httpz_media_jsont: " ^ sanitize error))
    (* Jsont's reader decoder predates portable-mode annotations. Each
       decoding invocation owns its parser and structural guard state. *)
    ~decode:(Obj.magic_portable decode_reader)

let json = v Jsont.json

let lines ?(media = "application/jsonl")
    ?(accept =
      [ "application/ndjson";
        "application/x-ndjson";
        "application/jsonlines";
        "application/x-jsonlines" ])
    ?(max_depth = default_max_depth) t =
  check_max_depth "lines" max_depth;
  Media.lines ~accept media (v ~max_depth t)
