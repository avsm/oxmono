let default_max_depth = 128

exception Nesting_too_deep of int

(* Count structure before handing each slice to Jsont. Strings and escaped
   bytes carry state across slices, but tokens are never decoded twice.
   Jsont remains responsible for the complete JSON grammar and diagnostics. *)
let depth_limited_reader ~max_depth reader =
  let depth = ref 0 in
  let in_string = ref false in
  let escaped = ref false in
  let parse slice =
    let bytes = Bytesrw.Bytes.Slice.bytes slice in
    let first = Bytesrw.Bytes.Slice.first slice in
    let last = first + Bytesrw.Bytes.Slice.length slice in
    for i = first to last - 1 do
      let c = Bytes.unsafe_get bytes i in
      if !in_string then begin
        if !escaped then escaped := false
        else if c = '\\' then escaped := true
        else if c = '"' then in_string := false
      end else match c with
        | '"' -> in_string := true
        | '[' | '{' ->
            if !depth >= max_depth then raise (Nesting_too_deep max_depth);
            incr depth
        | ']' | '}' -> if !depth > 0 then decr depth
        | _ -> ()
    done
  in
  Bytesrw.Bytes.Reader.tap parse reader

let check_max_depth caller max_depth =
  if max_depth < 0 then
    invalid_arg ("Httpz_media.Json." ^ caller ^ ": max_depth must be non-negative")

let nesting_error max_depth =
  Jsont.Error.make_msg Jsont.Error.Context.empty Jsont.Meta.none
    (Printf.sprintf "JSON nesting deeper than %d" max_depth)

let decode' ?layout ?locs ?file ?(max_depth = default_max_depth) t reader =
  check_max_depth "decode'" max_depth;
  try
    Jsont_bytesrw.decode' ?layout ?locs ?file t
      (depth_limited_reader ~max_depth reader)
  with Nesting_too_deep max_depth -> Error (nesting_error max_depth)

let decode_string' ?layout ?locs ?file ?(max_depth = default_max_depth) t source =
  check_max_depth "decode_string'" max_depth;
  decode' ?layout ?locs ?file ~max_depth t
    (Bytesrw.Bytes.Reader.of_string source)

type Media.detail += Error of Jsont.Error.t

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
  Media.malformed ?loc:(loc_of_error error) ~detail:(Error error)
    (sanitize (Jsont.Error.to_string error))

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
      | Result.Error error -> invalid_arg ("Httpz_media.Json: " ^ sanitize error))
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
