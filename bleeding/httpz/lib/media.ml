module Bytes = Bytesrw.Bytes
module Buffer = Base.Buffer
module Char_u = Stdlib_stable.Char_u

module Loc = struct
  type t : immutable_data =
    { first_byte : int
    ; last_byte : int
    ; first_line : int
    ; first_col : int
    ; last_line : int
    ; last_col : int
    }

  let v
        ~first_byte
        ~last_byte
        ~first_line:(first_line, first_line_byte)
        ~last_line:(last_line, last_line_byte)
    =
    { first_byte
    ; last_byte
    ; first_line
    ; first_col = first_byte - first_line_byte + 1
    ; last_line
    ; last_col = last_byte - last_line_byte + 1
    }
  ;;

  let pp ppf t =
    if t.first_line = t.last_line
    then
      if t.first_col = t.last_col
      then Format.fprintf ppf "line %d, column %d" t.first_line t.first_col
      else Format.fprintf ppf "line %d, columns %d-%d" t.first_line t.first_col t.last_col
    else
      Format.fprintf
        ppf
        "line %d, column %d to line %d, column %d"
        t.first_line
        t.first_col
        t.last_line
        t.last_col
  ;;
end

type detail = ..
type detail += No_detail

type malformed =
  { message : string
  ; loc : Loc.t option
  ; detail : detail
  }

type error =
  | Unsupported of string option
  | Malformed of malformed
  | Too_large of int

let malformed ?loc ?(detail = No_detail) message = { message; loc; detail }

let pp_error ppf = function
  | Unsupported None -> Format.fprintf ppf "no media type given"
  | Unsupported (Some ct) -> Format.fprintf ppf "unsupported media type %S" ct
  | Malformed { message; loc; detail = _ } ->
    Format.fprintf ppf "malformed body: ";
    Option.iter (fun loc -> Format.fprintf ppf "at %a: " Loc.pp loc) loc;
    Format.pp_print_string ppf message
  | Too_large limit -> Format.fprintf ppf "body exceeds the %d-byte limit" limit
;;

let error_to_string e = Format.asprintf "%a" pp_error e

(* Diagnostic strings are often copied into logs or terminal output. Preserve
   printable UTF-8, but render C0 controls, DEL, and UTF-8-encoded C1 controls
   visibly so OSC/DCS/CSI sequences and line injection cannot survive. *)
let sanitize_diagnostic s =
  let b = Buffer.create (String.length s) in
  let valid_utf8 = String.is_valid_utf_8 s in
  let escaped_byte byte = Buffer.add_string b (Printf.sprintf "\\x%02X" byte) in
  let rec go i =
    if i >= String.length s then ()
    else
      let byte = Char.code (String.unsafe_get s i) in
      if byte <= 0x1f || byte = 0x7f then begin
        escaped_byte byte;
        go (i + 1)
      end
      else if
        byte = 0xc2
        && i + 1 < String.length s
        &&
        let next = Char.code (String.unsafe_get s (i + 1)) in
        next >= 0x80 && next <= 0x9f
      then begin
        let control = Char.code (String.unsafe_get s (i + 1)) in
        Buffer.add_string b (Printf.sprintf "\\u{%04X}" control);
        go (i + 2)
      end
      else if (not valid_utf8) && byte >= 0x80 && byte <= 0x9f then begin
        escaped_byte byte;
        go (i + 1)
      end
      else begin
        Buffer.add_char b (String.unsafe_get s i);
        go (i + 1)
      end
  in
  go 0;
  Buffer.contents b
;;

type 'a t : value mod portable contended =
  { media : string
  ; params : (string * string) list
  ; accept : string list
  ; encode : ('a -> Bytes.Writer.t -> unit) option @@ portable contended
  ; decode : (Bytes.Reader.t -> ('a, malformed) result) option @@ portable contended
  ; encode_string : ('a -> string) option @@ portable contended
  ; decode_string : (string -> ('a, malformed) result) option @@ portable contended
  }

(* Field grammar is shared with negotiation and typed header codecs. The
   unboxed bounds keep hot matching paths allocation-free. *)
module Syntax = struct
  let[@inline always] char_at (local_ s : string) i =
    Char_u.of_char (String.unsafe_get s i)
  ;;

  let[@inline] valid_slice (local_ s : string) pos len =
    pos >= 0 && len >= 0 && len <= String.length s && pos <= String.length s - len
  ;;

  let rec left (local_ s : string) i stop =
    if i < stop && Buf_read.is_space (char_at s i) then left s (i + 1) stop else i
  ;;

  let rec right (local_ s : string) start i =
    if i > start && Buf_read.is_space (char_at s (i - 1))
    then right s start (i - 1)
    else i
  ;;

  let rec find (local_ s : string) i stop (c : char#) =
    if i = stop || Char_u.equal (char_at s i) c then i else find s (i + 1) stop c
  ;;

  let rec token (local_ s : string) i stop =
    i = stop || (Buf_read.is_token_char (char_at s i) && token s (i + 1) stop)
  ;;

  let[@inline] parts (local_ s : string) pos len =
    let first = left s pos (pos + len) in
    let last = right s first (pos + len) in
    let slash = find s first last #'/' in
    if slash = first || slash + 1 >= last
       || not (token s first slash && token s (slash + 1) last)
    then #(-1, 0, 0, 0)
    else #(first, slash, slash + 1, last)
  ;;

  let[@inline] wildcard (local_ s : string) first last =
    last = first + 1 && Char_u.equal (char_at s first) #'*'
  ;;

  let rec no_star (local_ s : string) first last =
    first = last
    || (not (Char_u.equal (char_at s first) #'*') && no_star s (first + 1) last)
  ;;

  let[@inline] valid_range_parts (local_ s : string) t0 t1 s0 s1 =
    t0 >= 0
    && (if wildcard s t0 t1
        then wildcard s s0 s1
        else
          no_star s t0 t1
          && (wildcard s s0 s1
              || no_star s s0 s1
              || (s1 > s0 + 2
                  && Char_u.equal (char_at s s0) #'*'
                  && Char_u.equal (char_at s (s0 + 1)) #'+'
                  && no_star s (s0 + 2) s1)))
  ;;

  let[@zero_alloc] valid_type (local_ s : string) ~pos ~len =
    valid_slice s pos len
    && let #(t0, t1, s0, s1) = parts s pos len in
       t0 >= 0 && no_star s t0 t1 && no_star s s0 s1
  ;;

  let[@zero_alloc] valid_range (local_ s : string) ~pos ~len =
    valid_slice s pos len
    && let #(t0, t1, s0, s1) = parts s pos len in
       valid_range_parts s t0 t1 s0 s1
  ;;

  let rec equal_from
      (local_ a : string) a0 (local_ b : string) b0 i len =
    i = len
    || (Char_u.equal
          (Buf_read.to_lower (char_at a (a0 + i)))
          (Buf_read.to_lower (char_at b (b0 + i)))
        && equal_from a a0 b b0 (i + 1) len)
  ;;

  let[@inline] equal_part
      (local_ a : string) a0 a1 (local_ b : string) b0 b1 =
    a1 - a0 = b1 - b0 && equal_from a a0 b b0 0 (a1 - a0)
  ;;

  let[@zero_alloc] specificity
      ~(range : string @ local) ~pos ~len (media : string @ local) =
    if not (valid_slice range pos len)
    then -1
    else (
      let #(rt0, rt1, rs0, rs1) = parts range pos len in
      let stop = find media 0 (String.length media) #';' in
      let #(mt0, mt1, ms0, ms1) = parts media 0 stop in
      if not (valid_range_parts range rt0 rt1 rs0 rs1)
         || mt0 < 0
         || not (no_star media mt0 mt1 && no_star media ms0 ms1)
         || not (Header.Syntax.is_field_value media)
      then -1
      else if not
          (wildcard range rt0 rt1 || equal_part range rt0 rt1 media mt0 mt1)
      then -1
      else if wildcard range rs0 rs1
      then if wildcard range rt0 rt1 then 0 else 1
      else if equal_part range rs0 rs1 media ms0 ms1
              || (rs1 > rs0 + 2
                  && Char_u.equal (char_at range rs0) #'*'
                  && Char_u.equal (char_at range (rs0 + 1)) #'+'
                  && ms1 - ms0 >= rs1 - rs0 - 1
                  && equal_part
                       range
                       (rs0 + 1)
                       rs1
                       media
                       (ms1 - (rs1 - rs0 - 1))
                       ms1)
      then 2
      else -1)
  ;;
end

let is_token = Header.Syntax.is_token

let check_type ~wildcards ~what s =
  let valid = if wildcards then Syntax.valid_range else Syntax.valid_type in
  if valid s ~pos:0 ~len:(String.length s)
     && s <> ""
     && not (Buf_read.is_space (Char_u.of_char s.[0]))
     && not (Buf_read.is_space (Char_u.of_char s.[String.length s - 1]))
  then String.lowercase_ascii s
  else invalid_arg (Printf.sprintf "Media: %s %S is not a media type" what s)
;;

let check_param (name, value) =
  if not (is_token name)
  then invalid_arg (Printf.sprintf "Media: parameter name %S is not a token" name);
  if String.exists (fun c -> Char.code c < 0x20 || c = '\127') value
  then invalid_arg (Printf.sprintf "Media: parameter %S has a control byte" name)
;;

let make ?(accept = []) ?(params = []) media ~encode ~decode ~encode_string
    ~decode_string =
  let media = check_type ~wildcards:false ~what:"type" media in
  let accept = List.map (check_type ~wildcards:true ~what:"accept entry") accept in
  List.iter check_param params;
  { media
  ; params
  ; accept = media :: accept
  ; encode
  ; decode
  ; encode_string
  ; decode_string
  }
;;

let v
    ?accept
    ?params
    ?(decode_reader @ portable)
    media
    ~(encode @ portable)
    ~(decode @ portable)
  =
  let encode_string x =
    let b = Buffer.create 256 in
    encode x (Bytes.Writer.of_buffer b);
    Buffer.contents b
  in
  let decode_reader, decode_string =
    match decode_reader with
    | Some decode -> decode, fun s -> decode (Bytes.Reader.of_string s)
    | None ->
      let decode_string s = Result.map_error malformed (decode s) in
      (fun r -> decode_string (Bytes.Reader.to_string r)), decode_string
  in
  make
    ?accept
    ?params
    media
    ~encode:(Some encode)
    ~decode:(Some decode_reader)
    ~encode_string:(Some encode_string)
    ~decode_string:(Some decode_string)
;;

let v_reader ?accept ?params media ~(encode @ portable) ~(decode @ portable) =
  let encode_string x =
    let b = Buffer.create 256 in
    encode x (Bytes.Writer.of_buffer b);
    Buffer.contents b
  in
  let decode_string s = decode (Bytes.Reader.of_string s) in
  make
    ?accept
    ?params
    media
    ~encode:(Some encode)
    ~decode:(Some decode)
    ~encode_string:(Some encode_string)
    ~decode_string:(Some decode_string)
;;

let of_strings ?accept ?params media ~(encode @ portable) ~(decode @ portable) =
  make
    ?accept
    ?params
    media
    ~encode:(Some (fun x w -> Bytes.Writer.write_string w (encode x)))
    ~decode:
      (Some (fun r -> Result.map_error malformed (decode (Bytes.Reader.to_string r))))
    ~encode_string:(Some encode)
    ~decode_string:(Some (fun s -> Result.map_error malformed (decode s)))
;;

let encoder ?params media (f @ portable) =
  make
    ?params
    media
    ~encode:(Some (fun x w -> Bytes.Writer.write_string w (f x)))
    ~decode:None
    ~encode_string:(Some f)
    ~decode_string:None
;;

let decoder ?accept media (f @ portable) =
  make
    ?accept
    media
    ~encode:None
    ~decode:(Some (fun r -> Result.map_error malformed (f (Bytes.Reader.to_string r))))
    ~encode_string:None
    ~decode_string:(Some (fun s -> Result.map_error malformed (f s)))
;;

let map ~(decode @ portable) ~(encode @ portable) t =
  let encode_writer = match t.encode with
  | None -> None
  | Some enc ->
      Some ((fun y w -> enc (encode y) w) : _ @ portable)
  in
  let encode_string = match t.encode_string with
  | None -> None
  | Some enc -> Some ((fun y -> enc (encode y)) : _ @ portable)
  in
  let decode_reader = match t.decode with
  | None -> None
  | Some dec ->
      Some ((fun r ->
        match dec r with
        | Ok x -> Result.map_error malformed (decode x)
        | Error _ as e -> e) : _ @ portable)
  in
  let decode_string = match t.decode_string with
  | None -> None
  | Some dec ->
      Some ((fun s ->
        match dec s with
        | Ok x -> Result.map_error malformed (decode x)
        | Error _ as e -> e) : _ @ portable)
  in
  { t with encode = encode_writer; encode_string; decode = decode_reader;
           decode_string }
;;

let media_type t = t.media

let parameter_value value =
  if is_token value then value else Header.Syntax.quote_string value

let content_type_of media params =
  match params with
  | [] -> media
  | ps ->
    media
    ^ String.concat ""
        (List.map
           (fun (k, v) -> Printf.sprintf "; %s=%s" k (parameter_value v))
           ps)
;;

let content_type t = content_type_of t.media t.params

let[@zero_alloc] matches ~(range : string @ local) (media : string @ local) =
  let len = Syntax.find range 0 (String.length range) #';' in
  Header.Syntax.is_field_value range
  && Syntax.specificity ~range ~pos:0 ~len media >= 0
;;

let[@zero_alloc] accepts_in accept (media : string option @ local) =
  match media with
  | media ->
    let mutable ranges = accept in
    let mutable found = false in
    while not found && ranges != [] do
      match ranges with
      | [] -> ()
      | range :: rest ->
        ranges <- rest;
        let matches =
          match media with
          | None -> String.equal range "*/*"
          | Some media -> matches ~range media
        in
        if matches then found <- true
    done;
    found
;;

let accepts t ct = accepts_in t.accept ct

let accept_header medias =
  let count = List.length medias in
  if count > 1000 then invalid_arg "Media.accept_header: at most 1000 preferences";
  let step = if count <= 10 then 100 else 999 / (count - 1) in
  List.mapi
    (fun i media ->
       if i = 0 then media
       else
         let q = Printf.sprintf "%03d" (1000 - (i * step)) in
         let rec stop n = if n > 1 && q.[n - 1] = '0' then stop (n - 1) else n in
         media ^ ";q=0." ^ String.sub q 0 (stop 3))
    medias
  |> String.concat ", "
;;

let can_encode t = Option.is_some t.encode
let can_decode t = Option.is_some t.decode

let encode_writer t x w =
  match t.encode with
  | Some enc -> enc x w
  | None -> invalid_arg (Printf.sprintf "Media: %s cannot encode" t.media)
;;

let encode t x =
  match t.encode_string with
  | Some enc -> enc x
  | None -> invalid_arg (Printf.sprintf "Media: %s cannot encode" t.media)
;;

let decode_reader t r =
  match t.decode with
  | Some dec -> Result.map_error (fun error -> Malformed error) (dec r)
  | None -> invalid_arg (Printf.sprintf "Media: %s cannot decode" t.media)
;;

let decode t s =
  match t.decode_string with
  | Some dec -> Result.map_error (fun error -> Malformed error) (dec s)
  | None -> invalid_arg (Printf.sprintf "Media: %s cannot decode" t.media)
;;
let utf8 = [ "charset", "utf-8" ]
let text = of_strings ~params:utf8 "text/plain" ~encode:Fun.id ~decode:Result.ok
let html = of_strings ~params:utf8 "text/html" ~encode:Fun.id ~decode:Result.ok

let octets =
  of_strings ~accept:[ "*/*" ] "application/octet-stream" ~encode:Fun.id ~decode:Result.ok
;;

let form =
  of_strings
    "application/x-www-form-urlencoded"
    ~encode:Urlencoded.encode
    ~decode:(fun s -> Ok (Urlencoded.decode s))
;;

type 'a seq : value mod portable contended =
  { smedia : string
  ; sparams : (string * string) list
  ; saccept : string list
  ; item : 'a t
  }

let lines ?(accept = []) ?(params = []) media item =
  let smedia = check_type ~wildcards:false ~what:"type" media in
  let saccept = List.map (check_type ~wildcards:true ~what:"accept entry") accept in
  List.iter check_param params;
  { smedia; sparams = params; saccept = smedia :: saccept; item }
;;

let item s = s.item
let seq_media_type s = s.smedia
let seq_content_type s = content_type_of s.smedia s.sparams
let seq_accepts s ct = accepts_in s.saccept ct
let encode_item s x = encode s.item x ^ "\n"

let strip_cr line =
  let n = String.length line in
  if n > 0 && line.[n - 1] = '\r' then String.sub line 0 (n - 1) else line
;;

let decode_item s line = decode s.item (strip_cr line)

let encode_items s items =
  let b = Buffer.create 1024 in
  Seq.iter (fun x -> Buffer.add_string b (encode_item s x)) items;
  Buffer.contents b
;;

let decode_items s body =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | line :: rest ->
      if String.trim line = ""
      then go acc rest
      else (
        match decode_item s line with
        | Ok x -> go (x :: acc) rest
        | Error _ as e -> e)
  in
  go [] (String.split_on_char '\n' body)
;;
