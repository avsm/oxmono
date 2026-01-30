(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)


module SM = Map.Make(String)

module Tloc = struct
  type fpath = string
  let pp_path = Format.pp_print_string

  type pos = int
  type line = int

  type t =
    { file : fpath;
      sbyte : pos; ebyte : pos;
      sline : pos * line; eline : pos * line }

  let no_file = "-"
  let v ~file ~sbyte ~ebyte ~sline ~eline = { file; sbyte; ebyte; sline; eline }

  let pf = Format.fprintf

  let pp_gnu ppf l =
    if l.ebyte < 0 then pf ppf "%a:" pp_path l.file
    else
      let pp_lines ppf l =
        let col_s = l.sbyte - snd l.sline + 1 in
        let col_e = l.ebyte - snd l.eline + 1 in
        if fst l.sline = fst l.eline then
          pf ppf "%d.%d-%d" (fst l.sline) col_s col_e
        else
          pf ppf "%d.%d-%d.%d" (fst l.sline) col_s (fst l.eline) col_e
      in
      pf ppf "%a:%a" pp_path l.file pp_lines l

  let pp = pp_gnu
end

module Utf_8 = struct
  type case =
  | L1 | L2 | L3_E0 | L3_E1_EC_or_EE_EF | L3_ED | L4_F0 | L4_F1_F3 | L4_F4 | E

  let case =
(*
  (* See https://tools.ietf.org/html/rfc3629#section-4 *)
  Printf.printf "[|";
  for i = 0 to 255 do
    if i mod 16 = 0 then Printf.printf "\n";
    if 0x00 <= i && i <= 0x7F then Printf.printf "L1; " else
    if 0xC2 <= i && i <= 0xDF then Printf.printf "L2; " else
    if 0xE0 = i then Printf.printf "L3_E0; " else
    if 0xE1 <= i && i <= 0xEC || 0xEE <= i && i <= 0xEF
    then Printf.printf "L3_E1_EC_or_EE_EF; " else
    if 0xED = i then Printf.printf "L3_ED;" else
    if 0xF0 = i then Printf.printf "L4_F0; " else
    if 0xF1 <= i && i <= 0xF3 then Printf.printf "L4_F1_F3; " else
    if 0xF4 = i then Printf.printf "L4_F4; " else
    Printf.printf "E; "
  done;
  Printf.printf "\n|]"
*)
  [|
    L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
    L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
    L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
    L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
    L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
    L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
    L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
    L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
    E; E; E; E; E; E; E; E; E; E; E; E; E; E; E; E;
    E; E; E; E; E; E; E; E; E; E; E; E; E; E; E; E;
    E; E; E; E; E; E; E; E; E; E; E; E; E; E; E; E;
    E; E; E; E; E; E; E; E; E; E; E; E; E; E; E; E;
    E; E; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2;
    L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2;
    L3_E0; L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF;
    L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF;
    L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF;
    L3_E1_EC_or_EE_EF; L3_ED;L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF;
    L4_F0; L4_F1_F3; L4_F1_F3; L4_F1_F3; L4_F4; E; E; E; E; E; E; E; E; E; E; E;
  |]
end

module Tdec = struct
  type t =
    { file : Tloc.fpath; i : string; tok : Buffer.t;
      mutable pos : int; mutable line : int; mutable line_pos : int; }

  let create ?(file = Tloc.no_file) i =
    { file; i; tok = Buffer.create 255; pos = 0; line = 1; line_pos = 0 }

  let pos d = d.pos
  let line d = d.line, d.line_pos

  let loc_to_here d ~sbyte ~sline =
    Tloc.v ~file:d.file ~sbyte ~ebyte:d.pos ~sline ~eline:(d.line, d.line_pos)

  let loc_here d = loc_to_here d ~sbyte:d.pos ~sline:(d.line, d.line_pos)

  exception Err of Tloc.t * string

  let err loc msg = raise_notrace (Err (loc, msg))

  let err_to_here d ~sbyte ~sline fmt =
    Format.kasprintf (err (loc_to_here d ~sbyte ~sline)) fmt

  let err_here d fmt = Format.kasprintf (err (loc_here d)) fmt

  let incr_line d =
    match d.i.[d.pos] with
    | '\r' -> d.line <- d.line + 1; d.line_pos <- d.pos + 1
    | '\n' ->
        (if d.pos = 0 || d.i.[d.pos - 1] <> '\r' then d.line <- d.line + 1);
        d.line_pos <- d.pos + 1
    | _ -> ()
  [@@ocaml.inline]

  let eoi d = d.pos >= String.length d.i [@@ocaml.inline]
  let byte d = if eoi d then 0xFFFF else Char.code d.i.[d.pos] [@@ocaml.inline]
  let accept_byte d = incr_line d; d.pos <- d.pos + 1 [@@ocaml.inline]

  let accept_utf_8 accept d =
    let err d =
      match byte d with
      | 0xFFFF -> err_here d "UTF-8 decoding error: unexpected end of input"
      | b -> err_here d "UTF-8 decoding error: byte %02x illegal here" b
    in
    let accept_tail d = if byte d lsr 6 = 0b10 then accept d else err d in
    match byte d with
    | 0xFFFF -> err d
    | b ->
        match Utf_8.case.(b) with
        | L1 -> accept d
        | L2 -> accept d; accept_tail d
        | L3_E0 ->
            accept d;
            if byte d - 0xA0 < 0xBF - 0xA0 then accept d else err d;
            accept_tail d
        | L3_E1_EC_or_EE_EF -> accept d; accept_tail d; accept_tail d
        | L3_ED ->
            accept d;
            if byte d - 0x80 < 0x9F - 0x80 then accept d else err d;
            accept_tail d
        | L4_F0 ->
            accept d;
            if byte d - 0x90 < 0xBF - 0x90 then accept d else err d;
            accept_tail d; accept_tail d
        | L4_F1_F3 ->
            accept d;
            accept_tail d; accept_tail d; accept_tail d
        | L4_F4 ->
            accept d;
            if byte d - 0x80 < 0x8F - 0x80 then accept d else err d
        | E -> err d

  let tok_reset d = Buffer.reset d.tok [@@ocaml.inline]
  let tok_pop d = let t = Buffer.contents d.tok in tok_reset d; t [@@ocaml.inline]
  let tok_accept_byte d = Buffer.add_char d.tok d.i.[d.pos]; accept_byte d [@@ocaml.inline]
  let tok_accept_uchar d = accept_utf_8 tok_accept_byte d [@@ocaml.inline]
end

module Url = struct
  let string_subrange ?(first = 0) ?last s =
    let max = String.length s - 1 in
    let last = match last with
    | None -> max
    | Some l when l > max -> max
    | Some l -> l
    in
    let first = if first < 0 then 0 else first in
    if first > last then "" else String.sub s first (last - first + 1)

  let alpha = function 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false
  let digit = function '0' .. '9' -> true | _ -> false

  let scheme_char c =
    alpha c || digit c || Char.equal c '+' || Char.equal c '-' ||
    Char.equal '.' c

  let find_scheme_colon u =
    if u = "" || not (alpha u.[0]) then None
    else
      let max = String.length u - 1 in
      let i = ref 1 in
      while !i <= max && scheme_char u.[!i] do incr i done;
      if !i > max || u.[!i] <> ':' then None else Some !i

  let find_authority_last ~start u =
    let max = String.length u - 1 in
    if start > max then None
    else if start + 1 > max then Some (start - 1)
    else if not (u.[start] = '/' && u.[start + 1] = '/') then Some (start - 1)
    else
      let i = ref (start + 2) in
      while !i <= max && u.[!i] <> '/' && u.[!i] <> '?' && u.[!i] <> '#' do
        incr i
      done;
      Some (!i - 1)

  let scheme u =
    Option.map (fun i -> String.sub u 0 i) (find_scheme_colon u)

  let path_first u =
    let start = Option.value ~default:0 (Option.map succ (find_scheme_colon u)) in
    let first = Option.value ~default:start (Option.map succ (find_authority_last ~start u)) in
    let max = String.length u - 1 in
    if first > max || u.[first] = '#' || u.[first] = '?' then None else Some first

  let path_last u ~first =
    let max = String.length u - 1 in
    let i = ref (first + 1) in
    while !i <= max && u.[!i] <> '?' && u.[!i] <> '#' do incr i done;
    !i - 1

  let path u =
    Option.map (fun first -> string_subrange ~first ~last:(path_last u ~first) u) (path_first u)
end

let escape = (* The escape rules are a bit unclear. These are those of LaTeX *)
  let byte_replaced_length char_len s =
    let rec loop s max i l = match i > max with
    | true -> l
    | false -> loop s max (i + 1) (l + char_len s.[i])
    in
    loop s (String.length s - 1) 0 0
  in
  let byte_replace set_char s ~len ~replaced_len =
    let b = Bytes.create replaced_len in
    let rec loop s max i k = match i > max with
    | true -> Bytes.unsafe_to_string b
    | false -> loop s max (i + 1) (set_char b k s.[i])
    in
    loop s (len - 1) 0 0
  in
  let byte_escaper char_len set_char s =
    let len = String.length s in
    let replaced_len = byte_replaced_length char_len s in
    match replaced_len = len with
    | true -> s
    | false -> byte_replace set_char s ~len ~replaced_len
  in
  let tilde_esc = "\\textasciitilde" in
  let tilde_len = String.length tilde_esc in
  let circ_esc = "\\textasciicircum" in
  let circ_len = String.length circ_esc in
  let bslash_esc = "\\textbackslash" in
  let bslash_len = String.length bslash_esc in
  let char_len = function
  | '&' | '%' | '$' | '#' | '_' | '{' | '}' -> 2
  | '~' -> tilde_len
  | '^' -> circ_len
  | '\\' -> bslash_len
  | _ -> 1
  in
  let set_char b i = function
  | '&' | '%' | '$' | '#' | '_' | '{' | '}' as c ->
      Bytes.set b i '\\'; Bytes.set b (i + 1) c; i + 2
  | '~' -> Bytes.blit_string tilde_esc 0 b i tilde_len; i + tilde_len
  | '^' -> Bytes.blit_string circ_esc 0 b i circ_len; i + circ_len
  | '\\' -> Bytes.blit_string bslash_esc 0 b i bslash_len; i + bslash_len
  | c -> Bytes.set b i c; i + 1
  in
  byte_escaper char_len set_char

(* TODO unescape on decode. *)

type t =
  { type' : string;
    cite_key : string;
    fields : string SM.t; }

let v ~type' ~cite_key ~fields () = { type'; cite_key; fields }

let type' e = e.type'
let cite_key e = e.cite_key
let fields e = e.fields

let pp ppf e =
  let pp_field ppf (k, v) = Fmt.pf ppf "@[<h>%s = {%s}@]" k (escape v) in
  Fmt.pf ppf "@[<v2>@%s{%s,@,%a}@]" e.type' e.cite_key
    (Fmt.iter_bindings ~sep:Fmt.comma SM.iter pp_field) e.fields

(* Field values *)

let list_value s =
  List.filter (fun s -> s <> "") @@
  List.map String.trim (String.split_on_char ',' s)

let doi e = match SM.find_opt "doi" e.fields with
| None -> None
| Some doi ->
    let ret doi = match String.trim doi with
    | "" -> None
    | doi -> Some doi
    in
    (* chop scheme and authority in case there is one *)
    match Url.scheme doi with
    | None -> ret doi
    | Some _ ->
        match Url.path doi with
        | None -> ret doi
        | Some p -> ret p

let keywords e = Option.map list_value (SM.find_opt "keywords" e.fields)
let annote e = SM.find_opt "annote" e.fields

(* Codec *)

type error_kind = string
type error = error_kind * Tloc.t

let pp_error ppf (err, l) =
  Fmt.pf ppf "@[<v>%a:@,%a: %s@]"
    Tloc.pp l Fmt.string "Error" err

let curr_char d = (* TODO better escaping (this is for error reports) *)
  Tdec.tok_reset d; Tdec.tok_accept_uchar d; Tdec.tok_pop d

let err_illegal_uchar d = Tdec.err_here d "illegal character: %s" (curr_char d)
let err_illegal_byte d b = Tdec.err_here d "illegal character U+%04X" b
let err_expected d exp = Tdec.err_here d "expected %s" exp
let err_eoi msg d ~sbyte ~sline =
  Tdec.err_to_here d ~sbyte ~sline "end of input: %s" msg

let err_eoi_entry = err_eoi "unclosed BibTeX entry"
let err_eoi_field = err_eoi "unfinished BibTeX entry field"
let err_eoi_value = err_eoi "unfinished BibTeX field value"

let dec_byte d = match Tdec.byte d with
| c when 0x00 <= c && c <= 0x08 || 0x0E <= c && c <= 0x1F || c = 0x7F ->
    err_illegal_byte d c
| c -> c
[@@ ocaml.inline]

let rec skip_white d = match dec_byte d with
| 0x20 | 0x09 | 0x0A | 0x0B | 0x0C | 0x0D -> Tdec.accept_byte d; skip_white d
| _ -> ()

let dec_token ~stop d =
  let rec loop d = match dec_byte d with
  | 0x28 | 0x29 | 0x3B | 0x22
  | 0x20 | 0x09 | 0x0A | 0x0B | 0x0C | 0x0D
  | 0xFFFF -> Tdec.tok_pop d
  | c when c = stop -> Tdec.tok_pop d
  | _ -> Tdec.tok_accept_uchar d; loop d
  in
  loop d

let rec dec_string ~sbyte ~sline ~stop d = match dec_byte d with
| 0xFFFF -> err_eoi_value ~sbyte ~sline d
| c when c = stop -> Tdec.accept_byte d; Tdec.tok_pop d
| _ -> Tdec.tok_accept_uchar d; dec_string ~sbyte ~sline ~stop d

let rec dec_tex i ~sbyte ~sline d = match dec_byte d with
| 0xFFFF -> err_eoi_value ~sbyte ~sline d
| 0x007D ->
    if i = 0 then (Tdec.accept_byte d; Tdec.tok_pop d) else
    (Tdec.tok_accept_uchar d; dec_tex (i - 1) ~sbyte ~sline d)
| c ->
    let i = if c = 0x007B then i + 1 else i in
    Tdec.tok_accept_uchar d; dec_tex i ~sbyte ~sline d

let dec_value d =
  let sbyte = Tdec.pos d and sline = Tdec.line d in
  match dec_byte d with
  | 0x007B (* { *) -> Tdec.accept_byte d; dec_tex 0 ~sbyte ~sline d
  | 0x0022 -> Tdec.accept_byte d; dec_string ~sbyte ~sline ~stop:0x0022 d
  | _ -> dec_token ~stop:0x002C d

let dec_field d acc =
  let sbyte = Tdec.pos d and sline = Tdec.line d in
  let id = dec_token ~stop:0x003D (* = *) d in
  skip_white d;
  match dec_byte d with
  | 0xFFFF -> err_eoi_field ~sbyte ~sline d
  | 0x003D (* = *) ->
      Tdec.accept_byte d;
      skip_white d;
      begin match dec_byte d with
      | 0xFFFF -> err_eoi_field ~sbyte ~sline d
      | _ ->
          SM.add (String.lowercase_ascii id) (dec_value d) acc
      end
  | _ -> err_expected d "'='"

let rec dec_fields ~sbyte ~sline d acc =
  skip_white d;
  match dec_byte d with
  | 0xFFFF -> err_eoi_entry ~sbyte ~sline d
  | 0x007D (* } *) -> acc
  | _ ->
      let acc = dec_field d acc in
      skip_white d;
      match dec_byte d with
      | 0x002C (* , *) -> Tdec.accept_byte d; dec_fields ~sbyte ~sline d acc
      | 0x007D (* } *) -> acc
      | 0xFFFF -> err_eoi_entry ~sbyte ~sline d
      | _ -> err_expected d "',' or '}'"

let dec_entry d =
  let sbyte = Tdec.pos d and sline = Tdec.line d in
  Tdec.accept_byte d (* @ *);
  let type' = dec_token ~stop:0x007B d (* { *) in
  match dec_byte d with
  | 0x007B ->
      Tdec.accept_byte d;
      let cite_key = dec_token ~stop:0x002C d (* , *) in
      skip_white d;
      begin match dec_byte d with
      | 0x002C (* , *) ->
          Tdec.accept_byte d;
          let fields = dec_fields ~sbyte ~sline d SM.empty in
          Tdec.accept_byte d;
          { type'; cite_key; fields }
      | _ -> err_expected d "','"
      end
  | _ -> err_expected d "'{'"

let dec_entries d =
  let rec loop d acc =
    skip_white d;
    match dec_byte d with
    | 0x0040 (* @ *) -> loop d (dec_entry d :: acc)
    | 0xFFFF -> List.rev acc
    | _ -> err_illegal_uchar d
  in
  loop d []

let of_string ?(file = Fpath.v "-") s =
  try
    let file = Fpath.to_string file in
    let d = Tdec.create ~file s in
    Ok (dec_entries d)
  with Tdec.Err (loc, msg) -> Error (msg, loc)

let of_string' ?file s =
  Result.map_error (fun e -> Fmt.str "%a" pp_error e) @@
  (of_string ?file s)

let to_string es = Fmt.str "@[<v>%a@]" (Fmt.list pp) es

(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
