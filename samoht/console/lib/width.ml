(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* ANSI escape sequence detection *)
type ansi_state = Normal | Escape | Csi | Osc | Osc_escape

let next_ansi_state state c =
  match state with
  | Normal -> if c = 0x1b then Escape else Normal
  | Escape ->
      if c = 0x5b (* '[' *) then Csi
      else if c = 0x5d (* ']' *) then Osc
      else Normal
  | Csi ->
      (* CSI sequence ends with a byte in the range 0x40-0x7E. *)
      if c >= 0x40 && c <= 0x7e then Normal else Csi
  | Osc ->
      if c = 0x07 (* BEL *) then Normal
      else if c = 0x1b then Osc_escape
      else Osc
  | Osc_escape -> if c = 0x5c (* '\\' *) then Normal else Osc

(* The per-codepoint fallback used for an explicitly supplied width function.
   Default string measurement below is grapheme-aware: terminals render a
   cluster, not the sum of its scalar values. *)
let default_char_width u =
  match Uucp.Break.tty_width_hint u with -1 -> 0 | width -> width

let update_width_state ~char_width ~width ~state ~zwj u =
  let c = Uchar.to_int u in
  match !state with
  | Normal ->
      if c = 0x1b then state := next_ansi_state Normal c
      else if c = 0x200d (* zero-width joiner *) then
        (* The next glyph fuses into this grapheme cluster -- an emoji ZWJ
           sequence like a family renders as a single cell, not the sum of its
           parts -- so it must add no width. *)
        zwj := true
      else if !zwj then zwj := false
      else width := !width + char_width u
  | current -> state := next_ansi_state current c

let codepoint_string_width ~char_width str =
  let len = String.length str in
  let width = ref 0 in
  let state = ref Normal in
  let zwj = ref false in
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String str) in
  let rec loop () =
    match Uutf.decode decoder with
    | `Uchar u ->
        update_width_state ~char_width ~width ~state ~zwj u;
        loop ()
    | `End -> ()
    | `Await -> assert false
    | `Malformed _ ->
        update_width_state ~char_width ~width ~state ~zwj Uutf.u_rep;
        loop ()
  in
  if len = 0 then 0
  else (
    loop ();
    !width)

let ansi_end str start =
  let len = String.length str in
  if start + 1 >= len then len
  else
    match str.[start + 1] with
    | '[' ->
        let rec csi i =
          if i >= len then len
          else
            let c = Char.code str.[i] in
            if c >= 0x40 && c <= 0x7e then i + 1 else csi (i + 1)
        in
        csi (start + 2)
    | ']' ->
        let rec osc i =
          if i >= len then len
          else if str.[i] = '\007' then i + 1
          else if i + 1 < len && str.[i] = '\027' && str.[i + 1] = '\\' then
            i + 2
          else osc (i + 1)
        in
        osc (start + 2)
    | next when Char.code next >= 0x80 ->
        let decoded = String.get_utf_8_uchar str (start + 1) in
        min len (start + 1 + Uchar.utf_decode_length decoded)
    | _ -> min len (start + 2)

let grapheme_string_width str =
  (* Styling controls do not break a terminal grapheme. Strip them before
     segmentation so a combining mark or ZWJ sequence spanning an ANSI reset
     has exactly the same width here as it does in the terminal. Measuring each
     visible run separately over-counts standalone combining marks. *)
  let visible = Buffer.create (String.length str) in
  let rec copy i =
    if i >= String.length str then ()
    else if str.[i] = '\027' then copy (ansi_end str i)
    else begin
      Buffer.add_char visible str.[i];
      copy (i + 1)
    end
  in
  copy 0;
  Glyph.String.measure ~width_method:`Unicode ~tab_width:2
    (Buffer.contents visible)

let string_width ?(char_width = default_char_width) str =
  if char_width == default_char_width then grapheme_string_width str
  else codepoint_string_width ~char_width str

let handle_truncate_normal ~char_width ~buf ~width ~target_width ~state u =
  let c = Uchar.to_int u in
  if c = 0x1b then begin
    state := Escape;
    Uutf.Buffer.add_utf_8 buf u;
    `Continue
  end
  else
    let w = char_width u in
    if !width + w <= target_width then begin
      width := !width + w;
      Uutf.Buffer.add_utf_8 buf u;
      `Continue
    end
    else `Stop

let handle_truncate_ansi ~buf ~state u =
  let c = Uchar.to_int u in
  Uutf.Buffer.add_utf_8 buf u;
  state := next_ansi_state !state c;
  `Continue

let handle_truncate_uchar ~char_width ~buf ~width ~target_width ~state u =
  match !state with
  | Normal ->
      handle_truncate_normal ~char_width ~buf ~width ~target_width ~state u
  | Escape | Csi | Osc | Osc_escape -> handle_truncate_ansi ~buf ~state u

let rec truncate_loop ~char_width decoder ~buf ~width ~target_width ~state =
  match Uutf.decode decoder with
  | `Uchar u -> (
      match
        handle_truncate_uchar ~char_width ~buf ~width ~target_width ~state u
      with
      | `Continue ->
          truncate_loop ~char_width decoder ~buf ~width ~target_width ~state
      | `Stop -> true)
  | `End -> false
  | `Await -> assert false
  | `Malformed _ -> (
      match
        handle_truncate_uchar ~char_width ~buf ~width ~target_width ~state
          Uutf.u_rep
      with
      | `Continue ->
          truncate_loop ~char_width decoder ~buf ~width ~target_width ~state
      | `Stop -> true)

let starts_at str prefix at =
  let plen = String.length prefix in
  at + plen <= String.length str && String.sub str at plen = prefix

let contains str prefix =
  let rec loop at =
    at + String.length prefix <= String.length str
    && (starts_at str prefix at || loop (at + 1))
  in
  loop 0

let hyperlink_is_open str =
  let prefix = "\027]8;;" and terminator = "\027\\" in
  let rec find_terminator at =
    if at + 1 >= String.length str then None
    else if starts_at str terminator at then Some at
    else find_terminator (at + 1)
  in
  let rec loop at open_ =
    if at + String.length prefix > String.length str then open_
    else if starts_at str prefix at then
      let uri_start = at + String.length prefix in
      match find_terminator uri_start with
      | None -> true
      | Some stop -> loop (stop + String.length terminator) (stop > uri_start)
    else loop (at + 1) open_
  in
  loop 0 false

let normalize_utf8 str =
  let buffer = Buffer.create (String.length str) in
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String str) in
  let rec loop () =
    match Uutf.decode decoder with
    | `Uchar u ->
        Uutf.Buffer.add_utf_8 buffer u;
        loop ()
    | `Malformed _ ->
        Uutf.Buffer.add_utf_8 buffer Uutf.u_rep;
        loop ()
    | `End -> Buffer.contents buffer
    | `Await -> assert false
  in
  loop ()

(* Where the next escape sequence begins, so a run of drawable text can be
   segmented on its own. *)
let visible_run_end str start =
  let rec loop i =
    if i >= String.length str || str.[i] = '\027' then i else loop (i + 1)
  in
  loop start

let truncate_graphemes target_width str =
  let str = normalize_utf8 str in
  let buffer = Buffer.create (String.length str) in
  let width = ref 0 and stopped = ref false in
  let append_visible run =
    Glyph.String.iter_graphemes
      (fun ~offset ~len ->
        if not !stopped then
          let grapheme_width =
            Glyph.String.measure_sub ~width_method:`Unicode ~tab_width:2 run
              ~pos:offset ~len
          in
          if !width + grapheme_width <= target_width then (
            Buffer.add_substring buffer run offset len;
            width := !width + grapheme_width)
          else stopped := true)
      run
  in
  let rec loop i =
    if !stopped || i >= String.length str then ()
    else if str.[i] = '\027' then (
      let stop = ansi_end str i in
      Buffer.add_substring buffer str i (stop - i);
      loop stop)
    else
      let stop = visible_run_end str i in
      append_visible (String.sub str i (stop - i));
      loop stop
  in
  loop 0;
  if !stopped then begin
    let rendered = Buffer.contents buffer in
    if contains rendered "\027[" then Buffer.add_string buffer "\027[0m";
    if hyperlink_is_open rendered then Buffer.add_string buffer "\027]8;;\027\\"
  end;
  Buffer.contents buffer

let truncate_codepoints ~char_width target_width str =
  if target_width <= 0 then ""
  else
    let buf = Buffer.create (String.length str) in
    let width = ref 0 in
    let state = ref Normal in
    let decoder = Uutf.decoder ~encoding:`UTF_8 (`String str) in
    let truncated =
      truncate_loop ~char_width decoder ~buf ~width ~target_width ~state
    in
    if truncated then begin
      let rendered = Buffer.contents buf in
      if contains rendered "\027[" then Buffer.add_string buf "\027[0m";
      if hyperlink_is_open rendered then Buffer.add_string buf "\027]8;;\027\\"
    end;
    Buffer.contents buf

let truncate ?(char_width = default_char_width) target_width str =
  if target_width <= 0 then ""
  else if char_width == default_char_width then
    truncate_graphemes target_width str
  else truncate_codepoints ~char_width target_width str

let ellipsis = "\xe2\x80\xa6"

(* [str] as the pieces a terminal draws it in: an escape sequence, which takes
   no column and is never cut, or one grapheme cluster with the width it
   occupies. Fitting a value by its middle has to take from both of its ends,
   which neither direction of truncation on its own can do. *)
let atoms ~char_width str =
  let str = normalize_utf8 str in
  let pieces = ref [] in
  let push piece width = pieces := (piece, width) :: !pieces in
  let codepoints run =
    let decoder = Uutf.decoder ~encoding:`UTF_8 (`String run) in
    let rec loop () =
      match Uutf.decode decoder with
      | `End -> ()
      | `Await -> assert false
      | (`Uchar _ | `Malformed _) as decoded ->
          let u =
            match decoded with `Uchar u -> u | `Malformed _ -> Uutf.u_rep
          in
          let buffer = Buffer.create 4 in
          Uutf.Buffer.add_utf_8 buffer u;
          push (Buffer.contents buffer) (char_width u);
          loop ()
    in
    loop ()
  in
  let graphemes run =
    Glyph.String.iter_graphemes
      (fun ~offset ~len ->
        push
          (String.sub run offset len)
          (Glyph.String.measure_sub ~width_method:`Unicode ~tab_width:2 run
             ~pos:offset ~len))
      run
  in
  let rec loop i =
    if i >= String.length str then ()
    else if str.[i] = '\027' then begin
      let stop = ansi_end str i in
      push (String.sub str i (stop - i)) 0;
      loop stop
    end
    else
      let stop = visible_run_end str i in
      let run = String.sub str i (stop - i) in
      if char_width == default_char_width then graphemes run else codepoints run;
      loop stop
  in
  loop 0;
  List.rev !pieces

(* The pieces of [atoms], in the order they are given, that fit in [target]
   columns. Reversed atoms therefore yield the value's last [target] columns. *)
let take target atoms =
  let width = ref 0 and stopped = ref false in
  let kept =
    List.filter_map
      (fun (piece, piece_width) ->
        if !stopped then None
        else if !width + piece_width <= target then begin
          width := !width + piece_width;
          Some piece
        end
        else begin
          stopped := true;
          None
        end)
      atoms
  in
  kept

(* Whatever the cut left open, closed. *)
let closing rendered =
  let buffer = Buffer.create 8 in
  if contains rendered "\027[" then Buffer.add_string buffer "\027[0m";
  if hyperlink_is_open rendered then Buffer.add_string buffer "\027]8;;\027\\";
  Buffer.contents buffer

let ellipsize ?(char_width = default_char_width) ?(at = `Middle) target_width
    str =
  if target_width <= 0 then ""
  else if string_width ~char_width str <= target_width then str
  else if target_width = 1 then ellipsis
  else
    let atoms = atoms ~char_width str in
    let keep = target_width - 1 in
    match at with
    | `End ->
        let head = String.concat "" (take keep atoms) in
        String.concat "" [ head; closing head; ellipsis ]
    | `Middle ->
        let head_width = (keep + 1) / 2 in
        let head = String.concat "" (take head_width atoms) in
        let tail =
          String.concat ""
            (List.rev (take (keep - head_width) (List.rev atoms)))
        in
        String.concat "" [ head; closing head; ellipsis; tail; closing tail ]

let is_space (piece, _) = String.equal piece " "

let is_escape (piece, width) =
  width = 0 && String.length piece > 0 && piece.[0] = '\027'

(* Where a row may lose its end: a space outside any bracket, so a group such
   as "(300s bound)" goes as one word and never leaves "(300s" behind. A
   bracket that never closes groups nothing, and every space is a break. *)
let breaks atoms =
  let depth = ref 0 in
  let grouped =
    List.map
      (fun ((piece, _) as atom) ->
        (match piece with
        | "(" | "[" | "{" -> incr depth
        | ")" | "]" | "}" -> depth := max 0 (!depth - 1)
        | _ -> ());
        (atom, !depth = 0 && is_space atom))
      atoms
  in
  if !depth = 0 then grouped
  else List.map (fun atom -> (atom, is_space atom)) atoms

(* A run of words that lost its tail ends on a word, not on the separator the
   next word needed. *)
let rec trim_end = function
  | (piece, _) :: rest
    when String.equal piece " " || String.equal piece ","
         || String.equal piece ";" ->
      trim_end rest
  | atoms -> atoms

(* The words that only lead into the words after them: a row that lost those
   words does not end on one ("building in", "waiting for"). *)
let leading_words =
  [
    "a";
    "an";
    "and";
    "as";
    "at";
    "by";
    "for";
    "from";
    "in";
    "into";
    "of";
    "on";
    "or";
    "the";
    "to";
    "via";
    "with";
  ]

(* [kept_rev], reversed atoms, without the separators and the leading words at
   its end, nor a colon that introduced what went. *)
let rec settle kept_rev =
  match trim_end kept_rev with
  | (":", _) :: rest -> settle rest
  | kept_rev -> (
      let rec last_word word = function
        | atom :: rest when not (is_space atom) -> last_word (atom :: word) rest
        | rest -> (word, rest)
      in
      let word, rest = last_word [] kept_rev in
      let text =
        String.lowercase_ascii (String.concat "" (List.map fst word))
      in
      match rest with
      | _ :: _ when List.mem text leading_words -> settle rest
      | _ -> kept_rev)

let shorten ?(char_width = default_char_width) target_width str =
  if string_width ~char_width str <= target_width then str
  else if target_width <= 0 then ""
  else
    let rec walk kept_rev width best = function
      | [] -> best
      | (((_, piece_width) as atom), break) :: rest ->
          let best =
            if break && width <= target_width then kept_rev else best
          in
          if width > target_width then best
          else walk (atom :: kept_rev) (width + piece_width) best rest
    in
    let kept_rev = walk [] 0 [] (breaks (atoms ~char_width str)) in
    match List.rev (settle kept_rev) with
    | [] -> ""
    | kept ->
        let head = String.concat "" (List.map fst kept) in
        head ^ closing head

(* The escapes still in force after [piece]: a reset ends every style, a
   hyperlink's close ends the hyperlink, and anything else is added. *)
let in_force active piece =
  let sgr p = String.length p > 1 && p.[1] = '[' in
  let link p = String.starts_with ~prefix:"\027]8;" p in
  let closes_link p =
    String.equal p "\027]8;;\027\\" || String.equal p "\027]8;;\007"
  in
  if String.equal piece "\027[0m" || String.equal piece "\027[m" then
    List.filter (fun p -> not (sgr p)) active
  else if closes_link piece then List.filter (fun p -> not (link p)) active
  else active @ [ piece ]

(* [atoms] as runs of words and runs of spaces, each with its width. *)
let runs atoms =
  let flush current acc =
    match current with
    | [] -> acc
    | _ :: _ as run ->
        let run = List.rev run in
        (run, List.fold_left (fun n (_, w) -> n + w) 0 run) :: acc
  in
  let rec go current kind acc = function
    | [] -> List.rev (flush current acc)
    | atom :: rest ->
        let space = is_space atom in
        if (not (is_escape atom)) && space <> kind && current <> [] then
          go [ atom ] space (flush current acc) rest
        else if current = [] then go [ atom ] space acc rest
        else go (atom :: current) kind acc rest
  in
  go [] false [] atoms

(* The line [fold] is filling: its atoms, newest first, and its width; the
   lines it has finished, newest first; and the escapes in force, at the end
   of the line and where it began. *)
type folding = {
  mutable lines : string list;
  mutable line : (string * int) list;
  mutable width : int;
  mutable active : string list;
  mutable opened : string list;
}

let add f ((piece, piece_width) as atom) =
  f.line <- atom :: f.line;
  f.width <- f.width + piece_width;
  if is_escape atom then f.active <- in_force f.active piece

let finish f =
  let text = String.concat "" (List.rev_map fst (trim_end f.line)) in
  let text = String.concat "" f.opened ^ text in
  f.lines <- (text ^ closing (String.concat "" f.active)) :: f.lines;
  f.line <- [];
  f.width <- 0;
  f.opened <- f.active

let blank f = List.for_all is_escape f.line
let run_width run = List.fold_left (fun n (_, w) -> n + w) 0 run

(* The atoms of a word wider than a line that fit on the line under way,
   which starts blank; a glyph wider than the whole line goes on alone. What
   is left of the word is returned. *)
let rec fill f target_width = function
  | [] -> []
  | ((_, w) as atom) :: rest ->
      if f.width + w <= target_width || (blank f && w > target_width) then begin
        add f atom;
        fill f target_width rest
      end
      else atom :: rest

let rec place f ~split target_width run =
  let fits = f.width + run_width run <= target_width in
  match run with
  | [] -> ()
  | first :: _ when is_space first ->
      (* A line after a break starts at its word, not at the spaces the break
         stood on; the first line keeps its indentation. *)
      if blank f && f.lines <> [] then ()
      else if fits then List.iter (add f) run
      else if not (blank f) then finish f
  | _ when fits -> List.iter (add f) run
  | _ when not (blank f) ->
      finish f;
      place f ~split target_width run
  | _ when not split ->
      List.iter (add f) run;
      finish f
  | _ -> (
      (* The word alone is wider than a line: it goes on, whole, at the start
         of the next one. *)
      match fill f target_width run with
      | [] -> ()
      | rest ->
          finish f;
          place f ~split target_width rest)

let fold ?(char_width = default_char_width) ?(split = false) target_width str =
  if target_width <= 0 || string_width ~char_width str <= target_width then
    [ str ]
  else
    let f = { lines = []; line = []; width = 0; active = []; opened = [] } in
    List.iter
      (fun (run, _) -> place f ~split target_width run)
      (runs (atoms ~char_width str));
    if (not (blank f)) || f.lines = [] then finish f;
    List.rev f.lines

let pad_right ?(char_width = default_char_width) target_width str =
  let str_width = string_width ~char_width str in
  if str_width >= target_width then str
  else str ^ String.make (target_width - str_width) ' '

let pad_left ?(char_width = default_char_width) target_width str =
  let str_width = string_width ~char_width str in
  if str_width >= target_width then str
  else String.make (target_width - str_width) ' ' ^ str

let center ?(char_width = default_char_width) target_width str =
  let str_width = string_width ~char_width str in
  if str_width >= target_width then str
  else
    let total_pad = target_width - str_width in
    let left_pad = total_pad / 2 in
    let right_pad = total_pad - left_pad in
    String.make left_pad ' ' ^ str ^ String.make right_pad ' '

let rec wrap_words ~char_width ~effective acc line len = function
  | [] -> if line = "" then acc else line :: acc
  | word :: rest ->
      let wlen = string_width ~char_width word in
      let space = if line = "" then 0 else 1 in
      if len + space + wlen <= effective then
        let line = if line = "" then word else line ^ " " ^ word in
        wrap_words ~char_width ~effective acc line (len + space + wlen) rest
      else if line = "" then
        (* Word longer than width; accept it on its own line *)
        wrap_words ~char_width ~effective (word :: acc) "" 0 rest
      else wrap_words ~char_width ~effective (line :: acc) word wlen rest

(* A backquoted run is one word: a row never ends inside a command the reader
   copies back. A backquote that never closes groups nothing. *)
let backquoted words =
  let odd word =
    String.fold_left (fun n c -> if c = '`' then n + 1 else n) 0 word mod 2 = 1
  in
  let rec close run = function
    | [] -> None
    | word :: rest when odd word -> Some (List.rev (word :: run), rest)
    | word :: rest -> close (word :: run) rest
  in
  let rec go acc = function
    | [] -> List.rev acc
    | word :: rest when odd word -> (
        match close [ word ] rest with
        | Some (run, rest) -> go (String.concat " " run :: acc) rest
        | None -> go (word :: acc) rest)
    | word :: rest -> go (word :: acc) rest
  in
  go [] words

let wrap ?(char_width = default_char_width) ?(indent = 0) width text =
  let effective = width - indent in
  if effective <= 0 then text
  else
    let prefix = String.make indent ' ' in
    let normalized =
      text |> String.split_on_char '\n' |> List.map String.trim
      |> String.concat " "
    in
    let words = backquoted (String.split_on_char ' ' normalized) in
    let lines = List.rev (wrap_words ~char_width ~effective [] "" 0 words) in
    String.concat "\n" (List.map (fun l -> prefix ^ l) lines)
