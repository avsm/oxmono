(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* No closure: a width is measured for every atom of every frame. *)
let rec ascii_from s i n =
  i >= n
  || (Char.Ascii.is_print (String.unsafe_get s i) && ascii_from s (i + 1) n)

let is_ascii s = ascii_from s 0 (String.length s)

let is_ascii_sub s ~pos ~len =
  let rec go i = i >= pos + len || (Char.Ascii.is_print s.[i] && go (i + 1)) in
  go pos

(* A spacing mark is of general category Mc, or a SpacingMark of UAX #29
   that is not: Thai SARA AM U+0E33 and Lao AM U+0EB3, letters (Lo) that
   join their consonant's cluster and that terminals draw in a cell of
   their own. *)
let is_mark u =
  Uucp.Gc.general_category u = `Mc || Uucp.Break.grapheme_cluster u = `SM

let is_spacing_mark c = Uchar.is_valid c && is_mark (Uchar.unsafe_of_int c)

(* A character that is text by default and that VS16 shows as an emoji:
   emoji, with no emoji presentation, the bases of
   emoji-variation-sequences.txt. *)
let is_text_emoji u =
  Uucp.Emoji.is_emoji u && not (Uucp.Emoji.is_emoji_presentation u)

let is_regional_indicator u =
  let c = Uchar.to_int u in
  0x1f1e6 <= c && c <= 0x1f1ff

let vs16 = Uchar.of_int 0xfe0f

(* A character that no rule of UAX #29 but GB9b and GB999 reads starts a
   cluster after any character but a prepend, as printable ASCII does. *)
let is_alone u =
  Uucp.Break.grapheme_cluster u = `XX
  && Uucp.Break.indic_conjunct_break u = `None
  && not (Uucp.Emoji.is_extended_pictographic u)

(* What the widths read of a character, from uucp, in a byte: its cells
   (bits 0-1), whether it is a spacing mark (bit 2), a text emoji (bit 3)
   and {!is_alone} (bit 5), with bit 4 set. *)
let props_of u =
  let w = max 0 (Uucp.Break.tty_width_hint u) in
  w
  lor (if w > 0 && is_mark u then 4 else 0)
  lor (if is_text_emoji u then 8 else 0)
  lor 16
  lor if is_alone u then 32 else 0

(* The bytes of [props_of] for the first two planes, where most text is,
   each computed once as it is first read: uucp's lookups of general
   category and East Asian width take tens of nanoseconds and allocate. A
   byte read as 0 by a domain racing another's write is computed again. *)
let cached = Bytes.make 0x20000 '\000'

let props u =
  let c = Uchar.to_int u in
  if c >= 0x20000 then props_of u
  else
    let p = Char.code (Bytes.unsafe_get cached c) in
    if p <> 0 then p
    else
      let p = props_of u in
      Bytes.unsafe_set cached c (Char.unsafe_chr p);
      p

(* The character of [s] at [i], read no further than [stop], as
   [String.sub s i (stop - i)] reads it. *)
let decode s i stop =
  let d = String.get_utf_8_uchar s i in
  if i + Uchar.utf_decode_length d <= stop then d
  else String.get_utf_8_uchar (String.sub s i (stop - i)) 0

(* The cluster under way: where it starts, the cells of its widest
   character, those of its spacing marks, its regional indicators, whether
   a VS16 shows it as an emoji, and whether its last character is a text
   emoji. *)
type cluster = {
  mutable start : int;
  mutable widest : int;
  mutable marks : int;
  mutable ris : int;
  mutable emoji : bool;
  mutable after_text_emoji : bool;
}

(* A cluster takes the cells of its widest character, two for an emoji
   sequence, plus those of each spacing mark after its first character. *)
let cells c =
  (if c.emoji || c.ris >= 2 then max c.widest 2 else c.widest) + c.marks

let start c at =
  c.start <- at;
  c.widest <- 0;
  c.marks <- 0;
  c.ris <- 0;
  c.emoji <- false;
  c.after_text_emoji <- false

let take c u at =
  let p = props u in
  let w = p land 3 in
  if at > c.start && p land 4 <> 0 then c.marks <- c.marks + w
  else begin
    if c.after_text_emoji && Uchar.equal u vs16 then c.emoji <- true;
    if is_regional_indicator u then c.ris <- c.ris + 1;
    if w > c.widest then c.widest <- w
  end;
  c.after_text_emoji <- p land 8 <> 0

(* [segment f acc s ~pos ~len] folds [f] over the grapheme clusters
   (UAX #29) of [s] from [pos] for [len] bytes, given as their offset,
   length and cells. [out] is where the next character the segmenter gives
   back starts: the one just added at [i], of [n] bytes, or one before. *)
let segment f acc s ~pos ~len =
  let stop = pos + len in
  let seg = Uuseg.create `Grapheme_cluster in
  let c =
    {
      start = pos;
      widest = 0;
      marks = 0;
      ris = 0;
      emoji = false;
      after_text_emoji = false;
    }
  in
  let acc = ref acc and out = ref pos in
  let close at =
    if at > c.start then begin
      acc := f !acc ~offset:c.start ~len:(at - c.start) ~cells:(cells c);
      start c at
    end
  in
  let rec drain i n = function
    | `Boundary ->
        close !out;
        drain i n (Uuseg.add seg `Await)
    | `Uchar u ->
        take c u !out;
        out :=
          !out
          + if !out = i then n else Uchar.utf_decode_length (decode s !out stop);
        drain i n (Uuseg.add seg `Await)
    | `Await | `End -> ()
  in
  let rec go i =
    if i < stop then
      if
        i > pos && !out = i
        && Char.Ascii.is_print s.[i - 1]
        && Char.Ascii.is_print s.[i]
        && (i + 1 = stop || Char.Ascii.is_print s.[i + 1])
      then begin
        (* Printable ASCII between printable ASCII is a cluster of one
           cell (GB999), and the segmenter, whose last character is
           printable ASCII too, is in the state it would be after it. *)
        close i;
        acc := f !acc ~offset:i ~len:1 ~cells:1;
        start c (i + 1);
        out := i + 1;
        go (i + 1)
      end
      else begin
        let d = decode s i stop in
        let n = Uchar.utf_decode_length d in
        drain i n (Uuseg.add seg (`Uchar (Uchar.utf_decode_uchar d)));
        go (i + n)
      end
  in
  go pos;
  drain stop 0 (Uuseg.add seg `End);
  close stop;
  !acc

(* The bytes of [s] from [i] to [stop] are copies of the [k] at [pos]. No
   closure, as {!ascii_from}. *)
let rec copies s ~pos ~k j i stop =
  i >= stop
  || String.unsafe_get s i = String.unsafe_get s (pos + j)
     && copies s ~pos ~k (if j + 1 = k then 0 else j + 1) (i + 1) stop

(* A single character, such as a glyph of a frame, a bar or a spinner, is a
   cluster alone and takes its own cells, and so is each copy of one that
   {!is_alone}, such as the fill of a bar or a rule. *)
let fold_cells f acc s ~pos ~len =
  if len = 0 then acc
  else
    let d = decode s pos (pos + len) in
    let k = Uchar.utf_decode_length d in
    let p = props (Uchar.utf_decode_uchar d) in
    if k = len then f acc ~offset:pos ~len ~cells:(p land 3)
    else if
      p land 32 <> 0
      && Uchar.utf_decode_is_valid d
      && len mod k = 0
      && copies s ~pos ~k 0 (pos + k) (pos + len)
    then
      let rec each acc i =
        if i >= pos + len then acc
        else each (f acc ~offset:i ~len:k ~cells:(p land 3)) (i + k)
      in
      each acc pos
    else segment f acc s ~pos ~len

let iter_clusters f s =
  fold_cells
    (fun () ~offset ~len ~cells -> f ~offset ~len ~cells)
    () s ~pos:0 ~len:(String.length s)

let clusters s =
  List.rev
    (fold_cells
       (fun l ~offset ~len ~cells:_ -> String.sub s offset len :: l)
       [] s ~pos:0 ~len:(String.length s))

let sub_width s ~pos ~len =
  if is_ascii_sub s ~pos ~len then len
  else fold_cells (fun n ~offset:_ ~len:_ ~cells -> n + cells) 0 s ~pos ~len

(* The last character of [a], which ends with a byte that is not ASCII. *)
let last_char a =
  let n = String.length a in
  let rec ending i =
    if i < 0 || i < n - 4 then None
    else
      let d = String.get_utf_8_uchar a i in
      if Uchar.utf_decode_is_valid d && i + Uchar.utf_decode_length d = n then
        Some d
      else ending (i - 1)
  in
  match ending (n - 1) with
  | Some d -> d
  | None ->
      let rec last i d =
        if i >= n then d
        else
          let d = String.get_utf_8_uchar a i in
          last (i + Uchar.utf_decode_length d) d
      in
      last 0 (String.get_utf_8_uchar a 0)

(* Printable ASCII, and any character that {!is_alone}, starts a cluster
   after any character but a prepend (UAX #29 GB9b, GB999). *)
let joins a b =
  let n = String.length a in
  n > 0
  && String.length b > 0
  && ((not
         (Char.Ascii.is_print b.[0]
         ||
         let d = decode b 0 (String.length b) in
         Uchar.utf_decode_is_valid d
         && props (Uchar.utf_decode_uchar d) land 32 <> 0))
     || (not (Char.Ascii.is_print a.[n - 1]))
        &&
        let d = last_char a in
        (not (Uchar.utf_decode_is_valid d))
        || Uucp.Break.grapheme_cluster (Uchar.utf_decode_uchar d) = `PP)

let string_width s =
  if is_ascii s then String.length s
  else sub_width s ~pos:0 ~len:(String.length s)
