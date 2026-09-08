let case_fold_utf_8 value =
  let buffer = Buffer.create (String.length value) in
  let rec loop index =
    if index < String.length value then (
      let decoded = String.get_utf_8_uchar value index in
      let uchar = Uchar.utf_decode_uchar decoded in
      (match Uucp.Case.Fold.fold uchar with
      | `Self -> Buffer.add_utf_8_uchar buffer uchar
      | `Uchars chars -> List.iter (Buffer.add_utf_8_uchar buffer) chars);
      loop (index + Uchar.utf_decode_length decoded))
  in
  loop 0;
  Buffer.contents buffer

let is_combining_mark uchar =
  match Uucp.Gc.general_category uchar with
  | `Mn | `Mc | `Me -> true
  | _ -> false

let drop_combining_marks value =
  let buffer = Buffer.create (String.length value) in
  let rec loop index =
    if index < String.length value then (
      let decoded = String.get_utf_8_uchar value index in
      let uchar = Uchar.utf_decode_uchar decoded in
      if not (is_combining_mark uchar) then Buffer.add_utf_8_uchar buffer uchar;
      loop (index + Uchar.utf_decode_length decoded))
  in
  loop 0;
  Buffer.contents buffer

let search_key value =
  value
  |> Uunf_string.normalize_utf_8 `NFKD
  |> case_fold_utf_8
  |> Uunf_string.normalize_utf_8 `NFKD
  |> drop_combining_marks

let contains_bytes ~haystack ~needle =
  let haystack_length = String.length haystack in
  let needle_length = String.length needle in
  let rec at offset index =
    index = needle_length
    || (haystack.[offset + index] = needle.[index] && at offset (index + 1))
  in
  let rec search offset =
    needle_length = 0
    || offset + needle_length <= haystack_length
       && (at offset 0 || search (offset + 1))
  in
  search 0

let contains ~haystack ~needle =
  contains_bytes ~haystack:(search_key haystack) ~needle:(search_key needle)

(* There is no camel-case bonus, because [search_key] has folded case away
   before the scorer sees the text. *)

let score_match = 16
let score_gap_start = -3
let score_gap_extension = -1
let bonus_boundary = score_match / 2
let bonus_consecutive = -(score_gap_start + score_gap_extension)
let bonus_first_char_multiplier = 2

(* Far enough below any real score that adding a bounded penalty to it can
   never reach one, and never near enough to [min_int] to overflow. *)
let unreachable = min_int / 4

let ( +? ) value delta =
  if value <= unreachable then unreachable else value + delta

let uchars value =
  let rec loop index acc =
    if index >= String.length value then List.rev acc
    else
      let decoded = String.get_utf_8_uchar value index in
      loop
        (index + Uchar.utf_decode_length decoded)
        (Uchar.utf_decode_uchar decoded :: acc)
  in
  Array.of_list (loop 0 [])

let is_word uchar =
  match Uucp.Gc.general_category uchar with
  | `Ll | `Lu | `Lt | `Lm | `Lo | `Nd | `Nl | `No -> true
  | _ -> false

let fuzzy_score ~haystack ~needle =
  let pattern = uchars (search_key needle) in
  let subject = uchars (search_key haystack) in
  let m = Array.length pattern and n = Array.length subject in
  if m = 0 then Some 0
  else if n < m then None
  else begin
    let bonus =
      Array.init n (fun j ->
          if not (is_word subject.(j)) then 0
          else if j = 0 || not (is_word subject.(j - 1)) then bonus_boundary
          else 0)
    in
    (* [best] is the score of an alignment whose last matched pattern
       character sits at this column; [reach] is the same, relaxed to allow
       the alignment to end in a gap. *)
    let best = Array.make n unreachable in
    let reach = Array.make n unreachable in
    let best_above = Array.make n unreachable in
    let reach_above = Array.make n unreachable in
    for i = 0 to m - 1 do
      Array.blit best 0 best_above 0 n;
      Array.blit reach 0 reach_above 0 n;
      let in_gap = ref false in
      for j = 0 to n - 1 do
        best.(j) <-
          (if not (Uchar.equal pattern.(i) subject.(j)) then unreachable
           else if i = 0 then
             score_match + (bonus.(j) * bonus_first_char_multiplier)
           else if j = 0 then unreachable
           else
             (* Either the previous pattern character matched at [j - 1] — a
                consecutive run, which earns at least [bonus_consecutive] —
                or it matched further left, across a gap. *)
             Int.max
               (best_above.(j - 1) +? Int.max bonus.(j) bonus_consecutive)
               (reach_above.(j - 1) +? bonus.(j))
             +? score_match);
        if j = 0 then (
          reach.(0) <- best.(0);
          in_gap := false)
        else
          let across =
            reach.(j - 1)
            +? if !in_gap then score_gap_extension else score_gap_start
          in
          if best.(j) >= across then (
            reach.(j) <- best.(j);
            in_gap := false)
          else (
            reach.(j) <- across;
            in_gap := true)
      done
    done;
    let total = Array.fold_left Int.max unreachable best in
    if total <= unreachable then None else Some total
  end

let graphemes value =
  Uuseg_string.fold_utf_8 `Grapheme_cluster
    (fun segments segment -> segment :: segments)
    [] value
  |> List.rev

let truncate_graphemes ~max value =
  if max < 0 then invalid_arg "Matrix_ui.Matching.truncate_graphemes";
  graphemes value |> List.to_seq |> Seq.take max |> List.of_seq
  |> String.concat ""
