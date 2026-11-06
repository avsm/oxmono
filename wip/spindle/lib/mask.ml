(* SPDX-License-Identifier: ISC *)

(* [aligned value] is the base64 text of [value] wherever it starts in a
   longer encoded stream. After [k] earlier bytes, the first characters also
   encode those bytes and a last partial group encodes later ones, so only
   the characters that encode [value] alone are kept. Short ones would hide
   ordinary text, so they are left out. *)
let aligned value =
  List.filter_map
    (fun k ->
      let e = Base64.encode_string ~pad:false (String.make k '\000' ^ value) in
      let skip = [| 0; 2; 3 |].(k) in
      let last = if (k + String.length value) mod 3 = 0 then 0 else 1 in
      let n = String.length e - skip - last in
      if n >= 8 then Some (String.sub e skip n) else None)
    [ 0; 1; 2 ]

let patterns values =
  List.concat_map
    (fun value ->
      if value = "" then []
      else
        let lines =
          String.split_on_char '\n' value
          |> List.concat_map (String.split_on_char '\r')
          |> List.filter (( <> ) "")
        in
        (value :: lines)
        @ [ Base64.encode_string value; Base64.encode_string ~pad:false value ]
        @ aligned value)
    values
  |> List.sort_uniq (fun a b ->
      (* Longest first, so that a value wins over its own lines. *)
      match compare (String.length b) (String.length a) with
      | 0 -> compare a b
      | c -> c)

(* Patterns are indexed by their first byte, longest first in each list, so
   that each position of the text is compared only with patterns that can
   start there. *)
type t = { by_first : string list array; empty : bool; mutable pending : string }

let v values =
  let by_first = Array.make 256 [] in
  let patterns = patterns values in
  List.iter
    (fun p ->
      let c = Char.code p.[0] in
      by_first.(c) <- p :: by_first.(c))
    (List.rev patterns);
  { by_first; empty = patterns = []; pending = "" }

(* [same s i p n] holds when the [n] bytes of [s] from [i] are those of [p]. *)
let same s i p n =
  let rec go k = k >= n || (s.[i + k] = p.[k] && go (k + 1)) in
  go 0

let starts_at s i p =
  String.length p <= String.length s - i && same s i p (String.length p)

let partial s i p =
  let left = String.length s - i in
  left < String.length p && same s i p left

(* Before more text arrives, a longer pattern that may continue past the end
   of [s] is waited for even where a shorter one is complete, so that the
   longest pattern wins across chunks as it does within one. *)
let replace t ~eof s =
  let out = Buffer.create (String.length s) in
  let rec go i =
    if i >= String.length s then ""
    else
      let candidates = t.by_first.(Char.code s.[i]) in
      if (not eof) && List.exists (partial s i) candidates then
        String.sub s i (String.length s - i)
      else
        match List.find_opt (starts_at s i) candidates with
        | Some p ->
            Buffer.add_string out "***";
            go (i + String.length p)
        | None ->
            Buffer.add_char out s.[i];
            go (i + 1)
  in
  let rest = go 0 in
  (Buffer.contents out, rest)

let feed t chunk =
  if t.empty then chunk
  else
    let text, rest = replace t ~eof:false (t.pending ^ chunk) in
    t.pending <- rest;
    text

let finish t =
  let text, _ = replace t ~eof:true t.pending in
  t.pending <- "";
  text

let hide values s =
  let t = v values in
  if t.empty then s else fst (replace t ~eof:true s)
