(* SPDX-License-Identifier: ISC *)

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
        @ [ Base64.encode_string value; Base64.encode_string ~pad:false value ])
    values
  |> List.sort_uniq (fun a b ->
      (* Longest first, so that a value wins over its own lines. *)
      match compare (String.length b) (String.length a) with
      | 0 -> compare a b
      | c -> c)

type t = { patterns : string list; mutable pending : string }

let v values = { patterns = patterns values; pending = "" }

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
    else if (not eof) && List.exists (partial s i) t.patterns then
      String.sub s i (String.length s - i)
    else
      match List.find_opt (starts_at s i) t.patterns with
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
  if t.patterns = [] then chunk
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
  if t.patterns = [] then s else fst (replace t ~eof:true s)
