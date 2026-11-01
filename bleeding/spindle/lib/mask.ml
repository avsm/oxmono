(* SPDX-License-Identifier: ISC *)

let base64 s =
  let table =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  in
  let b = Buffer.create ((String.length s + 2) / 3 * 4) in
  let byte i = if i < String.length s then Char.code s.[i] else 0 in
  let rec go i =
    if i < String.length s then (
      let n = (byte i lsl 16) lor (byte (i + 1) lsl 8) lor byte (i + 2) in
      let left = String.length s - i in
      Buffer.add_char b table.[(n lsr 18) land 63];
      Buffer.add_char b table.[(n lsr 12) land 63];
      Buffer.add_char b (if left > 1 then table.[(n lsr 6) land 63] else '=');
      Buffer.add_char b (if left > 2 then table.[n land 63] else '=');
      go (i + 3))
  in
  go 0;
  Buffer.contents b

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
        let encoded = base64 value in
        let bare =
          let n = ref (String.length encoded) in
          while !n > 0 && encoded.[!n - 1] = '=' do
            decr n
          done;
          String.sub encoded 0 !n
        in
        (value :: lines) @ [ encoded; bare ])
    values
  |> List.sort_uniq (fun a b ->
      (* Longest first, so that a value wins over its own lines. *)
      match compare (String.length b) (String.length a) with
      | 0 -> compare a b
      | c -> c)

type t = { patterns : string list; mutable pending : string }

let v values = { patterns = patterns values; pending = "" }

let starts_at s i p =
  String.length p <= String.length s - i && String.sub s i (String.length p) = p

(* A pattern that may continue past the end of [s] from [i]. *)
let partial s i p =
  let left = String.length s - i in
  left < String.length p && String.sub p 0 left = String.sub s i left

let replace t ~eof s =
  let out = Buffer.create (String.length s) in
  let rec go i =
    if i >= String.length s then ""
    else
      match List.find_opt (starts_at s i) t.patterns with
      | Some p ->
          Buffer.add_string out "***";
          go (i + String.length p)
      | None ->
          if (not eof) && List.exists (partial s i) t.patterns then
            String.sub s i (String.length s - i)
          else (
            Buffer.add_char out s.[i];
            go (i + 1))
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
