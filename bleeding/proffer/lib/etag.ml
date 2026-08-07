(* An entity-tag is held unquoted. [to_string] adds the quotes and, for a weak
   tag, the "W/" prefix. The opaque value must not contain a double quote. *)

type t = [ `Strong of string | `Weak of string ]

let opaque = function `Strong s | `Weak s -> s
let to_string = function
  | `Strong s -> "\"" ^ s ^ "\""
  | `Weak s -> "W/\"" ^ s ^ "\""

(* Weak comparison per RFC 9110 section 8.8.3.2: the opaque values match and
   the strength is ignored. It is the only comparison a conditional GET needs.
   Strong comparison exists for Range requests, which are out of scope. *)
let weak_equal a b = String.equal (opaque a) (opaque b)

let trim s =
  let n = String.length s in
  let i = ref 0 and j = ref n in
  while !i < !j && (s.[!i] = ' ' || s.[!i] = '\t') do
    incr i
  done;
  while !j > !i && (s.[!j - 1] = ' ' || s.[!j - 1] = '\t') do
    decr j
  done;
  String.sub s !i (!j - !i)

(* [of_field_value s] is one entity-tag as it appears in an If-None-Match list.
   A tag that is not correctly quoted is rejected, which makes the whole
   condition fail to match and so sends the full response. *)
let of_field_value s =
  let s = trim s in
  let strong, body =
    if String.length s >= 2 && String.sub s 0 2 = "W/" then
      (false, String.sub s 2 (String.length s - 2))
    else (true, s)
  in
  let n = String.length body in
  if n >= 2 && body.[0] = '"' && body.[n - 1] = '"' then
    let v = String.sub body 1 (n - 2) in
    Some (if strong then `Strong v else `Weak v)
  else None
