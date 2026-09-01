module H = Httpz.Header_name

type name = H.t

type field =
  { name : name
  ; spelling : string
  ; value : string
  }

type t = field list

let h name value =
  match name with
  | H.Other -> invalid_arg "Headers.h: use Headers.other for a custom name"
  | _ -> { name; spelling = H.canonical name; value }
;;

let of_name name spelling value =
  match name with
  | H.Other -> { name; spelling; value }
  | _ -> h name value
;;

let[@zero_alloc] of_string (s : string @ local) =
  let name =
    H.of_span
      (Bytes.unsafe_of_string s)
      (Httpz.Span.make
         ~off:(Stdlib_stable.Int16_u.of_int 0)
         ~len:(Stdlib_stable.Int16_u.of_int (String.length s)))
  in
  name
;;

let other spelling value = of_name (of_string spelling) spelling value

(* [h_local] and [other_local] build the field in the caller's region. [stack_] on a list
   literal covers its cons cells and not the calls inside it, so a block written
   [stack_ [ h n v ]] still put every record on the heap. [exclave_] is what moves them.
   The global forms stay for the blocks that are heap values by nature, which is every one
   [of_list] builds. *)
let[@zero_alloc] h_local name (value : string @ local) = exclave_
  match name with
  | H.Other -> invalid_arg "Headers.h_local: use Headers.other_local"
  | _ -> { name; spelling = H.canonical name; value }
;;

let[@zero_alloc] other_local (spelling : string @ local) (value : string @ local) = exclave_
  match of_string spelling with
  | H.Other -> { name = H.Other; spelling; value }
  | known -> { name = known; spelling = H.canonical known; value }
;;

let[@zero_alloc] same_name (a : name @ local) (b : name @ local) = a = b

let[@zero_alloc] rec same_spelling_from (a : string @ local) (b : string @ local) i =
  i = String.length a
  || (Char.equal
        (Char.lowercase_ascii (String.unsafe_get a i))
        (Char.lowercase_ascii (String.unsafe_get b i))
      && same_spelling_from a b (i + 1))
;;

let[@zero_alloc] same_spelling (a : string @ local) (b : string @ local) =
  String.length a = String.length b && same_spelling_from a b 0
;;

let empty : t = []
let of_list l : t = List.map (fun (n, v) -> of_name (of_string n) n v) l

(* These walk with their own recursion because the [List] functions take a global list,
   and the block is read at [local] on the response path. *)

let[@zero_alloc] rec to_list (t : t @ local) = exclave_
  match t with
  | [] -> []
  | { spelling; value; _ } :: tl -> (spelling, value) :: to_list tl
;;

let rec iter (f : (name -> string @ local -> string @ local -> unit) @ local) (t : t @ local) =
  match t with
  | [] -> ()
  | { name; spelling; value } :: tl ->
    f name spelling value;
    iter f tl
;;

let[@zero_alloc] rec find_name (t : t @ local) (name : name) = exclave_
  match t with
  | [] -> None
  | f :: tl -> if same_name f.name name then Some f.value else find_name tl name
;;

let[@zero_alloc] rec find_other (t : t @ local) (spelling : string @ local) = exclave_
  match t with
  | [] -> None
  | f :: tl ->
    if same_name f.name H.Other && same_spelling f.spelling spelling
    then Some f.value
    else find_other tl spelling
;;

let[@zero_alloc] find (t : t @ local) (name : name) = exclave_
  match name with
  | H.Other -> None
  | _ -> find_name t name
;;

let[@zero_alloc] rec find_or_null (t : t @ local) (name : name) = exclave_
  match name, t with
  | H.Other, _ | _, [] -> Null
  | name, f :: tl -> if same_name f.name name then This f.value else find_or_null tl name
;;

let[@zero_alloc] rec mem (t : t @ local) name =
  match name, t with
  | H.Other, _ | _, [] -> false
  | name, f :: tl -> same_name f.name name || mem tl name
;;

(* Repeated fields combine with comma and SP per RFC 9110 section 5.3. The
   resulting string and option remain in the caller's region. *)
let[@zero_alloc] rec combined_size (t : t @ local) name count size =
  match t with
  | [] -> #(count, size)
  | f :: rest ->
    if same_name f.name name
    then combined_size rest name (count + 1) (size + String.length f.value)
    else combined_size rest name count size
;;

let[@zero_alloc] rec combined_write (t : t @ local) name
    (b : bytes @ local) pos first =
  match t with
  | [] -> pos
  | f :: rest ->
    if same_name f.name name
    then (
      let pos =
        if first
        then pos
        else (
          Bytes.unsafe_set b pos ',';
          Bytes.unsafe_set b (pos + 1) ' ';
          pos + 2)
      in
      let n = String.length f.value in
      Bytes.unsafe_blit_string f.value 0 b pos n;
      combined_write rest name b (pos + n) false)
    else combined_write rest name b pos first
;;

let[@zero_alloc] combined (t : t @ local) name = exclave_
  let #(count, size) = combined_size t name 0 0 in
  if count = 0
  then None
  else if count = 1
  then find t name
  else (
    let b = Pct.create_local (size + ((count - 1) * 2)) in
    let _ = combined_write t name b 0 true in
    Some (Bytes.unsafe_to_string b))
;;

(* [cat a b] is [a] then [b], built in the caller's region. [exclave_] is what puts the
   new cells there rather than in this frame's, which is what lets a decorator extend a
   block it was handed and pass the result on. *)
let[@zero_alloc] rec cat (a : t @ local) (b : t @ local) = exclave_
  match a with
  | [] -> b
  | { name; spelling; value } :: tl -> { name; spelling; value } :: cat tl b
;;

let[@zero_alloc] rec without (t : t @ local) (name : name) = exclave_
  match t with
  | [] -> []
  | ({ name = n; spelling; value } as f) :: tl ->
    if same_name n name
    then without tl name
    else { name = f.name; spelling; value } :: without tl name
;;

(* Vary is rewritten rather than appended to, so a response that already names a field
   keeps one Vary listing both rather than two fields. *)

let[@zero_alloc] is_ows c = Char.equal c ' ' || Char.equal c '\t'

let[@zero_alloc] rec skip_ows (v : string @ local) i j =
  if i < j && is_ows (String.unsafe_get v i) then skip_ows v (i + 1) j else i
;;

let[@zero_alloc] rec trim_ows (v : string @ local) i j =
  if j > i && is_ows (String.unsafe_get v (j - 1)) then trim_ows v i (j - 1) else j
;;

let[@zero_alloc] rec index_from (v : string @ local) i j c =
  if i >= j then j
  else if Char.equal (String.unsafe_get v i) c then i
  else index_from v (i + 1) j c
;;

let[@zero_alloc] rec same_token_from (v : string @ local) i (name : string @ local) k n =
  k = n
  || (Char.equal
        (Char.lowercase_ascii (String.unsafe_get v (i + k)))
        (Char.lowercase_ascii (String.unsafe_get name k))
      && same_token_from v i name (k + 1) n)
;;

let[@zero_alloc] rec has_token_from (v : string @ local) start (name : string @ local) =
  let n = String.length v in
  if start > n then false
  else (
    let stop = index_from v start n ',' in
    let a = skip_ows v start stop in
    let b = trim_ows v a stop in
    (b - a = String.length name && same_token_from v a name 0 (b - a))
    || has_token_from v (stop + 1) name)
;;

let[@zero_alloc] has_token (v : string @ local) (name : string @ local) =
  has_token_from v 0 name
;;

let[@zero_alloc] rec vary_has (t : t @ local) (name : string @ local) =
  match t with
  | [] -> false
  | f :: tl -> (same_name f.name H.Vary && has_token f.value name) || vary_has tl name
;;

let[@zero_alloc] rec vary_len (t : t @ local) acc =
  match t with
  | [] -> acc
  | f :: tl -> vary_len tl (if same_name f.name H.Vary then acc + String.length f.value + 2 else acc)
;;

let[@zero_alloc] rec vary_write (t : t @ local) (b : bytes @ local) pos =
  match t with
  | [] -> pos
  | f :: tl ->
    if same_name f.name H.Vary
    then (
      let n = String.length f.value in
      Bytes.unsafe_blit_string f.value 0 b pos n;
      Bytes.unsafe_set b (pos + n) ',';
      Bytes.unsafe_set b (pos + n + 1) ' ';
      vary_write tl b (pos + n + 2))
    else vary_write tl b pos
;;

let[@zero_alloc] vary (t : t @ local) name = exclave_
  if not (Httpz.Header.Syntax.is_token name)
  then invalid_arg "Proffer.Headers.vary: invalid field name"
  else if String.equal name "*" || vary_has t "*"
  then cat (without t H.Vary) (stack_ [ h_local H.Vary "*" ])
  else if vary_has t name then t
  else (
    let n = vary_len t 0 in
    if n = 0
    then cat t (stack_ [ h_local H.Vary name ])
    else (
      let b = Pct.create_local (n + String.length name) in
      let pos = vary_write t b 0 in
      Bytes.unsafe_blit_string name 0 b pos (String.length name);
      cat (without t H.Vary) (stack_ [ h_local H.Vary (Bytes.unsafe_to_string b) ])))
;;
