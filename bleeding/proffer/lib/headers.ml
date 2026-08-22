(* Field names are httpz's. It already enumerates the fields its parser
   recognises, and a server layer that kept its own copy would spend every
   request and every response translating between two spellings of the same
   idea.

   httpz names an unrecognised field [Other] and keeps its spelling elsewhere,
   because a parsed field's name is a span into the read buffer. A described
   field has no buffer to point at, so this record carries the spelling
   directly. It is the canonical one for a name httpz knows, which costs
   nothing: [H.canonical] answers a constant.

   [spelling] and [value] are [global_]. The block travels the response path at
   [local] so its cells are stack allocated, but a string is boxed and a socket
   write needs it at global. [name] needs no modality: it is read only to
   compare, and to hand back to httpz's writer. *)

module H = Httpz.Header_name

type name = H.t

type field = {
  name : name;
  global_ spelling : string;
  global_ value : string;
}

type t = field list

let h name value =
  { name; spelling = H.canonical name; value }

let of_name name spelling value =
  match name with
  | H.Other -> { name; spelling; value }
  | _ -> h name value

(* A name this module does not recognise keeps the spelling it was given,
   because that is what a backend echoes and what a log event reports. *)
let of_string s =
  match H.of_span (Bytes.unsafe_of_string s) 
          (Httpz.Span.make ~off:(Stdlib_stable.Int16_u.of_int 0)
             ~len:(Stdlib_stable.Int16_u.of_int (String.length s)))
  with
  | H.Other -> H.Other
  | known -> known

(* [other] resolves the spelling rather than forcing [Other], so a caller that
   spells a field httpz does name still gets its constructor. Without that a
   block could hold the same field under two names depending on how it was
   built, and a lookup would find it only one of those ways. *)
let other spelling value = of_name (of_string spelling) spelling value

(* [h_local] and [other_local] build the field in the caller's region.
   [stack_] on a list literal covers its cons cells and not the calls inside
   it, so a block written [stack_ [ h n v ]] still put every record on the
   heap. [exclave_] is what moves them. The global forms stay for the blocks
   that are heap values by nature, which is every one [of_list] builds. *)
let h_local name value = exclave_ { name; spelling = H.canonical name; value }

let other_local spelling value = exclave_
  match of_string spelling with
  | H.Other -> { name = H.Other; spelling; value }
  | known -> { name = known; spelling = H.canonical known; value }

let to_string f = f.spelling

(* Two fields name the same header. A name httpz knows is one comparison of an
   immediate. Only [Other] pays for a string walk, and it folds case because
   the two spellings may have come from different sides of the wire. *)
let same_name (a : name @ local) (b : name @ local) = a = b

let same_spelling a b =
  String.length a = String.length b
  &&
  let rec go i =
    i = String.length a
    || Char.equal
         (Char.lowercase_ascii (String.unsafe_get a i))
         (Char.lowercase_ascii (String.unsafe_get b i))
       && go (i + 1)
  in
  go 0

let empty : t = []

let of_list l : t =
  List.map (fun (n, v) -> of_name (of_string n) n v) l

(* These walk with their own recursion because the [List] functions take a
   global list, and the block is read at [local] on the response path. *)

let rec to_list (t : t @ local) =
  match t with
  | [] -> []
  | { spelling; value; _ } :: tl -> (spelling, value) :: to_list tl

let rec iter f (t : t @ local) =
  match t with
  | [] -> ()
  | { name; spelling; value } :: tl ->
      f name spelling value;
      iter f tl

let rec exists p (t : t @ local) =
  match t with
  | [] -> false
  | { name; spelling; value } :: tl -> p name spelling value || exists p tl

let rec find_name (t : t @ local) (name : name) =
  match t with
  | [] -> None
  | f :: tl -> if same_name f.name name then Some f.value else find_name tl name

let rec find_other (t : t @ local) spelling =
  match t with
  | [] -> None
  | f :: tl ->
      if
        same_name f.name H.Other
        && same_spelling f.spelling spelling
      then Some f.value
      else find_other tl spelling

let find (t : t @ local) (name : name) =
  match name with
  | H.Other -> None
  | _ -> find_name t name

let mem (t : t @ local) name = Option.is_some (find t name)

(* [cat a b] is [a] then [b], built in the caller's region. [exclave_] is what
   puts the new cells there rather than in this frame's, which is what lets a
   decorator extend a block it was handed and pass the result on. *)
let rec cat (a : t @ local) (b : t @ local) = exclave_
  match a with
  | [] -> b
  | { name; spelling; value } :: tl -> { name; spelling; value } :: cat tl b

(* Vary is rewritten rather than appended to, so a response that already names
   a field keeps one Vary listing both rather than two fields. *)
let rec without (t : t @ local) (name : name) = exclave_
  match t with
  | [] -> []
  | ({ name = n; spelling; value } as f) :: tl ->
      if same_name n name then without tl name
      else { name = f.name; spelling; value } :: without tl name

let vary (t : t @ local) name = exclave_
  let value =
    match find t H.Vary with
    | None -> name
    | Some prev -> prev ^ ", " ^ name
  in
  cat
    (without t H.Vary)
    [ h H.Vary value ]
