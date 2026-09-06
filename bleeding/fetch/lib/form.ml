open Middleware
module I64 = Stdlib_upstream_compatible.Int64_u

(* A part's content is either a string held now or a source read while
   the request is sent. A source cannot be read twice, so the checks
   the buffered path makes at build time move onto the wire for it. *)
type content =
  | Immediate of string
  | Streamed of {
      src : Eio.Flow.source_ty Eio.Resource.t;
      declared : int64 option;
    }

type part = {
  name : string;
  filename : string option;
  part_type : string option;
  extra : (string * string) list;
  content : content;
}

(* A name and a filename are written inside the quoted-string of
   Content-Disposition. The WHATWG multipart/form-data encoding algorithm
   escapes there the three bytes that would otherwise close the string or
   fold the field, and rejects nothing else in that range. A backslash is
   still refused: inside a quoted-string it is a quoted-pair to one parser
   and a literal to the next, so no spelling of it survives every server. *)
let is_parameter_byte = function
  | '\\' -> false
  | '"' | '\r' | '\n' | '\t' -> true
  | '\x00' .. '\x1F' | '\x7F' -> false
  | _ -> true

let check_parameter caller kind value =
  if not (String.for_all is_parameter_byte value) then
    invalid_arg
      (caller ^ ": " ^ kind
     ^ " may not contain a backslash or a forbidden control byte")

let escape value =
  if not (String.exists (fun c -> c = '"' || c = '\r' || c = '\n') value) then
    value
  else begin
    let buf = Buffer.create (String.length value + 8) in
    String.iter
      (fun c ->
        match c with
        | '"' -> Buffer.add_string buf "%22"
        | '\r' -> Buffer.add_string buf "%0D"
        | '\n' -> Buffer.add_string buf "%0A"
        | c -> Buffer.add_char buf c)
      value;
    Buffer.contents buf
  end

let check_content_type caller value =
  if not (is_field_value value) then
    invalid_arg (caller ^ ": content_type contains a forbidden control byte")

(* The part writes these two itself, from [name], [filename] and
   [content_type]; a second copy is a header the receiver picks between. *)
let derived_headers = [ "content-disposition"; "content-type" ]

let check_headers caller headers =
  List.iter
    (fun (name, value) ->
      if not (is_token name) then
        invalid_arg
          (Printf.sprintf "%s: part header name %S is not a token" caller name);
      let lower = String.lowercase_ascii name in
      if List.mem lower derived_headers then
        invalid_arg
          (Printf.sprintf "%s: part header %s is derived from the part" caller
             lower);
      if not (is_field_value value) then
        invalid_arg
          (Printf.sprintf "%s: part header %S contains a forbidden control byte"
             caller name))
    headers

let field ?content_type ?(headers = []) name value =
  check_parameter "Fetch.Form.field" "name" name;
  Option.iter (check_content_type "Fetch.Form.field") content_type;
  check_headers "Fetch.Form.field" headers;
  {
    name;
    filename = None;
    part_type = content_type;
    extra = headers;
    content = Immediate value;
  }

let file ?(headers = []) ~name ~filename ~content_type content =
  check_parameter "Fetch.Form.file" "name" name;
  check_parameter "Fetch.Form.file" "filename" filename;
  check_content_type "Fetch.Form.file" content_type;
  check_headers "Fetch.Form.file" headers;
  {
    name;
    filename = Some filename;
    part_type = Some content_type;
    extra = headers;
    content = Immediate content;
  }

let stream ?(headers = []) ~name ~filename ~content_type ?length src =
  check_parameter "Fetch.Form.stream" "name" name;
  check_parameter "Fetch.Form.stream" "filename" filename;
  check_content_type "Fetch.Form.stream" content_type;
  check_headers "Fetch.Form.stream" headers;
  (match length with
  | Some l when Int64.compare l 0L < 0 ->
      invalid_arg (Printf.sprintf "Fetch.Form.stream: length %Ld is negative" l)
  | _ -> ());
  {
    name;
    filename = Some filename;
    part_type = Some content_type;
    extra = headers;
    content =
      Streamed { src :> Eio.Flow.source_ty Eio.Resource.t; declared = length };
  }

(* Scanning for a first byte and comparing in place, rather than cutting a
   substring at every position, so a large part is searched without
   allocating once per byte of it. *)
let contains ~needle s =
  let n = String.length needle and m = String.length s in
  let rec same j k = k = n || (s.[j + k] = needle.[k] && same j (k + 1)) in
  let rec go i =
    i + n <= m
    &&
    match String.index_from_opt s i needle.[0] with
    | None -> false
    | Some j -> j + n <= m && (same j 0 || go (j + 1))
  in
  n > 0 && go 0

(* The boundary has to be absent from the headers a part serializes as
   well as from its content: a delimiter spliced into a Content-Disposition
   would end the part where the sender did not. [heads] is the serialized
   headers of [parts], which do not mention the boundary themselves. *)
let occurs b parts heads =
  List.exists (fun h -> contains ~needle:b h) heads
  || List.exists
       (fun p ->
         match p.content with
         | Immediate v -> contains ~needle:b v
         | Streamed _ -> false)
       parts

(* Two requests carrying the same parts must not expose a stable digest of
   their immediate values in the Content-Type field, so a fresh boundary
   seed is drawn per body: unpredictable, not cryptographic. Each domain
   keeps its own generator, drawn on first use, so two domains building a
   boundary concurrently never race on a shared [Random.State.t] and cannot
   draw the same seed. The occurrence scan below, rather than a content
   digest, establishes the delimiter invariant. *)
let boundary_state =
  Stdlib.Domain.Safe.DLS.new_key (fun () -> Stdlib.Random.State.make_self_init ())

let salt () =
  (* A domain-local value is reachable from its own domain alone, so the
     [contended] the safe accessor returns it at is weaker than what holds
     here. Nothing else ever draws from this state. *)
  let st = Obj.magic_uncontended (Stdlib.Domain.Safe.DLS.get boundary_state) in
  String.init 32 (fun _ -> "0123456789abcdef".[Stdlib.Random.State.int st 16])

let rec fresh parts heads digest n =
  let b = Printf.sprintf "form%sx%d" digest n in
  if occurs b parts heads then fresh parts heads digest (n + 1) else b

let part_headers p =
  let buf = Buffer.create 128 in
  Buffer.add_string buf
    (Printf.sprintf "Content-Disposition: form-data; name=\"%s\""
       (escape p.name));
  Option.iter
    (fun f ->
      Buffer.add_string buf (Printf.sprintf "; filename=\"%s\"" (escape f)))
    p.filename;
  Buffer.add_string buf "\r\n";
  Option.iter
    (fun ty -> Buffer.add_string buf ("Content-Type: " ^ ty ^ "\r\n"))
    p.part_type;
  List.iter
    (fun (n, v) -> Buffer.add_string buf (n ^ ": " ^ v ^ "\r\n"))
    p.extra;
  Buffer.add_string buf "\r\n";
  Buffer.contents buf

let separator = "\r\n"
let epilogue b = "--" ^ b ^ "--\r\n"

type segment =
  | Bytes of string
  | Source of {
      part : string;
      src : Eio.Flow.source_ty Eio.Resource.t;
      declared : int64 option;
    }

let segments b parts heads =
  let rec go parts heads = match parts, heads with
    | [], [] -> [Bytes (epilogue b)]
    | p :: ps, h :: hs ->
        let content = match p.content with
          | Immediate v -> Bytes v
          | Streamed { src; declared } -> Source { part = p.name; src; declared }
        in
        Bytes ("--" ^ b ^ "\r\n" ^ h) :: content :: Bytes separator :: go ps hs
    | _ -> invalid_arg "Fetch.Form: inconsistent part headers"
  in
  go parts heads

(* Knuth-Morris-Pratt, so the boundary can be matched against a
   streamed part one read at a time without holding the bytes. *)
let kmp_failure needle =
  let m = String.length needle in
  let f = Array.make m 0 in
  let k = ref 0 in
  for i = 1 to m - 1 do
    while !k > 0 && needle.[i] <> needle.[!k] do
      k := f.(!k - 1)
    done;
    if needle.[i] = needle.[!k] then incr k;
    f.(i) <- !k
  done;
  f

type composite = {
  needle : string;
  failure : int array;
  mutable todo : segment list;
  mutable pos : int;
  mutable seen : int64#;
  probe : Cstruct.t;
  mutable matched : int;
}

module Composite = struct
  type t = composite

  let read_methods = []

  (* The check is per part, as the buffered one is, so the state goes
     back to nothing whenever a segment ends. *)
  let advance t =
    t.todo <- List.tl t.todo;
    t.pos <- 0;
    t.seen <- #0L;
    t.matched <- 0

  let scan t buf n =
    for i = 0 to n - 1 do
      let c = Cstruct.get_char buf i in
      let k = ref t.matched in
      while !k > 0 && c <> t.needle.[!k] do
        k := t.failure.(!k - 1)
      done;
      if c = t.needle.[!k] then incr k;
      if !k = String.length t.needle then
        raise
          (err (Invalid_request "multipart boundary occurs in a streamed part"));
      t.matched <- !k
    done

  let mismatch part declared got =
    raise
      (err
         (Invalid_request
            (Fmt.str "streamed part %S declared %Ld bytes but produced %s" part
               declared got)))

  let rec single_read t (buf @ local) =
    (* Eio.Flow.single_read must return a positive count or raise
       End_of_file; an empty [buf] would force 0 out of the [Bytes] and
       [Source] arms below. Unreachable from Eio's own copy and read
       paths, which never pass an empty buffer, but guard it rather than
       silently violate the contract. *)
    if Cstruct.length buf = 0 then
      invalid_arg "Fetch.Form.multipart: single_read given an empty buffer";
    match t.todo with
    | [] -> raise End_of_file
    | Bytes s :: _ ->
        let left = String.length s - t.pos in
        if left = 0 then (
          advance t;
          single_read t buf)
        else begin
          let n = min (Cstruct.length buf) left in
          Cstruct.blit_from_string s t.pos buf 0 n;
          t.pos <- t.pos + n;
          n
        end
    | Source c :: _ ->
        begin match c.declared with
        | Some l when I64.equal t.seen (I64.of_int64 l) ->
            (* Reads stop at the declared count, so one more of them is
             what tells a source that is done from one with bytes to
             spare. Letting the surplus through would shift every part
             after this one and contradict the Content-Length. *)
            (match Eio.Flow.single_read c.src t.probe with
            | _ -> mismatch c.part l "more"
            | exception End_of_file -> ());
            advance t;
            single_read t buf
        | declared -> (
            let room = Cstruct.length buf in
            let room =
              match declared with
              | None -> room
              | Some l ->
                  let left = I64.sub (I64.of_int64 l) t.seen in
                  if I64.compare left (I64.of_int room) >= 0 then room
                  else I64.to_int left
            in
            match Eio.Flow.single_read c.src (Cstruct.sub_local buf 0 room) with
            | n ->
                scan t buf n;
                t.seen <- I64.add t.seen (I64.of_int n);
                n
            | exception End_of_file ->
                (match declared with
                | Some l -> mismatch c.part l (I64.to_string t.seen)
                | None -> ());
                advance t;
                single_read t buf)
        end
end

let composite_handler = Eio.Flow.Pi.source (module Composite)

let total segments =
  let add acc length =
    if Int64.compare acc (Int64.sub Int64.max_int length) > 0
    then invalid_arg "Fetch.Form.multipart: total Content-Length overflows int64"
    else Int64.add acc length
  in
  let rec go acc = function
    | [] -> Some acc
    | Bytes s :: rest ->
        go (add acc (Int64.of_int (String.length s))) rest
    | Source { declared = Some l; _ } :: rest -> go (add acc l) rest
    | Source { declared = None; _ } :: _ -> None
  in
  go 0L segments

let buffered segments =
  let rec size total = function
    | [] -> Some total
    | Source _ :: _ -> None
    | Bytes s :: rest ->
        if String.length s > Sys.max_string_length - total then
          invalid_arg "Fetch.Form.multipart: buffered body is too large";
        size (total + String.length s) rest
  in
  match size 0 segments with
  | None -> None
  | Some size ->
      let out = Bytes.create size in
      let rec write off = function
        | [] -> ()
        | Source _ :: _ -> assert false
        | Bytes s :: rest ->
            Bytes.blit_string s 0 out off (String.length s);
            write (off + String.length s) rest
      in
      write 0 segments;
      Some (Bytes.unsafe_to_string out)

(* RFC 2046 bcharsnospace, the alphabet a boundary delimiter itself may
   use: alnum plus these twelve punctuation bytes. [is_token]'s tchar
   admits several bytes this excludes (! # $ % & * ^ ` | ~) and excludes
   several this admits (( ) , / : = ?), so a caller-supplied boundary that
   passed [is_token] could still be one a strict multipart parser rejects.
   bchars additionally allows space in every position but the last. *)
let is_bchar = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | '\'' | '(' | ')' | '+' | '_' | ',' | '-' | '.' | '/' | ':' | '=' | '?' ->
      true
  | _ -> false

let is_boundary b =
  let n = String.length b in
  n > 0
  &&
  let ok = ref true in
  String.iteri
    (fun i c -> if not (is_bchar c || (c = ' ' && i < n - 1)) then ok := false)
    b;
  !ok

let multipart ?boundary parts =
  let heads = List.map part_headers parts in
  let b =
    match boundary with
    | None ->
        fresh parts heads (salt ()) 0
    | Some b ->
        if not (is_boundary b) then
          invalid_arg
            "Fetch.Form.multipart: boundary is not a valid RFC 2046 boundary";
        if String.length b > 70 then
          invalid_arg
            "Fetch.Form.multipart: boundary is longer than 70 characters";
        if occurs b parts heads then
          invalid_arg "Fetch.Form.multipart: boundary occurs in a part";
        b
  in
  let segments = segments b parts heads in
  let body =
    match buffered segments with
    | Some s -> String s
    | None ->
        let t =
          {
            needle = b;
            failure = kmp_failure b;
            todo = segments;
            pos = 0;
            seen = #0L;
            probe = Cstruct.create 1;
            matched = 0;
          }
        in
        Stream
          {
            length = total segments;
            flow = Eio.Resource.T (t, composite_handler);
          }
  in
  ( Header.
      [
        (content_type, media ~params:[ ("boundary", b) ] "multipart/form-data");
      ],
    body )

let urlencoded ps =
  ( Header.[ (content_type, media "application/x-www-form-urlencoded") ],
    String (Httpz_media.Urlencoded.encode ps) )
