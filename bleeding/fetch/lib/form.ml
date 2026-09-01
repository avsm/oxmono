open Middleware

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
   their immediate values in the Content-Type field. The generator is seeded
   from the system once per process and a fresh boundary seed is drawn per
   body: unpredictable, not cryptographic. The occurrence scan below, rather
   than a content digest, establishes the delimiter invariant. *)
let generator = lazy (Random.State.make_self_init ())

let salt () =
  let st = Lazy.force generator in
  String.init 32 (fun _ -> "0123456789abcdef".[Random.State.int st 16])

(* A boundary must not occur in any part, so derive it from the parts
   and step a counter until it does not. *)
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
  List.concat
    (List.map2
       (fun p h ->
         [
           Bytes ("--" ^ b ^ "\r\n" ^ h);
           (match p.content with
           | Immediate v -> Bytes v
           | Streamed { src; declared } ->
               Source { part = p.name; src; declared });
           Bytes separator;
         ])
       parts heads)
  @ [ Bytes (epilogue b) ]

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
  mutable seen : int64;
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
    t.seen <- 0L;
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
        | Some l when Int64.equal t.seen l ->
            (* Reads stop at the declared count, so one more of them is
             what tells a source that is done from one with bytes to
             spare. Letting the surplus through would shift every part
             after this one and contradict the Content-Length. *)
            (match Eio.Flow.single_read c.src (Cstruct.create 1) with
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
                  let left = Int64.sub l t.seen in
                  if Int64.compare left (Int64.of_int room) >= 0 then room
                  else Int64.to_int left
            in
            match Eio.Flow.single_read c.src (Cstruct.sub_local buf 0 room) with
            | n ->
                scan t buf n;
                t.seen <- Int64.add t.seen (Int64.of_int n);
                n
            | exception End_of_file ->
                (match declared with
                | Some l -> mismatch c.part l (Int64.to_string t.seen)
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
  let rec go acc = function
    | [] -> Some (String.concat "" (List.rev acc))
    | Bytes s :: rest -> go (s :: acc) rest
    | Source _ :: _ -> None
  in
  go [] segments

let multipart ?boundary parts =
  let heads = List.map part_headers parts in
  let b =
    match boundary with
    | None ->
        fresh parts heads (salt ()) 0
    | Some b ->
        if not (is_token b) then
          invalid_arg "Fetch.Form.multipart: boundary is not a token";
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
            seen = 0L;
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
    String (Httpz.Urlencoded.encode ps) )
