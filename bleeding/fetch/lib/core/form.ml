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
  content : content;
}

let field name value =
  { name; filename = None; part_type = None; content = Immediate value }

(* A quote would close the [filename] parameter early and a line break
   would end the header, either way letting a name forge framing. *)
let check_filename caller filename =
  if String.exists (function '"' | '\r' | '\n' -> true | _ -> false) filename
  then
    invalid_arg (caller ^ ": filename may not hold a quote or a line break")

let file ~name ~filename ~content_type content =
  check_filename "Fetch.Form.file" filename;
  { name; filename = Some filename; part_type = Some content_type;
    content = Immediate content }

let stream ~name ~filename ~content_type ?length src =
  check_filename "Fetch.Form.stream" filename;
  (match length with
   | Some l when Int64.compare l 0L < 0 ->
     invalid_arg
       (Printf.sprintf "Fetch.Form.stream: length %Ld is negative" l)
   | _ -> ());
  { name; filename = Some filename; part_type = Some content_type;
    content =
      Streamed { src = (src :> Eio.Flow.source_ty Eio.Resource.t);
                 declared = length } }

let contains ~needle s =
  let n = String.length needle and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = needle || go (i + 1)) in
  n > 0 && go 0

let occurs b parts =
  List.exists
    (fun p ->
       (match p.content with
        | Immediate v -> contains ~needle:b v
        | Streamed _ -> false)
       || (match p.filename with
           | None -> false
           | Some f -> contains ~needle:b f))
    parts

(* A boundary must not occur in any part, so derive it from the parts
   and step a counter until it does not. *)
let rec fresh parts digest n =
  let b = Printf.sprintf "form%sx%d" digest n in
  if occurs b parts then fresh parts digest (n + 1) else b

(* Both paths take their framing from here, so a streamed body is the
   buffered one's bytes by construction. *)
let prologue b p =
  let buf = Buffer.create 128 in
  Buffer.add_string buf ("--" ^ b ^ "\r\n");
  Buffer.add_string buf
    (Printf.sprintf "Content-Disposition: form-data; name=\"%s\"" p.name);
  Option.iter
    (fun f -> Buffer.add_string buf (Printf.sprintf "; filename=\"%s\"" f))
    p.filename;
  Buffer.add_string buf "\r\n";
  Option.iter
    (fun ty -> Buffer.add_string buf ("Content-Type: " ^ ty ^ "\r\n"))
    p.part_type;
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

let segments b parts =
  List.concat_map
    (fun p ->
       [ Bytes (prologue b p);
         (match p.content with
          | Immediate v -> Bytes v
          | Streamed { src; declared } ->
            Source { part = p.name; src; declared });
         Bytes separator ])
    parts
  @ [ Bytes (epilogue b) ]

(* Knuth-Morris-Pratt, so the boundary can be matched against a
   streamed part one read at a time without holding the bytes. *)
let kmp_failure needle =
  let m = String.length needle in
  let f = Array.make m 0 in
  let k = ref 0 in
  for i = 1 to m - 1 do
    while !k > 0 && needle.[i] <> needle.[!k] do k := f.(!k - 1) done;
    if needle.[i] = needle.[!k] then incr k;
    f.(i) <- !k
  done;
  f

type composite = {
  needle : string;
  failure : int array;  (* [kmp_failure needle] *)
  mutable todo : segment list;
  mutable pos : int;  (* read offset into the leading [Bytes] *)
  mutable seen : int64;  (* bytes taken from the leading [Source] *)
  mutable matched : int;  (* length of the [needle] prefix matched *)
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
      while !k > 0 && c <> t.needle.[!k] do k := t.failure.(!k - 1) done;
      if c = t.needle.[!k] then incr k;
      if !k = String.length t.needle then
        raise
          (err (Invalid_request
                  "multipart boundary occurs in a streamed part"));
      t.matched <- !k
    done

  let mismatch part declared got =
    raise
      (err (Invalid_request
              (Fmt.str "streamed part %S declared %Ld bytes but produced %s"
                 part declared got)))

  let rec single_read t buf =
    match t.todo with
    | [] -> raise End_of_file
    | Bytes s :: _ ->
      let left = String.length s - t.pos in
      if left = 0 then (advance t; single_read t buf)
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
        | declared ->
          let room = Cstruct.length buf in
          let room =
            match declared with
            | None -> room
            | Some l ->
              let left = Int64.sub l t.seen in
              if Int64.compare left (Int64.of_int room) >= 0 then room
              else Int64.to_int left
          in
          (match Eio.Flow.single_read c.src (Cstruct.sub buf 0 room) with
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

(* [Some total] once every source has declared its size, which is what
   lets the request carry a Content-Length. *)
let total segments =
  let rec go acc = function
    | [] -> Some acc
    | Bytes s :: rest -> go (Int64.add acc (Int64.of_int (String.length s))) rest
    | Source { declared = Some l; _ } :: rest -> go (Int64.add acc l) rest
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
  let b =
    match boundary with
    | None ->
      let all =
        String.concat ""
          (List.filter_map
             (fun p ->
                match p.content with
                | Immediate v -> Some v
                | Streamed _ -> None)
             parts)
      in
      fresh parts (Digest.to_hex (Digest.string all)) 0
    | Some b ->
      if not (is_token b) then
        invalid_arg "Fetch.Form.multipart: boundary is not a token";
      if String.length b > 70 then
        invalid_arg
          "Fetch.Form.multipart: boundary is longer than 70 characters";
      if occurs b parts then
        invalid_arg "Fetch.Form.multipart: boundary occurs in a part";
      b
  in
  let segments = segments b parts in
  let body =
    match buffered segments with
    | Some s -> String s
    | None ->
      let t = { needle = b; failure = kmp_failure b; todo = segments;
                pos = 0; seen = 0L; matched = 0 } in
      Stream { length = total segments;
               flow = Eio.Resource.T (t, composite_handler) }
  in
  ( Header.[ (content_type,
              media ~params:[ ("boundary", b) ] "multipart/form-data") ],
    body )

(* RFC 3986 unreserved characters, so a value is safe wherever it
   lands. *)
let urlencode s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
       match c with
       | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' | '~' ->
         Buffer.add_char buf c
       | c -> Buffer.add_string buf (Printf.sprintf "%%%02X" (Char.code c)))
    s;
  Buffer.contents buf

let urlencoded ps =
  let body =
    String.concat "&"
      (List.map (fun (k, v) -> urlencode k ^ "=" ^ urlencode v) ps)
  in
  ( Header.[ (content_type, media "application/x-www-form-urlencoded") ],
    String body )
