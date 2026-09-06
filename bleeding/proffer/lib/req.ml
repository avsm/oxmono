module H = Httpz.Header_name

(* Every string is read at [local]. A backend builds them in the request's
   region straight out of its parse buffer, and a handler that keeps one copies
   it. *)
type t = {
  meth : Method.t;
  version : Httpz.Version.t;
  connection_upgrade : bool;
  target : string;
  path : string;
  qs : string;
  headers : Headers.t;
  body : string;
}

let split_target target =
  match String.index_opt target '?' with
  | None -> #(target, "")
  | Some i ->
      #( String.sub target 0 i,
         String.sub target (i + 1) (String.length target - i - 1) )

let v ~meth ~target ?(version = Httpz.Version.Http_1_1)
    ?(connection_upgrade = false) ?path ?query ?(headers = Headers.empty)
    ?(body = "") () =
  exclave_
  let #(path, qs) =
    match path, query with
    | Some path, Some query -> #(path, query)
    | _ ->
      let #(default_path, default_query) = split_target target in
      #( Option.value path ~default:default_path,
         Option.value query ~default:default_query )
  in
  {
    meth;
    version;
    connection_upgrade;
    target;
    path;
    qs;
    headers;
    body;
  }

(* Concrete backends know every component, so this takes them all and keeps
   the field block local rather than routing the wire path through [v]'s
   optional arguments. *)
let[@zero_alloc] backend ~meth ~version ~connection_upgrade
    ~(target : string @ local) ~(path : string @ local) ~(query : string @ local)
    (headers : Headers.t @ local) ~(body : string @ local) =
  exclave_ { meth; version; connection_upgrade; target; path; qs = query; headers; body }

let meth (t : t @ local) = t.meth
let version (t : t @ local) = t.version
let connection_upgrade (t : t @ local) = t.connection_upgrade
let target (t : t @ local) = t.target
let path (t : t @ local) = t.path

let segments (t : t @ local) = Pct.segments t.path
let query (t : t @ local) = Pct.pairs t.qs
let headers (t : t @ local) = t.headers
let[@zero_alloc] header (t : t @ local) name = exclave_ Headers.find t.headers name

let[@zero_alloc] header_other (t : t @ local) (spelling : string @ local) =
  exclave_ Headers.find_other t.headers spelling

let[@zero_alloc] rec cookies_size (hs : Headers.t @ local) count size =
  match hs with
  | [] -> #(count, size)
  | f :: rest ->
    if Headers.same_name f.Headers.name H.Cookie
    then cookies_size rest (count + 1) (size + String.length f.Headers.value)
    else cookies_size rest count size

let rec cookies_write (hs : Headers.t @ local) (b : bytes @ local) pos first =
  match hs with
  | [] -> pos
  | f :: rest ->
    if Headers.same_name f.Headers.name H.Cookie
    then (
      let pos =
        if first
        then pos
        else (
          Bytes.unsafe_set b pos ';';
          Bytes.unsafe_set b (pos + 1) ' ';
          pos + 2)
      in
      let n = String.length f.Headers.value in
      Bytes.unsafe_blit_string f.Headers.value 0 b pos n;
      cookies_write rest b (pos + n) false)
    else cookies_write rest b pos first

let cookies (t : t @ local) : string =
  let #(count, size) = cookies_size t.headers 0 0 in
  if count = 0
  then ""
  else (
    let b = Bytes.create (size + ((count - 1) * 2)) in
    let _ = cookies_write t.headers b 0 true in
    Bytes.unsafe_to_string b)

let[@zero_alloc] cookies_local (t : t @ local) = exclave_
  let #(count, size) = cookies_size t.headers 0 0 in
  if count = 0 then ""
  else if count = 1 then
    (match Headers.find_or_null t.headers H.Cookie with Null -> "" | This value -> value)
  else
    let b = Pct.create_local (size + ((count - 1) * 2)) in
    let _ = cookies_write t.headers b 0 true in
    Bytes.unsafe_to_string b

let body (t : t @ local) = t.body
let query_param (t : t @ local) (name : string @ local) = Pct.param ~plus:true t.qs name

let globalize (s : string @ local) = Pct.copy_all s

let globalize_opt (o : string option @ local) =
  match o with None -> None | Some s -> Some (globalize s)

(* A codec decodes a heap string, so the body is copied once for it. *)
let decode codec (t : t @ local) =
  let ct = header t H.Content_type in
  if Httpz_media.accepts codec ct then Httpz_media.decode codec (Pct.copy_all t.body)
  else Error (Httpz_media.Unsupported (globalize_opt ct))

let[@zero_alloc] is_ows c = Char.equal c ' ' || Char.equal c '\t'

let[@zero_alloc] rec skip_ows (s : string @ local) i j =
  if i < j && is_ows (String.unsafe_get s i) then skip_ows s (i + 1) j else i

let[@zero_alloc] rec trim_ows (s : string @ local) i j =
  if j > i && is_ows (String.unsafe_get s (j - 1)) then trim_ows s i (j - 1)
  else j

let[@zero_alloc] rec index_from (s : string @ local) i j c =
  if i >= j then j
  else if Char.equal (String.unsafe_get s i) c then i
  else index_from s (i + 1) j c

let[@zero_alloc] rec same_lower (s : string @ local) i lit k n =
  k = n
  || (Char.equal
        (Char.lowercase_ascii (String.unsafe_get s (i + k)))
        (String.unsafe_get lit k)
     && same_lower s i lit (k + 1) n)

let[@zero_alloc] media_is (ct : string @ local) lit =
  let n = String.length ct in
  let stop = index_from ct 0 n ';' in
  let a = skip_ows ct 0 stop in
  let b = trim_ows ct a stop in
  let m = String.length lit in
  b - a = m && same_lower ct a lit 0 m

(* One rule answers all three form accessors, so a body {!Httpz_media.form}
   refuses is exactly the one [is_form] denies. The media type and the decoder
   are spelled out rather than reached through that codec because a codec
   value carries closures and so cannot be read from a portable handler.
   [Httpz_media.form] is built from the same two pieces. *)
let[@zero_alloc] is_form (t : t @ local) =
  match Headers.find_or_null t.headers H.Content_type with
  | Null -> false
  | This ct ->
      let form = media_is ct "application/x-www-form-urlencoded" in
      form

let form_result (t : t @ local) =
  if is_form t then Ok (Httpz_media.Urlencoded.decode t.body)
  else Error (Httpz_media.Unsupported (globalize_opt (header t H.Content_type)))

let form (t : t @ local) =
  match form_result t with Ok ps -> ps | Error _ -> []

let form_param (t : t @ local) (name : string @ local) =
  if is_form t then Pct.param ~plus:true t.body name else None

let forwarded_for (t : t @ local) =
  match Headers.find_or_null t.headers H.X_forwarded_for with
  | Null -> None
  | This v ->
      let n = String.length v in
      let stop = index_from v 0 n ',' in
      let a = skip_ows v 0 stop in
      let b = trim_ows v a stop in
      Some (Pct.copy v a (b - a))

let forwarded_proto (t : t @ local) =
  match Headers.find_or_null t.headers H.X_forwarded_proto with
  | Null -> None
  | This v ->
      let n = String.length v in
      let a = skip_ows v 0 n in
      let b = trim_ows v a n in
      let out = Bytes.create (b - a) in
      for i = a to b - 1 do
        Bytes.unsafe_set out (i - a)
          (Char.lowercase_ascii (String.unsafe_get v i))
      done;
      Some (Bytes.unsafe_to_string out)

let decode_seq sq (t : t @ local) =
  let ct = header t H.Content_type in
  if Httpz_media.seq_accepts sq ct then
    Httpz_media.decode_items sq (Pct.copy_all t.body)
  else Error (Httpz_media.Unsupported (globalize_opt ct))

let iter_query (t : t @ local)
    (f : (string @ local -> string @ local -> unit) @ local) = Pct.iter_pairs t.qs f
let iter_segments (t : t @ local) (f @ local) = Pct.iter_segments t.path f
