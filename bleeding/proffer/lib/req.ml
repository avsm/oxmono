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

(* Parsed views are produced only when the handler asks for them. *)
let segments (t : t @ local) = Pct.segments t.path
let query (t : t @ local) = Pct.pairs t.qs
let headers (t : t @ local) = t.headers
let[@zero_alloc] header (t : t @ local) name = exclave_ Headers.find t.headers name

let[@zero_alloc] header_other (t : t @ local) (spelling : string @ local) =
  exclave_ Headers.find_other t.headers spelling

let body (t : t @ local) = t.body
let query_param (t : t @ local) (name : string @ local) = Pct.param ~plus:true t.qs name

let globalize (s : string @ local) = Pct.copy_all s

let globalize_opt (o : string option @ local) =
  match o with None -> None | Some s -> Some (globalize s)

(* A codec decodes a heap string, so the body is copied once for it. *)
let decode codec (t : t @ local) =
  let ct = header t H.Content_type in
  if Httpz.Media.accepts codec ct then Httpz.Media.decode codec (Pct.copy_all t.body)
  else Error (Httpz.Media.Unsupported (globalize_opt ct))

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

(* One rule answers all three form accessors, so a body {!Httpz.Media.form}
   refuses is exactly the one [is_form] denies. The media type and the decoder
   are spelled out rather than reached through that codec because a codec
   value carries closures and so cannot be read from a portable handler.
   [Httpz.Media.form] is built from the same two pieces. *)
let[@zero_alloc] is_form (t : t @ local) =
  match header t H.Content_type with
  | None -> false
  | Some ct ->
      let form = media_is ct "application/x-www-form-urlencoded" in
      form

let form_result (t : t @ local) =
  if is_form t then Ok (Httpz.Urlencoded.decode t.body)
  else Error (Httpz.Media.Unsupported (globalize_opt (header t H.Content_type)))

let form (t : t @ local) =
  match form_result t with Ok ps -> ps | Error _ -> []

(* Scan the body for the one name rather than building the whole list. *)
let form_param (t : t @ local) (name : string @ local) =
  if is_form t then Pct.param ~plus:true t.body name else None

let forwarded_for (t : t @ local) =
  match header t H.X_forwarded_for with
  | None -> None
  | Some v ->
      let n = String.length v in
      let stop = index_from v 0 n ',' in
      let a = skip_ows v 0 stop in
      let b = trim_ows v a stop in
      Some (Pct.copy v a (b - a))

let forwarded_proto (t : t @ local) =
  match header t H.X_forwarded_proto with
  | None -> None
  | Some v ->
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
  if Httpz.Media.seq_accepts sq ct then
    Httpz.Media.decode_items sq (Pct.copy_all t.body)
  else Error (Httpz.Media.Unsupported (globalize_opt ct))
