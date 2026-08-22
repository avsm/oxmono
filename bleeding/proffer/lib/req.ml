module H = Httpz.Header_name

(* A request reaches a handler at [local], so it costs no heap and a handler
   cannot stash one. The strings are [global_]: they are heap values already,
   copied out of the backend's read buffer, and a handler that concatenates a
   path or hands a body to a parser needs them at global. [headers] is left at
   the record's own mode, since the block is the part worth keeping on the
   stack and every lookup takes it at [local]. *)
type t = {
  meth : Method.t;
  global_ target : string;
  global_ path : string;
  global_ qs : string;
  headers : Headers.t;
  global_ body : string;
}

let v ~meth ~target ?(headers = Headers.empty) ?(body = "") () = exclave_
  let path, qs =
    match String.index_opt target '?' with
    | None -> (target, "")
    | Some i ->
        ( String.sub target 0 i,
          String.sub target (i + 1) (String.length target - i - 1) )
  in
  {
    meth;
    target;
    path;
    qs;
    headers;
    body;
  }

let meth (t : t @ local) = t.meth
let target (t : t @ local) = t.target
let path (t : t @ local) = t.path
(* The segments are decoded on demand. Dispatch walks the path where it lies
   and never asks for them, so building the list for every request paid for
   something almost nothing read. *)
let segments (t : t @ local) = Pct.segments t.path
(* The query is kept as it arrived and parsed on demand. Almost every request
   reads no parameter at all, and building the association list for all of
   them cost more than every other part of [v] put together. *)
let query (t : t @ local) = Pct.pairs t.qs
let headers (t : t @ local) = t.headers
let header (t : t @ local) name = Headers.find t.headers name

(* A field httpz does not name is found by its spelling instead, since its
   constructor carries none. *)
let header_other (t : t @ local) spelling =
  Headers.find_other t.headers spelling
let body (t : t @ local) = t.body

let query_param (t : t @ local) name = Pct.param ~plus:true t.qs name

let is_form (t : t @ local) =
  match header t H.Content_type with
  | None -> false
  | Some ct ->
      let ct = String.lowercase_ascii ct in
      let media =
        match String.index_opt ct ';' with
        | None -> ct
        | Some i -> String.sub ct 0 i
      in
      String.equal (String.trim media) "application/x-www-form-urlencoded"

let form (t : t @ local) =
  if String.equal t.body "" || not (is_form t) then [] else Pct.pairs t.body

let form_param (t : t @ local) name = List.assoc_opt name (form t)

(* The first entry of X-Forwarded-For is the client the nearest trusted proxy
   saw. Trusting it is the deployment's decision, not this library's. *)
let forwarded_for (t : t @ local) =
  match header t H.X_forwarded_for with
  | None -> None
  | Some v -> (
      match String.index_opt v ',' with
      | None -> Some (String.trim v)
      | Some i -> Some (String.trim (String.sub v 0 i)))

let forwarded_proto (t : t @ local) =
  match header t H.X_forwarded_proto with
  | None -> None
  | Some v -> Some (String.lowercase_ascii (String.trim v))
