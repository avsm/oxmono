type t = {
  meth : Method.t;
  target : string;
  path : string;
  segments : string list;
  query : (string * string) list;
  headers : Headers.t;
  body : string;
}

let v ~meth ~target ?(headers = []) ?(body = "") () =
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
    segments = Pct.segments path;
    query = Pct.pairs qs;
    headers = Headers.of_list headers;
    body;
  }

let meth t = t.meth
let target t = t.target
let path t = t.path
let segments t = t.segments
let query t = t.query
let headers t = t.headers
let header t name = Headers.find t.headers name
let body t = t.body

let query_param t name = List.assoc_opt name t.query

let is_form t =
  match header t "content-type" with
  | None -> false
  | Some ct ->
      let ct = String.lowercase_ascii ct in
      let media =
        match String.index_opt ct ';' with
        | None -> ct
        | Some i -> String.sub ct 0 i
      in
      String.equal (String.trim media) "application/x-www-form-urlencoded"

let form t =
  if String.equal t.body "" || not (is_form t) then [] else Pct.pairs t.body

let form_param t name = List.assoc_opt name (form t)

(* The first entry of X-Forwarded-For is the client the nearest trusted proxy
   saw. Trusting it is the deployment's decision, not this library's. *)
let forwarded_for t =
  match header t "x-forwarded-for" with
  | None -> None
  | Some v -> (
      match String.index_opt v ',' with
      | None -> Some (String.trim v)
      | Some i -> Some (String.trim (String.sub v 0 i)))

let forwarded_proto t =
  match header t "x-forwarded-proto" with
  | None -> None
  | Some v -> Some (String.lowercase_ascii (String.trim v))
