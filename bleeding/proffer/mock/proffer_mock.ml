module M = Httpz.Method

type response = {
  status : Proffer.Status.t;
  headers : Proffer.Headers.t;
  body : string;
  content_length : int64 option;
}

(* The outcome reaches the writer at [local], so what a test reads has to be
   copied out of it. [Headers.to_list] then [of_list] is that copy: the block a
   test holds is a heap value, and the one the backend was handed was never
   one. The length is copied for the same reason: the option itself is built
   in the backend's region now, so rebuilding it here is what moves it to the
   heap a test can keep. *)
let copy_length (l : int64 option @ local) =
  match l with None -> None | Some n -> Some (Int64.of_int (Int64.to_int n))

(* The block a test holds is a heap value, and the one the backend was handed
   was never one, so its strings are copied out of the region. *)
let globalize (s : string @ local) = Bytes.unsafe_to_string (Bytes.of_string s)

let rec globalize_list (l : (string * string) list @ local) =
  match l with
  | [] -> []
  | (spelling, value) :: tl ->
      (globalize spelling, globalize value) :: globalize_list tl

(* A backend renders Last-Modified from the time in the outcome. Here it
   becomes the field a test looks up. *)
let last_modified = function
  | None -> []
  | Some t ->
      [ ("Last-Modified",
         Httpz.Date.format (Stdlib_upstream_compatible.Float_u.of_float t)) ]

let snapshot out (o : Proffer.Backend.outcome @ local) =
  let headers =
    Proffer.Headers.of_list
      (globalize_list (Proffer.Headers.to_list o.Proffer.Backend.headers)
       @ last_modified o.Proffer.Backend.last_modified)
  in
  let content_length = copy_length o.Proffer.Backend.content_length in
  let status = o.Proffer.Backend.status in
  let response body content_length =
    { status; headers; body; content_length }
  in
  match o.Proffer.Backend.body with
  | Proffer.Backend.Stream { write = w; _ } ->
    (* Selecting the response is the mock equivalent of sending its head.
       Retain it, including partial bytes, if the stream later fails. *)
    out := Some (response "" content_length);
    let b = Buffer.create 256 in
    let emit s =
      Buffer.add_string b s;
      out := Some (response (Buffer.contents b) content_length)
    in
    w (Proffer.Backend.sink emit);
    out := Some (response (Buffer.contents b) content_length)
  | Proffer.Backend.Empty -> out := Some (response "" content_length)
  | Proffer.Backend.String s -> out := Some (response (globalize s) content_length)
  | Proffer.Backend.Handoff _ -> out := Some (response "" None)

let taken out =
  match !out with
  | Some r -> r
  | None -> failwith "Proffer_mock: the backend wrote no response"

let request ?version ?connection_upgrade ?headers ?body ?on_error ?now site env
    meth target =
  let headers = Option.map Proffer.Headers.of_list headers in
  let req =
    Proffer.Req.v ~meth ~target ?version ?connection_upgrade ?headers ?body ()
  in
  let out = ref None in
  let local_ write : Proffer.Backend.writer = fun o -> snapshot out o in
  let () = Proffer.Backend.handle ?on_error ?now site env req write in
  taken out

let describe ?version ?connection_upgrade ?headers ?body ?on_error ?now
    ?(meth = M.Get) ?(target = "/") f =
  let headers = Option.map Proffer.Headers.of_list headers in
  let req =
    Proffer.Req.v ~meth ~target ?version ?connection_upgrade ?headers ?body ()
  in
  let out = ref None in
  let local_ write : Proffer.Backend.writer = fun o -> snapshot out o in
  let local_ f (r : Proffer.Resp.respond @ local) = f r in
  let () = Proffer.Backend.run ?on_error ?now req f write in
  taken out

let status t = t.status
let headers t = t.headers
let header t name =
  match Proffer.Headers.find t.headers name with
  | None -> None
  | Some v -> Some (globalize v)

let header_other t spelling =
  match Proffer.Headers.find_other t.headers spelling with
  | None -> None
  | Some v -> Some (globalize v)
let body t = t.body
let content_length t = t.content_length
