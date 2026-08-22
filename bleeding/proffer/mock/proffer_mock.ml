module M = Httpz.Method

type response = {
  status : Proffer.Status.t;
  headers : Proffer.Headers.t;
  body : string;
  content_length : int64 option;
}

(* The outcome reaches the writer at [local], so what a test reads has to be
   copied out of it here. [Headers.to_list] then [of_list] is that copy: the
   block a test holds is a heap value, and the one the backend was handed was
   never one. *)
(* The outcome reaches the writer at [local], so what a test reads has to be
   copied out of it. [Headers.to_list] then [of_list] is that copy: the block a
   test holds is a heap value, and the one the backend was handed was never
   one. The length is copied for the same reason: the option itself is built
   in the backend's region now, so rebuilding it here is what moves it to the
   heap a test can keep. *)
let copy_length (l : int64 option @ local) =
  match l with None -> None | Some n -> Some (Int64.of_int (Int64.to_int n))

let snapshot out (o : Proffer.Backend.outcome @ local) =
  let body, content_length =
    match o.Proffer.Backend.body with
    | Proffer.Backend.Empty ->
        ("", copy_length o.Proffer.Backend.content_length)
    | Proffer.Backend.String s ->
        (s, copy_length o.Proffer.Backend.content_length)
    | Proffer.Backend.Stream { write = w; _ } ->
        let b = Buffer.create 256 in
        w (Proffer.Backend.sink (fun s -> Buffer.add_string b s));
        let s = Buffer.contents b in
        (s, Some (Int64.of_int (String.length s)))
  in
  out :=
    Some
      {
        status = o.Proffer.Backend.status;
        headers =
          Proffer.Headers.of_list
            (Proffer.Headers.to_list o.Proffer.Backend.headers);
        body;
        content_length;
      }

let taken out =
  match !out with
  | Some r -> r
  | None ->
      (* The backend writes exactly once, so this is unreachable. It is a
         failure rather than an assertion because a test that saw it would
         otherwise read a response the backend never produced. *)
      failwith "Proffer_mock: the backend wrote no response"

let request ?headers ?body ?on_error compiled env meth target =
  let headers = Option.map Proffer.Headers.of_list headers in
  let req = Proffer.Req.v ~meth ~target ?headers ?body () in
  let out = ref None in
  let local_ write : Proffer.Backend.writer = fun o -> snapshot out o in
  let () = Proffer.Backend.handle ?on_error compiled env req write in
  taken out

let describe ?headers ?body ?on_error ?(meth = M.Get) ?(target = "/") f =
  let headers = Option.map Proffer.Headers.of_list headers in
  let req = Proffer.Req.v ~meth ~target ?headers ?body () in
  let out = ref None in
  let local_ write : Proffer.Backend.writer = fun o -> snapshot out o in
  let local_ f (r : Proffer.Resp.respond @ local) = f r in
  let () = Proffer.Backend.run ?on_error req f write in
  taken out

let status t = t.status
let headers t = t.headers
let header t name = Proffer.Headers.find t.headers name
let header_other t spelling = Proffer.Headers.find_other t.headers spelling
let body t = t.body
let content_length t = t.content_length
