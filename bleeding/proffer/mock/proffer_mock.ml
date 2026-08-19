type response = {
  status : Proffer.Status.t;
  headers : Proffer.Headers.t;
  body : string;
  content_length : int64 option;
}

let request ?headers ?body ?on_error compiled env meth target =
  let req = Proffer.Req.v ~meth ~target ?headers ?body () in
  let o = Proffer.Backend.handle ?on_error compiled env req in
  let body, content_length =
    match o.Proffer.Backend.body with
    | `Empty -> ("", o.Proffer.Backend.content_length)
    | `String s -> (s, o.Proffer.Backend.content_length)
    | `Stream (_, write) ->
        let b = Buffer.create 256 in
        write (Proffer.Backend.sink (fun s -> Buffer.add_string b s));
        let s = Buffer.contents b in
        (s, Some (Int64.of_int (String.length s)))
  in
  {
    status = o.Proffer.Backend.status;
    headers = o.Proffer.Backend.headers;
    body;
    content_length;
  }

let status t = t.status
let headers t = t.headers
let header t name = Proffer.Headers.find t.headers name
let body t = t.body
let content_length t = t.content_length
