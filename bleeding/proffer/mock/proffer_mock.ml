let request ?headers ?body compiled env meth target =
  let req = Proffer.Req.v ~meth ~target ?headers ?body () in
  let outcome = Proffer.Serve.handle compiled env req in
  match outcome.Proffer.Serve.body with
  | `Stream (_, write) ->
      let b = Buffer.create 256 in
      write (Proffer.Serve.sink (fun s -> Buffer.add_string b s));
      let s = Buffer.contents b in
      {
        outcome with
        Proffer.Serve.body = `String s;
        content_length = Some (Int64.of_int (String.length s));
      }
  | `Empty | `String _ -> outcome
