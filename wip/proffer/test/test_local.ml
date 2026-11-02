open Proffer

let[@zero_alloc] header (s : string @ local) = exclave_
  Resp.h_local Httpz.Header_name.Location s

let echo (respond : Resp.respond @ local) (s : string @ local) =
  let local_ headers = [ header s ] in
  let () = Resp.text respond ~headers s in
  ()

let stream (respond : Resp.respond @ local) (s : string @ local) =
  let () =
    Resp.stream respond "text/plain" (fun sink ->
        Body.Sink.write sink s;
        let () =
          Body.Sink.write_sub sink (Bytes.unsafe_of_string s) ~off:0
            ~len:(String.length s)
        in
        ())
  in
  ()

let delayed (respond : Resp.respond @ local) (s : string @ local) =
  let () =
    Resp.v respond ~headers:[] ~content_type:Null
      (stack_ Body.Delayed { length = None; gen = (fun () -> s) })
  in
  ()

let check (describe : Resp.respond @ local -> string @ local -> unit) =
  let result = ref "" in
  let req = Req.v ~meth:Httpz.Method.Get ~target:"/?value=borrowed%20body" () in
  let local_ value = Req.query_param_local req "value" in
  match value with
  | None -> assert false
  | Some value ->
      let req = Req.v ~meth:Httpz.Method.Get ~target:value ~body:value () in
      let () =
        Backend.run req
          (fun respond ->
            let () = describe respond (Req.body req) in
            ())
          (fun outcome ->
            match outcome.Backend.body with
            | Backend.String s -> result := Req.globalize s
            | Backend.Stream { write; _ } ->
                let sink = Backend.sink (fun s -> result := Req.globalize s) in
                let () = write sink in
                ()
            | _ -> assert false)
      in
      assert (!result = "borrowed body")

let () =
  check echo;
  check stream;
  check delayed;
  check (fun respond (s : string @ local) ->
      let () = Resp.not_found respond ~html:s () in
      ());
  check (fun respond (s : string @ local) ->
      let () = Resp.bad_request respond ~html:s () in
      ())
