(* SPDX-License-Identifier: ISC *)

let subscribe env ~service ~nsid ~params consume =
  let url = Tangled.Api.service_url service ^ "/xrpc/" ^ nsid in
  let url =
    Uriz.of_string_exn url |> fun uri ->
    Uriz.with_query_params uri params |> Uriz.to_string
  in
  let url =
    if String.starts_with ~prefix:"https:" url then
      "wss:" ^ String.sub url 6 (String.length url - 6)
    else "ws:" ^ String.sub url 5 (String.length url - 5)
  in
  Httpz_websocket_eio.with_connection ~max_message:(1024 * 1024) env url
  @@ fun socket ->
  let receive kind bytes ~off ~len =
    if kind <> Httpz_websocket.Binary then
      invalid_arg "Expected an XRPC binary event";
    let slice = ref (Bytesrw.Bytes.Slice.make bytes ~first:off ~length:len) in
    let reader =
      Bytesrw.Bytes.Reader.make (fun () ->
          let current = !slice in
          slice := Bytesrw.Bytes.Slice.eod;
          current)
    in
    let header =
      Atp.Dagcbor.decode_prefix ~strict:false ~max_bytes:(1024 * 1024) reader
    in
    let body =
      Atp.Dagcbor.decode_prefix ~strict:false ~max_bytes:(1024 * 1024) reader
    in
    if not (Bytesrw.Bytes.Slice.is_eod (Bytesrw.Bytes.Reader.read reader)) then
      invalid_arg "Trailing bytes in XRPC event";
    let body = Tangled.Api.encode Atp.Lex.jsont (Atp.Lex.of_dagcbor body) in
    match header with
    | `Map fields -> (
        match List.assoc_opt "op" fields with
        | Some (`Int 1L) ->
            let type_ =
              match List.assoc_opt "t" fields with
              | Some (`String value) -> value
              | _ -> invalid_arg "Missing XRPC event type"
            in
            consume type_ body
        | Some (`Int -1L) ->
            let message =
              match Tangled.Schema.member "message" body with
              | Some value -> Tangled.Schema.text value
              | None -> "Subscription error"
            in
            failwith message
        | _ -> invalid_arg "Invalid XRPC event opcode")
    | _ -> invalid_arg "Invalid XRPC event header"
  in
  while Httpz_websocket.receive socket ~f:receive do
    ()
  done
