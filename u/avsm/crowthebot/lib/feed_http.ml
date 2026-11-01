module Url = Fetch.Middleware.Url

let excluded_v4 =
  List.map Ipaddr.V4.Prefix.of_string_exn
    [
      "0.0.0.0/8";
      "10.0.0.0/8";
      "100.64.0.0/10";
      "127.0.0.0/8";
      "169.254.0.0/16";
      "172.16.0.0/12";
      "192.0.0.0/24";
      "192.0.2.0/24";
      "192.168.0.0/16";
      "198.18.0.0/15";
      "198.51.100.0/24";
      "203.0.113.0/24";
      "224.0.0.0/4";
      "240.0.0.0/4";
    ]

let global_v6 = Ipaddr.V6.Prefix.of_string_exn "2000::/3"

let excluded_v6 =
  List.map Ipaddr.V6.Prefix.of_string_exn
    [ "2001::/23"; "2001:db8::/32"; "2002::/16"; "3fff::/20" ]

let public_address ip =
  match (Ipaddr.to_v4 ip, ip) with
  | Some ip, _ -> not (List.exists (Ipaddr.V4.Prefix.mem ip) excluded_v4)
  | None, Ipaddr.V6 ip ->
      Ipaddr.V6.Prefix.mem ip global_v6
      && not (List.exists (Ipaddr.V6.Prefix.mem ip) excluded_v6)
  | None, Ipaddr.V4 _ -> assert false

let validate url =
  if Url.has_fragment url then invalid_arg "Feed URLs cannot contain fragments.";
  match Ipaddr.of_string (Url.host url) with
  | Ok ip when not (public_address ip) ->
      invalid_arg "Feed URLs must use public network addresses."
  | _ -> ()

let normalize text =
  if String.length text > 2048 then invalid_arg "Feed URL is too long.";
  match Url.of_string text with
  | Error _ ->
      invalid_arg "Use an absolute HTTP(S) feed URL without credentials."
  | Ok url ->
      validate url;
      Url.to_string url

let public_connect net ~sw ~host ~port =
  let addresses =
    Eio.Net.getaddrinfo_stream ~service:(string_of_int port) net host
  in
  let permitted (address : Eio.Net.Sockaddr.stream) =
    match address with
    | `Tcp (ip, _) -> public_address (Ipaddr.of_octets_exn (ip :> string))
    | `Unix _ -> false
  in
  let rec connect = function
    | [] -> invalid_arg "Feed host has no reachable public network address."
    | address :: rest -> (
        if not (permitted address) then connect rest
        else
          match Eio.Net.connect ~sw net address with
          | flow -> (flow :> Fetch_httpz.conn)
          | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
          | exception _ -> connect rest)
  in
  connect addresses

type response =
  | Unchanged
  | Document of {
      body : string;
      url : string;
      etag : string option;
      last_modified : string option;
    }

type t =
  url:string -> etag:string option -> last_modified:string option -> response

let max_bytes = 64 * 1024 * 1024

let create ~fetch ~clock =
  let fetch =
    Fetch.restrict ~methods:[ `GET ]
      ~filter:(fun req ->
        try
          validate req.url;
          `Allow
        with Invalid_argument m -> `Reject m)
      fetch
  in
  fun ~url ~etag ~last_modified ->
    let url = normalize url in
    Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock 60.) @@ fun () ->
    let headers =
      match etag with
      | None -> Fetch.Header.[]
      | Some value -> Fetch.Header.[ raw "If-None-Match" value ]
    in
    let headers =
      match last_modified with
      | None -> headers
      | Some value ->
          Fetch.Header.(append headers [ raw "If-Modified-Since" value ])
    in
    Fetch.with_response ~headers ~redirects:3 fetch `GET url (fun response ->
        match Fetch.status response with
        | 304 -> Unchanged
        | 200 ->
            let input =
              Eio.Buf_read.of_flow ~max_size:65536 (Fetch.body response)
            in
            let buffer = Buffer.create 65536 in
            let rec read () =
              if Eio.Buf_read.at_end_of_input input then ()
              else begin
                let chunk =
                  Eio.Buf_read.take
                    (min 65536 (Eio.Buf_read.buffered_bytes input))
                    input
                in
                if Buffer.length buffer + String.length chunk > max_bytes then
                  invalid_arg "Feed page exceeds 64 MiB. Use a paginated feed.";
                Buffer.add_string buffer chunk;
                read ()
              end
            in
            read ();
            let body = Buffer.contents buffer in
            let header name =
              Option.bind
                (Fetch.header (Fetch.Header.text name) response)
                (fun s ->
                  if
                    String.length s <= 1024
                    && not (String.contains s '\r' || String.contains s '\n')
                  then Some s
                  else None)
            in
            Document
              {
                body;
                url = Fetch.url response;
                etag = header "ETag";
                last_modified = header "Last-Modified";
              }
        | status -> failwith (Printf.sprintf "Feed returned HTTP %d." status))
