open Fetch

type tag = [ `Generic | `Macos ]

type t = tag Fetch.ty Eio.Resource.t

let available = Nssessionurl.available

type Eio.Exn.Backend.t += Nsurl_error of string * int * string

let () =
  Eio.Exn.Backend.register_pp (fun f -> function
      | Nsurl_error (domain, code, msg) ->
        Fmt.pf f "Nsurl_error(%s %d, %S)" domain code msg;
        true
      | _ -> false
    )

type config = {
  session : Nssessionurl.Session.t;
  max_response : int;
  user_agent : string;
  decode : bool;
}

(* {2 Errors}

   NSURLSession reports failures as an NSError; the NSURLErrorDomain
   codes (Foundation's NSURLError.h) say which {!Fetch.error} each one
   is. Cancellation surfaces as Eio cancellation before the -999 code is
   ever seen, so it needs no case here. *)
let map_nsurl_error (e : Nssessionurl.error) =
  let detail = Nsurl_error (e.domain, e.code, e.message) in
  if e.domain <> "NSURLErrorDomain" then err (Protocol_error e.message)
  else match e.code with
    | -1000 (* badURL *) | -1002 (* unsupportedURL *) ->
      err (Invalid_url e.message)
    | -1001 (* timedOut *) -> err (Connection_failure Timeout)
    | -1003 (* cannotFindHost *) | -1006 (* dnsLookupFailed *)
    | -1004 (* cannotConnectToHost *) | -1009 (* notConnectedToInternet *) ->
      err (Connection_failure (Refused detail))
    | -1022 (* appTransportSecurityRequiresSecureConnection *) ->
      err (Denied e.message)
    | c when c <= -1200 && c >= -1206 (* the TLS and certificate block *) ->
      err (Tls_failure e.message)
    | _ -> err (Protocol_error e.message)

let map_exn = function
  | Nssessionurl.Error e -> map_nsurl_error e
  | Failure msg -> err (Invalid_url msg)
  | ex -> ex

let reraise ex =
  let bt = Printexc.get_raw_backtrace () in
  Printexc.raise_with_backtrace (map_exn ex) bt

(* {2 Response bodies}

   nssessionurl already bounds the memory between the task and the
   reader ([max_buffer], with the task suspended past it); this wrapper
   adds the cap on the body's total size and turns a mid-body
   {!Nssessionurl.Error} into a {!Fetch.error}. The cap counts the bytes
   NSURLSession delivers, which are post-decoding ones when it negotiated
   the coding, so it bounds a compression bomb too. *)
type response_body = {
  src : Eio.Flow.source_ty Eio.Resource.t;
  max_response : int;
  mutable seen : int;
  close : unit -> unit;
}

module Response_body = struct
  type t = response_body

  let read_methods = []

  let single_read t buf =
    try
      let n = Eio.Flow.single_read t.src buf in
      t.seen <- t.seen + n;
      if t.seen > t.max_response then
        raise (err (Protocol_error
                      (Fmt.str "response body exceeds %d bytes"
                         t.max_response)));
      n
    with
    | End_of_file -> t.close (); raise End_of_file
    | ex -> t.close (); reraise ex
end

let response_body_handler = Eio.Flow.Pi.source (module Response_body)

(* {2 One exchange} *)

module Backend = struct
  type t = config
  type tag = [ `Generic | `Macos ]

  let request cfg ~sw (req : Middleware.request) =
    let caller_encoding = Http.Header.mem req.headers "accept-encoding" in
    let auto_decode = cfg.decode && not caller_encoding in
    let headers = req.headers in
    let headers =
      if cfg.decode || caller_encoding then headers
      else Http.Header.add headers "accept-encoding" "identity"
    in
    let headers =
      Http.Header.add_unless_exists headers "user-agent" cfg.user_agent
    in
    let body, headers =
      match req.body with
      | Empty -> (None, headers)
      | String s -> (Some (Nssessionurl.Fixed s), headers)
      | Stream { length; flow } ->
        (match length with
         | Some l when Int64.compare l 0L < 0 ->
           raise (err (Invalid_request
                         (Fmt.str "request body has a negative declared \
                                   length of %Ld" l)))
         | Some l ->
           (* nssessionurl frames a streamed body as chunked unless a
              Content-Length request header names its size; deriving that
              header from the declared length is the backend's job. *)
           ( Some (Nssessionurl.Stream flow),
             Http.Header.replace headers "content-length" (Int64.to_string l) )
         | None -> (Some (Nssessionurl.Stream flow), headers))
    in
    match
      Exchange.run ~sw (fun ~sw ->
        Nssessionurl.fetch ~sw ~session:cfg.session
          ~meth:(Http.Method.to_string req.meth)
          ~headers:(Http.Header.to_list headers)
          ?body ~follow_redirects:false
          (Middleware.Url.to_string req.url))
    with
    | exception ex -> reraise ex
    | (info, flow), close ->
      let headers = Http.Header.of_list info.Nssessionurl.headers in
      let headers =
        (* Present the decoded view, as a backend must. *)
        if auto_decode && Http.Header.mem headers "content-encoding" then
          Http.Header.remove
            (Http.Header.remove headers "content-encoding")
            "content-length"
        else headers
      in
      let body =
        Eio.Resource.T
          ({ src = flow; max_response = cfg.max_response; seen = 0; close },
           response_body_handler)
      in
      (* NSHTTPURLResponse does not say which HTTP version answered
         (the protocol is only in the task metrics, which arrive after
         the transfer), so no version claim is made. *)
      Fetch.Middleware.Pi.response ~status:info.Nssessionurl.status ~headers
        ~version:(`Other "unknown") ~body ~close ~url:req.url ()
end

let handler = Fetch.Middleware.Pi.client (module Backend)

let default_user_agent = "fetch-macos"

let v ?request_timeout ?resource_timeout ?max_connections_per_host
    ?allows_cellular ?allows_expensive ?allows_constrained
    ?waits_for_connectivity ?min_tls
    ?(max_response = 256 * 1024 * 1024) ?(user_agent = default_user_agent)
    ?(decode = true) () : t =
  let session =
    Nssessionurl.Session.create ~ephemeral:true ?request_timeout
      ?resource_timeout ?max_connections_per_host ?allows_cellular
      ?allows_expensive ?allows_constrained ?waits_for_connectivity
      ~cache_policy:Nssessionurl.Session.Reload_ignoring_local_cache
      ~set_cookies:false ~cookie_accept_policy:Nssessionurl.Session.Never
      ?min_tls ()
  in
  Eio.Resource.T ({ session; max_response; user_agent; decode }, handler)

let std ?(cookies = `Memory) ?retry ?(max_concurrent = 6) ?min_interval env =
  let clock = env#clock in
  let mono_clock = env#mono_clock in
  let backend = v () in
  let with_cookies =
    match cookies with
    | `Off -> fun t -> Fetch.Middleware.of_handler (Fetch.Middleware.handler t)
    | `Memory -> Fetch_cookies.with_jar (Fetch_cookies.Jar.in_memory ~clock ())
    | `File path -> Fetch_cookies.with_jar (Fetch_cookies.Jar.of_file ~clock path)
  in
  backend
  |> with_cookies
  |> Fetch.with_limits ~clock:mono_clock ?min_interval ~max_concurrent
  |> Fetch.with_retry ~clock:mono_clock ~random:env#secure_random ?config:retry
