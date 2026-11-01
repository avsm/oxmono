open Fetch

module Jar = struct
  type t = Cookie_jar.t

  let in_memory = Cookie_jar.in_memory
  let of_file = Cookie_jar.of_file
  let flush = Cookie_jar.flush
  let clear = Cookie_jar.clear

  let set_url t url line =
    match
      Cookie_jar.set t
        ~host:(Middleware.Url.host url)
        ~path:(Httpz_uri.encoded_path (Middleware.Url.to_uri url))
        ~https:(Middleware.Url.scheme url = `Https)
        line
    with
    | Ok () -> ()
    | Error reason ->
        Eio.Private.Trace.log ("fetch: ignoring Set-Cookie: " ^ reason)

  let header_for_url t url =
    Cookie_jar.header_for t
      ~host:(Middleware.Url.host url)
      ~path:(Httpz_uri.encoded_path (Middleware.Url.to_uri url))
      ~https:(Middleware.Url.scheme url = `Https)

  let set t url line =
    match Middleware.Url.of_string url with
    | Error _ -> ()
    | Ok u -> set_url t u line

  let header_for t url =
    match Middleware.Url.of_string url with
    | Error _ -> None
    | Ok u -> header_for_url t u
end

let with_jar ?scope jar client =
  let scope =
    Option.map (Middleware.Scope.list ~caller:"Fetch_cookies.with_jar") scope
  in
  Fetch.Middleware.middleware
    (fun next ~sw (req : Middleware.request) ->
      if not (Middleware.Scope.matches_any scope req.url) then next ~sw req
      else
        let req =
          if Http.Header.mem req.headers "cookie" then req
          else
            match Jar.header_for_url jar req.url with
            | None -> req
            | Some value ->
                {
                  req with
                  headers = Http.Header.add req.headers "Cookie" value;
                }
        in
        let resp = next ~sw req in
        (match Http.Header.get_multi (headers resp) "set-cookie" with
        | [] -> ()
        | values -> (
            Eio.Private.Trace.log
              (Fmt.str "fetch: storing %d cookie(s) from %s"
                 (List.length values) (Middleware.Url.host req.url));
            (* A store error (an on-change file jar's atomic replace, for
               instance) must not leak [resp]: close it before re-raising. *)
            match List.iter (Jar.set_url jar req.url) values with
            | () -> ()
            | exception exn ->
                let bt = Printexc.get_raw_backtrace () in
                close resp;
                Printexc.raise_with_backtrace exn bt));
        resp)
    client

let std ?(cookies = `Memory) ?retry ?(max_concurrent = 6) ?min_interval env
    backend =
  let clock = env#clock in
  let mono_clock = env#mono_clock in
  let with_cookies =
    match cookies with
    | `Off -> Fetch.Middleware.middleware Fun.id
    | `Memory -> with_jar (Jar.in_memory ~clock ())
    | `File path -> with_jar (Jar.of_file ~clock path)
  in
  backend |> with_cookies
  |> Fetch.with_limits ~clock:mono_clock ?min_interval ~max_concurrent
  |> Fetch.with_retry ~clock:mono_clock ~random:env#secure_random ~wall:clock
       ?config:retry
