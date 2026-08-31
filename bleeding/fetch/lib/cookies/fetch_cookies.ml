(* Cookie jars for Fetch, attached as a middleware.

   The cookie model and the jar itself live in cookeio (whose client
   side was reworked here before being folded back upstream; see
   design.md s21). This module contributes only what Fetch knows: the
   URL of each request in flight and the middleware hook. *)

open Fetch

(* The host a URL names, for domain matching, and whether it was reached
   over https, for the Secure rules. [Fetch] validates and canonicalizes
   a URL before a middleware sees it, so a host is always present and
   already lowercase ASCII, and an IPv6 literal comes back unbracketed,
   which is the form stored cookies are compared against. *)
let url_host url = Option.get (Uri.host (Middleware.Url.to_uri url))
let url_is_https url = Uri.scheme (Middleware.Url.to_uri url) = Some "https"

(* The request's path, for the default-path and path-match rules. *)
let url_path url =
  let pq = Middleware.Url.path_and_query url in
  match String.index_opt pq '?' with
  | None -> pq
  | Some i -> String.sub pq 0 i

module Jar = struct
  type t = Cookeio_jar.t

  let in_memory = Cookeio_jar.in_memory
  let of_file = Cookeio_jar.of_file
  let flush = Cookeio_jar.flush
  let clear = Cookeio_jar.clear

  let set_url t url line =
    match
      Cookeio_jar.set t ~host:(url_host url) ~path:(url_path url)
        ~https:(url_is_https url) line
    with
    | Ok () -> ()
    | Error reason ->
      Eio.Private.Trace.log ("fetch: ignoring Set-Cookie: " ^ reason)

  let header_for_url t url =
    Cookeio_jar.header_for t ~host:(url_host url) ~path:(url_path url)
      ~https:(url_is_https url)

  (* String-URL forms, for manual and testing use. *)
  let set t url line =
    match Middleware.Url.of_string url with
    | Error _ -> () (* an unparseable context URL cannot set cookies *)
    | Ok u -> set_url t u line

  let header_for t url =
    match Middleware.Url.of_string url with
    | Error _ -> None
    | Ok u -> header_for_url t u
end

let in_scope scope url =
  match scope with
  | None -> true
  | Some ps -> List.exists (fun s -> Middleware.Scope.matches s url) ps

let with_jar ?scope jar client =
  let scope =
    Option.map
      (List.map (Middleware.Scope.v ~caller:"Fetch_cookies.with_jar"))
      scope
  in
  Fetch.Middleware.middleware
    (fun next ~sw (req : Middleware.request) ->
       if not (in_scope scope req.url) then next ~sw req
       else (
         let req =
           if Http.Header.mem req.headers "cookie" then req
           else
             match Jar.header_for_url jar req.url with
             | None -> req
             | Some value ->
               { req with headers = Http.Header.add req.headers "Cookie" value }
         in
         let resp = next ~sw req in
         (match Http.Header.get_multi (headers resp) "set-cookie" with
          | [] -> ()
          | values ->
            Eio.Private.Trace.log
              (Fmt.str "fetch: storing %d cookie(s) from %s"
                 (List.length values) (url_host req.url));
            List.iter (Jar.set_url jar req.url) values);
         resp))
    client
