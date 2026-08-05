(* Cookie jars for Fetch, attached as a middleware.

   The Cookie module (with the vendored ocaml-publicsuffix and its
   pre-generated PSL table) descends from ocaml-cookeio, reworked here
   into a purely client-side model — one store, no server-side
   delta/Set-Cookie machinery; see design.md s21. This module owns the
   jar: storage per RFC 6265 s5.3, retrieval per s5.4, Netscape-format
   persistence via an Eio.Path capability, and the [with_jar]
   middleware. *)

open Fetch

(* The host a URL names, for domain matching, and whether it was reached
   over https, for the Secure rules. [Fetch] validates and canonicalizes
   a URL before a middleware sees it, so a host is always present and
   already lowercase ASCII, and an IPv6 literal comes back unbracketed,
   which is the form stored cookies are compared against. *)
let url_host url = Option.get (Uri.host (Middleware.Url.to_uri url))
let url_is_https url = Uri.scheme (Middleware.Url.to_uri url) = Some "https"

module Jar = struct
  type t = {
    mutable cookies : Cookie.t list;
    now : unit -> Ptime.t;
    persist : (string -> unit) option;
    auto_save : bool;
    (* One lock for the store and its persistence: a save always writes
       a consistent snapshot. *)
    mutex : Eio.Mutex.t;
  }

  (* {2 Netscape cookies.txt (curl-compatible)}

     domain \t include_subdomains \t path \t secure \t expires \t name \t value

     curl marks HttpOnly cookies by prefixing the line with
     "#HttpOnly_"; other '#' lines are comments. Session cookies are
     written with expiry 0 and survive a round-trip, as curl's do. *)

  let http_only_prefix = "#HttpOnly_"

  let to_netscape cookies =
    let buf = Buffer.create 1024 in
    Buffer.add_string buf "# Netscape HTTP Cookie File\n";
    Buffer.add_string buf "# This is a generated file!  Do not edit.\n\n";
    List.iter
      (fun c ->
         let expires =
           match Cookie.expiry c with
           | `Session -> "0"
           | `At t -> string_of_int (int_of_float (Ptime.to_float_s t))
         in
         if Cookie.http_only c then Buffer.add_string buf http_only_prefix;
         Buffer.add_string buf
           (Fmt.str "%s\t%s\t%s\t%s\t%s\t%s\t%s\n" (Cookie.domain c)
              (if Cookie.host_only c then "FALSE" else "TRUE")
              (Cookie.path c)
              (if Cookie.secure c then "TRUE" else "FALSE")
              expires (Cookie.name c) (Cookie.value c)))
      cookies;
    Buffer.contents buf

  let of_netscape ~now content =
    let parse_line line =
      let line = String.trim line in
      let http_only = String.starts_with ~prefix:http_only_prefix line in
      let line =
        if http_only then
          String.sub line (String.length http_only_prefix)
            (String.length line - String.length http_only_prefix)
        else line
      in
      if line = "" || line.[0] = '#' then None
      else
        match String.split_on_char '\t' line with
        | [ domain; subdomains; path; secure; expires; name; value ] ->
          let domain =
            if String.length domain > 1 && domain.[0] = '.' then
              String.sub domain 1 (String.length domain - 1)
            else domain
          in
          let expiry =
            match int_of_string_opt expires with
            | Some e when e <> 0 ->
              (match Ptime.of_float_s (float_of_int e) with
               | Some t -> `At t
               | None -> `Session)
            | _ -> `Session
          in
          Some
            (Cookie.v ~domain ~path ~name ~value ~secure:(secure = "TRUE")
               ~http_only ~host_only:(subdomains <> "TRUE") ~expiry ~now ())
        | _ ->
          Eio.Private.Trace.log
            "fetch: skipping a malformed line in the cookie file";
          None
    in
    List.filter_map parse_line (String.split_on_char '\n' content)

  (* {2 Creation} *)

  let make ~clock ?persist ~auto_save cookies =
    let clock = (clock :> float Eio.Time.clock_ty Eio.Resource.t) in
    let now () =
      Ptime.of_float_s (Eio.Time.now clock)
      |> Option.value ~default:Ptime.epoch
    in
    { cookies; now; persist; auto_save; mutex = Eio.Mutex.create () }

  let in_memory ~clock () = make ~clock ~auto_save:false []

  let of_file ~clock ?(save : [ `On_change | `Manual ] = `On_change) path =
    let path = (path :> Eio.Fs.dir_ty Eio.Path.t) in
    let clock' = (clock :> float Eio.Time.clock_ty Eio.Resource.t) in
    let now () =
      Ptime.of_float_s (Eio.Time.now clock')
      |> Option.value ~default:Ptime.epoch
    in
    let cookies =
      match Eio.Path.load path with
      | content -> of_netscape ~now:(now ()) content
      | exception Eio.Io _ -> [] (* not there yet; created on first save *)
    in
    let persist content =
      (* Atomic: write a temporary sibling, then rename over the target. *)
      match Eio.Path.split path with
      | None -> Eio.Path.save ~create:(`Or_truncate 0o600) path content
      | Some (dir, name) ->
        let tmp = Eio.Path.(dir / (name ^ ".tmp")) in
        Eio.Path.save ~create:(`Or_truncate 0o600) tmp content;
        Eio.Path.rename tmp path
    in
    make ~clock ~persist ~auto_save:(save = `On_change) cookies

  (* Call with the mutex held. *)
  let save_locked t =
    match t.persist with
    | None -> ()
    | Some persist -> persist (to_netscape t.cookies)

  let maybe_save_locked t = if t.auto_save then save_locked t

  let flush t = Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> save_locked t)

  let clear t =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        t.cookies <- [];
        maybe_save_locked t)

  (* {2 Storage (RFC 6265 s5.3)} *)

  (* 6265bis s5.5 step 13: over plaintext, a non-Secure cookie must not
     collide with a stored Secure cookie whose name matches, whose
     domain domain-matches the new cookie's (either direction), and
     whose path the new cookie's path path-matches — otherwise http
     could shadow an https session cookie, exactly (or nearly) in
     place. *)
  let shadows_secure t (cookie : Cookie.t) =
    List.exists
      (fun c ->
         Cookie.secure c
         && String.equal (Cookie.name c) (Cookie.name cookie)
         && (Cookie.domain_suffix_matches ~sub:(Cookie.domain cookie)
               (Cookie.domain c)
             || Cookie.domain_suffix_matches ~sub:(Cookie.domain c)
                  (Cookie.domain cookie))
         && Cookie.path_matches ~request_path:(Cookie.path cookie) c)
      t.cookies

  (* RFC 6265 s6.1 expects a client to bound what a server can store, or
     a hostile origin grows the jar — and the file behind it — without
     limit. Browser-scale numbers: per-cookie name+value, per-domain
     count, and a total across the jar. When the per-domain cap is hit,
     evict that domain's least-recently-used cookie (s5.3 step 11's
     eviction order); when the total is hit, the jar's LRU. *)
  let max_cookie_bytes = 4096
  let max_per_domain = 50
  let max_total = 3000

  let too_large (cookie : Cookie.t) =
    String.length (Cookie.name cookie) + String.length (Cookie.value cookie)
    > max_cookie_bytes

  (* Drop the least recently used of [candidates] from the store. *)
  let evict_lru t candidates =
    match
      List.sort
        (fun a b -> Ptime.compare (Cookie.last_access a) (Cookie.last_access b))
        candidates
    with
    | [] -> ()
    | victim :: _ ->
      Eio.Private.Trace.log "fetch: cookie jar full, evicting the oldest entry";
      t.cookies <- List.filter (fun c -> not (Cookie.same_identity victim c)) t.cookies

  (* Insert or replace on (name, domain, path); an already-expired
     arrival is a deletion (the Max-Age=0 removal idiom). Replacement
     keeps the old creation time (s5.3 step 12). *)
  let store_locked t ~now cookie =
    let existing, rest =
      List.partition (Cookie.same_identity cookie) t.cookies
    in
    (if Cookie.is_expired ~now cookie then t.cookies <- rest
     else
       let cookie =
         match existing with
         | old :: _ ->
           Cookie.with_creation_time (Cookie.creation_time old) cookie
         | [] -> cookie
       in
       t.cookies <- cookie :: rest;
       (* Replacing an entry cannot exceed a cap, so only a fresh one
          needs the eviction pass. *)
       if existing = [] then begin
         let domain = Cookie.domain cookie in
         let same_domain =
           List.filter (fun c -> String.equal (Cookie.domain c) domain) t.cookies
         in
         if List.length same_domain > max_per_domain then
           evict_lru t
             (List.filter (fun c -> not (Cookie.same_identity cookie c))
                same_domain);
         if List.length t.cookies > max_total then
           evict_lru t
             (List.filter (fun c -> not (Cookie.same_identity cookie c))
                t.cookies)
       end);
    maybe_save_locked t

  (* The request's path, for the default-path and path-match rules. *)
  let request_path url =
    let pq = Middleware.Url.path_and_query url in
    match String.index_opt pq '?' with
    | None -> pq
    | Some i -> String.sub pq 0 i

  (* [url]-typed forms, used by [with_jar] on requests in flight.
     (RFC 6265 s5.3 step 10 — a "non-HTTP" API may not overwrite an
     HttpOnly cookie — has no analogue here: the only way in is holding
     the jar itself, and that already carries the authority to clear it,
     so the rule would restrict nothing while surprising callers who
     seed a jar by hand.) *)
  let set_url t url line =
    let host = url_host url in
    let now = t.now () in
    let refuse reason = Eio.Private.Trace.log ("fetch: " ^ reason) in
    match Cookie.parse_set_cookie ~now ~host ~path:(request_path url) line with
    | Error reason -> refuse ("ignoring Set-Cookie: " ^ reason)
    | Ok cookie ->
      let plaintext = not (url_is_https url) in
      if too_large cookie then
        refuse
          (Fmt.str "refusing a cookie over %d bytes of name and value"
             max_cookie_bytes)
      else if plaintext && Cookie.has_secure_prefix (Cookie.name cookie) then
        refuse "refusing a __Secure-/__Host- cookie set over plaintext http"
      else
        (* The checks against stored state and the store itself must see
           one consistent jar. *)
        Eio.Mutex.use_rw ~protect:true t.mutex @@ fun () ->
        if plaintext && not (Cookie.secure cookie) && shadows_secure t cookie
        then refuse "refusing a plaintext cookie that would shadow a Secure one"
        else store_locked t ~now cookie

  (* Retrieval (RFC 6265 s5.4): filter, sort, update last-access.
     Expired cookies found on the way are evicted from the store. *)
  let header_for_url t url =
    let host = url_host url in
    let path = request_path url in
    let is_secure = url_is_https url in
    let now = t.now () in
    let matched =
      Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
          let live, expired =
            List.partition (fun c -> not (Cookie.is_expired ~now c)) t.cookies
          in
          if expired <> [] then begin
            t.cookies <- live;
            maybe_save_locked t
          end;
          let applies c =
            Cookie.domain_matches ~host c
            && Cookie.path_matches ~request_path:path c
            && ((not (Cookie.secure c)) || is_secure)
          in
          t.cookies <-
            List.map (fun c -> if applies c then Cookie.touch ~now c else c)
              t.cookies;
          List.sort Cookie.compare_order (List.filter applies t.cookies))
    in
    match matched with
    | [] -> None
    | cookies -> Some (Cookie.cookie_header cookies)

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
