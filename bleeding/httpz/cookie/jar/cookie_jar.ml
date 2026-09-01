type t =
  { mutable cookies : Cookie.t list
  ; now : unit -> Ptime.t
  ; persist : (string -> unit) option
  ; auto_save : bool
  ; (* One lock for the store and its persistence: a save always writes
     a consistent snapshot. *)
    mutex : Eio.Mutex.t
  }

(* Netscape records contain domain, subdomain inclusion, path, secure status,
   expiry, name, and value. Curl prefixes HttpOnly records with [#HttpOnly_]. *)

let http_only_prefix = "#HttpOnly_"

(* Browser-scale storage bounds apply equally to persisted input. The file cap
   has room for [max_total] maximum-sized records plus separators and comments,
   while preventing [of_file] from reading an attacker-sized file wholesale. *)
let max_name_value_bytes = 4096
let max_cookie_bytes = 8192
let max_per_domain = 50
let max_total = 3000
let max_cookie_file_bytes = 32 * 1024 * 1024
let temp_salt = Random.State.(bits (make_self_init ()))
let temp_serial = Atomic.make 0

let name_value_too_large (cookie : Cookie.t) =
  String.length (Cookie.name cookie) + String.length (Cookie.value cookie)
  > max_name_value_bytes
;;

let too_large (cookie : Cookie.t) =
  String.length (Cookie.name cookie)
  + String.length (Cookie.value cookie)
  + String.length (Cookie.path cookie)
  + String.length (Cookie.domain cookie)
  > max_cookie_bytes
;;

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
       Buffer.add_string
         buf
         (Fmt.str
            "%s\t%s\t%s\t%s\t%s\t%s\t%s\n"
            (Cookie.domain c)
            (if Cookie.host_only c then "FALSE" else "TRUE")
            (Cookie.path c)
            (if Cookie.secure c then "TRUE" else "FALSE")
            expires
            (Cookie.name c)
            (Cookie.value c)))
    cookies;
  Buffer.contents buf
;;

let of_netscape ~now content =
  let parse_line line =
    let line =
      let len = String.length line in
      if len > 0 && line.[len - 1] = '\r' then String.sub line 0 (len - 1) else line
    in
    let http_only = String.starts_with ~prefix:http_only_prefix line in
    let line =
      if http_only
      then
        String.sub
          line
          (String.length http_only_prefix)
          (String.length line - String.length http_only_prefix)
      else line
    in
    if line = "" || line.[0] = '#'
    then None
    else (
      match String.split_on_char '\t' line with
      | [ domain; subdomains; path; secure; expires; name; value ] ->
        let domain =
          if String.length domain > 1 && domain.[0] = '.'
          then String.sub domain 1 (String.length domain - 1)
          else domain
        in
        let expiry =
          if expires = "0"
          then Some `Session
          else if
            expires <> ""
            && String.for_all (function '0' .. '9' -> true | _ -> false) expires
          then
            Option.bind (int_of_string_opt expires) (fun e ->
              Option.map (fun t -> `At t) (Ptime.of_float_s (float_of_int e)))
          else None
        in
        (* The file is a trust boundary of its own: a value carrying ';' would
           become a second pair in every Cookie header the jar emits. Such a
           line is dropped like any other malformed one. *)
        if
          (subdomains <> "TRUE" && subdomains <> "FALSE")
          || (secure <> "TRUE" && secure <> "FALSE")
          || not (Cookie.valid_domain (String.lowercase_ascii domain))
          || not (Cookie.valid_path path)
          || not (Cookie.valid_name name && Cookie.valid_value value)
        then None
        else (
          let domain = String.lowercase_ascii domain in
          let host_only = subdomains = "FALSE" in
          let safe_scope =
            host_only
            || ((not (Httpz.Ip.is_literal domain))
                && match Pubsuffix.is_public_suffix domain with
                   | Ok false -> true
                   | Ok true | Error _ -> false)
          in
          match expiry with
          | None -> None
          | Some expiry when safe_scope ->
            (match
               Cookie.v
                 ~domain
                 ~path
                 ~name
                 ~value
                 ~secure:(secure = "TRUE")
                 ~http_only
                 ~host_only
                 ~expiry
                 ~now
                 ()
             with
             | cookie when Cookie.is_expired ~now cookie -> None
             | cookie -> Some cookie
             | exception Invalid_argument _ -> None)
          | Some _ -> None)
      | _ -> None)
  in
  let counts = Hashtbl.create 64 in
  let identities = Hashtbl.create max_total in
  let total = ref 0 in
  let cookies = ref [] in
  let add_line line =
    match parse_line line with
    | None -> ()
    | Some cookie ->
        let domain = Cookie.domain cookie in
        let count = Option.value ~default:0 (Hashtbl.find_opt counts domain) in
        if
          count < max_per_domain
          && not (name_value_too_large cookie || too_large cookie)
          && not
               (Hashtbl.mem identities
                  (Cookie.name cookie, Cookie.domain cookie, Cookie.path cookie))
        then (
          Hashtbl.replace counts domain (count + 1);
          Hashtbl.add identities
            (Cookie.name cookie, Cookie.domain cookie, Cookie.path cookie) ();
          incr total;
          cookies := cookie :: !cookies)
  in
  let length = String.length content in
  let rec lines pos =
    if pos < length && !total < max_total then
      match String.index_from_opt content pos '\n' with
      | None -> add_line (String.sub content pos (length - pos))
      | Some stop ->
          add_line (String.sub content pos (stop - pos));
          lines (stop + 1)
  in
  lines 0;
  List.rev !cookies
;;

let make ~now ?persist ~auto_save cookies =
  { cookies; now; persist; auto_save; mutex = Eio.Mutex.create () }
;;

let clock_now clock =
  let clock = (clock :> float Eio.Time.clock_ty Eio.Resource.t) in
  fun () -> Ptime.of_float_s (Eio.Time.now clock) |> Option.value ~default:Ptime.epoch
;;

let in_memory ~clock () = make ~now:(clock_now clock) ~auto_save:false []

let of_file ~clock ?(save : [ `On_change | `Manual ] = `On_change)
      ?(missing : [ `Empty | `Error ] = `Empty) path =
  let path = (path :> Eio.Fs.dir_ty Eio.Path.t) in
  let dir, name =
    match Eio.Path.split path with
    | Some pair -> pair
    | None -> invalid_arg "Cookie_jar.of_file: path has no destination filename"
  in
  let now = clock_now clock in
  let cookies =
    match
      Eio.Path.with_open_in path (fun flow ->
        let reader =
          Eio.Buf_read.of_flow ~max_size:(max_cookie_file_bytes + 1) flow
        in
        Eio.Buf_read.take_all reader)
    with
    | content -> of_netscape ~now:(now ()) content
    | exception Eio.Buf_read.Buffer_limit_exceeded -> []
    | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) when missing = `Empty -> []
  in
  let persist content =
    (* Atomic visibility, without a power-loss/fsync durability promise. *)
      let rec create attempts =
        if attempts = 0 then failwith "could not create a unique cookie-jar temporary file";
        let serial = Atomic.fetch_and_add temp_serial 1 in
        let tmp =
          Eio.Path.(dir / Fmt.str ".%s.httpz-tmp-%08x-%d" name temp_salt serial)
        in
        let owned = ref false in
        match
          Eio.Path.with_open_out ~create:(`Exclusive 0o600) tmp (fun flow ->
            owned := true;
            Eio.Flow.copy_string content flow)
        with
        | () -> tmp
        | exception Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _)
          when not !owned ->
            create (attempts - 1)
        | exception exn ->
            let backtrace = Printexc.get_raw_backtrace () in
            if !owned then
              (try Eio.Cancel.protect (fun () -> Eio.Path.unlink ~missing_ok:true tmp)
               with _ -> ());
            Printexc.raise_with_backtrace exn backtrace
      in
      let tmp = create 100 in
      (match Eio.Path.rename tmp path with
       | () -> ()
       | exception exn ->
           let backtrace = Printexc.get_raw_backtrace () in
           (try Eio.Cancel.protect (fun () -> Eio.Path.unlink ~missing_ok:true tmp)
            with _ -> ());
           Printexc.raise_with_backtrace exn backtrace)
  in
  make ~now ~persist ~auto_save:(save = `On_change) cookies
;;

(* The caller must hold [mutex]. *)
let save_locked t =
  match t.persist with
  | None -> ()
  | Some persist -> persist (to_netscape t.cookies)
;;

let maybe_save_locked t = if t.auto_save then save_locked t
let flush t = Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> save_locked t)

let clear t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    t.cookies <- [];
    maybe_save_locked t)
;;

let cookies t = Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> t.cookies)

let pp ppf t =
  let cookies = cookies t in
  Format.fprintf ppf "@[<v>jar with %d cookie(s):@," (List.length cookies);
  List.iter (fun c -> Format.fprintf ppf "  %a@," Cookie.pp c) cookies;
  Format.fprintf ppf "@]"
;;

(* 6265bis s5.5 step 13: over plaintext, a non-Secure cookie must not
   collide with a stored Secure cookie whose name matches, whose
   domain domain-matches the new cookie's (either direction), and
   whose path the new cookie's path path-matches. Without this an http
   response could shadow an https session cookie, exactly or nearly in
   place. *)
let shadows_secure t (cookie : Cookie.t) =
  List.exists
    (fun c ->
       Cookie.secure c
       && String.equal (Cookie.name c) (Cookie.name cookie)
       && (Cookie.domain_suffix_matches ~sub:(Cookie.domain cookie) (Cookie.domain c)
           || Cookie.domain_suffix_matches ~sub:(Cookie.domain c) (Cookie.domain cookie))
       && Cookie.path_matches ~request_path:(Cookie.path cookie) c)
    t.cookies
;;

let prune_expired_locked t ~now =
  t.cookies <- List.filter (fun cookie -> not (Cookie.is_expired ~now cookie)) t.cookies
;;

(* draft-ietf-httpbis-rfc6265bis s5.7 asks a user agent to support at least
   4096 octets for the name and value alone, and conventional practice draws
   the line there rather than higher, so [max_name_value_bytes] is a hard cap
   on name and value together, separate from [max_cookie_bytes], which bounds
   Path and Domain on top of that name and value budget rather than sharing
   it: a cookie at the full 4096-octet minimum with an ordinary path and
   domain must not be rejected for a shortfall this module introduced. *)
let evict_lru t candidates =
  match
    List.sort
      (fun a b -> Ptime.compare (Cookie.last_access a) (Cookie.last_access b))
      candidates
  with
  | [] -> ()
  | victim :: _ ->
    t.cookies <- List.filter (fun c -> not (Cookie.same_identity victim c)) t.cookies
;;

(* Replacement preserves creation time so request-header ordering remains
   stable. An expired replacement deletes the existing cookie. *)
let store_locked t ~now cookie =
  let existing, rest = List.partition (Cookie.same_identity cookie) t.cookies in
  if Cookie.is_expired ~now cookie
  then t.cookies <- rest
  else (
    let cookie =
      match existing with
      | old :: _ -> Cookie.with_creation_time (Cookie.creation_time old) cookie
      | [] -> cookie
    in
    t.cookies <- cookie :: rest;
    (* Replacing an entry cannot exceed a cap, so only a fresh one
        needs the eviction pass. *)
    if existing = []
    then (
      let domain = Cookie.domain cookie in
      let same_domain =
        List.filter (fun c -> String.equal (Cookie.domain c) domain) t.cookies
      in
      if List.length same_domain > max_per_domain
      then
        evict_lru
          t
          (List.filter (fun c -> not (Cookie.same_identity cookie c)) same_domain);
      if List.length t.cookies > max_total
      then
        evict_lru t (List.filter (fun c -> not (Cookie.same_identity cookie c)) t.cookies)));
  maybe_save_locked t
;;

(* Holding the jar already grants authority to clear it, so [set] does not
   model the RFC's separate non-HTTP API restriction for HttpOnly cookies. *)
let set t ~host ~path ~https line =
  let now = t.now () in
  match Cookie.parse_set_cookie ~now ~host ~path line with
  | Error _ as e -> e
  | Ok cookie ->
    let plaintext = not https in
    if name_value_too_large cookie
    then
      Error
        (Fmt.str "a cookie over %d bytes of name and value" max_name_value_bytes)
    else if too_large cookie
    then
      Error
        (Fmt.str
           "a cookie over %d bytes of name, value, path and domain"
           max_cookie_bytes)
    else if plaintext && Cookie.secure cookie
    then Error "a Secure cookie set over plaintext http"
    else if plaintext && Cookie.has_secure_prefix (Cookie.name cookie)
    then Error "a __Secure-/__Host- cookie set over plaintext http"
    else
      (* The checks against stored state and the store itself must see
         one consistent jar. *)
      Eio.Mutex.use_rw ~protect:true t.mutex
      @@ fun () ->
      prune_expired_locked t ~now;
      if plaintext && shadows_secure t cookie
      then Error "a plaintext cookie that would shadow a Secure one"
      else Ok (store_locked t ~now cookie)
;;

let header_for t ~host ~path ~https =
  let now = t.now () in
  let matched =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      let live, expired =
        List.partition (fun c -> not (Cookie.is_expired ~now c)) t.cookies
      in
      if expired <> []
      then (
        t.cookies <- live;
        maybe_save_locked t);
      let applies c =
        Cookie.domain_matches ~host c
        && Cookie.path_matches ~request_path:path c
        && ((not (Cookie.secure c)) || https)
      in
      t.cookies
      <- List.map (fun c -> if applies c then Cookie.touch ~now c else c) t.cookies;
      List.sort Cookie.compare_order (List.filter applies t.cookies))
  in
  match matched with
  | [] -> None
  | cookies -> Some (Cookie.cookie_header cookies)
;;
