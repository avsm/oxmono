type stored = { mutable cookie : Cookie.t; mutable seq : int }

type t =
  { mutable cookies : stored list
  ; domains : (string, stored list) Hashtbl.t
  ; mutable next_seq : int
  ; mutable earliest_expiry : Ptime.t option
  ; now : unit -> Ptime.t
  ; persist : (string -> unit) option
  ; auto_save : bool
  ; (* One lock for the store and its persistence: a save always writes
     a consistent snapshot. *)
    mutex : Eio.Mutex.t
  }

let http_only_prefix = "#HttpOnly_"

(* The seven Netscape columns carry neither SameSite, Partitioned, nor the two
   timestamps that decide header order and eviction, so each record is preceded
   by a comment line holding them. A reader of the plain format skips it as it
   skips every other line opening with '#'. *)
let meta_prefix = "# Httpz-Meta\t"

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

let seconds_of_time t = int_of_float (Ptime.to_float_s t)

let time_of_seconds s =
  Option.bind (int_of_string_opt s) (fun n -> Ptime.of_float_s (float_of_int n))
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
         | `At t ->
           (* Zero marks a session cookie in this format, so an expiry at or
              before the epoch is written as the first second after it, which
              loads as the expired record it is. *)
           let secs = seconds_of_time t in
           string_of_int (if secs < 1 then 1 else secs)
       in
       Buffer.add_string
         buf
         (Fmt.str
            "%s%s\t%s\t%d\t%d\n"
            meta_prefix
            (match Cookie.same_site c with
             | None -> ""
             | Some site -> Cookie.Same_site.to_string site)
            (if Cookie.partitioned c then "TRUE" else "FALSE")
            (seconds_of_time (Cookie.creation_time c))
            (seconds_of_time (Cookie.last_access c)));
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

type meta =
  { m_same_site : Cookie.Same_site.t option
  ; m_partitioned : bool
  ; m_creation : Ptime.t
  ; m_last_access : Ptime.t
  }

let meta_of_line line =
  if not (String.starts_with ~prefix:meta_prefix line)
  then None
  else (
    match String.split_on_char '\t' line with
    | [ _; same_site; partitioned; creation; last_access ] ->
      let site =
        match same_site with
        | "" -> Some None
        | "Strict" -> Some (Some `Strict)
        | "Lax" -> Some (Some `Lax)
        | "None" -> Some (Some `None)
        | _ -> None
      in
      (match
         site, partitioned, time_of_seconds creation, time_of_seconds last_access
       with
       | Some m_same_site, ("TRUE" | "FALSE"), Some m_creation, Some m_last_access ->
         Some
           { m_same_site
           ; m_partitioned = String.equal partitioned "TRUE"
           ; m_creation
           ; m_last_access
           }
       | _ -> None)
    | _ -> None)
;;

let of_netscape ~now content =
  let parse_line ~meta line =
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
          then Option.map (fun t -> `At t) (time_of_seconds expires)
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
            || ((not (Httpz_uri.Ip.is_literal domain))
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
                 ~partitioned:
                   (match meta with
                    | Some m -> m.m_partitioned
                    | None -> false)
                 ?same_site:(Option.bind meta (fun m -> m.m_same_site))
                 ~expiry
                 ~now
                 ()
             with
             | cookie ->
               let cookie =
                 match meta with
                 | None -> cookie
                 | Some m ->
                   Cookie.touch
                     ~now:m.m_last_access
                     (Cookie.with_creation_time m.m_creation cookie)
               in
               (* A file is no more trusted than a response, and the name-prefix
                  rules are otherwise reachable only through
                  [Cookie.parse_set_cookie]. *)
               if Cookie.is_expired ~now cookie
                  || not (Cookie.prefix_is_satisfied cookie)
               then None
               else Some cookie
             | exception Invalid_argument _ -> None)
          | Some _ -> None)
      | _ -> None)
  in
  let counts = Hashtbl.create 64 in
  let identities = Hashtbl.create max_total in
  let total = ref 0 in
  let cookies = ref [] in
  let pending_meta = ref None in
  let add_cookie cookie =
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
  (* A metadata comment describes the record that follows it, and nothing
     else. *)
  let add_line line =
    let line =
      let len = String.length line in
      if len > 0 && line.[len - 1] = '\r' then String.sub line 0 (len - 1) else line
    in
    match meta_of_line line with
    | Some _ as meta -> pending_meta := meta
    | None ->
        let meta = !pending_meta in
        pending_meta := None;
        Option.iter add_cookie (parse_line ~meta line)
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

let snapshot t = List.map (fun c -> c.cookie) t.cookies

let note_expiry t cookie = match Cookie.expiry cookie with
  | `Session -> ()
  | `At time ->
      match t.earliest_expiry with
      | Some old when Ptime.compare old time <= 0 -> ()
      | _ -> t.earliest_expiry <- Some time

let rebuild_domains t =
  Hashtbl.clear t.domains;
  t.earliest_expiry <- None;
  List.iter (fun entry ->
    let domain = Cookie.domain entry.cookie in
    let bucket = Option.value (Hashtbl.find_opt t.domains domain) ~default:[] in
    Hashtbl.replace t.domains domain (entry :: bucket);
    note_expiry t entry.cookie) t.cookies

let make ~now ?persist ~auto_save cookies =
  let count = List.length cookies in
  let cookies = List.mapi (fun i cookie -> { cookie; seq = count - i }) cookies in
  let t = { cookies; domains = Hashtbl.create 16; next_seq = count + 1;
            earliest_expiry = None; now; persist; auto_save; mutex = Eio.Mutex.create () } in
  rebuild_domains t;
  t
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
      let rec create attempts =
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
          when not !owned && attempts > 1 ->
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

(* [Eio.Mutex.use_rw] disables a mutex for good when its body raises, so a
   single failed write would leave every later operation raising
   [Eio.Mutex.Poisoned] over an in-memory store that is still intact. A save
   failure is therefore carried out of the critical section and re-raised once
   the lock is released. The caller must hold [mutex]. *)
let save_locked t =
  match t.persist with
  | None -> None
  | Some persist ->
    (match persist (to_netscape (snapshot t)) with
     | () -> None
     | exception exn -> Some (exn, Printexc.get_raw_backtrace ()))
;;

let maybe_save_locked t = if t.auto_save then save_locked t else None

let reraise_save = function
  | None -> ()
  | Some (exn, backtrace) -> Printexc.raise_with_backtrace exn backtrace
;;

let flush t =
  reraise_save (Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> save_locked t))
;;

let clear t =
  reraise_save
    (Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
       t.cookies <- [];
       rebuild_domains t;
       maybe_save_locked t))
;;

let cookies t = Eio.Mutex.use_ro t.mutex (fun () -> snapshot t)

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
let shadows_secure t ~now (cookie : Cookie.t) =
  List.exists
    (fun entry ->
       let c = entry.cookie in
       (not (Cookie.is_expired ~now c))
       && Cookie.secure c
       && String.equal (Cookie.name c) (Cookie.name cookie)
       && (Cookie.domain_suffix_matches ~sub:(Cookie.domain cookie) (Cookie.domain c)
           || Cookie.domain_suffix_matches ~sub:(Cookie.domain c) (Cookie.domain cookie))
       && Cookie.path_matches ~request_path:(Cookie.path cookie) c)
    t.cookies
;;

let prune_expired_locked t ~now =
  match t.earliest_expiry with
  | None -> false
  | Some time when Ptime.compare now time < 0 -> false
  | Some _ ->
      let expired = ref false in
      t.cookies <- List.filter (fun entry ->
        let keep = not (Cookie.is_expired ~now entry.cookie) in
        if not keep then expired := true;
        keep) t.cookies;
      rebuild_domains t;
      !expired
;;

let evict_lru t candidates =
  match candidates with
  | [] -> ()
  | first :: rest ->
      let victim = List.fold_left (fun best entry ->
        let c = Ptime.compare (Cookie.last_access entry.cookie) (Cookie.last_access best.cookie) in
        if c < 0 || (c = 0 && entry.seq > best.seq) then entry else best) first rest in
      t.cookies <- List.filter (fun entry -> entry != victim) t.cookies
;;

let store_locked t ~now cookie =
  let existing = List.find_opt (fun entry -> Cookie.same_identity cookie entry.cookie) t.cookies in
  let rest = match existing with
    | None -> t.cookies
    | Some old -> List.filter (fun entry -> entry != old) t.cookies
  in
  if Cookie.is_expired ~now cookie then t.cookies <- rest
  else begin
    (* Rebase the ordering before integer wraparound. *)
    if t.next_seq = max_int then begin
      let n = List.length t.cookies in
      List.iteri (fun i entry -> entry.seq <- n - i) t.cookies;
      t.next_seq <- n + 1
    end;
    let entry = match existing with
      | Some entry ->
          entry.cookie <- Cookie.with_creation_time (Cookie.creation_time entry.cookie) cookie;
          entry.seq <- t.next_seq;
          entry
      | None -> { cookie; seq = t.next_seq }
    in
    t.next_seq <- t.next_seq + 1;
    t.cookies <- entry :: rest;
    if existing = None then begin
      let same_domain = List.filter (fun e -> String.equal (Cookie.domain e.cookie) (Cookie.domain cookie)) t.cookies in
      if List.length same_domain > max_per_domain then
        evict_lru t (List.filter (fun e -> e != entry) same_domain);
      if List.length t.cookies > max_total then
        evict_lru t (List.filter (fun e -> e != entry) t.cookies)
    end
  end;
  rebuild_domains t;
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
    else (
      (* The checks against stored state and the store itself must see
         one consistent jar. A rejection leaves it untouched, so the
         expiry sweep belongs on the accepting path alone. *)
      let pending, result =
        Eio.Mutex.use_rw ~protect:true t.mutex
        @@ fun () ->
        if plaintext && shadows_secure t ~now cookie
        then None, Error "a plaintext cookie that would shadow a Secure one"
        else (
          ignore (prune_expired_locked t ~now : bool);
          store_locked t ~now cookie, Ok ())
      in
      reraise_save pending;
      result)
;;

let header_for t ~host ~path ~https =
  let now = t.now () in
  let pending, matched =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      let pending =
        if prune_expired_locked t ~now then maybe_save_locked t else None
      in
      let matched = ref [] in
      let collect domain =
        match Hashtbl.find_opt t.domains domain with
        | None -> ()
        | Some entries -> List.iter (fun entry ->
            let c = entry.cookie in
            if Cookie.domain_matches ~host c && Cookie.path_matches ~request_path:path c
               && (https || not (Cookie.secure c)) then begin
              entry.cookie <- Cookie.touch ~now c;
              matched := entry :: !matched
            end) entries
      in
      collect host;
      if not (Httpz_uri.Ip.is_literal host) then begin
        let rec suffixes off = match String.index_from_opt host off '.' with
          | None -> ()
          | Some dot ->
              collect (String.sub host (dot + 1) (String.length host - dot - 1));
              suffixes (dot + 1)
        in
        suffixes 0
      end;
      let ordered = List.sort (fun a b ->
        let c = Cookie.compare_order a.cookie b.cookie in
        if c = 0 then Int.compare b.seq a.seq else c) !matched in
      pending, List.map (fun entry -> entry.cookie) ordered)
  in
  reraise_save pending;
  match matched with
  | [] -> None
  | cookies -> Some (Cookie.cookie_header cookies)
;;
