(* webdavz_handler.ml - STORE module type + WebDAV route generation
   RFC 4918 — HTTP Extensions for Web Distributed Authoring and Versioning *)

open Webdavz_xml

module type RO_STORE = sig
  type t
  val is_collection : t -> path:string -> bool
  val exists : t -> path:string -> bool
  val get_properties : t -> path:string -> (fqname * tree list) list
  val read : t -> path:string -> string option
  val children : t -> path:string -> string list
end

module type STORE = sig
  include RO_STORE
  val write : t -> path:string -> content_type:string -> string -> unit
  val delete : t -> path:string -> bool
  val mkdir : t -> path:string -> unit
end

(* Respond with a DAV:error XML body (RFC 4918 Section 16) *)
let xml_error status element (local_ respond) =
  Httpz_server.Route.respond_string respond ~status
    ~headers:[(Httpz.Header_name.Content_type, "application/xml; charset=utf-8")]
    (serialize (error_xml element))

(* Compute parent path: "/foo/bar" -> "/foo", "/" -> "/" *)
let parent_path p =
  match String.rindex_opt p '/' with
  | None | Some 0 -> "/"
  | Some i -> String.sub p 0 i

(* Extract lock token from Lock-Token or If header.
   Lock-Token: <opaquelocktoken:...>
   If: (<opaquelocktoken:...>)
   RFC 4918 Section 9.5 (If) and Section 10.5 (Lock-Token) *)
let extract_lock_token lock_token_hdr if_hdr =
  let strip_angles s =
    let len = String.length s in
    if len >= 2 && Char.equal (String.get s 0) '<'
                 && Char.equal (String.get s (len - 1)) '>'
    then String.sub s 1 (len - 2)
    else s
  in
  match lock_token_hdr with
  | Some s -> Some (strip_angles (String.trim s))
  | None ->
    match if_hdr with
    | Some s ->
      (* Simple extraction: find opaquelocktoken: inside parens/angles *)
      begin match String.index_opt s '<' with
      | Some i ->
        begin match String.index_from_opt s i '>' with
        | Some j -> Some (String.sub s (i + 1) (j - i - 1))
        | None -> None
        end
      | None -> None
      end
    | None -> None

(* Parse LOCK request body for owner (RFC 4918 Section 14.17) *)
let parse_lock_owner body =
  match body with
  | None | Some "" -> None
  | Some xml ->
    match parse xml with
    | Some (Node (_, "lockinfo", _, children)) ->
      let owners = find_children dav_ns "owner" children in
      begin match owners with
      | Node (_, _, _, [Pcdata s]) :: _ -> Some s
      | Node (_, _, _, [Node (_, "href", _, [Pcdata s])]) :: _ -> Some s
      | _ -> None
      end
    | _ -> None

(* Build a LOCK success response body.
   RFC 4918 Section 9.10.1 *)
let lock_response_xml (lock : Webdavz_lock.lock_info) =
  dav_node "prop" [
    dav_node "lockdiscovery" [
      dav_node "activelock" [
        dav_node "locktype" [dav_node "write" []];
        dav_node "lockscope" [dav_node "exclusive" []];
        dav_node "depth" [Pcdata (match lock.depth with
          | `Zero -> "0" | `Infinity -> "infinity")];
        (match lock.owner with
         | Some o -> dav_node "owner" [Pcdata o]
         | None -> dav_node "owner" []);
        dav_node "timeout" [Pcdata (Printf.sprintf "Second-%d" lock.timeout_s)];
        dav_node "locktoken" [dav_node "href" [Pcdata lock.token]];
        dav_node "lockroot" [dav_node "href" [Pcdata lock.path]];
      ]
    ]
  ]

(* Check lock and respond with 423 Locked if blocked.
   Returns true if the write is permitted, false if blocked (423 already sent). *)
let check_lock_or_respond locks ~path ~lock_token (local_ respond) =
  match Webdavz_lock.check_write locks ~path ~lock_token with
  | Ok () -> true
  | Error _lock ->
    xml_error Httpz.Res.Locked "lock-token-submitted" respond;
    false

(* Build PROPFIND responses for a resource and optionally its children.
   RFC 4918 Section 9.1 — PROPFIND *)
let propfind_response (type s) (module S : RO_STORE with type t = s) store ~path ~depth ~pf =
  let open Webdavz_response in
  let open Webdavz_request in
  if not (S.exists store ~path) then []
  else
    let make_response path =
      let all_props = S.get_properties store ~path in
      match pf with
      | Propname ->
        let prop_names = List.map (fun (name, _) ->
          empty_prop_node name) all_props
        in
        { href = path; propstats = [propstat_ok prop_names] }
      | Allprop _include ->
        let found = List.map (fun (name, values) ->
          prop_node name values) all_props
        in
        { href = path; propstats = [propstat_ok found] }
      | Props requested ->
        let found, not_found = List.partition_map (fun name ->
          match Webdavz_prop.find name all_props with
          | Some values -> Either.Left (prop_node name values)
          | None -> Either.Right (empty_prop_node name))
          requested
        in
        let propstats = [] in
        let propstats = if found <> [] then propstat_ok found :: propstats else propstats in
        let propstats = if not_found <> [] then propstat_not_found not_found :: propstats else propstats in
        { href = path; propstats = List.rev propstats }
    in
    let self = make_response path in
    match depth with
    | Zero -> [self]
    | One | Infinity when S.is_collection store ~path ->
      let child_names = S.children store ~path in
      let child_prefix = if String.equal path "/" then "/" else path ^ "/" in
      let child_responses = List.map (fun name ->
        make_response (child_prefix ^ name)) child_names
      in
      self :: child_responses
    | One | Infinity -> [self]

(* Shared PROPFIND handler *)
let propfind_handler (type s) (module S : RO_STORE with type t = s) store
    path_segs (depth_hdr, ()) ctx (local_ respond) =
  let open Httpz_server.Route in
  let path = "/" ^ String.concat "/" path_segs in
  match Webdavz_request.parse_depth depth_hdr with
  | Error `Bad_request ->
    respond_string respond ~status:Httpz.Res.Bad_request "Invalid Depth header"
  | Ok depth ->
    let pf = Webdavz_request.propfind_of_body (body_string ctx) in
    if not (S.exists store ~path) then
      respond_string respond ~status:Httpz.Res.Not_found "Not Found"
    else begin
      let responses = propfind_response (module S) store ~path ~depth ~pf in
      xml_multistatus respond (Webdavz_response.multistatus responses)
    end

(* Header extraction pattern for Lock-Token and If headers *)
let lock_headers =
  let open Httpz_server.Route in
  Httpz.Header_name.Lock_token +> (Httpz.Header_name.If +> h0)

(* RFC 4918 Section 18.2 — class 2 compliance (with locking) *)
let routes (type s) (module S : STORE with type t = s) (store : s) ~locks =
  let open Httpz_server.Route in
  [
    (* PROPFIND — retrieve properties (RFC 4918 Section 9.1) *)
    propfind_h tail (Httpz.Header_name.Depth +> h0)
      (propfind_handler (module S) store);

    (* PROPPATCH — set/remove properties (RFC 4918 Section 9.2)
       Stub: returns 403. *)
    proppatch_h tail lock_headers
      (fun path_segs (lock_token_hdr, (if_hdr, ())) _ctx respond ->
        let path = "/" ^ String.concat "/" path_segs in
        if not (S.exists store ~path) then
          respond_string respond ~status:Httpz.Res.Not_found "Not Found"
        else begin
          let lock_token = extract_lock_token lock_token_hdr if_hdr in
          if check_lock_or_respond locks ~path ~lock_token respond then
            xml_error Httpz.Res.Forbidden "cannot-modify-protected-property" respond
        end);

    (* MKCOL — create collection (RFC 4918 Section 9.3) *)
    mkcol_h tail lock_headers
      (fun path_segs (lock_token_hdr, (if_hdr, ())) ctx respond ->
        let path = "/" ^ String.concat "/" path_segs in
        if S.exists store ~path then
          xml_error Httpz.Res.Method_not_allowed "resource-must-be-null" respond
        else if not (S.exists store ~path:(parent_path path)) then
          respond_string respond ~status:Httpz.Res.Conflict
            "Parent collection does not exist"
        else begin
          let lock_token = extract_lock_token lock_token_hdr if_hdr in
          if check_lock_or_respond locks ~path:(parent_path path) ~lock_token respond then
            match body_string ctx with
            | Some _ ->
              respond_string respond ~status:Httpz.Res.Unsupported_media_type
                "Request body not supported for MKCOL"
            | None ->
              S.mkdir store ~path;
              respond_string respond ~status:Httpz.Res.Created "Created"
        end);

    (* GET — read resource (RFC 4918 Section 9.4) *)
    get tail (fun path_segs _ctx respond ->
      let path = "/" ^ String.concat "/" path_segs in
      match S.read store ~path with
      | Some content -> respond_string respond ~status:Httpz.Res.Success content
      | None -> not_found respond);

    (* PUT — write resource (RFC 4918 Section 9.7) *)
    put_h tail lock_headers
      (fun path_segs (lock_token_hdr, (if_hdr, ())) ctx respond ->
        let path = "/" ^ String.concat "/" path_segs in
        if not (S.exists store ~path:(parent_path path)) then
          respond_string respond ~status:Httpz.Res.Conflict
            "Parent collection does not exist"
        else begin
          let lock_token = extract_lock_token lock_token_hdr if_hdr in
          if check_lock_or_respond locks ~path ~lock_token respond then
            match body_string ctx with
            | None ->
              respond_string respond ~status:Httpz.Res.Bad_request "No body"
            | Some content ->
              let existed = S.exists store ~path in
              let ct = "application/octet-stream" in
              S.write store ~path ~content_type:ct content;
              let etag = Printf.sprintf "\"%08x\"" (Hashtbl.hash content) in
              if existed then
                respond_string respond ~status:Httpz.Res.No_content
                  ~headers:[(Httpz.Header_name.Etag, etag)] ""
              else
                respond_string respond ~status:Httpz.Res.Created
                  ~headers:[
                    (Httpz.Header_name.Etag, etag);
                    (Httpz.Header_name.Location, path);
                  ] "Created"
        end);

    (* DELETE — remove resource (RFC 4918 Section 9.6) *)
    delete_h tail lock_headers
      (fun path_segs (lock_token_hdr, (if_hdr, ())) _ctx respond ->
        let path = "/" ^ String.concat "/" path_segs in
        let lock_token = extract_lock_token lock_token_hdr if_hdr in
        if check_lock_or_respond locks ~path ~lock_token respond then begin
          if S.delete store ~path then
            respond_string respond ~status:Httpz.Res.No_content ""
          else
            not_found respond
        end);

    (* LOCK — acquire exclusive write lock.
       RFC 4918 Section 9.10 *)
    lock_h tail (Httpz.Header_name.Depth +> (Httpz.Header_name.Lock_token +> h0))
      (fun path_segs (depth_hdr, (refresh_token_hdr, ())) ctx respond ->
        let path = "/" ^ String.concat "/" path_segs in
        (* Lock refresh: If Lock-Token header present, refresh existing lock *)
        match refresh_token_hdr with
        | Some tok_raw ->
          let tok = String.trim tok_raw in
          let tok = if String.length tok >= 2
                    && Char.equal (String.get tok 0) '<'
                    && Char.equal (String.get tok (String.length tok - 1)) '>'
                    then String.sub tok 1 (String.length tok - 2)
                    else tok in
          begin match Webdavz_lock.refresh locks ~token:tok ~timeout_s:600 with
          | Some lock ->
            let body = serialize (lock_response_xml lock) in
            respond_string respond ~status:Httpz.Res.Success
              ~headers:[
                (Httpz.Header_name.Content_type, "application/xml; charset=utf-8");
                (Httpz.Header_name.Lock_token, "<" ^ lock.token ^ ">");
              ] body
          | None ->
            xml_error Httpz.Res.Locked "lock-token-matches-request-uri" respond
          end
        | None ->
          let depth = match depth_hdr with
            | Some "0" -> `Zero
            | _ -> `Infinity
          in
          let owner = parse_lock_owner (body_string ctx) in
          begin match Webdavz_lock.lock locks ~path ~depth ~owner ~timeout_s:600 with
          | Ok lock ->
            let body = serialize (lock_response_xml lock) in
            let status = if S.exists store ~path then Httpz.Res.Success
                         else Httpz.Res.Created in
            respond_string respond ~status
              ~headers:[
                (Httpz.Header_name.Content_type, "application/xml; charset=utf-8");
                (Httpz.Header_name.Lock_token, "<" ^ lock.token ^ ">");
              ] body
          | Error (`Locked _) ->
            xml_error Httpz.Res.Locked "no-conflicting-lock" respond
          end);

    (* UNLOCK — release a lock.
       RFC 4918 Section 9.11 *)
    unlock_h tail (Httpz.Header_name.Lock_token +> h0)
      (fun _path_segs (lock_token_hdr, ()) _ctx respond ->
        match lock_token_hdr with
        | None ->
          respond_string respond ~status:Httpz.Res.Bad_request
            "Missing Lock-Token header"
        | Some tok_raw ->
          let tok = String.trim tok_raw in
          let tok = if String.length tok >= 2
                    && Char.equal (String.get tok 0) '<'
                    && Char.equal (String.get tok (String.length tok - 1)) '>'
                    then String.sub tok 1 (String.length tok - 2)
                    else tok in
          if Webdavz_lock.unlock locks ~token:tok then
            respond_string respond ~status:Httpz.Res.No_content ""
          else
            respond_string respond ~status:Httpz.Res.Conflict
              "Lock token not found");

    (* OPTIONS — advertise DAV class 1,2 compliance.
       Class 2 = locking support.
       RFC 4918 Section 18 + Section 10.1 (DAV header) *)
    route Httpz.Method.Options tail h0
      (fun _path_segs () _ctx respond ->
        respond_string respond ~status:Httpz.Res.Success
          ~headers:[
            (Httpz.Header_name.Allow,
             "OPTIONS, GET, HEAD, PUT, DELETE, PROPFIND, PROPPATCH, MKCOL, LOCK, UNLOCK");
            (Httpz.Header_name.Dav, "1, 2");
          ] "");
  ]

(* Read-only WebDAV routes — PROPFIND, GET, OPTIONS only.
   PUT/DELETE/MKCOL/PROPPATCH return 403 Forbidden. *)
let read_only_routes (type s) (module S : RO_STORE with type t = s) (store : s) =
  let open Httpz_server.Route in
  let forbidden _path_segs _ctx respond =
    respond_string respond ~status:Httpz.Res.Forbidden "Read-only"
  in
  [
    propfind_h tail (Httpz.Header_name.Depth +> h0)
      (propfind_handler (module S) store);
    get tail (fun path_segs _ctx respond ->
      let path = "/" ^ String.concat "/" path_segs in
      match S.read store ~path with
      | Some content -> respond_string respond ~status:Httpz.Res.Success content
      | None -> not_found respond);
    put tail forbidden;
    delete tail forbidden;
    mkcol tail forbidden;
    proppatch tail forbidden;
    lock tail (fun _path_segs _ctx respond ->
      respond_string respond ~status:Httpz.Res.Not_implemented "Not implemented");
    unlock tail (fun _path_segs _ctx respond ->
      respond_string respond ~status:Httpz.Res.Not_implemented "Not implemented");
    route Httpz.Method.Options tail h0
      (fun _path_segs () _ctx respond ->
        respond_string respond ~status:Httpz.Res.Success
          ~headers:[
            (Httpz.Header_name.Allow, "OPTIONS, GET, HEAD, PROPFIND");
            (Httpz.Header_name.Dav, "1");
          ] "");
  ]
