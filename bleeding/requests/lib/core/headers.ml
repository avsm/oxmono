(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let src = Logs.Src.create "requests.headers" ~doc:"HTTP Headers"
module Log = (val Logs.src_log src : Logs.LOG)

(* Use a map with lowercase keys for case-insensitive lookup *)
module StringMap = Map.Make(String)

(** The internal representation stores: (canonical_name, values) *)
type t = (string * string list) StringMap.t

let empty = StringMap.empty

(** {1 Header Injection Prevention}

    Per Recommendation #3: Validate that header names and values do not contain
    newlines (CR/LF) which could enable HTTP request smuggling attacks.

    Note: We use Invalid_argument here to avoid a dependency cycle with Error module.
    The error will be caught and wrapped appropriately by higher-level code. *)

exception Invalid_header of { name: string; reason: string }

(** {1 Basic Auth Credential Validation}

    Per RFC 7617 Section 2:
    - Username must not contain a colon character
    - Neither username nor password may contain control characters (0x00-0x1F, 0x7F) *)

exception Invalid_basic_auth of { reason: string }

let contains_control_chars s =
  String.exists (fun c ->
    let code = Char.code c in
    code <= 0x1F || code = 0x7F
  ) s

let validate_basic_auth_credentials ~username ~password =
  (* RFC 7617 Section 2: "a user-id containing a colon character is invalid" *)
  if String.contains username ':' then
    raise (Invalid_basic_auth {
      reason = "Username contains colon character (RFC 7617 Section 2)"
    });
  (* RFC 7617 Section 2: "The user-id and password MUST NOT contain any control characters" *)
  if contains_control_chars username then
    raise (Invalid_basic_auth {
      reason = "Username contains control characters (RFC 7617 Section 2)"
    });
  if contains_control_chars password then
    raise (Invalid_basic_auth {
      reason = "Password contains control characters (RFC 7617 Section 2)"
    })

let validate_header_name_str name =
  if String.contains name '\r' || String.contains name '\n' then
    raise (Invalid_header {
      name;
      reason = "Header name contains CR/LF characters (potential HTTP smuggling)"
    })

let validate_header_value name value =
  if String.contains value '\r' || String.contains value '\n' then
    raise (Invalid_header {
      name;
      reason = "Header value contains CR/LF characters (potential HTTP smuggling)"
    })

(** {1 Core Operations with Typed Header Names} *)

let add (name : Header_name.t) value t =
  (* Store header names in lowercase for HTTP/2 compatibility.
     HTTP/1.x headers are case-insensitive per RFC 9110. *)
  let canonical = Header_name.to_lowercase_string name in
  let nkey = canonical in
  validate_header_value canonical value;
  let existing =
    match StringMap.find_opt nkey t with
    | Some (_, values) -> values
    | None -> []
  in
  (* Append to maintain order, avoiding reversal on retrieval *)
  StringMap.add nkey (canonical, existing @ [value]) t

let set (name : Header_name.t) value t =
  (* Store header names in lowercase for HTTP/2 compatibility.
     HTTP/1.x headers are case-insensitive per RFC 9110. *)
  let canonical = Header_name.to_lowercase_string name in
  let nkey = canonical in
  validate_header_value canonical value;
  StringMap.add nkey (canonical, [value]) t

let get (name : Header_name.t) t =
  let nkey = Header_name.to_lowercase_string name in
  match StringMap.find_opt nkey t with
  | Some (_, values) -> List.nth_opt values 0
  | None -> None

let get_all (name : Header_name.t) t =
  let nkey = Header_name.to_lowercase_string name in
  match StringMap.find_opt nkey t with
  | Some (_, values) -> values
  | None -> []

let remove (name : Header_name.t) t =
  let nkey = Header_name.to_lowercase_string name in
  StringMap.remove nkey t

let mem (name : Header_name.t) t =
  let nkey = Header_name.to_lowercase_string name in
  StringMap.mem nkey t

(** {1 String-based Operations for Wire Format Compatibility}

    These are used internally when parsing HTTP messages from the wire,
    where header names come as strings. *)

let add_string key value t =
  validate_header_name_str key;
  validate_header_value key value;
  let nkey = String.lowercase_ascii key in
  let existing =
    match StringMap.find_opt nkey t with
    | Some (_, values) -> values
    | None -> []
  in
  StringMap.add nkey (key, existing @ [value]) t

let set_string key value t =
  validate_header_name_str key;
  validate_header_value key value;
  let nkey = String.lowercase_ascii key in
  StringMap.add nkey (key, [value]) t

let get_string key t =
  let nkey = String.lowercase_ascii key in
  match StringMap.find_opt nkey t with
  | Some (_, values) -> List.nth_opt values 0
  | None -> None

let get_all_string key t =
  let nkey = String.lowercase_ascii key in
  match StringMap.find_opt nkey t with
  | Some (_, values) -> values
  | None -> []

let remove_string key t =
  let nkey = String.lowercase_ascii key in
  StringMap.remove nkey t

let mem_string key t =
  let nkey = String.lowercase_ascii key in
  StringMap.mem nkey t

(** {1 Conversion} *)

let of_list lst =
  List.fold_left (fun acc (k, v) -> add_string k v acc) empty lst

let to_list t =
  StringMap.fold (fun _ (orig_key, values) acc ->
    (* Values are already in correct order, build list in reverse then reverse at end *)
    List.fold_left (fun acc v -> (orig_key, v) :: acc) acc values
  ) t []
  |> List.rev

let merge t1 t2 =
  StringMap.union (fun _ _ v2 -> Some v2) t1 t2

(** {1 Common Header Builders} *)

let content_type mime t =
  set `Content_type (Mime.to_string mime) t

let content_length len t =
  set `Content_length (Int64.to_string len) t

let accept mime t =
  set `Accept (Mime.to_string mime) t

let accept_language lang t =
  set `Accept_language lang t

let authorization value t =
  set `Authorization value t

let bearer token t =
  set `Authorization (Printf.sprintf "Bearer %s" token) t

let basic ~username ~password t =
  validate_basic_auth_credentials ~username ~password;
  let credentials = Printf.sprintf "%s:%s" username password in
  let encoded = Base64.encode_exn credentials in
  set `Authorization (Printf.sprintf "Basic %s" encoded) t

let user_agent ua t =
  set `User_agent ua t

let host h t =
  set `Host h t

let cookie name value t =
  add `Cookie (Printf.sprintf "%s=%s" name value) t

let range ~start ?end_ () t =
  let range_value = match end_ with
    | None -> Printf.sprintf "bytes=%Ld-" start
    | Some e -> Printf.sprintf "bytes=%Ld-%Ld" start e
  in
  set `Range range_value t

(** {1 HTTP 100-Continue Support}

    Per Recommendation #7: Expect: 100-continue protocol for large uploads.
    RFC 9110 Section 10.1.1 (Expect) *)

let expect value t =
  set `Expect value t

let expect_100_continue t =
  set `Expect "100-continue" t

(** {1 TE Header Support}

    Per RFC 9110 Section 10.1.4: The TE header indicates what transfer codings
    the client is willing to accept in the response, and whether the client is
    willing to accept trailer fields in a chunked transfer coding. *)

let te value t =
  set `Te value t

let te_trailers t =
  set `Te "trailers" t

(** {1 Cache Control Headers}

    Per Recommendation #17 and #19: Response caching and conditional requests.
    RFC 9111 (HTTP Caching), RFC 9110 Section 8.8.2-8.8.3 (Last-Modified, ETag) *)

let if_none_match etag t =
  set `If_none_match etag t

let if_match etag t =
  set `If_match etag t

let if_modified_since date t =
  set `If_modified_since date t

let if_unmodified_since date t =
  set `If_unmodified_since date t

(** Format a Ptime.t as an HTTP-date (RFC 9110 Section 5.6.7) *)
let http_date_of_ptime time =
  (* HTTP-date format: "Sun, 06 Nov 1994 08:49:37 GMT" *)
  let (year, month, day), ((hour, min, sec), _tz_offset) = Ptime.to_date_time time in
  let weekday = match Ptime.weekday time with
    | `Sun -> "Sun" | `Mon -> "Mon" | `Tue -> "Tue" | `Wed -> "Wed"
    | `Thu -> "Thu" | `Fri -> "Fri" | `Sat -> "Sat"
  in
  let month_name = [| ""; "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
                      "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |].(month) in
  Printf.sprintf "%s, %02d %s %04d %02d:%02d:%02d GMT"
    weekday day month_name year hour min sec

let if_modified_since_ptime time t =
  if_modified_since (http_date_of_ptime time) t

let if_unmodified_since_ptime time t =
  if_unmodified_since (http_date_of_ptime time) t

let cache_control directives t =
  set `Cache_control directives t

(** Build Cache-Control header from common directive components.
    For max_stale: [None] = not present, [Some None] = any staleness, [Some (Some n)] = n seconds *)
let cache_control_directives
    : ?max_age:int ->
      ?max_stale:int option option ->
      ?min_fresh:int ->
      ?no_cache:bool ->
      ?no_store:bool ->
      ?no_transform:bool ->
      ?only_if_cached:bool ->
      unit -> t -> t
  = fun
    ?max_age
    ?max_stale
    ?min_fresh
    ?(no_cache = false)
    ?(no_store = false)
    ?(no_transform = false)
    ?(only_if_cached = false)
    () t ->
  let directives = [] in
  let directives = match max_age with
    | Some age -> Printf.sprintf "max-age=%d" age :: directives
    | None -> directives
  in
  let directives = match max_stale with
    | Some (Some None) -> "max-stale" :: directives
    | Some (Some (Some secs)) -> Printf.sprintf "max-stale=%d" secs :: directives
    | Some None | None -> directives
  in
  let directives = match min_fresh with
    | Some secs -> Printf.sprintf "min-fresh=%d" secs :: directives
    | None -> directives
  in
  let directives = if no_cache then "no-cache" :: directives else directives in
  let directives = if no_store then "no-store" :: directives else directives in
  let directives = if no_transform then "no-transform" :: directives else directives in
  let directives = if only_if_cached then "only-if-cached" :: directives else directives in
  match directives with
  | [] -> t
  | _ -> set `Cache_control (String.concat ", " (List.rev directives)) t

let etag value t =
  set `Etag value t

let last_modified date t =
  set `Last_modified date t

let last_modified_ptime time t =
  last_modified (http_date_of_ptime time) t

(* Additional helper for getting multiple header values *)
let get_multi name t = get_all name t

(** {1 Connection Header Handling}

    Per RFC 9110 Section 7.6.1: The Connection header field lists hop-by-hop
    header fields that MUST be removed before forwarding the message. *)

(** Parse Connection header value into list of header names.
    The Connection header lists additional hop-by-hop headers. *)
let parse_connection_header t =
  match get `Connection t with
  | None -> []
  | Some value ->
      String.split_on_char ',' value
      |> List.map (fun s -> Header_name.of_string (String.trim s))
      |> List.filter (fun n -> not (Header_name.equal n (`Other "")))

(** Get all hop-by-hop headers from a response.
    Returns the union of default hop-by-hop headers and any headers
    listed in the Connection header. *)
let get_hop_by_hop_headers t =
  let connection_headers = parse_connection_header t in
  Header_name.hop_by_hop_headers @ connection_headers
  |> List.sort_uniq Header_name.compare

(** Remove hop-by-hop headers from a header collection.
    This should be called before caching or forwarding a response.
    Per RFC 9110 Section 7.6.1. *)
let remove_hop_by_hop t =
  let hop_by_hop = get_hop_by_hop_headers t in
  List.fold_left (fun headers name -> remove name headers) t hop_by_hop

(** Check if a response indicates the connection should be closed.
    Returns true if Connection: close is present. *)
let connection_close t =
  match get `Connection t with
  | Some value ->
      String.split_on_char ',' value
      |> List.exists (fun s -> String.trim (String.lowercase_ascii s) = "close")
  | None -> false

(** Check if a response indicates the connection should be kept alive.
    Returns true if Connection: keep-alive is present (HTTP/1.0 behavior). *)
let connection_keep_alive t =
  match get `Connection t with
  | Some value ->
      String.split_on_char ',' value
      |> List.exists (fun s -> String.trim (String.lowercase_ascii s) = "keep-alive")
  | None -> false

(* Pretty printer for headers *)
let pp ppf t =
  Format.fprintf ppf "@[<v>Headers:@,";
  let headers = to_list t in
  List.iter (fun (k, v) ->
    Format.fprintf ppf "  %s: %s@," k v
  ) headers;
  Format.fprintf ppf "@]"

let pp_brief ppf t =
  let headers = to_list t in
  let count = List.length headers in
  Format.fprintf ppf "Headers(%d entries)" count

(** {1 HTTP/2 Pseudo-Header Support}

    Per {{:https://datatracker.ietf.org/doc/html/rfc9113#section-8.3}RFC 9113 Section 8.3}. *)

let is_pseudo_header name =
  String.length name > 0 && name.[0] = ':'

let get_pseudo name t =
  let key = ":" ^ name in
  get_string key t

let set_pseudo name value t =
  let key = ":" ^ name in
  set_string key value t

let remove_pseudo name t =
  let key = ":" ^ name in
  remove_string key t

let mem_pseudo name t =
  let key = ":" ^ name in
  mem_string key t

let has_pseudo_headers t =
  StringMap.exists (fun key _ -> String.length key > 0 && key.[0] = ':') t

let pseudo_headers t =
  StringMap.fold (fun key (orig_key, values) acc ->
    if is_pseudo_header key then
      (* Remove the colon prefix for the returned name *)
      let name = String.sub orig_key 1 (String.length orig_key - 1) in
      List.fold_left (fun acc v -> (name, v) :: acc) acc values
    else
      acc
  ) t []
  |> List.rev

let regular_headers t =
  StringMap.fold (fun key (orig_key, values) acc ->
    if not (is_pseudo_header key) then
      List.fold_left (fun acc v -> (orig_key, v) :: acc) acc values
    else
      acc
  ) t []
  |> List.rev

let to_list_ordered t =
  (* RFC 9113 Section 8.3: pseudo-headers MUST appear before regular headers *)
  let pseudos = StringMap.fold (fun key (orig_key, values) acc ->
    if is_pseudo_header key then
      List.fold_left (fun acc v -> (orig_key, v) :: acc) acc values
    else
      acc
  ) t [] |> List.rev in
  let regulars = StringMap.fold (fun key (orig_key, values) acc ->
    if not (is_pseudo_header key) then
      List.fold_left (fun acc v -> (orig_key, v) :: acc) acc values
    else
      acc
  ) t [] |> List.rev in
  pseudos @ regulars

let h2_request ~meth ~scheme ?authority ~path t =
  let t = set_pseudo "method" meth t in
  let t = set_pseudo "scheme" scheme t in
  let t = match authority with
    | Some auth -> set_pseudo "authority" auth t
    | None -> t
  in
  set_pseudo "path" path t

(** {2 HTTP/2 Header Validation} *)

type h2_validation_error =
  | Missing_pseudo of string
  | Invalid_pseudo of string
  | Pseudo_after_regular
  | Invalid_header_name of string
  | Uppercase_header_name of string
  | Connection_header_forbidden
  | Te_header_invalid

let pp_h2_validation_error ppf = function
  | Missing_pseudo name ->
      Format.fprintf ppf "Missing required pseudo-header: :%s" name
  | Invalid_pseudo name ->
      Format.fprintf ppf "Invalid or unknown pseudo-header: :%s" name
  | Pseudo_after_regular ->
      Format.fprintf ppf "Pseudo-header appeared after regular header"
  | Invalid_header_name name ->
      Format.fprintf ppf "Invalid header name: %s" name
  | Uppercase_header_name name ->
      Format.fprintf ppf "Header name contains uppercase (forbidden in HTTP/2): %s" name
  | Connection_header_forbidden ->
      Format.fprintf ppf "Connection-specific header forbidden in HTTP/2"
  | Te_header_invalid ->
      Format.fprintf ppf "TE header must only contain 'trailers' in HTTP/2"

(** HTTP/2 forbidden headers per RFC 9113 Section 8.2.2 *)
let h2_forbidden_headers : Header_name.t list = [
  `Connection;
  `Keep_alive;
  `Other "Proxy-Connection";
  `Transfer_encoding;
  `Upgrade;
]

let remove_h2_forbidden t =
  List.fold_left (fun headers name -> remove name headers) t h2_forbidden_headers

(** Check if a string contains uppercase ASCII letters *)
let contains_uppercase s =
  String.exists (fun c -> c >= 'A' && c <= 'Z') s

(** Valid request pseudo-headers per RFC 9113 Section 8.3.1 *)
let valid_request_pseudos = [":method"; ":scheme"; ":authority"; ":path"; ":protocol"]

(** Valid response pseudo-headers per RFC 9113 Section 8.3.2 *)
let valid_response_pseudos = [":status"]

let validate_h2_request t =
  let headers_list = to_list t in

  (* Check ordering: pseudo-headers must come before regular headers *)
  let rec check_order seen_regular = function
    | [] -> Ok ()
    | (name, _) :: rest ->
        if is_pseudo_header name then
          if seen_regular then Error Pseudo_after_regular
          else check_order false rest
        else
          check_order true rest
  in

  match check_order false headers_list with
  | Error e -> Error e
  | Ok () ->
      (* Check for required pseudo-headers *)
      let has_method = mem_pseudo "method" t in
      let has_scheme = mem_pseudo "scheme" t in
      let has_path = mem_pseudo "path" t in

      (* CONNECT requests don't require :scheme and :path *)
      let is_connect = get_pseudo "method" t = Some "CONNECT" in

      (* :protocol is only valid with CONNECT (RFC 8441 extended CONNECT) *)
      let has_protocol = mem_pseudo "protocol" t in

      if not has_method then
        Error (Missing_pseudo "method")
      else if has_protocol && not is_connect then
        (* :protocol pseudo-header requires :method to be CONNECT *)
        Error (Invalid_pseudo "protocol (requires CONNECT method)")
      else if not is_connect && not has_scheme then
        Error (Missing_pseudo "scheme")
      else if not is_connect && not has_path then
        Error (Missing_pseudo "path")
      else
        (* Check all pseudo-headers are valid *)
        let invalid_pseudo = List.find_opt (fun (name, _) ->
          is_pseudo_header name && not (List.mem name valid_request_pseudos)
        ) headers_list in
        match invalid_pseudo with
        | Some (name, _) ->
            let name_without_colon = String.sub name 1 (String.length name - 1) in
            Error (Invalid_pseudo name_without_colon)
        | None ->
            (* Check for uppercase in regular header names *)
            let uppercase_header = List.find_opt (fun (name, _) ->
              not (is_pseudo_header name) && contains_uppercase name
            ) headers_list in
            match uppercase_header with
            | Some (name, _) -> Error (Uppercase_header_name name)
            | None ->
                (* Check for forbidden connection-specific headers *)
                let has_forbidden = List.exists (fun h ->
                  mem h t
                ) h2_forbidden_headers in
                if has_forbidden then
                  Error Connection_header_forbidden
                else
                  (* Check TE header - only "trailers" is allowed *)
                  match get `Te t with
                  | Some te when String.lowercase_ascii (String.trim te) <> "trailers" ->
                      Error Te_header_invalid
                  | _ -> Ok ()

let validate_h2_response t =
  let headers_list = to_list t in

  (* Check ordering: pseudo-headers must come before regular headers *)
  let rec check_order seen_regular = function
    | [] -> Ok ()
    | (name, _) :: rest ->
        if is_pseudo_header name then
          if seen_regular then Error Pseudo_after_regular
          else check_order false rest
        else
          check_order true rest
  in

  match check_order false headers_list with
  | Error e -> Error e
  | Ok () ->
      (* Check for required :status pseudo-header *)
      if not (mem_pseudo "status" t) then
        Error (Missing_pseudo "status")
      else
        (* Check all pseudo-headers are valid (only :status allowed) *)
        let invalid_pseudo = List.find_opt (fun (name, _) ->
          is_pseudo_header name && not (List.mem name valid_response_pseudos)
        ) headers_list in
        match invalid_pseudo with
        | Some (name, _) ->
            let name_without_colon = String.sub name 1 (String.length name - 1) in
            Error (Invalid_pseudo name_without_colon)
        | None ->
            (* Check for uppercase in regular header names *)
            let uppercase_header = List.find_opt (fun (name, _) ->
              not (is_pseudo_header name) && contains_uppercase name
            ) headers_list in
            match uppercase_header with
            | Some (name, _) -> Error (Uppercase_header_name name)
            | None ->
                (* Check for forbidden connection-specific headers *)
                let has_forbidden = List.exists (fun h ->
                  mem h t
                ) h2_forbidden_headers in
                if has_forbidden then
                  Error Connection_header_forbidden
                else
                  Ok ()

let validate_h2_user_headers t =
  (* Validate user-provided headers for HTTP/2 (before pseudo-headers are added).
     Per RFC 9113 Section 8.2.2 and 8.3, validates:
     - No pseudo-headers (user should not provide them)
     - No connection-specific headers
     - TE header only contains "trailers" if present

     Note: We don't reject uppercase header names here because the library
     internally stores headers with canonical HTTP/1.x names (e.g., "Accept-Encoding").
     The h2_adapter lowercases all header names before sending to HTTP/2. *)
  let headers_list = to_list t in

  (* Check for any pseudo-headers (user should not provide them) *)
  let pseudo = List.find_opt (fun (name, _) -> is_pseudo_header name) headers_list in
  match pseudo with
  | Some (name, _) ->
      let name_without_colon = String.sub name 1 (String.length name - 1) in
      Error (Invalid_pseudo (name_without_colon ^ " (user-provided headers must not contain pseudo-headers)"))
  | None ->
      (* Check for forbidden connection-specific headers *)
      let has_forbidden = List.exists (fun h -> mem h t) h2_forbidden_headers in
      if has_forbidden then
        Error Connection_header_forbidden
      else
        (* Check TE header - only "trailers" is allowed *)
        match get `Te t with
        | Some te when String.lowercase_ascii (String.trim te) <> "trailers" ->
            Error Te_header_invalid
        | _ -> Ok ()
