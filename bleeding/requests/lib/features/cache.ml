(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP Response Caching per RFC 9111

    This module provides an in-memory cache for HTTP responses following
    RFC 9111 (HTTP Caching). *)

let src = Logs.Src.create "requests.cache" ~doc:"HTTP Response Caching"
module Log = (val Logs.src_log src : Logs.LOG)

(** {1 Cache Entry} *)

type entry = {
  url : string;
  method_ : Method.t;
  status : int;
  headers : Headers.t;
  body : string;
  request_time : Ptime.t;
  response_time : Ptime.t;
  date_value : Ptime.t option;
  age_value : int;
  cache_control : Cache_control.response;
  etag : string option;
  last_modified : string option;
  vary_headers : (string * string) list;
  freshness_lifetime : int option;
}

type lookup_status =
  | Fresh
  | Stale

(** {1 Cache Key} *)

type key = {
  method_key : Method.t;
  uri : string;
  vary_values : (string * string) list;
}

let make_key ~method_ ~uri ?request_headers ?vary () =
  let vary_values = match vary, request_headers with
    | Some vary_names, Some headers ->
      List.filter_map (fun name ->
        (* Vary header names come from the wire, use string lookup *)
        match Headers.get_string name headers with
        | Some value -> Some (String.lowercase_ascii name, value)
        | None -> None
      ) vary_names
    | _ -> []
  in
  { method_key = method_; uri; vary_values }

(** {1 Helper Functions} *)

let parse_vary header =
  String.split_on_char ',' header
  |> List.map String.trim
  |> List.filter (fun s -> s <> "")
  |> List.map String.lowercase_ascii

let vary_matches ~cached_vary ~request_headers =
  List.for_all (fun (name, cached_value) ->
    (* Vary header names are dynamic strings from the wire *)
    match Headers.get_string name request_headers with
    | Some req_value -> req_value = cached_value
    | None -> cached_value = ""
  ) cached_vary

(** Parse Age header value *)
let parse_age headers =
  match Headers.get `Age headers with
  | Some age_str ->
    (try int_of_string age_str with _ -> 0)
  | None -> 0

(** Calculate freshness lifetime for a response *)
let calculate_freshness ~cache_control ~headers ~response_time =
  (* First try explicit freshness from Cache-Control or Expires *)
  match Cache_control.freshness_lifetime
          ~response_cc:cache_control
          ?expires:(Headers.get `Expires headers)
          ?date:(Headers.get `Date headers)
          () with
  | Some lifetime -> Some lifetime
  | None ->
    (* Fall back to heuristic freshness *)
    Cache_control.heuristic_freshness
      ?last_modified:(Headers.get `Last_modified headers)
      ~response_time
      ()

(** {1 In-Memory Cache} *)

module Memory = struct
  type stats = {
    mutable hits : int;
    mutable misses : int;
    mutable stores : int;
  }

  type t = {
    entries : (string, entry list) Hashtbl.t;
    max_entries : int;
    mutable total_entries : int;
    stats : stats;
    mutex : Eio.Mutex.t;
  }

  let create ?(max_entries = 10000) () = {
    entries = Hashtbl.create 1024;
    max_entries;
    total_entries = 0;
    stats = { hits = 0; misses = 0; stores = 0 };
    mutex = Eio.Mutex.create ();
  }

  (** Check if a method is cacheable *)
  let is_cacheable_method = function
    | `GET | `HEAD -> true
    | _ -> false

  (** Evict oldest entries if over limit *)
  let evict_if_needed t =
    if t.total_entries > t.max_entries then begin
      (* Simple eviction: remove ~10% of entries *)
      let to_remove = t.max_entries / 10 in
      let removed = ref 0 in
      Hashtbl.filter_map_inplace (fun _uri entries ->
        if !removed >= to_remove then Some entries
        else begin
          let len = List.length entries in
          removed := !removed + len;
          t.total_entries <- t.total_entries - len;
          None
        end
      ) t.entries
    end

  let store t ~url ~method_ ~status ~headers ~body ~request_time ~response_time
      ?request_headers () =
    (* Check if cacheable *)
    if not (is_cacheable_method method_) then begin
      Log.debug (fun m -> m "Not caching: method %s is not cacheable"
        (Method.to_string method_));
      false
    end else begin
      let cache_control =
        match Headers.get `Cache_control headers with
        | Some cc -> Cache_control.parse_response cc
        | None -> Cache_control.empty_response
      in
      if not (Cache_control.is_cacheable ~response_cc:cache_control ~status) then begin
        Log.debug (fun m -> m "Not caching: response is not cacheable");
        false
      end else begin
        let date_value =
          match Headers.get `Date headers with
          | Some date_str -> Http_date.parse date_str
          | None -> None
        in
        let age_value = parse_age headers in
        let etag = Headers.get `Etag headers in
        let last_modified = Headers.get `Last_modified headers in
        let vary_headers =
          match Headers.get `Vary headers, request_headers with
          | Some vary, Some req_hdrs ->
            let vary_names = parse_vary vary in
            List.filter_map (fun name ->
              (* Vary header names are dynamic strings from the wire *)
              match Headers.get_string name req_hdrs with
              | Some value -> Some (name, value)
              | None -> None
            ) vary_names
          | _ -> []
        in
        let freshness_lifetime =
          calculate_freshness ~cache_control ~headers ~response_time
        in
        let entry = {
          url; method_; status; headers; body;
          request_time; response_time; date_value; age_value;
          cache_control; etag; last_modified; vary_headers;
          freshness_lifetime;
        } in
        Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
          (* Remove any existing entries that match *)
          let existing = Hashtbl.find_opt t.entries url |> Option.value ~default:[] in
          let filtered = List.filter (fun e ->
            e.method_ <> method_ ||
            not (vary_matches ~cached_vary:e.vary_headers
                   ~request_headers:(Option.value ~default:Headers.empty request_headers))
          ) existing in
          Hashtbl.replace t.entries url (entry :: filtered);
          t.total_entries <- t.total_entries + 1;
          t.stats.stores <- t.stats.stores + 1;
          evict_if_needed t
        );
        Log.debug (fun m -> m "Cached response for %s (freshness: %s)"
          url
          (match freshness_lifetime with
           | Some s -> Printf.sprintf "%ds" s
           | None -> "unknown"));
        true
      end
    end

  let lookup t ~method_ ~uri ?request_headers ~now () =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      match Hashtbl.find_opt t.entries uri with
      | None ->
        t.stats.misses <- t.stats.misses + 1;
        None
      | Some entries ->
        (* Find matching entry *)
        let request_headers = Option.value ~default:Headers.empty request_headers in
        let matching = List.find_opt (fun e ->
          e.method_ = method_ &&
          vary_matches ~cached_vary:e.vary_headers ~request_headers
        ) entries in
        match matching with
        | None ->
          t.stats.misses <- t.stats.misses + 1;
          None
        | Some entry ->
          t.stats.hits <- t.stats.hits + 1;
          (* Calculate current age and freshness *)
          let inputs : Cache_control.age_inputs = {
            date_value = entry.date_value;
            age_value = entry.age_value;
            request_time = entry.request_time;
            response_time = entry.response_time;
          } in
          let current_age = Cache_control.calculate_age ~inputs ~now in
          let status = match entry.freshness_lifetime with
            | Some lifetime when Cache_control.is_fresh ~current_age ~freshness_lifetime:lifetime ->
              Fresh
            | _ -> Stale
          in
          Log.debug (fun m -> m "Cache %s for %s (age: %ds)"
            (match status with Fresh -> "hit" | Stale -> "stale")
            uri current_age);
          Some (entry, status)
    )

  let invalidate t ~uri =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      match Hashtbl.find_opt t.entries uri with
      | Some entries ->
        t.total_entries <- t.total_entries - List.length entries;
        Hashtbl.remove t.entries uri;
        Log.debug (fun m -> m "Invalidated cache for %s" uri)
      | None -> ()
    )

  let clear t =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      Hashtbl.clear t.entries;
      t.total_entries <- 0;
      Log.debug (fun m -> m "Cleared cache")
    )

  let size t =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> t.total_entries)

  let stats t =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      (t.stats.hits, t.stats.misses, t.stats.stores)
    )
end

(** {1 Cache Validation} *)

let needs_validation entry =
  Cache_control.must_revalidate ~response_cc:entry.cache_control

let validation_headers entry =
  let headers = Headers.empty in
  let headers = match entry.etag with
    | Some etag -> Headers.if_none_match etag headers
    | None -> headers
  in
  let headers = match entry.last_modified with
    | Some lm -> Headers.if_modified_since lm headers
    | None -> headers
  in
  headers

let is_not_modified ~status = status = 304
