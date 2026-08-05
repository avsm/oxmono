(* The defining module for the types the whole library shares, and the
   extension API built on them. Fetch includes it, and the modules Fetch
   depends on (form, retry, credential) name the types here rather than
   in fetch.ml, which would be a cycle. *)

type url = Url.t

(* {2 Errors} *)

type error =
  | Invalid_url of string
  | Invalid_request of string
  | Denied of string
  | Connection_failure of Eio.Net.connection_failure
  | Tls_failure of string
  | Protocol_error of string
  | Too_many_redirects
  | Body_not_replayable

type Eio.Exn.err += E of error

let err e = Eio.Exn.create (E e)

let () =
  Eio.Exn.register_pp (fun f -> function
      | E e ->
        Fmt.string f "Http ";
        begin match e with
          | Invalid_url msg -> Fmt.pf f "Invalid_url %S" msg
          | Invalid_request msg -> Fmt.pf f "Invalid_request %S" msg
          | Denied reason -> Fmt.pf f "Denied %S" reason
          | Connection_failure (Refused b) ->
            Fmt.pf f "Connection_failure Refused %a" Eio.Exn.Backend.pp b
          | Connection_failure Timeout ->
            Fmt.string f "Connection_failure Timeout"
          | Tls_failure msg -> Fmt.pf f "Tls_failure %S" msg
          | Protocol_error msg -> Fmt.pf f "Protocol_error %S" msg
          | Too_many_redirects -> Fmt.string f "Too_many_redirects"
          | Body_not_replayable -> Fmt.string f "Body_not_replayable"
        end;
        true
      | _ -> false
    )

(* {2 Bodies} *)

type body =
  | Empty
  | String of string
  | Stream of { length : int64 option; flow : Eio.Flow.source_ty Eio.Resource.t }

let body_length = function
  | Empty -> Some 0L
  | String s -> Some (Int64.of_int (String.length s))
  | Stream { length; _ } -> length

let body_replayable = function
  | Empty | String _ -> true
  | Stream _ -> false

(* {2 Requests} *)

type request = {
  meth : Http.Method.t;
  url : Url.t;
  headers : Http.Header.t;
  body : body;
  sensitive : string list;
}

(* Paranoid method validation so a middleware cannot reintroduce a
   dodgy method. *)

let is_tchar = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9'
  | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.'
  | '^' | '_' | '`' | '|' | '~' -> true
  | _ -> false

let is_token s = s <> "" && String.for_all is_tchar s

(* RFC 9110 field-value, minus the obs-fold that CR would introduce,
   includes visible characters, space and horizontal tab. *)
let is_field_value s =
  String.for_all
    (function '\t' -> true | '\x00' .. '\x1F' | '\x7F' -> false | _ -> true)
    s

let invalid_request msg = raise (err (Invalid_request msg))

(* These are all set by the library itself or other wrappers *)
let reserved_headers =
  [ "host"; "content-length"; "transfer-encoding" ]

let check_request (req : request) =
  let meth = Http.Method.to_string req.meth in
  if not (is_token meth) then
    invalid_request (Fmt.str "method %S is not a token" meth);
  List.iter
    (fun (name, value) ->
       if not (is_token name) then
         invalid_request (Fmt.str "header name %S is not a token" name);
       if not (is_field_value value) then
         invalid_request
           (Fmt.str "value of header %S contains a control character" name);
       if List.mem (String.lowercase_ascii name) reserved_headers then
         invalid_request
           (Fmt.str "header %S is the backend's to set, not a request's" name))
    (Http.Header.to_list req.headers)

let pp_token f s = if is_token s then Fmt.string f s else Fmt.pf f "%S" s

(* Headers that always carry credentials. The redirect loop strips them
   on a cross-origin hop and pp_request redacts their values; a
   request's [sensitive] field extends the set. *)
let sensitive_headers = [ "authorization"; "cookie"; "proxy-authorization" ]

let is_sensitive (req : request) name =
  let name = String.lowercase_ascii name in
  List.mem name sensitive_headers
  || List.exists (fun s -> String.equal (String.lowercase_ascii s) name)
       req.sensitive

let pp_field req f (name, value) =
  if is_sensitive req name then Fmt.pf f "%a: <redacted>" pp_token name
  else if is_field_value value then Fmt.pf f "%a: %s" pp_token name value
  else Fmt.pf f "%a: %S" pp_token name value

let pp_request f t =
  match Http.Header.to_list t.headers with
  | [] -> Fmt.pf f "%a %a" pp_token (Http.Method.to_string t.meth) Url.pp t.url
  | fields ->
    Fmt.pf f "%a %a (%a)" pp_token (Http.Method.to_string t.meth) Url.pp t.url
      Fmt.(list ~sep:comma (pp_field t)) fields

(* {2 Responses} *)

type version = [ Http.Version.t | `HTTP_2 ]

type response = {
  status : int;
  resp_headers : Http.Header.t;
  resp_version : version;
  resp_body : Eio.Flow.source_ty Eio.Resource.t;
  resp_url : Url.t;
  resp_trailers : unit -> Http.Header.t option;
}

let status r = r.status
let headers r = r.resp_headers
let version r = r.resp_version
let body r = r.resp_body
let url r = Url.to_string r.resp_url
let trailers r = r.resp_trailers ()

let pp_response f r = Fmt.pf f "%d %a" r.status Url.pp r.resp_url

(* {2 Scopes} *)

(* A scope entry is a URL prefix: an origin and a path, parsed where it
   is written so a mistyped address fails there rather than on the first
   request. A query cannot be a prefix of anything, so refusing it is
   better than accepting a scope that would quietly mean less than it
   says. *)
let check_url ~caller ~what s =
  match Url.of_string s with
  | Error msg ->
    invalid_arg (Printf.sprintf "%s: %s %S is not a URL: %s" caller what s msg)
  | Ok u ->
    if Url.has_query u then
      invalid_arg
        (Printf.sprintf
           "%s: %s %S has a query, which names more than an origin and a path"
           caller what s);
    u

(* {2 The HTTP client capability} *)

type 'tag ty = [ `Fetch | `Platform of 'tag ]

type 'a t = 'a Eio.Resource.t constraint 'a = [> [> `Generic ] ty ]

type plain = [ `Generic ] ty Eio.Resource.t

module Pi = struct
  module type CLIENT = sig
    type t
    type tag

    val request : t -> sw:Eio.Switch.t -> request -> response
  end

  type (_, _, _) Eio.Resource.pi +=
    | Client : ('t, (module CLIENT with type t = 't and type tag = 'tag),
                [> 'tag ty ]) Eio.Resource.pi

  let client (type t tag) (module X : CLIENT with type t = t and type tag = tag) =
    Eio.Resource.handler [ H (Client, (module X)) ]

  let response ~status ~headers ~version ~body ?(trailers = fun () -> None) ~url () =
    { status; resp_headers = headers; resp_version = version;
      resp_body = body; resp_url = url; resp_trailers = trailers }
end

let dispatch ~sw (type tag) (Eio.Resource.T (v, ops) : [> tag ty] Eio.Resource.t) req =
  check_request req;
  let module X = (val (Eio.Resource.get ops Pi.Client)) in
  X.request v ~sw req

let request ~sw t (req : request) =
  Eio.Private.Trace.with_span
    (Fmt.str "%a %a" pp_token (Http.Method.to_string req.meth) Url.pp req.url)
  @@ fun () ->
  try dispatch ~sw t req
  with Eio.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Eio.Exn.reraise_with_context ex bt "%a" pp_request req

(* {2 Handlers} *)

type handler = sw:Eio.Switch.t -> request -> response
type middleware = handler -> handler

module Fn_client = struct
  type t = sw:Eio.Switch.t -> request -> response
  type tag = [ `Generic ]
  let request fn ~sw req = fn ~sw req
end

let handler t : handler = fun ~sw req -> dispatch ~sw t req

let of_handler (h : handler) : plain =
  Eio.Resource.T (h, Pi.client (module Fn_client))

let middleware (m : middleware) inner = of_handler (m (handler inner))

let map_request fn inner =
  of_handler (fun ~sw req -> dispatch ~sw inner (fn req))

let map_response fn inner =
  of_handler (fun ~sw req -> fn (dispatch ~sw inner req))

(* {2 Scope entries} *)

module Scope = struct
  type t = Url.t

  let v ~caller s = check_url ~caller ~what:"scope" s

  let list ~caller ?(what = "scope") entries =
    List.map (check_url ~caller ~what) entries

  let matches prefix url = Url.under ~prefix url
end

module Url = Url
