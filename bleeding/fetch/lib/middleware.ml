type url = Url.t

type error =
  | Invalid_url of string
  | Invalid_request of string
  | Denied of string
  | Connection_failure of Eio.Net.connection_failure
  | Tls_failure of string
  | Protocol_error of string
  | Too_many_redirects
  | Body_not_replayable
  | Decode_failure of { media : string; error : Httpz.Media.error }

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
          | Decode_failure { media; error } ->
            Fmt.pf f "Decode_failure expected %s, %a" media
              Httpz.Media.pp_error error
        end;
        true
      | _ -> false
    )

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

type request = {
  meth : Http.Method.t;
  url : Url.t;
  headers : Http.Header.t;
  body : body;
  sensitive : string list;
  sensitive_query : string list;
}

let is_token = Httpz.Header.Syntax.is_token

(* RFC 9110 field-value, minus the obs-fold that CR would introduce,
   includes visible characters, space and horizontal tab. *)
let is_field_value = Httpz.Header.Syntax.is_field_value

let invalid_request msg = raise (err (Invalid_request msg))

(* Names whose value is the backend's, not a request's: the framing and
   authority fields it derives, and the hop-by-hop fields that govern the
   connection itself. A request that set [Expect], [TE] or [Upgrade] would
   be negotiating on behalf of a connection it does not own, and the
   backend answers none of them. *)
let reserved_headers =
  [ "host"; "content-length"; "transfer-encoding"; "connection"; "expect";
    "te"; "upgrade" ]

let check_request (req : request) =
  let meth = Http.Method.to_string req.meth in
  if not (is_token meth) then
    invalid_request (Fmt.str "method %S is not a token" meth);
  (* A tunnel needs authority-form and a connection the caller keeps; a
     backend that sends origin-form would ask the origin to proxy for
     itself. *)
  if String.equal meth "CONNECT" then
    invalid_request "CONNECT is not supported";
  (* libcurl changes its transfer mode to POST or PUT when a body is attached
     after CURLOPT_NOBODY.  More importantly, RFC 9110 assigns no generally
     defined semantics to content on HEAD.  Refuse the ambiguous operation at
     the shared boundary so every backend observes the same method. *)
  (match req.body with
   | (String _ | Stream _) when String.equal meth "HEAD" || String.equal meth "TRACE" ->
     invalid_request ("a " ^ meth ^ " request cannot carry a body")
   | _ -> ());
  (* [Http.Method.t] exposes [`Other] for extension methods, but its record is
     public and callers can spell a standard method through that constructor.
     Backends dispatch on the constructor as well as the serialized token, so
     accepting such an alias would make their framing decisions disagree. *)
  (match req.meth with
   | `Other raw ->
     (match Http.Method.of_string raw with
      | `Other _ -> ()
      | _ ->
        invalid_request
          (Fmt.str "standard method %S must use its standard constructor" raw))
   | _ -> ());
  (match req.body with
   | Stream { length = Some length; _ } when Int64.compare length 0L < 0 ->
     invalid_request (Fmt.str "request body length %Ld is negative" length)
   | _ -> ());
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

let pp_url req f url =
  match req.sensitive_query with
  | [] -> Url.pp f url
  | names -> Url.pp_redacted ~names f url

let pp_request f t =
  match Http.Header.to_list t.headers with
  | [] ->
    Fmt.pf f "%a %a" pp_token (Http.Method.to_string t.meth) (pp_url t) t.url
  | fields ->
    Fmt.pf f "%a %a (%a)" pp_token (Http.Method.to_string t.meth) (pp_url t)
      t.url Fmt.(list ~sep:comma (pp_field t)) fields

type version = [ Http.Version.t | `HTTP_2 ]

type response = {
  status : int;
  resp_headers : Http.Header.t;
  resp_version : version;
  resp_body : Eio.Flow.source_ty Eio.Resource.t;
  resp_url : Url.t;
  resp_scope : string list;
  resp_trailers : unit -> Http.Header.t option;
  resp_sensitive : string list;
  resp_close : unit -> unit;
}

let status r = r.status
let headers r = r.resp_headers
let version r = r.resp_version
let body r = r.resp_body
let url r = Url.effective_string r.resp_url
let scope r = r.resp_scope
let trailers r = r.resp_trailers ()
let sensitive r = r.resp_sensitive
let close r = r.resp_close ()

let pp_response f r =
  Fmt.pf f "%d %s" r.status (Url.effective_string r.resp_url)

(* A scope entry is a URL prefix: an origin and a path, parsed where it
   is written so a mistyped address fails there rather than on the first
   request. A query or fragment cannot be a prefix of anything, so refusing it is
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
    if Url.has_fragment u then
      invalid_arg
        (Printf.sprintf
           "%s: %s %S has a fragment, which names more than an origin and a path"
           caller what s);
    u

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

  let response ~status ~headers ~version ~body ~close ?(trailers = fun () -> None)
      ?(scope = []) ?(sensitive = []) ~url () =
    let closed = Atomic.make false in
    let resp_close () =
      Eio.Cancel.protect (fun () ->
        if Atomic.compare_and_set closed false true then close ())
    in
    { status; resp_headers = headers; resp_version = version;
      resp_body = body; resp_url = url; resp_scope = scope;
      resp_trailers = trailers; resp_sensitive = List.map String.lowercase_ascii sensitive;
      resp_close }

  let with_metadata ?url ?scope ?(sensitive = []) response =
    let resp_url = Option.value url ~default:response.resp_url in
    let resp_scope = Option.value scope ~default:response.resp_scope in
    let resp_sensitive =
      List.fold_left (fun names name ->
        let name = String.lowercase_ascii name in
        if List.mem name names then names else name :: names)
        response.resp_sensitive sensitive
    in
    { response with resp_url; resp_scope; resp_sensitive }
end

let dispatch ~sw (type tag) (Eio.Resource.T (v, ops) : [> tag ty] Eio.Resource.t) req =
  check_request req;
  let module X = (val (Eio.Resource.get ops Pi.Client)) in
  Pi.with_metadata ~sensitive:req.sensitive (X.request v ~sw req)

let request ~sw t (req : request) =
  Eio.Private.Trace.with_span
    (Fmt.str "%a %a" pp_token (Http.Method.to_string req.meth)
       (pp_url req) req.url)
  @@ fun () ->
  try dispatch ~sw t req
  with Eio.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    (* A wrapper may have marked a copy of [req] sensitive before failing.
       Its response metadata is then unavailable. Never repeat header values
       from the original request in automatic error context. *)
    Eio.Exn.reraise_with_context ex bt "%a %a"
      pp_token (Http.Method.to_string req.meth) (pp_url req) req.url

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

module Scope = struct
  type t = Url.t

  let v ~caller s = check_url ~caller ~what:"scope" s

  let list ~caller ?(what = "scope") entries =
    List.map (check_url ~caller ~what) entries

  let matches prefix url = Url.under ~prefix url
  let to_string = Url.to_string
end

module Url = Url
