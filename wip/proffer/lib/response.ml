(* Prepare local transport outcomes, preserving the checked allocation boundary
   around application writers and delayed body generators. *)
module M = Httpz.Method
module H = Httpz.Header_name
module I64 = Stdlib_upstream_compatible.Int64_u

type body =
  | Empty
  | String of string @@ global
  | Stream of {
      length : int64 option;
      write : (Body.Sink.t -> unit) @@ global;
      trailers : Headers.t;
    }
  | Handoff of {
      kind : Body.handoff_kind;
      run : (Body.Socket.t -> unit) @@ global;
    }

type outcome = {
  status : Status.t;
  headers : Headers.t;
  last_modified : float option;
  body : body;
  content_length : int64 option;
}

type writer = outcome @ local -> unit

let text_type = Resp.text_type

let[@inline never][@zero_alloc assume strict] call_writer
    (write : writer @ local) (o : outcome @ local) =
  write o

let[@inline never][@zero_alloc assume] call_gen (gen : unit -> string) = gen ()

(* [exclave_], so the option and the [Int64] box it holds are built in the
   caller's region rather than on the heap. Without it a top-level function
   returns a global value and the whole point of the local outcome is lost. *)
let[@zero_alloc] len s = exclave_ Some (Int64.of_int (String.length s))

let[@zero_alloc] upgrade_offered (req : Req.t @ local)
    (selected : string @ local) =
  match Headers.combined (Req.headers req) H.Upgrade with
  | None -> false
  | Some offer ->
      let matched = Httpz.Upgrade.matches_offer ~offer ~selected in
      matched
;;

let[@zero_alloc] valid_upgrade_request (req : Req.t @ local)
    (protocol : string @ local) =
  Req.version req = Httpz.Version.Http_1_1
  && Req.connection_upgrade req
  && upgrade_offered req protocol
;;

let trailer_name = "Trailer"

let[@zero_alloc] rec trailer_declarations (trailers : Headers.t @ local) =
  exclave_
  match trailers with
  | [] -> []
  | { Headers.spelling; _ } :: rest ->
      Headers.other_local trailer_name spelling :: trailer_declarations rest

(* Every field is built with [h_local], so the block is entirely in the
   region. Last-Modified is not a field here. It travels in the outcome as a
   time the backend writes itself. *)
let[@zero_alloc] block (d : Resp.description @ local) = exclave_
  let local_ extra = [] in
  let local_ extra =
    match d.Resp.body with
    | Body.Handoff { kind = Body.Upgrade protocol; _ } ->
        Headers.h_local H.Upgrade protocol :: extra
    | _ -> extra
  in
  let local_ extra =
    match d.Resp.etag with
    | None -> extra
    | Some e -> Headers.h_local H.Etag (Etag.to_string e) :: extra
  in
  let local_ extra =
    match d.Resp.cache with
    | None -> extra
    | Some c -> Headers.h_local H.Cache_control c :: extra
  in
  let local_ extra =
    match d.Resp.content_type with
    | Null -> extra
    | This ct -> Headers.h_local H.Content_type ct :: extra
  in
  let local_ headers = Headers.cat d.Resp.headers extra in
  match d.Resp.body with
  | Body.Stream { trailers = _ :: _ as trailers; _ } ->
      let local_ declarations = trailer_declarations trailers in
      Headers.cat headers declarations
  | _ -> headers

(* A 304 carries only the metadata needed to update a stored response, as
   RFC 9110 section 15.4.5 requires. The name is httpz's constructor, so this
   is a comparison of an immediate rather than a case-folding walk over a
   string. *)
let[@zero_alloc] is_revalidation_name (name : Headers.name @ local) =
  match name with
  | H.Etag | H.Last_modified | H.Cache_control | H.Content_location | H.Expires
  | H.Vary ->
      true
  | _ -> false

let[@zero_alloc] rec revalidation (b : Headers.t @ local) = exclave_
  match b with
  | [] -> []
  | { Headers.name; spelling; value } :: tl ->
      if is_revalidation_name name then
        { Headers.name; spelling; value } :: revalidation tl
      else revalidation tl

let[@zero_alloc] without_trailer (b : Headers.t @ local) = exclave_
  Headers.without b H.Trailer
;;

(* Trailers force chunked framing, so a stream that carries them declares no
   length. HEAD reports what the GET would frame rather than what the body
   declares, since a client acts on that length without seeing the body. *)
let[@zero_alloc] framed_length (body : Body.t @ local) = exclave_
  match body with
  | Body.Stream { length; trailers; _ } ->
      (match trailers with [] -> length | _ :: _ -> None)
  | _ -> Body.declared_length body
;;

let[@zero_alloc] send (write : writer @ local) status
    (headers : Headers.t @ local) ~(last_modified : float option @ local)
    (body : body @ local)
    (content_length : int64 option @ local) =
  let local_ o = { status; headers; last_modified; body; content_length } in
  (* Not a tail call, since [o] lives in this frame. *)
  let () = call_writer write o in
  ()

(* Built once. [h] can raise, and a raise under [run]'s guard counts as an
   ordinary path for the checker. *)
let precondition_failed_headers = [ Headers.h H.Content_type text_type ]

(* A 412 does not carry the representation, so the fields that describe it go.
   What remains is the rest of the response's header block, which is where a
   site wrapper's fields are, so a 412 carries them as its 200 and 304 do. *)
let[@zero_alloc] is_entity_name (name : Headers.name @ local) =
  match name with
  | H.Content_type | H.Content_range | H.Content_encoding | H.Content_language
  | H.Content_location | H.Etag | H.Expires | H.Last_modified | H.Trailer ->
      true
  | _ -> false

let[@zero_alloc] rec precondition_failed_block (b : Headers.t @ local) = exclave_
  match b with
  | [] -> precondition_failed_headers
  | { Headers.name; spelling; value } :: tl ->
      if is_entity_name name then precondition_failed_block tl
      else { Headers.name; spelling; value } :: precondition_failed_block tl

let[@zero_alloc] write_precondition_failed (block : Headers.t @ local)
    (req : Req.t @ local) (write : writer @ local) =
  let message = "Precondition Failed\n" in
  let local_ headers = precondition_failed_block block in
  let local_ body =
    if Method.equal (Req.meth req) M.Head then Empty else String message
  in
  let local_ content_length = len message in
  let () =
    send write Httpz.Res.Precondition_failed headers ~last_modified:None body
      content_length
  in
  ()

let[@inline always][@zero_alloc] decide ~has_now (now : float#) (req : Req.t @ local)
    (d : Resp.description @ local) (write : writer @ local) =
  let local_ b = block d in
  let last_modified = d.Resp.last_modified in
  match Conditional.evaluate ~has_now now req d with
  | Conditional.Failed -> let () = write_precondition_failed b req write in ()
  | Conditional.Revalidated ->
      let local_ headers = revalidation b in
      let () =
        send write Httpz.Res.Not_modified headers ~last_modified Empty None
      in
      ()
  | Conditional.Proceed -> (
      let status = d.Resp.status in
      let code = Status.code status in
      match d.Resp.body with
      | Body.Handoff { kind = Body.Tunnel; run }
        when Method.equal (Req.meth req) M.Connect && code >= 200 && code < 300
        ->
          let local_ body = Handoff { kind = Body.Tunnel; run } in
          let () = send write status b ~last_modified body None in
          ()
      | Body.Handoff { kind = Body.Upgrade protocol; run }
        when code = 101 && valid_upgrade_request req protocol ->
          let local_ body = Handoff { kind = Body.Upgrade protocol; run } in
          let () = send write status b ~last_modified body None in
          ()
      | Body.Handoff _ ->
          invalid_arg
            "Proffer.Resp: a tunnel needs a successful CONNECT response and an \
             upgrade needs status 101 and a matching HTTP/1.1 Upgrade offer"
      | _ when code = 204 ->
        let local_ headers = without_trailer b in
        let () = send write status headers ~last_modified Empty None in
        ()
      | _ when code = 205 ->
        let local_ headers = without_trailer b in
        let () = send write status headers ~last_modified Empty (Some 0L) in
        ()
      | _ when code = 304 ->
        let local_ headers = revalidation b in
        let local_ content_length = Body.declared_length d.Resp.body in
        let () = send write status headers ~last_modified Empty content_length in
        ()
      | _ when Method.equal (Req.meth req) M.Head ->
        let local_ headers = without_trailer b in
        let local_ content_length = framed_length d.Resp.body in
        let () = send write status headers ~last_modified Empty content_length in
        ()
      | Body.Empty ->
            let () = send write status b ~last_modified Empty (Some 0L) in
            ()
        | Body.String s ->
            let local_ content_length = len s in
            let () =
              send write status b ~last_modified (String s) content_length
            in
            ()
        | Body.Stream { length; write = w; trailers } ->
            let local_ body = Stream { length; write = w; trailers } in
            let local_ content_length = framed_length d.Resp.body in
            let () =
              send write status b ~last_modified body content_length
            in
            ()
        | Body.Delayed { length; gen } ->
            (* Run here, so HEAD and contentless statuses never pay for a body
               they drop. The generator runs under [handle]'s guard, which is
               why it is handed back rather than run in the handler. *)
            let s = call_gen gen in
            (* [Int64.equal] takes its arguments at global, and the declared
               length is read out of a local description. *)
            (match length with
            | Some expected
              when not
                     (I64.equal (I64.of_int64 expected)
                        (I64.of_int (String.length s))) ->
                invalid_arg
                  "Proffer.Resp: a delayed body did not produce its declared \
                   length"
            | _ -> ());
            let local_ content_length = len s in
            let () =
              send write status b ~last_modified (String s) content_length
            in
            ())

let[@zero_alloc] write_internal_error (req : Req.t @ local) (write : writer @ local) =
    let message = "Internal Server Error\n" in
    let local_ body =
      if Method.equal (Req.meth req) M.Head then Empty else String message
    in
    let local_ content_length = len message in
    let local_ headers =
      stack_ [ Headers.h_local H.Content_type text_type ]
    in
    let () = send write Httpz.Res.Internal_server_error headers ~last_modified:None body
      content_length in
    ()
