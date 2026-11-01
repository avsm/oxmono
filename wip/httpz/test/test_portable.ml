(* test_portable.ml - the wire parser's [portable] annotations are checked.

   Two checks.  [require_portable] takes its argument at mode [portable], so
   each application below is a typing obligation that only discharges if the
   .mli really carries the annotation.  Then the parser is
   driven from two spawned domains, which is the property the annotations
   exist to support. *)

let[@inline never] require_portable (_ @ portable) = ()

(* Written out in full once, to show the obligation is on the type and not on
   [require_portable] being lenient. *)
let span_to_string : (bytes @ local -> Httpz.Span.t -> string) @ portable =
  Httpz.Span.to_string
;;

(* One or two per module, chosen to cover the shapes that could have gone
   wrong: table-driven scans, word-at-a-time comparison, allocation,
   higher-order arguments, exceptions, and [Format]. *)
let () =
  require_portable span_to_string;
  require_portable Httpz.Scan.skip_token;
  require_portable Httpz.Scan.find_sp_or_cr;
  require_portable Httpz.Span.equal_caseless;
  require_portable Httpz.Span.parse_content_length;
  require_portable Httpz.Buf_read.find_crlf_check_bare_cr;
  require_portable Httpz.Buf_write.int;
  require_portable Httpz.Version.to_string;
  require_portable Httpz.Method.to_string;
  require_portable Httpz.Header_name.of_span;
  require_portable Httpz.Header.find;
  require_portable Httpz.Req.body_span;
  require_portable Httpz.Target.parse;
  (* Higher-order: [f] stays at the legacy mode, so a caller in a portable
     context may pass a closure over its own portable data. *)
  require_portable Httpz.Target.fold_query_params;
  require_portable Httpz.Chunk.parse;
  require_portable Httpz.Res.write_status_line;
  require_portable Httpz.Etag.parse;
  require_portable Httpz.Date.parse;
  require_portable Httpz.Date.format;
  require_portable Httpz.Range.parse_string;
  (* Raising is portable: the payload is immutable. *)
  require_portable Httpz.Err.fail;
  (* [Format] printers too; the formatter is an argument, not a capture. *)
  require_portable Httpz.Res.pp_status;
  require_portable Httpz.parse
;;

(* Parse on a freshly allocated buffer, as a connection would. *)
let round_trip target =
  let text = "GET " ^ target ^ " HTTP/1.1\r\nHost: x\r\n\r\n" in
  let len = String.length text in
  let buf = Bytes.create Httpz.buffer_size in
  Bytes.blit_string text 0 buf 0 len;
  let #(status, r, _headers) =
    Httpz.parse buf ~len:(Httpz.Buf_read.i16 len) ~limits:Httpz.default_limits
  in
  match status with
  | Httpz.Buf_read.Complete ->
    Httpz.Span.to_string buf r.#path
  | _ -> "parse failed"
;;

let () =
  let d1 =
    (Domain.Safe.spawn (fun () -> round_trip "/api/status?ready=true")
     [@alert "-do_not_spawn_domains"])
  in
  let d2 =
    (Domain.Safe.spawn (fun () -> round_trip "/users/42")
     [@alert "-do_not_spawn_domains"])
  in
  let r1 = Domain.join d1 in
  let r2 = Domain.join d2 in
  assert (String.equal r1 "/api/status");
  assert (String.equal r2 "/users/42");
  print_endline
    "test_portable: annotations check out; parsing ran on two domains"
;;
