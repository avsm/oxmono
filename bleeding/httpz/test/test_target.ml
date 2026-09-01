(* Differential request-target checks compare in-buffer scans with [Scanner.parse]
   at several absolute offsets.

   The corpus is biased towards the characters that decide the grammar: the
   slashes that separate an authority from a path, '?' and '#', '%' with and
   without two hex digits after it, ':' and '@', brackets, and the sub-delims
   that reg-name and pchar admit but scheme does not. *)

open Base

module I16 = Stdlib_stable.Int16_u
module T = Httpz.Target
module Scanner = Httpz.Uriz.Scanner

let[@inline] i16 x = I16.of_int x
let failures = ref 0

let check name cond detail =
  if not cond
  then begin
    Int.incr failures;
    if !failures <= 20 then Stdio.printf "FAIL [%s] %s\n" name (detail ())
  end
;;

(* Junk that a correct scanner never reads, made of bytes that would change
   the answer if it did: a trailing "%4" completed by a following hex digit,
   a path continued past its end, a query started outside the window. *)
let junk = "0123456789abcdefABCDEF%/?#&=[]:@+.~-*'!$,;\000\255 \r\n"

let mk_span ~off ~len = Httpz.Span.make ~off:(i16 off) ~len:(i16 len)

let place target ~off ~size =
  let buf = Bytes.create size in
  for i = 0 to size - 1 do
    Bytes.unsafe_set buf i (String.unsafe_get junk (i % String.length junk))
  done;
  Bytes.From_string.blit ~src:target ~src_pos:0 ~dst:buf ~dst_pos:off
    ~len:(String.length target);
  buf
;;

let parse_at target ~off : #(bytes * T.t) =
  let buf = place target ~off ~size:(off + String.length target + 64) in
  #(buf, T.parse buf (mk_span ~off ~len:(String.length target)))
;;

let str buf sp = Httpz.Span.to_string buf sp
let off_of sp = Httpz.Span.off sp
let len_of sp = Httpz.Span.len sp

let form_name (f : T.form) =
  match f with
  | T.Origin -> "Origin"
  | T.Absolute -> "Absolute"
  | T.Authority -> "Authority"
  | T.Asterisk -> "Asterisk"
  | T.Invalid -> "Invalid"
;;

(* The reference works on the target on its own, through [Scanner.parse] rather
   than the in-buffer [Scanner.parse_sub] the library uses. The
   authority-form branch is written out here instead, since [host:port] is not
   a URI-reference and [Uriz] has no entry point for it. *)

let is_unreserved c =
  match c with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '.' | '_' | '~' -> true
  | _ -> false
;;

let is_sub_delim c =
  match c with
  | '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ';' | '=' -> true
  | _ -> false
;;

(* Components as plain strings, with [None] for absent, so a reference result
   can be compared against the spans without repeating the offset arithmetic
   that is under test. *)
type ref_result =
  { rform : string
  ; rpath : string
  ; rquery : string option
  ; rscheme : string
  ; rhost : string
  ; rport : int
  }

let invalid_ref =
  { rform = "Invalid"; rpath = ""; rquery = None; rscheme = ""; rhost = ""; rport = -1 }
;;

let ref_authority t =
  let n = String.length t in
  let host_end, host =
    if Char.equal (String.get t 0) '['
    then (
      let e6 = Scanner.ipv6_end t 1 n in
      let e =
        if e6 >= 0 && e6 < n && Char.equal (String.get t e6) ']'
        then e6
        else (
          let ef = Scanner.ipvfuture_end t 1 n in
          if ef >= 0 && ef < n && Char.equal (String.get t ef) ']' then ef else -1)
      in
      if e < 0 then -1, "" else e + 1, String.sub t ~pos:1 ~len:(e - 1))
    else (
      let mutable i = 0 in
      let mutable go = true in
      while go && i < n do
        let c = String.get t i in
        if is_unreserved c || is_sub_delim c
        then i <- i + 1
        else go <- false
      done;
      if i = 0 then -1, "" else i, String.sub t ~pos:0 ~len:i)
  in
  if host_end < 0 || host_end >= n || not (Char.equal (String.get t host_end) ':')
  then invalid_ref
  else (
    let digits = String.sub t ~pos:(host_end + 1) ~len:(n - host_end - 1) in
    if String.is_empty digits
       || not (String.for_all digits ~f:(fun c -> Char.is_digit c))
       || String.length digits > 5
    then invalid_ref
    else (
      let v = Int.of_string digits in
      if v > 65535
      then invalid_ref
      else { invalid_ref with rform = "Authority"; rhost = host; rport = v }))
;;

let ref_parse t =
  let n = String.length t in
  if n = 0
  then invalid_ref
  else if Char.equal (String.get t 0) '/'
  then (
    (* Leading slashes past the first are empty path segments; drop all but
       one so the reference reads them as a path rather than an authority. *)
    let mutable k = 0 in
    while k < n && Char.equal (String.get t k) '/' do
      k <- k + 1
    done;
    let skip = k - 1 in
    let sub = String.sub t ~pos:skip ~len:(n - skip) in
    let sp = Scanner.parse sub in
    if sp.#err <> 0 || sp.#frag_off >= 0
    then invalid_ref
    else
      { invalid_ref with
        rform = "Origin"
      ; rpath = String.sub t ~pos:0 ~len:(sp.#path_off + sp.#path_len + skip)
      ; rquery =
          (if sp.#query_off < 0
           then None
           else Some (String.sub sub ~pos:sp.#query_off ~len:sp.#query_len))
      })
  else if n = 1 && Char.equal (String.get t 0) '*'
  then { invalid_ref with rform = "Asterisk" }
  else (
    let sp = Scanner.parse t in
    if sp.#err = 0 && sp.#scheme_len >= 0 && sp.#host_off >= 0 && sp.#host_len > 0
    then
      if sp.#userinfo_off >= 0 || sp.#frag_off >= 0 || sp.#port_val > 65535
      then invalid_ref
      else
        { rform = "Absolute"
        ; rpath = String.sub t ~pos:sp.#path_off ~len:sp.#path_len
        ; rquery =
            (if sp.#query_off < 0
             then None
             else Some (String.sub t ~pos:sp.#query_off ~len:sp.#query_len))
        ; rscheme = String.sub t ~pos:0 ~len:sp.#scheme_len
        ; rhost = String.sub t ~pos:sp.#host_off ~len:sp.#host_len
        ; rport = sp.#port_val
        }
    else ref_authority t)
;;

let compare_one ~why t ~off =
  let #(buf, got) = parse_at t ~off in
  let want = ref_parse t in
  let show () =
    Printf.sprintf "%s: %S at off %d -> %s path=%S query=%S scheme=%S host=%S port=%d; \
                    want %s path=%S query=%s scheme=%S host=%S port=%d"
      why t off
      (form_name (T.form got))
      (str buf (T.path got)) (str buf (T.query got))
      (str buf (T.scheme got)) (str buf (T.host got)) (T.port got)
      want.rform want.rpath
      (match want.rquery with None -> "<absent>" | Some q -> Printf.sprintf "%S" q)
      want.rscheme want.rhost want.rport
  in
  check "form" (String.equal (form_name (T.form got)) want.rform) show;
  if String.equal want.rform (form_name (T.form got))
     && not (String.equal want.rform "Invalid")
  then begin
    check "path" (String.equal (str buf (T.path got)) want.rpath) show;
    check "scheme" (String.equal (str buf (T.scheme got)) want.rscheme) show;
    check "host" (String.equal (str buf (T.host got)) want.rhost) show;
    check "port" (T.port got = want.rport) show;
    (match want.rquery with
     | None | Some "" -> check "query-empty" (len_of (T.query got) = 0) show
     | Some q ->
       check "query" (String.equal (str buf (T.query got)) q) show;
       (* The query span must point into the target, not merely hold the right
          bytes: a scanner that over-read would still produce the right text. *)
       check "query-off"
         (off_of (T.query got) >= off
          && off_of (T.query got) + len_of (T.query got) <= off + String.length t)
         show);
    (* Spans must lie inside the target, wherever it was placed. *)
    if not (String.is_empty want.rpath)
    then
      check "path-off"
        (off_of (T.path got) >= off
         && off_of (T.path got) + len_of (T.path got) <= off + String.length t)
        show
  end
;;

let offsets = [ 0; 1; 4; 7; 63; 64; 65; 1021; 4096 ]

let differential corpus ~why =
  List.iter corpus ~f:(fun t -> List.iter offsets ~f:(fun off -> compare_one ~why t ~off))
;;

let random_corpus n =
  let pool =
    "///??##%%%%22444aazzAZ09-._~!$&'()*+,;=::@@[]|<>\"\\^{}` \r\n\000\255\127abcdef"
  in
  let st = Random.State.make [| 0x9112 |] in
  List.init n ~f:(fun _ ->
    let len = Random.State.int st 26 in
    String.init len ~f:(fun _ -> String.get pool (Random.State.int st (String.length pool))))
;;

let fixed_corpus =
  [ ""
  ; "/"
  ; "//"
  ; "///"
  ; "///a/b?q"
  ; "/index.html"
  ; "/foo/bar?a=1&b=2"
  ; "/users/42/"
  ; "/a?"
  ; "/?"
  ; "?"
  ; "?a=1"
  ; "*"
  ; "**"
  ; "*?x"
  ; "/*"
  ; "/a%41b"
  ; "/a%zzb"
  ; "/a%4"
  ; "/a%"
  ; "/a%4z"
  ; "/a b"
  ; "/a\x01b"
  ; "/a\x7fb"
  ; "/caf\xc3\xa9"
  ; "/a#frag"
  ; "/a?b#c"
  ; "#"
  ; "/a:b/c"
  ; "/a@b/c"
  ; "/a[b]"
  ; "/;p=1"
  ; "/a?x=%20&y=+"
  ; "http://host/where?q=now"
  ; "http://host:8080/where?q=now"
  ; "http://host"
  ; "http://host/"
  ; "http://host:/"
  ; "http://host:99999999999999999999/"
  ; "http://host:70000/x"
  ; "http://host:65535/x"
  ; "http://user@host/"
  ; "http://user:pw@host/x"
  ; "http://[::1]:8080/x"
  ; "http://[::1]/x"
  ; "http://1.2.3.4:80/x"
  ; "http://host/x#f"
  ; "HTTP://HOST/X"
  ; "https://host/a/b/c?d=e&f=g"
  ; "ftp://host/x"
  ; "a://b"
  ; "http:/foo"
  ; "http:"
  ; "http://"
  ; "host:8080"
  ; "host:0"
  ; "host:65535"
  ; "host:65536"
  ; "host:"
  ; "host"
  ; "1.2.3.4:80"
  ; "[::1]:443"
  ; "[v7.xy]:443"
  ; "[::1]"
  ; "[::zz]:443"
  ; "host:80x"
  ; "host:8 0"
  ; "ho st:80"
  ; "ho%2Fst:80"
  ; "ho%2st:80"
  ; "example.com:443"
  ; "sub.example.com:443"
  ]
;;

let expect_form t want =
  let #(buf, got) = parse_at t ~off:9 in
  ignore (buf : bytes);
  check "expect_form"
    (String.equal (form_name (T.form got)) want)
    (fun () -> Printf.sprintf "%S -> %s, wanted %s" t (form_name (T.form got)) want)
;;

let expect_parts t ~form ~path ~query =
  let #(buf, got) = parse_at t ~off:9 in
  check "expect_parts"
    (String.equal (form_name (T.form got)) form
     && String.equal (str buf (T.path got)) path
     && String.equal (str buf (T.query got)) query)
    (fun () ->
      Printf.sprintf "%S -> %s path=%S query=%S, wanted %s path=%S query=%S" t
        (form_name (T.form got)) (str buf (T.path got)) (str buf (T.query got))
        form path query)
;;

let test_forms () =
  expect_parts "/where?q=now" ~form:"Origin" ~path:"/where" ~query:"q=now";
  expect_parts "/" ~form:"Origin" ~path:"/" ~query:"";
  expect_parts "//x/y" ~form:"Origin" ~path:"//x/y" ~query:"";
  expect_parts "///x?a" ~form:"Origin" ~path:"///x" ~query:"a";
  expect_parts "http://host:8080/where?q=now" ~form:"Absolute" ~path:"/where" ~query:"q=now";
  expect_parts "http://host" ~form:"Absolute" ~path:"" ~query:"";
  expect_parts "host:8080" ~form:"Authority" ~path:"" ~query:"";
  expect_parts "*" ~form:"Asterisk" ~path:"" ~query:"";
  List.iter
    [ ""; "?"; "#"; "**"; "*?x"; "/a#f"; "http://u@h/"; "host"; "host:"; "host:65536"
    ; "1.2.3.4:"; "[::zz]:1"; "/a b"; "/a\x01"; "/caf\xc3\xa9"; "http:"; "http://" ]
    ~f:(fun t -> expect_form t "Invalid");
  let #(buf, got) = parse_at "http://[::1]:8080/x?y" ~off:5 in
  check "absolute-parts"
    (String.equal (str buf (T.scheme got)) "http"
     && String.equal (str buf (T.host got)) "::1"
     && T.port got = 8080
     && String.equal (str buf (T.path got)) "/x"
     && String.equal (str buf (T.query got)) "y")
    (fun () ->
      Printf.sprintf "scheme=%S host=%S port=%d path=%S query=%S"
        (str buf (T.scheme got)) (str buf (T.host got)) (T.port got)
        (str buf (T.path got)) (str buf (T.query got)));
  let #(buf, got) = parse_at "[::1]:443" ~off:5 in
  check "authority-ipv6"
    (String.equal (str buf (T.host got)) "::1" && T.port got = 443)
    (fun () -> Printf.sprintf "host=%S port=%d" (str buf (T.host got)) (T.port got));
  let #(buf, got) = parse_at "example.com:443" ~off:5 in
  check "authority-reg-name"
    (String.equal (str buf (T.host got)) "example.com" && T.port got = 443)
    (fun () -> Printf.sprintf "host=%S port=%d" (str buf (T.host got)) (T.port got));
  let #(_, got) = parse_at "/x" ~off:3 in
  check "origin-no-port" (T.port got = -1) (fun () -> Int.to_string (T.port got));
  let #(_, got) = parse_at "/a#f" ~off:11 in
  check "error-offset" (T.error_offset got = 13) (fun () ->
    Int.to_string (T.error_offset got));
  let #(_, got) = parse_at "/ok" ~off:11 in
  check "error-offset-valid" (T.error_offset got = -1) (fun () ->
    Int.to_string (T.error_offset got))
;;

(* The path span keeps the leading slash: the router splits on it, and a path
   that lost it would match a different route. *)
let test_leading_slash () =
  List.iter [ "/"; "/a"; "/a/b"; "/a?q"; "//a"; "/a/"; "/%41" ] ~f:(fun t ->
    let #(buf, got) = parse_at t ~off:13 in
    let p = str buf (T.path got) in
    check "leading-slash"
      (String.length p > 0 && Char.equal (String.get p 0) '/' && String.is_prefix t ~prefix:p)
      (fun () -> Printf.sprintf "%S -> path %S" t p))
;;

(* A truncated triplet must not be completed by whatever follows the target in
   the buffer, which is what a window-unaware scanner would do. *)
let test_pct () =
  List.iter [ "/a%41"; "/%41"; "/a%41b"; "/a?q=%2F"; "/%ff" ] ~f:(fun t ->
    expect_form t "Origin");
  List.iter [ "/a%"; "/a%4"; "/a%zz"; "/a%4z"; "/a%z4"; "/a?q=%"; "/a?q=%4" ]
    ~f:(fun t -> expect_form t "Invalid");
  (* The junk filling the buffer starts with hex digits, so a scanner reading
     one byte past the window would accept these. *)
  List.iter [ 0; 1; 2; 3; 8; 100 ] ~f:(fun off ->
    let #(_, got) = parse_at "/a%4" ~off in
    check "pct-truncated"
      (match T.form got with
       | T.Invalid -> true
       | _ -> false)
      (fun () -> Printf.sprintf "/a%%4 at off %d -> %s" off (form_name (T.form got))))
;;

let pairs t =
  let #(buf, got) = parse_at t ~off:6 in
  T.query_to_string_pairs buf (T.query got)
;;

let show_pairs ps =
  String.concat ~sep:";" (List.map ps ~f:(fun (k, v) -> Printf.sprintf "%S=%S" k v))
;;

let expect_pairs t want =
  let got = pairs t in
  check "query-pairs"
    (List.equal
       (fun (a, b) (c, d) -> String.equal a c && String.equal b d)
       got want)
    (fun () -> Printf.sprintf "%S -> [%s], wanted [%s]" t (show_pairs got) (show_pairs want))
;;

let test_query_split () =
  expect_pairs "/x" [];
  expect_pairs "/x?" [];
  expect_pairs "/x?a=1" [ "a", "1" ];
  expect_pairs "/x?a" [ "a", "" ];
  expect_pairs "/x?a=" [ "a", "" ];
  expect_pairs "/x?=v" [ "", "v" ];
  expect_pairs "/x?a=1&b=2" [ "a", "1"; "b", "2" ];
  expect_pairs "/x?a=1&&b=2" [ "a", "1"; "", ""; "b", "2" ];
  expect_pairs "/x?a=1&" [ "a", "1"; "", "" ];
  expect_pairs "/x?&" [ "", ""; "", "" ];
  expect_pairs "/x?a=b=c" [ "a", "b=c" ];
  let find t name =
    let #(buf, got) = parse_at t ~off:6 in
    let #(found, v) = T.find_query_param buf (T.query got) name in
    if found then Some (Httpz.Span.to_string buf v) else None
  in
  let eq name t k want =
    check name
      (Option.equal String.equal (find t k) want)
      (fun () ->
        Printf.sprintf "%S[%S] -> %s" t k
          (match find t k with None -> "<none>" | Some s -> Printf.sprintf "%S" s))
  in
  eq "find" "/x?a=1&b=2" "b" (Some "2");
  eq "find" "/x?a=1&b=2" "c" None;
  eq "find-no-value" "/x?a&b=2" "a" (Some "");
  eq "find-empty-key" "/x?a=1&" "" (Some "");
  eq "find-empty-query" "/x?" "a" None;
  eq "find-first-wins" "/x?a=1&a=2" "a" (Some "1")
;;

(* Splitting must agree with {!Httpz.Uriz.query_params}. Corpus items whose
   canonical form differs from their text are skipped so both sides compare the
   same percent-encoding. *)
let test_query_vs_uriz () =
  let pool = "ab&=&&==+%20%41-_.~" in
  let st = Random.State.make [| 0x3986 |] in
  let queries =
    List.init 4000 ~f:(fun _ ->
      let len = Random.State.int st 14 in
      String.init len ~f:(fun _ ->
        String.get pool (Random.State.int st (String.length pool))))
  in
  List.iter ("" :: "a=1&" :: "&" :: "a" :: queries) ~f:(fun q ->
    let text = "/x?" ^ q in
    match Httpz.Uriz.of_string text with
    | Null -> ()
    | This u ->
      if String.equal (Httpz.Uriz.to_string u) text
      then begin
        let want =
          List.map (Httpz.Uriz.query_params u) ~f:(fun (key, value) ->
            key, Option.value value ~default:"")
        in
        let decode s =
          match Httpz.Uriz.percent_decode s with
          | This s -> s
          | Null -> assert false
        in
        let got = List.map (pairs text) ~f:(fun (key, value) -> decode key, decode value) in
        check "query-vs-Uriz"
          (List.equal
             (fun (a, b) (c, d) -> String.equal a c && String.equal b d)
             got want)
          (fun () ->
            Printf.sprintf "%S -> [%s], Uriz [%s]" text (show_pairs got) (show_pairs want))
      end)
;;

let test_alloc () =
  let cases =
    [ "/"; "/index.html"; "/api/v1/users/12345?q=1&r=2"; "//a/b"; "*"
    ; "http://host:8080/a/b?c=d"; "example.com:443"; "/bad%zz"; "/a#f" ]
  in
  (* Arrays and a [for] loop: a [List.iter] would allocate the closure inside
     the measured region and drown the figure being checked. *)
  let bufs =
    Array.of_list
      (List.map cases ~f:(fun t -> place t ~off:37 ~size:(37 + String.length t + 64)))
  in
  let lens = Array.of_list (List.map cases ~f:String.length) in
  let run () =
    let mutable n = 0 in
    for i = 0 to Array.length bufs - 1 do
      let got =
        T.parse (Array.unsafe_get bufs i) (mk_span ~off:37 ~len:(Array.unsafe_get lens i))
      in
      n <- n + Httpz.Span.len (T.path got) + T.port got
    done;
    n
  in
  for _ = 1 to 100 do
    ignore (Sys.opaque_identity (run ()) : int)
  done;
  let before = Stdlib.Gc.minor_words () in
  for _ = 1 to 20_000 do
    ignore (Sys.opaque_identity (run ()) : int)
  done;
  let after = Stdlib.Gc.minor_words () in
  let words = (after -. before) /. 20_000.0 in
  check "alloc" Float.(words = 0.0) (fun () ->
    Printf.sprintf "Target.parse allocated %.4f words per batch of %d" words
      (List.length cases))
;;

(* [text] is written verbatim and may be unterminated to model a short read. *)
let status_lim ~limits text =
  let buf = Bytes.make Httpz.buffer_size '\000' in
  Bytes.From_string.blit ~src:text ~src_pos:0 ~dst:buf ~dst_pos:0
    ~len:(String.length text);
  let #(status, _, _) = Httpz.parse buf ~len:(i16 (String.length text)) ~limits in
  status
;;

let status_of text = status_lim ~limits:Httpz.default_limits text

(* An absolute-form target must agree with Host (RFC 9112 3.2.2), so a test
   using one names the same authority here. *)
let request_line ?(host = "x") ~limits ~meth target =
  status_lim
    ~limits
    (Printf.sprintf "%s %s HTTP/1.1\r\nHost: %s\r\n\r\n" meth target host)
;;

let request ?host ?(meth = "GET") target =
  request_line ?host ~limits:Httpz.default_limits ~meth target
;;

let is_complete s =
  match s with
  | Httpz.Buf_read.Complete -> true
  | _ -> false
;;

let is_invalid_target s =
  match s with
  | Httpz.Buf_read.Invalid_target -> true
  | _ -> false
;;

let test_parser_rejects () =
  List.iter [ "/"; "/index.html"; "//a"; "/a?b=c"; "/a%41" ] ~f:(fun t ->
    check "accept" (is_complete (request t)) (fun () -> Printf.sprintf "%S rejected" t));
  check "accept" (is_complete (request ~host:"h" "http://h/x")) (fun () ->
    "\"http://h/x\" rejected");
  List.iter [ "/a%zz"; "/a%4"; "/a#f"; "/caf\xc3\xa9"; "/a\x01b"; "?" ] ~f:(fun t ->
    check "reject" (is_invalid_target (request t)) (fun () ->
      Printf.sprintf "%S accepted" t))
;;

(* RFC 9112 §3.2: authority-form is CONNECT's alone, asterisk-form is
   OPTIONS's, and CONNECT takes nothing else. *)
let test_form_method () =
  let ok ?host meth target =
    check "form-method-ok" (is_complete (request ?host ~meth target)) (fun () ->
      Printf.sprintf "%s %S rejected" meth target)
  and no meth target =
    check "form-method-no" (is_invalid_target (request ~meth target)) (fun () ->
      Printf.sprintf "%s %S accepted" meth target)
  in
  ok "OPTIONS" "*";
  ok "CONNECT" "h.com:443";
  ok "CONNECT" "1.2.3.4:80";
  ok "CONNECT" "[::1]:443";
  ok "OPTIONS" "/a";
  ok "GET" "/a";
  ok ~host:"h" "GET" "http://h/x";
  List.iter [ "GET"; "HEAD"; "POST"; "PUT"; "DELETE"; "CONNECT" ] ~f:(fun m -> no m "*");
  List.iter [ "GET"; "HEAD"; "POST"; "OPTIONS" ] ~f:(fun m -> no m "h.com:443");
  no "CONNECT" "/path";
  no "CONNECT" "http://h/x";
  no "CONNECT" "*";
  no "CONNECT" "a,b:443";
  no "CONNECT" "exa%6dple.com:443"
;;

(* RFC 9110 4.2.1 gives no http(s) URI an empty host, and RFC 9112 3.2.2 makes
   an absolute-form authority and the Host field one statement rather than two.
*)
let test_host_field () =
  let buf = Bytes.create 256 in
  let place_at s ~off =
    let n = String.length s in
    Bytes.From_string.blit ~src:s ~src_pos:0 ~dst:buf ~dst_pos:off ~len:n;
    mk_span ~off ~len:n
  in
  List.iter
    [ ""; ","; "a,b"; "exa%6dple.com"; "u@h"; "h:65536"; "h:"; "[::1" ]
    ~f:(fun v ->
      check "host-invalid" (not (T.valid_host buf (place_at v ~off:0))) (fun () ->
        Printf.sprintf "%S accepted as a Host value" v));
  List.iter [ "h"; "h.example"; "h:80"; "1.2.3.4"; "[::1]"; "[::1]:8080" ] ~f:(fun v ->
    check "host-valid" (T.valid_host buf (place_at v ~off:0)) (fun () ->
      Printf.sprintf "%S rejected as a Host value" v));
  let matches target host =
    let tsp = place_at target ~off:0 in
    let hsp = place_at host ~off:(String.length target) in
    let t = T.parse buf tsp in
    check "absolute-form" (T.is_absolute t) (fun () ->
      Printf.sprintf "%S is not absolute-form" target);
    T.authority_matches buf t hsp
  in
  List.iter
    [ "http://b/", "b"
    ; "http://b/", "B"
    ; "http://B/", "b"
    ; "http://b:8080/", "b:8080"
    ; "http://[::1]:443/x", "[::1]:443"
    ]
    ~f:(fun (target, host) ->
      check "authority-match" (matches target host) (fun () ->
        Printf.sprintf "%S and Host %S judged different" target host));
  List.iter
    [ "http://b/", "a"
    ; "http://b/", "b:80"
    ; "http://b:8080/", "b"
    ; "http://b:8080/", "b:8081"
    ; "http://[::1]/x", "[::2]"
    ; "http://b/", ""
    ; "http://b/", "b.example"
    ]
    ~f:(fun (target, host) ->
      check "authority-mismatch" (not (matches target host)) (fun () ->
        Printf.sprintf "%S and Host %S judged the same" target host))
;;

(* [max_target_length] is a bound on the target alone, so it must bite before
   the request as a whole runs into [max_header_size]. *)
let test_target_length () =
  let target n = "/" ^ String.make (n - 1) 'a' in
  let is_long s =
    match s with
    | Httpz.Buf_read.Uri_too_long -> true
    | _ -> false
  in
  let max = Httpz.Buf_read.to_int Httpz.default_limits.#max_target_length in
  check "len-under" (is_complete (request (target (max - 1)))) (fun () -> "max-1 rejected");
  check "len-at" (is_complete (request (target max))) (fun () -> "max rejected");
  check "len-over" (is_long (request (target (max + 1)))) (fun () -> "max+1 accepted");
  let tight =
    #{ Httpz.Buf_read.max_content_length = #104857600L
     ; max_header_size = i16 16384
     ; max_header_count = i16 100
     ; max_chunk_size = 16777216
     ; max_target_length = i16 16
     }
  in
  check "len-tight-ok"
    (is_complete (request_line ~limits:tight ~meth:"GET" (target 16)))
    (fun () -> "16 rejected under tight limit");
  check "len-tight-over"
    (is_long (request_line ~limits:tight ~meth:"GET" (target 17)))
    (fun () -> "17 accepted under tight limit");
  (* An over-long target is over-long whether or not the rest has arrived, so
     it is answered rather than waited on. *)
  check "len-unterminated"
    (is_long (status_lim ~limits:tight ("GET " ^ target 40)))
    (fun () -> "unterminated over-long target not reported")
;;

(* A target that has not yet been terminated by SP is a prefix of one, and a
   prefix of a valid target need not be valid. Rejecting it would fail any
   request whose first read stops inside a percent-triplet. *)
let test_partial_target () =
  let is_partial s =
    match s with
    | Httpz.Buf_read.Partial -> true
    | _ -> false
  in
  List.iter
    [ "GET /index.htm"
    ; "GET /a%"
    ; "GET /a%4"
    ; "GET /a%c3"
    ; "GET http:/"
    ; "GET http://h"
    ; "CONNECT [::1"
    ; "CONNECT h.com:"
    ; "OPTIONS *"
    ; "GET /a "
    ; "GET /a HTTP/1.1\r"
    ]
    ~f:(fun text ->
      check "partial" (is_partial (status_of text)) (fun () ->
        Printf.sprintf "%S -> %s, want Partial" text
          (Httpz.Buf_read.status_to_string (status_of text))));
  (* Every proper prefix of a valid request is Partial, never an error. A read
     may end at any byte, so any prefix that parses as something other than
     "not yet" turns a segment boundary into a rejected request. *)
  List.iter
    [ "GET /a%c3%a9/b?q=%41+x HTTP/1.1\r\nHost: x\r\n\r\n"
    ; "OPTIONS * HTTP/1.1\r\nHost: x\r\n\r\n"
    ; "CONNECT [::1]:443 HTTP/1.1\r\nHost: x\r\n\r\n"
    ; "GET http://h.com:80/a?b HTTP/1.1\r\nHost: h.com:80\r\n\r\n"
    ; "PROPPATCH /d HTTP/1.1\r\nHost: x\r\nContent-Length: 0\r\n\r\n"
    ; "POST /u HTTP/1.1\r\nHost: x\r\nTransfer-Encoding: chunked\r\n\r\n"
    ; "GET / HTTP/1.1\r\nHost: x\r\nAccept: */*\r\nConnection: keep-alive\r\n\r\n"
    ]
    ~f:(fun full ->
      for n = 1 to String.length full - 1 do
        let s = status_of (String.sub full ~pos:0 ~len:n) in
        check "prefix-partial" (is_partial s) (fun () ->
          Printf.sprintf "%S (prefix %d of %d) -> %s" (String.sub full ~pos:0 ~len:n) n
            (String.length full) (Httpz.Buf_read.status_to_string s))
      done;
      check "prefix-whole" (is_complete (status_of full)) (fun () ->
        Printf.sprintf "%S -> %s" full
          (Httpz.Buf_read.status_to_string (status_of full))))
;;

let () =
  differential fixed_corpus ~why:"fixed";
  differential (random_corpus 20_000) ~why:"random";
  test_forms ();
  test_leading_slash ();
  test_pct ();
  test_query_split ();
  test_query_vs_uriz ();
  test_alloc ();
  test_parser_rejects ();
  test_form_method ();
  test_host_field ();
  test_target_length ();
  test_partial_target ();
  if !failures = 0
  then Stdio.printf "test_target: all checks passed\n"
  else begin
    Stdio.printf "test_target: %d FAILURES\n" !failures;
    Stdlib.exit 1
  end
;;
