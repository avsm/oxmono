open Proffer
open Proffer.Route

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

(* A codec with a string form, standing in for JSON without a JSON library. *)
let pair =
  Media.of_strings "application/x-pair"
    ~encode:(fun (a, b) -> a ^ "=" ^ b)
    ~decode:(fun s ->
      match String.index_opt s '=' with
      | Some i -> Ok (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))
      | None -> Error "no equals sign")

let pairs = Media.lines "text/x-pairs" pair
let upper = Media.encoder "text/x-upper" (fun (a, b) -> String.uppercase_ascii (a ^ "=" ^ b))
let text_pair =
  Media.map Media.text ~encode:(fun (a, b) -> a ^ " " ^ b)
    ~decode:(fun _ -> Error "x")

let site =
  Site.of_routes
    [ get (s "one") (fun () _req respond ->
          Resp.encode respond pair ("a", "1"));
      post (s "echo")
        (with_body (fun () -> pair) (fun (k, v) () _req respond ->
             Resp.encode respond pair (v, k)));
      get (s "many") (fun () _req respond ->
          Resp.encode_seq respond pairs
            (List.to_seq [ ("a", "1"); ("b", "2") ]));
      post (s "sum") (fun () req respond ->
          match Req.decode_seq pairs req with
          | Ok ps -> Resp.text respond (string_of_int (List.length ps))
          | Error e -> Resp.text respond ~status:Bad_request (Media.error_to_string e));
      get (s "pick") (fun () req respond ->
          Negotiate.encode respond req [ pair; upper; text_pair ]
            ("k", "v")) ]

let req ?headers ?body meth target =
  Proffer_mock.request site () meth target ?headers ?body
let code r = Status.code (Proffer_mock.status r)
let body = Proffer_mock.body
let ct r = Proffer_mock.header r Content_type

let () =
  let r = req Get "/one" in
  check "encode status" (code r = 200);
  check "encode type" (ct r = Some "application/x-pair");
  check "encode body" (body r = "a=1");
  check "encode length" (Proffer_mock.content_length r = Some 3L);
  let r = req Head "/one" in
  check "head" (body r = "" && Proffer_mock.content_length r = Some 3L)

let () =
  let r = req Post "/echo" ~headers:[ ("Content-Type", "application/x-pair; v=1") ] ~body:"k=v" in
  check "with_body ok" (code r = 200 && body r = "v=k");
  let r = req Post "/echo" ~headers:[ ("Content-Type", "text/plain") ] ~body:"k=v" in
  check "with_body 415" (code r = 415);
  let r = req Post "/echo" ~body:"k=v" in
  check "with_body no type" (code r = 415);
  let r = req Post "/echo" ~headers:[ ("Content-Type", "application/x-pair") ] ~body:"kv" in
  check "with_body 400" (code r = 400 && body r = "Bad Request: no equals sign\n")

let () =
  let r = req Get "/many" in
  check "seq type" (ct r = Some "text/x-pairs");
  check "seq body" (body r = "a=1\nb=2\n");
  check "seq length is unknown" (Proffer_mock.content_length r = None);
  let r = req Post "/sum" ~headers:[ ("Content-Type", "text/x-pairs") ] ~body:"a=1\r\n\nb=2\nc=3" in
  check "decode_seq" (body r = "3");
  let r = req Post "/sum" ~headers:[ ("Content-Type", "text/x-pairs") ] ~body:"a=1\nnope\n" in
  check "decode_seq malformed" (code r = 400 && body r = "malformed body: no equals sign")

let () =
  let r = req Get "/pick" in
  check "negotiate default" (ct r = Some "application/x-pair" && body r = "k=v");
  check "negotiate vary" (Proffer_mock.header r Vary = Some "Accept");
  let r = req Get "/pick" ~headers:[ ("Accept", "text/x-upper") ] in
  check "negotiate exact" (body r = "K=V");
  let r = req Get "/pick" ~headers:[ ("Accept", "text/*") ] in
  check "negotiate wildcard" (ct r = Some "text/x-upper; charset=utf-8" || body r = "K=V");
  let r = req Get "/pick" ~headers:[ ("Accept", "text/plain;q=0.5, application/x-pair;q=0.9") ] in
  check "negotiate q" (body r = "k=v");
  let r = req Get "/pick" ~headers:[ ("Accept", "application/x-pair;q=0, text/plain") ] in
  check "negotiate q0" (body r = "k v");
  let r = req Get "/pick" ~headers:[ ("Accept", "image/png") ] in
  check "negotiate 406"
    (code r = 406
     && Proffer_mock.header r Vary = Some "Accept"
     && String.length (body r) > 0);
  let request = Req.v ~meth:Get ~target:"/" ~headers:(Headers.of_list [ ("Accept", "*/*") ]) () in
  check "select star" (Media.media_type (Negotiate.select [ upper; pair ] request) = "text/x-upper");
  check "select empty" (try ignore (Negotiate.select [] request); false with Invalid_argument _ -> true)

(* The CommonMark parser is exponential in bracket nesting, so the codec
   bounds the depth before the parser sees the body. Nothing below hands
   cmarkit a document deeper than it can finish quickly. *)
let () =
  let nested n = String.make n '[' ^ "x" ^ String.make n ']' in
  let md = Markdown.markdown () in
  check "shallow markdown decodes" (Result.is_ok (Media.decode md (nested 8)));
  check "deep markdown is refused"
    (match Media.decode md (nested 30) with
     | Error (Media.Malformed { message; _ }) ->
       message = "bracket nesting deeper than 16"
     | _ -> false);
  check "an escaped bracket does not nest"
    (Result.is_ok
       (Media.decode md (String.concat "" (List.init 40 (fun _ -> "\\[")))));
  check "a lower bound refuses more"
    (match
       Media.decode (Markdown.markdown ~max_bracket_depth:4 ()) (nested 8)
     with
     | Error (Media.Malformed _) -> true
     | _ -> false);
  check "the bound must be positive"
    (match Markdown.markdown ~max_bracket_depth:0 () with
     | exception Invalid_argument _ -> true
     | _ -> false)

let () = Printf.printf "test_media: %d checks passed\n" !checks
