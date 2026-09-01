open Proffer
module M = Httpz.Method

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let boundary = "abc123"

(* Parts are joined with the delimiter the boundary names, so a test body is
   written as its part texts and never as one long escaped literal. *)
let body_of parts =
  let delim = "--" ^ boundary ^ "\r\n" in
  String.concat "" (List.map (fun p -> delim ^ p ^ "\r\n") parts)
  ^ "--" ^ boundary ^ "--\r\n"

let form_data = "multipart/form-data; boundary=" ^ boundary

(* A request is local, so a helper that makes one returns it into the
   caller's region. *)
let req ?(content_type = form_data) body = exclave_
  Req.v ~meth:M.Post ~target:"/upload"
    ~headers:(Headers.of_list [ ("Content-Type", content_type) ])
    ~body ()

let name_part =
  "Content-Disposition: form-data; name=\"name\"\r\n\r\nAda L."

let org_part =
  "Content-Disposition: form-data; name=\"org\"\r\n\r\n\xc3\x89cole"

(* Content with CRLF and a run of dashes, which must not be read as framing. *)
let file_content = "\x89PNG\r\n--abc123X\r\nend"

let file_part =
  "Content-Disposition: form-data; name=\"avatar\"; filename=\"a b.png\"\r\n\
   Content-Type: image/png\r\n\r\n" ^ file_content

(* A request is local, so the three-part body is kept as a string and a
   request is built inside each region that needs one. *)
let three_body = body_of [ name_part; org_part; file_part ]

let parts_of r =
  match Multipart.of_req r with
  | Ok ps -> ps
  | Error e -> failwith (Media.error_to_string e)

let () =
  let three = req three_body in
  let ps = parts_of three in
  check "every part is found" (List.length ps = 3);
  check "names in order"
    (List.map (fun (p : Multipart.part) -> p.name) ps
    = [ "name"; "org"; "avatar" ]);
  check "a field has no filename"
    (List.for_all
       (fun (p : Multipart.part) -> p.filename = None)
       (List.filteri (fun i _ -> i < 2) ps));
  check "field" (Multipart.field three ps "name" = Some "Ada L.");
  check "field decodes bytes as they came"
    (Multipart.field three ps "org" = Some "\xc3\x89cole");
  check "a file is not a field" (Multipart.field three ps "avatar" = None);
  check "an absent field" (Multipart.field three ps "nope" = None);
  check "fields"
    (Multipart.fields three ps
    = [ ("name", "Ada L."); ("org", "\xc3\x89cole") ]);
  match Multipart.file ps "avatar" with
  | None -> check "file" false
  | Some p ->
      check "filename" (p.filename = Some "a b.png");
      check "part content type" (p.content_type = Some "image/png");
      check "content is not confused by CRLF or dashes"
        (Multipart.content three p = file_content);
      check "len is the content length"
        (p.len = String.length file_content);
      check "off locates the content in the body"
        (String.sub (Req.globalize (Req.body three)) p.off p.len = file_content);
      check "headers keep what was read"
        (List.mem_assoc "content-disposition" p.headers
        && List.assoc_opt "content-type" p.headers = Some "image/png");
      check "a field is not a file" (Multipart.file ps "name" = None)

let () =
  let r = req ~content_type:"application/x-www-form-urlencoded" "a=1" in
  check "another media type is unsupported"
    (Multipart.of_req r
    = Error (Media.Unsupported (Some "application/x-www-form-urlencoded")));
  let r = req ~content_type:"multipart/form-data" (body_of [ name_part ]) in
  check "no boundary is unsupported"
    (Multipart.of_req r
    = Error (Media.Unsupported (Some "multipart/form-data")));
  let r =
    Req.v ~meth:M.Post ~target:"/upload" ~body:(body_of [ name_part ]) ()
  in
  check "no content type is unsupported"
    (Multipart.of_req r = Error (Media.Unsupported None))

let () =
  let truncated =
    "--" ^ boundary ^ "\r\n" ^ name_part ^ "\r\n"
  in
  check "a missing closing delimiter is malformed"
    (match Multipart.of_req (req truncated) with
    | Error (Media.Malformed { message; _ }) -> message <> ""
    | _ -> false);
  let bare_lf =
    "--" ^ boundary ^ "\n" ^ name_part ^ "\r\n--" ^ boundary ^ "--\r\n"
  in
  check "a bare LF is malformed"
    (match Multipart.of_req (req bare_lf) with
    | Error (Media.Malformed _) -> true
    | _ -> false);
  let unnamed =
    body_of [ "Content-Disposition: form-data\r\n\r\nx" ]
  in
  check "a part with no name is malformed"
    (match Multipart.of_req (req unnamed) with
    | Error (Media.Malformed _) -> true
    | _ -> false)

let () =
  let three = req three_body in
  check "more parts than the bound is malformed, not a byte overrun"
    (match Multipart.of_req ~max_parts:2 three with
    | Error (Media.Malformed { message; _ }) -> message <> ""
    | _ -> false);
  check "the bound itself is accepted"
    (match Multipart.of_req ~max_parts:3 three with
    | Ok ps -> List.length ps = 3
    | Error _ -> false);
  check "a negative part bound raises"
    (match Multipart.of_req ~max_parts:(-1) three with
    | _ -> false
    | exception Invalid_argument _ -> true)

(* End to end: the same body through dispatch, as a handler would see it. *)
let site =
  Site.of_routes
    [
      Route.post (Route.s "upload") (fun () request respond ->
          match Multipart.of_req request with
          | Error e ->
              Resp.text respond ~status:Unsupported_media_type
                (Media.error_to_string e)
          | Ok ps -> (
              match Multipart.file ps "avatar" with
              | None -> Resp.text respond ~status:Bad_request "no file\n"
              | Some p ->
                  Resp.text respond
                    (Printf.sprintf "%s %s %d"
                       (Option.value (Multipart.field request ps "name")
                          ~default:"?")
                       (Option.value p.filename ~default:"?")
                       p.len)));
    ]

let () =
  let r =
    Proffer_mock.request site () M.Post "/upload"
      ~headers:[ ("Content-Type", form_data) ]
      ~body:three_body
  in
  check "upload route status" (Status.code (Proffer_mock.status r) = 200);
  check "upload route body"
    (Proffer_mock.body r
    = Printf.sprintf "Ada L. a b.png %d" (String.length file_content));
  let r =
    Proffer_mock.request site () M.Post "/upload"
      ~headers:[ ("Content-Type", "text/plain") ]
      ~body:"x"
  in
  check "upload route refuses another media type"
    (Status.code (Proffer_mock.status r) = 415)

let () = Printf.printf "test_multipart: %d checks ok\n" !checks
