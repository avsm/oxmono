(* The LinkedIn client against a mock transport. *)

let failures = ref 0

let check name ok =
  if not ok then (
    incr failures;
    prerr_endline ("FAIL: " ^ name))

let header name (req : Fetch.Middleware.request) =
  Http.Header.get req.headers name

let body (req : Fetch.Middleware.request) =
  match req.body with Fetch.String s -> s | _ -> ""

let json text =
  match Jsont_bytesrw.decode_string Jsont.json text with
  | Ok json -> json
  | Error e -> failwith e

let rec member path json =
  match (path, json) with
  | [], json -> Some json
  | name :: rest, Jsont.Object (members, _) ->
      Option.bind (Jsont.Json.find_mem name members) (fun (_, v) ->
          member rest v)
  | _ -> None

let string_at path text =
  match member path (json text) with
  | Some (Jsont.String (s, _)) -> Some s
  | _ -> None

let bool_at path text =
  match member path (json text) with
  | Some (Jsont.Bool (b, _)) -> Some b
  | _ -> None

let client handler =
  Linkedin.create ~session:(Fetch_mock.client handler) ~access_token:"tok"

let author = "urn:li:person:abc"

let test_bodies () =
  let text = Linkedin.post_text_json ~author "hello \"world\"" in
  check "text author" (string_at [ "author" ] text = Some author);
  check "text commentary is escaped"
    (string_at [ "commentary" ] text = Some "hello \"world\"");
  check "default visibility is public"
    (string_at [ "visibility" ] text = Some "PUBLIC");
  check "default distribution is the main feed"
    (string_at [ "distribution"; "feedDistribution" ] text = Some "MAIN_FEED");
  check "resharing allowed by default"
    (bool_at [ "isReshareDisabledByAuthor" ] text = Some false);
  check "text post has no content" (member [ "content" ] (json text) = None);
  let text =
    Linkedin.post_text_json ~author ~visibility:Connections ~distribution:None_
      ~reshare_disabled:true "x"
  in
  check "connections visibility"
    (string_at [ "visibility" ] text = Some "CONNECTIONS");
  check "no distribution"
    (string_at [ "distribution"; "feedDistribution" ] text = Some "NONE");
  check "resharing disabled"
    (bool_at [ "isReshareDisabledByAuthor" ] text = Some true);
  let article =
    Linkedin.post_article_json ~author ~url:"https://example.com/a"
      ~title:"T" "read"
  in
  check "article source"
    (string_at [ "content"; "article"; "source" ] article
    = Some "https://example.com/a");
  check "article title"
    (string_at [ "content"; "article"; "title" ] article = Some "T");
  check "absent description is omitted"
    (member [ "content"; "article"; "description" ] (json article) = None)

let test_post () =
  Eio_mock.Backend.run_full @@ fun _ ->
  let seen = ref None in
  let t =
    client (fun req ->
        seen := Some req;
        Fetch_mock.respond ~status:201
          ~headers:(Http.Header.of_list [ ("x-restli-id", "urn:li:share:1") ])
          "" req)
  in
  let result = Linkedin.post_text t ~author "hi" in
  check "created post id" (result = Ok { Linkedin.id = "urn:li:share:1" });
  (match !seen with
  | None -> check "post was sent" false
  | Some req ->
      check "posts endpoint"
        (Fetch.Middleware.Url.to_string req.url
        = "https://api.linkedin.com/rest/posts");
      check "bearer token" (header "authorization" req = Some "Bearer tok");
      check "API version" (header "linkedin-version" req = Some "202603");
      check "Rest.li protocol"
        (header "x-restli-protocol-version" req = Some "2.0.0");
      check "JSON body" (string_at [ "commentary" ] (body req) = Some "hi"));
  let t =
    client (fun req -> Fetch_mock.respond ~status:422 {|{"message":"no"}|} req)
  in
  check "rejected post is an error"
    (Result.is_error (Linkedin.post_text t ~author "hi"))

let test_userinfo () =
  Eio_mock.Backend.run_full @@ fun _ ->
  let t =
    client (fun req ->
        Fetch_mock.respond
          {|{"sub":"abc","name":"A B","given_name":"A","family_name":"B","email":"a@b"}|}
          req)
  in
  let info = Linkedin.userinfo t in
  check "person URN" (info.person_urn = "urn:li:person:abc");
  check "email" (info.email = Some "a@b");
  check "missing picture" (info.picture = None);
  let t = client (fun req -> Fetch_mock.respond ~status:401 "" req) in
  check "HTTP error raises"
    (match Linkedin.userinfo t with
    | _ -> false
    | exception Failure _ -> true)

(* The token and API headers must not follow a redirect off LinkedIn. *)
let test_credential_scope () =
  Eio_mock.Backend.run_full @@ fun _ ->
  let requests = ref [] in
  let t =
    client (fun req ->
        requests := req :: !requests;
        match Fetch.Middleware.Url.host req.url with
        | "api.linkedin.com" ->
            Fetch_mock.respond ~status:302
              ~headers:
                (Http.Header.of_list
                   [ ("location", "https://elsewhere.example/userinfo") ])
              "" req
        | _ ->
            Fetch_mock.respond
              {|{"sub":"s","name":"n","given_name":"g","family_name":"f"}|}
              req)
  in
  ignore (Linkedin.userinfo t);
  match List.rev !requests with
  | [ first; second ] ->
      check "token sent to LinkedIn"
        (header "authorization" first = Some "Bearer tok");
      check "token not sent elsewhere" (header "authorization" second = None);
      check "API version not sent elsewhere"
        (header "linkedin-version" second = None)
  | rs ->
      check
        (Printf.sprintf "expected two requests, got %d" (List.length rs))
        false

let () =
  test_bodies ();
  test_post ();
  test_userinfo ();
  test_credential_scope ();
  if !failures > 0 then exit 1;
  print_endline "test_linkedin: LinkedIn client tests passed"
