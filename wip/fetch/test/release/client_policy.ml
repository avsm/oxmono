open Fetch

let check name condition = if not condition then failwith name

let contains text fragment =
  let n = String.length fragment in
  let rec at i =
    i + n <= String.length text
    && (String.sub text i n = fragment || at (i + 1))
  in
  at 0

let secret = "PRIVATE_CREDENTIAL"
let bad_token = secret ^ " not a token!!"

let invalid name fn =
  match fn () with
  | _ -> failwith (name ^ ": accepted invalid credential")
  | exception Invalid_argument message ->
      check (name ^ ": diagnostic exposed secret") (not (contains message secret))

let basic_pairs =
  ["", ""; "", "password"; "user name", "open sesame"; "user", "p:a:s:s"]

let invalid_basic_pairs =
  (secret ^ ":user", "password")
  :: List.concat_map (fun code ->
      let value = secret ^ String.make 1 (Char.chr code) in
      [value, "password"; "user", value])
       (List.init 32 Fun.id @ List.init 129 (fun i -> i + 127))

let test_constant_credentials () =
  List.iter (fun token ->
    invalid "constant bearer" (fun () -> Credential.bearer token))
    [""; bad_token; "="; "ab=c"; secret ^ "\r"; secret ^ "\127"; secret ^ "\255"];
  List.iter (fun token -> ignore (Credential.bearer token))
    ["abc"; "a-Z._~+/09=="];
  List.iter (fun (user, password) ->
    invalid "constant Basic" (fun () -> Credential.basic ~user ~password);
    invalid "Basic encoder" (fun () ->
      Header.encode Header.authorization (`Basic (user, password)))) invalid_basic_pairs;
  List.iter (fun (user, password) ->
    ignore (Credential.basic ~user ~password);
    let encoded = Header.encode Header.authorization (`Basic (user, password)) in
    check "valid Basic round-trip"
      (Header.decode Header.authorization encoded = Some (`Basic (user, password))))
    basic_pairs

let target = "https://127.0.0.1:9/"
let scope = [target; "http://127.0.0.1:9/"]

let test_backend ~sw name backend =
  let exchanges = ref 0 in
  let backend = Middleware.middleware (fun next ~sw req ->
      incr exchanges; next ~sw req) backend in
  let reject credential =
    let client = with_credentials ~scope [credential] backend in
    match get ~sw client target with
    | r -> close r; failwith (name ^ ": invalid credential sent")
    | exception Eio.Io (E (Denied message), _) ->
        check (name ^ ": diagnostic exposed secret") (not (contains message secret))
  in
  reject (Credential.Bearer (fun () -> bad_token));
  List.iter (fun (user, password) ->
    reject (Credential.Basic (fun () -> user, password))) invalid_basic_pairs;
  reject (Credential.Bearer (fun () -> invalid_arg secret));
  reject (Credential.Basic (fun () -> invalid_arg secret));
  let file = Filename.temp_file "httpz-credential-policy-" ".txt" in
  Fun.protect ~finally:(fun () -> Sys.remove file) (fun () ->
    let reads = ref 0 in
    (* File policy belongs to the application callback. Its validation errors
       must receive the same request-scoped treatment as codec failures. *)
    let read_line () =
      incr reads;
      let line = In_channel.with_open_bin file input_line in
      if String.length line > 65536 then invalid_arg "credential line too long";
      line
    in
    let basic_file () =
      let line = read_line () in
      match String.index_opt line ':' with
      | None -> invalid_arg ("missing colon: " ^ line)
      | Some i -> String.sub line 0 i, String.sub line (i + 1) (String.length line - i - 1)
    in
    List.iter (fun (contents, credential) ->
      Out_channel.with_open_bin file (fun oc -> output_string oc (contents ^ "\n"));
      reads := 0;
      ignore (with_credentials ~scope [credential] backend);
      check (name ^ ": lazy file read at construction") (!reads = 0);
      reject credential;
      check (name ^ ": lazy file read count") (!reads = 1))
      [bad_token, Credential.Bearer read_line;
       secret ^ ":bad\rpassword", Credential.Basic basic_file;
       secret, Credential.Basic basic_file;
       String.make 65537 'x', Credential.Bearer read_line;
       String.make 65537 'x', Credential.Basic basic_file]);
  List.iter (fun allow_insecure ->
    let reads = ref 0 in
    let client = with_credentials ~scope ~allow_insecure
      [Credential.Bearer (fun () -> incr reads; secret)] backend in
    List.iter (fun url ->
      match get ~sw client url with
      | r -> close r; failwith (name ^ ": non-HTTP URL sent")
      | exception Eio.Io (E (Invalid_url _), _) -> ())
      ["ftp://127.0.0.1:9/"; "file:///tmp/example";
       "gopher://127.0.0.1:9/"; "javascript:alert(1)";
       "//127.0.0.1:9/"; "/relative"];
    check (name ^ ": rejected URL evaluated credentials") (!reads = 0))
    [false; true];
  let reads = ref 0 in
  let client = with_credentials ~scope
    [Credential.Bearer (fun () -> incr reads; secret)] backend in
  (match get ~sw client "HTTP://127.0.0.1:9/" with
   | r -> close r; failwith (name ^ ": credentials sent over plaintext")
   | exception Eio.Io (E (Denied _), _) -> ());
  check (name ^ ": plaintext denial evaluated credentials") (!reads = 0);
  check (name ^ ": rejected request reached backend") (!exchanges = 0)

let test_fresh_credentials ~sw =
  let calls = ref 0 and seen = ref [] in
  let server req =
    seen := Http.Header.get req.Middleware.headers "authorization" :: !seen;
    Fetch_mock.respond "ok" req
  in
  let client = with_credentials ~scope ~allow_insecure:true
    [Credential.Bearer (fun () -> incr calls; "token" ^ string_of_int !calls)]
    (Fetch_mock.client server) in
  check "construction does not evaluate a thunk" (!calls = 0);
  for _ = 1 to 2 do close (get ~sw client "HTTP://127.0.0.1:9/") done;
  check "credentials are fresh per exchange"
    (!calls = 2 && List.rev !seen = [Some "Bearer token1"; Some "Bearer token2"]);
  List.iter (fun (user, password) ->
    let client = with_credentials ~scope [Credential.Basic (fun () -> user, password)]
      (Fetch_mock.client server) in
    close (get ~sw client target)) basic_pairs;
  let calls_before = !calls in
  close (get ~sw client "https://other.example/");
  check "out-of-scope request does not evaluate a thunk" (!calls = calls_before);
  let callback_failure = Failure "callback failed" in
  List.iter (fun credential ->
    let client = with_credentials ~scope [credential] (Fetch_mock.client server) in
    match get ~sw client target with
    | r -> close r; failwith "callback exception swallowed"
    | exception ex -> check "other callback exceptions propagate" (ex == callback_failure))
    [Credential.Bearer (fun () -> raise callback_failure);
     Credential.Basic (fun () -> raise callback_failure)]

let () =
  test_constant_credentials ();
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  test_backend ~sw "mock" (Fetch_mock.client (fun _ -> failwith "unexpected exchange"));
  test_backend ~sw "httpz" (Fetch_httpz.v (Eio.Stdenv.net env) ());
  test_backend ~sw "curl" (Fetch_curl.v ~sw ());
  test_fresh_credentials ~sw;
  print_endline "client boundary policy regressions passed"
