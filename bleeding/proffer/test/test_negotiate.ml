open Proffer
open Proffer.Route
module H = Httpz.Header_name
module M = Httpz.Method

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let () =
  check "absent accept is empty" (Negotiate.of_accept None = []);
  check "html preferred"
    (Negotiate.of_accept (Some "text/html,text/markdown;q=0.9")
    = [ `Html; `Markdown ]);
  check "q-value orders"
    (Negotiate.of_accept (Some "text/markdown;q=0.4, text/html;q=0.9")
    = [ `Html; `Markdown ])

let handler =
  Negotiate.v
    [
      (`Html, fun _env _req respond -> Resp.html respond "<h1>hi</h1>");
      ( `Markdown,
        fun _env _req respond ->
          Resp.media respond "text/markdown" "# hi" );
    ]

let site = Site.of_routes [ get (s "p" /? nil) handler ]
let compiled = Compiled.compile site

let () =
  let r =
    Proffer_mock.request compiled () M.Get "/p"
      ~headers:[ ("Accept", "text/markdown") ]
  in
  check "markdown chosen" (Proffer_mock.body r = "# hi");
  check "vary added" (Proffer_mock.header r H.Vary = Some "Accept");
  let r =
    Proffer_mock.request compiled () M.Get "/p"
      ~headers:[ ("Accept", "text/html") ]
  in
  check "html chosen" (Proffer_mock.body r = "<h1>hi</h1>");
  (* The client accepts both, and ranks Markdown first. Its order decides,
     not the order the variants were offered in. *)
  let r =
    Proffer_mock.request compiled () M.Get "/p"
      ~headers:[ ("Accept", "text/markdown, text/html") ]
  in
  check "client order decides" (Proffer_mock.body r = "# hi");
  let r = Proffer_mock.request compiled () M.Get "/p" in
  check "no accept falls back to first" (Proffer_mock.body r = "<h1>hi</h1>");
  Printf.printf "test_negotiate: %d checks ok\n" !checks
