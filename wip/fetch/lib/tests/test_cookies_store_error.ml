open Fetch

(* Regression for R34: a jar's store error must close the response it
   arrived on rather than leak it, and the error must still reach the
   caller. *)
let test_store_error_closes_response env =
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let native_dir = Filename.temp_dir ~perms:0o700 "fetch-cookies-failed-save-" "" in
  let dir = Eio.Path.(Eio.Stdenv.fs env / native_dir) in
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true dir)
    (fun () ->
      let path = Eio.Path.(dir / "jar.txt") in
      let jar = Fetch_cookies.Jar.of_file ~clock path in
      (* A directory in place of the file makes the atomic rename that
         [`On_change] performs on the first [Set-Cookie] fail. *)
      Eio.Path.mkdir ~perm:0o700 path;
      let closed = ref false in
      let source = Eio.Flow.string_source "" in
      let server (req : Middleware.request) =
        Middleware.Pi.response ~status:200
          ~headers:(Http.Header.of_list [ ("Set-Cookie", "a=1; Path=/") ])
          ~version:`HTTP_1_1
          ~body:(source :> Eio.Flow.source_ty Eio.Resource.t)
          ~close:(fun () -> closed := true)
          ~url:req.url ()
      in
      let client = Fetch_cookies.with_jar jar (Fetch_mock.client server) in
      (match get ~sw client "https://example.com/" with
      | _ -> Alcotest.fail "a failed cookie store should have raised"
      | exception Eio.Io _ -> ()
      | exception exn ->
          Alcotest.failf "wrong exception: %s" (Printexc.to_string exn));
      Alcotest.(check bool)
        "response closed despite the store error" true !closed)

let () =
  Eio_main.run @@ fun env ->
  Alcotest.run "fetch-cookies-store-error"
    [
      ( "with_jar",
        [
          Alcotest.test_case "store error closes response" `Quick (fun () ->
              test_store_error_closes_response env);
        ] );
    ]
