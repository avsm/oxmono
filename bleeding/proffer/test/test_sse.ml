open Proffer

let checks = ref 0

let check name condition =
  incr checks;
  if not condition then failwith ("test_sse: " ^ name)
;;

let test_response () =
  let response =
    Proffer_mock.describe
    @@ fun respond ->
    Sse.respond respond ~retry:100
    @@ fun sink ->
    Sse.comment sink "keep\nalive";
    Sse.send sink ~name:"state" ~id:"7" "one\r\ntwo\n"
  in
  check "status" (Status.code (Proffer_mock.status response) = 200);
  check
    "content type"
    (Proffer_mock.header response Httpz.Header_name.Content_type
     = Some Httpz.Sse.media_type);
  check
    "no store"
    (Proffer_mock.header response Httpz.Header_name.Cache_control = Some "no-store");
  check
    "unknown stream length"
    (Proffer_mock.content_length response = None);
  check
    "framing"
    (Proffer_mock.body response
     = "retry: 100\n\n\
        : keep\n\
        : alive\n\n\
        event: state\n\
        data: one\n\
        data: two\n\
        data: \n\
        id: 7\n\n")
;;

let test_head () =
  let wrote = ref false in
  let response =
    Proffer_mock.describe ~meth:Httpz.Method.Head
    @@ fun respond -> Sse.respond respond (fun _ -> wrote := true)
  in
  check "HEAD does not run stream" (not !wrote);
  check "HEAD is empty" (Proffer_mock.body response = "");
  check "HEAD length unknown" (Proffer_mock.content_length response = None)
;;

let invalid name f =
  check
    name
    (match f () with
     | exception Invalid_argument _ -> true
     | () -> false)
;;

let test_validation () =
  invalid "event name newline" (fun () -> Httpz.Sse.send ignore ~name:"bad\nname" "data");
  invalid "id NUL" (fun () -> Httpz.Sse.send ignore ~id:"bad\000id" "data");
  invalid "comment NUL" (fun () -> Httpz.Sse.comment ignore "bad\000comment");
  invalid "comment DEL" (fun () -> Httpz.Sse.comment ignore "bad\127comment");
  invalid "negative retry" (fun () -> Httpz.Sse.retry ignore (-1))
;;

let () =
  test_response ();
  test_head ();
  test_validation ();
  Printf.printf "test_sse: %d checks ok\n" !checks
;;
