(* The IDLE deadline in Selected.Idle.wait_for_change, asserted on the exact
   bytes written and the mock clock. *)
module C = Imap_eio.Client
module S = Imap_eio.Selected
module E = Imap_eio.Error
module M = Imap_eio.Mailbox

let ok = function Ok x -> x | Error e -> failwith (C.error_to_string e)
let tag n = Printf.sprintf "A%08d" n

(* Each DONE written resolves the next of [dones], so a scripted read that
   awaits it models a server that answers only after DONE. *)
module Recording = struct
  type t = {
    input : Eio_mock.Flow.t;
    written : Buffer.t;
    dones : unit Eio.Promise.u Queue.t;
  }
  let read_methods = []
  let single_read t buffer = Eio.Flow.single_read t.input buffer
  let single_write t (buffers @ local) =
    let buffers = Cstruct.globalize_list buffers in
    List.iter (fun data ->
      let data = Cstruct.to_string data in
      Buffer.add_string t.written data;
      if data = "DONE\r\n" then
        Option.iter (fun r -> Eio.Promise.resolve r ())
          (Queue.take_opt t.dones)) buffers;
    Cstruct.lenv buffers
  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
  let shutdown _ _ = ()
  let close _ = ()
end

let recording_handler = Eio.Resource.handler (
  Eio.Resource.H (Eio.Resource.Close, Recording.close) ::
  Eio.Resource.bindings (Eio.Flow.Pi.two_way (module Recording)))

type read = Now of string | After_done of string | Never

(* A PREAUTH server answers CAPABILITY as A00000001 and SELECT as
   A00000002. [After_done reply] is read once the next DONE is written.
   The mock clock advances whenever every fiber is blocked on it. *)
let with_client reads f =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let input = Eio_mock.Flow.make "idle" in
  let dones = Queue.create () in
  let never, _ = Eio.Promise.create () in
  let actions = List.map (function
    | Now s -> `Return s
    | After_done reply ->
        let written, resolver = Eio.Promise.create () in
        Queue.add resolver dones;
        `Run (fun () -> Eio.Promise.await written; reply)
    | Never -> `Run (fun () -> Eio.Promise.await never)) reads in
  Eio_mock.Flow.on_read input ([`Return "* PREAUTH ready\r\n";
    `Return ("* CAPABILITY IMAP4rev1 UNSELECT IDLE\r\n" ^ tag 1 ^
      " OK caps\r\n");
    `Return ("* 8 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n" ^
      "* OK [UIDNEXT 9] next\r\n" ^ tag 2 ^ " OK selected\r\n")] @ actions);
  let transport = { Recording.input; written = Buffer.create 128; dones } in
  let client = ok (C.of_flow ~sw (Eio.Resource.T (transport,
    recording_handler))) in
  Buffer.clear transport.written;
  Fun.protect ~finally:(fun () -> C.close client) (fun () ->
    f client transport.written env#clock)

let select = tag 2 ^ " SELECT INBOX\r\n"

let wire label written expected =
  let got = Buffer.contents written in
  if got <> expected then
    failwith (Printf.sprintf "%s: expected\n%s\ngot\n%s" label
      (String.escaped expected) (String.escaped got))

(* [idle client clock ~timeout] runs one IDLE on INBOX and is its result
   and the seconds that passed on [clock]. *)
let idle client clock ~timeout =
  ok (C.with_mailbox client ~mode:`Read_write "INBOX" (fun selected ->
    let idle = ok (S.Idle.require selected) in
    let start = Eio.Time.now clock in
    let result = S.Idle.wait_for_change idle ~clock ~timeout in
    Ok (result, Eio.Time.now clock -. start)))

let test_change_before_timeout () =
  with_client
    [Now "+ idling\r\n* 9 EXISTS\r\n"; After_done (tag 3 ^ " OK done\r\n");
     Now (tag 4 ^ " OK unselected\r\n")]
    (fun client written clock ->
      match idle client clock ~timeout:600. with
      | Ok [Imap.Response.Untagged (Imap.Response.Exists 9L)], 0. ->
          wire "change before timeout" written
            (select ^ tag 3 ^ " IDLE\r\nDONE\r\n" ^ tag 4 ^ " UNSELECT\r\n")
      | Ok _, 0. -> failwith "change before timeout lost the EXISTS"
      | Ok _, _ -> failwith "change before timeout waited for the timer"
      | Error e, _ -> failwith (C.error_to_string e))

let test_timeout_without_change () =
  with_client
    [Now "+ idling\r\n"; After_done (tag 3 ^ " OK done\r\n");
     Now (tag 4 ^ " OK unselected\r\n")]
    (fun client written clock ->
      match idle client clock ~timeout:600. with
      | Ok [], 600. ->
          wire "timeout without change" written
            (select ^ tag 3 ^ " IDLE\r\nDONE\r\n" ^ tag 4 ^ " UNSELECT\r\n")
      | Ok [], _ -> failwith "timeout did not wait for the deadline"
      | Ok _, _ -> failwith "timeout without change returned responses"
      | Error e, _ -> failwith (C.error_to_string e))

let test_timeout_with_keepalive () =
  with_client
    [Now "+ idling\r\n";
     After_done ("* OK Still here\r\n" ^ tag 3 ^ " OK done\r\n");
     Now (tag 4 ^ " OK unselected\r\n")]
    (fun client written clock ->
      match idle client clock ~timeout:600. with
      | Ok [Imap.Response.Untagged (Imap.Response.Ok (None, _))], 600. ->
          wire "timeout with keepalive" written
            (select ^ tag 3 ^ " IDLE\r\nDONE\r\n" ^ tag 4 ^ " UNSELECT\r\n")
      | Ok _, 600. -> failwith "timeout lost the keepalive"
      | Ok _, _ -> failwith "keepalive after DONE moved the deadline"
      | Error e, _ -> failwith (C.error_to_string e))

let test_timeout_refused () =
  with_client [Now (tag 3 ^ " OK unselected\r\n")]
    (fun client written clock ->
      ok (C.with_mailbox client ~mode:`Read_write "INBOX" (fun selected ->
        let idle = ok (S.Idle.require selected) in
        List.iter (fun timeout ->
          match S.Idle.wait_for_change idle ~clock ~timeout with
          | Error (E.State _) -> ()
          | _ -> failwith (Printf.sprintf "IDLE timeout %g accepted" timeout))
          [1741.; 0.; -1.; Float.nan; Float.infinity];
        Ok ()));
      wire "refused timeout" written (select ^ tag 3 ^ " UNSELECT\r\n");
      if not (C.is_open client) then
        failwith "refused timeout closed the connection")

let test_cancellation_closes () =
  with_client [Now "+ idling\r\n"; Never] (fun client written clock ->
    let outcome = ref None in
    ignore (C.with_mailbox client ~mode:`Read_write "INBOX" (fun selected ->
      let idle = ok (S.Idle.require selected) in
      outcome := Some (Eio.Time.with_timeout clock 10. (fun () ->
        Ok (S.Idle.wait_for_change idle ~clock ~timeout:600.)));
      Ok ()));
    (match !outcome with
     | Some (Error `Timeout) -> ()
     | _ -> failwith "cancelled IDLE did not end at the caller's deadline");
    wire "cancelled IDLE" written (select ^ tag 3 ^ " IDLE\r\n");
    if C.is_open client then
      failwith "cancelled IDLE kept the connection open")

(* An IDLE that times out is dropped by Mailbox.wait, which enters IDLE
   again on the same lease until a round reports a change. *)
let test_wait_renews () =
  with_client
    [Now "+ idling\r\n"; After_done (tag 3 ^ " OK done\r\n");
     Now "+ idling\r\n";
     After_done ("* 9 EXISTS\r\n" ^ tag 4 ^ " OK done\r\n");
     Now (tag 5 ^ " OK unselected\r\n")]
    (fun client written clock ->
      let start = Eio.Time.now clock in
      match ok (C.with_mailbox client ~mode:`Read_write "INBOX" (fun s ->
          Ok (M.wait ~timeout:300. (M.of_selected s) ~clock
            ~poll_seconds:60.))) with
      | {strategy = `Idle;
         result = Ok [Imap.Response.Untagged (Imap.Response.Exists 9L)]} ->
          if Eio.Time.now clock -. start <> 600. then
            failwith "wait did not renew IDLE at each timeout";
          wire "wait renews IDLE" written
            (select ^ tag 3 ^ " IDLE\r\nDONE\r\n" ^
             tag 4 ^ " IDLE\r\nDONE\r\n" ^ tag 5 ^ " UNSELECT\r\n")
      | {result = Error e; _} -> failwith (C.error_to_string e)
      | _ -> failwith "wait returned a timed-out round")

let () =
  test_change_before_timeout ();
  test_timeout_without_change ();
  test_timeout_with_keepalive ();
  test_timeout_refused ();
  test_cancellation_closes ();
  test_wait_renews ()
