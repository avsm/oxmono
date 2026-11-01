(* Watch.run renews IDLE on its waiting connection instead of reconnecting,
   asserted on the exact bytes of that connection and the mock clock. *)
let scope : Imap.Mirror.scope = {
  endpoint = "scripted.example"; account = "alice"; mailbox_key = "INBOX";
  raw_name = "INBOX"; encoding = Imap.Mailbox_name.Rev1; mailbox_id = None;
}

let root () =
  let path = Filename.temp_file "imap-watch-" "" in
  Sys.remove path;
  Unix.mkdir path 0o700;
  List.iter (fun child -> Unix.mkdir (Filename.concat path child) 0o700)
    ["blob"; "spool"];
  path

let rec remove_tree path =
  if Sys.is_directory path then (
    Sys.readdir path |> Array.iter (fun child ->
      remove_tree (Filename.concat path child));
    Unix.rmdir path)
  else Sys.remove path

(* Each DONE written resolves the next of [dones]. *)
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

(* [Renewal reply] advances the mock clock to the IDLE timer and is read
   once DONE is written. [Stop] ends the watch. *)
type read = Now of string | Renewal of string | Stop

exception Stopped

let caps = "IMAP4rev1 UNSELECT IDLE CONDSTORE"

let handshake = [
  Now "* OK ready\r\n";
  Now ("* CAPABILITY " ^ caps ^ "\r\nA00000001 OK done\r\n");
  Now "A00000002 OK logged in\r\n";
  Now ("* CAPABILITY " ^ caps ^ "\r\nA00000003 OK done\r\n")]

let handshake_wire =
  "A00000001 CAPABILITY\r\nA00000002 LOGIN \"alice\" \"secret\"\r\n\
   A00000003 CAPABILITY\r\n"

let select tag = Now (Printf.sprintf
  "* 0 EXISTS\r\n* OK [UIDVALIDITY 11] valid\r\n* OK [UIDNEXT 1] next\r\n\
   * OK [HIGHESTMODSEQ 5] anchor\r\nA%08d OK [READ-ONLY] selected\r\n" tag)

let examine tag = Printf.sprintf "A%08d EXAMINE INBOX (CONDSTORE)\r\n" tag

let test_renewal_keeps_connection () =
  let dir = root () in
  Fun.protect ~finally:(fun () -> remove_tree dir) @@ fun () ->
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let fs = Eio.Stdenv.fs env in
  let real_clock = Eio.Stdenv.clock env in
  let clock = Eio_mock.Clock.make () in
  let store = Imap_store.open_path ~sw
    ~blob_dir:Eio.Path.(fs / dir / "blob") Eio.Path.(fs / dir / "sync.db") in
  let spool_dir = Eio.Path.(fs / dir / "spool") in
  let renewed_at = ref None in
  let scan = handshake @ [select 4; Now "A00000005 OK unselected\r\n"] in
  let waiting = handshake @ [
    select 4; Now "+ idling\r\n"; Renewal "A00000005 OK idle done\r\n";
    Now "A00000006 OK unselected\r\n";
    select 7; Now "+ idling\r\n"; Stop] in
  let connections = ref [scan; waiting] in
  let wires = ref [] in
  let action dones = function
    | Now s -> `Return s
    | Renewal reply ->
        let written, resolver = Eio.Promise.create () in
        Queue.add resolver dones;
        `Run (fun () ->
          (* Only the IDLE timer and the later deadline of its round are
             pending on the mock clock. *)
          Eio_mock.Clock.advance clock;
          Eio.Time.with_timeout_exn real_clock 10. (fun () ->
            Eio.Promise.await written);
          renewed_at := Some (Eio.Time.now clock);
          reply)
    | Stop -> `Run (fun () -> raise Stopped) in
  let connect ~sw =
    match !connections with
    | [] -> failwith "watch opened an unexpected connection"
    | reads :: rest ->
        connections := rest;
        let dones = Queue.create () in
        let input = Eio_mock.Flow.make "watch" in
        Eio_mock.Flow.on_read input (List.map (action dones) reads);
        let transport = { Recording.input; written = Buffer.create 256;
          dones } in
        wires := transport.written :: !wires;
        let auth = Imap_eio.Auth.password ~username:"alice"
          ~password:"secret" ~allow_insecure_transport:true () in
        match Imap_eio.Client.of_flow ~sw ~auth
            (Eio.Resource.T (transport, recording_handler)) with
        | Error e -> Error (Imap_sync.Error.Client e)
        | Ok client ->
            match Imap_sync.Ctx.v ~client ~store ~scope ~mailbox:"INBOX"
                ~spool_dir ~next_id:(fun () -> failwith "unexpected ID") with
            | Ok ctx -> Ok ctx
            | Error e -> failwith (Format.asprintf "context: %a"
                Imap_sync.Error.pp e) in
  let published = ref 0 in
  (match Imap_sync.Watch.run ~clock ~connect
      ~next_stage_id:(fun () -> "renewal")
      ~on_publish:(fun receipt ->
        incr published;
        if receipt.cursor.anchor = None then
          failwith "scan published no CONDSTORE anchor")
      ~on_retry:(fun _ -> failwith "renewal watch retried") () with
   | exception Stopped -> ()
   | _ -> failwith "watch returned");
  if !published <> 1 then failwith "renewal started a scan";
  if !connections <> [] then failwith "watch skipped a scripted connection";
  (match !renewed_at with
   | Some 1500. -> ()
   | _ -> failwith "IDLE was not renewed after idle_renew_seconds");
  let wait_wire = match !wires with
    | [wait; _scan] -> Buffer.contents wait
    | _ -> failwith "watch did not open two connections" in
  let expected = handshake_wire ^ examine 4 ^ "A00000005 IDLE\r\nDONE\r\n\
    A00000006 UNSELECT\r\n" ^ examine 7 ^ "A00000008 IDLE\r\n" in
  if wait_wire <> expected then
    failwith (Printf.sprintf "waiting connection: expected\n%s\ngot\n%s"
      (String.escaped expected) (String.escaped wait_wire))

let () = test_renewal_keeps_connection ()
