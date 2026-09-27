(* Read 100,000 FETCH rows through Client, Session and Selected from a
   mock flow, as 100 fetch_range windows of 1,000 UIDs, the most one call
   accepts. *)

let windows = 100
let window = 1_000

let uid n =
  match Imap.Uid.of_int64 (Int64.of_int n) with
  | Ok u -> u
  | Error e -> failwith e

let tag n = Printf.sprintf "A%08d" n

(* Each window's response is one read of at most 64 KiB, which is what
   Session asks the flow for. *)
let window_response w =
  let b = Buffer.create (window * 48) in
  for n = w * window + 1 to (w + 1) * window do
    Printf.bprintf b "* %d FETCH (UID %d FLAGS (\\Seen) MODSEQ (%d))\r\n"
      n n n
  done;
  Printf.bprintf b "%s OK fetched\r\n" (tag (w + 5));
  let s = Buffer.contents b in
  assert (String.length s <= 65_536);
  s

let capability = "IMAP4rev1 CONDSTORE UNSELECT"

let script =
  [ "* OK ready\r\n";
    "* CAPABILITY " ^ capability ^ "\r\nA00000001 OK done\r\n";
    "A00000002 OK logged in\r\n";
    "* CAPABILITY " ^ capability ^ "\r\nA00000003 OK done\r\n";
    "* 100000 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n\
     * OK [UIDNEXT 100001] next\r\n* OK [HIGHESTMODSEQ 100000] modseq\r\n\
     A00000004 OK [READ-ONLY] selected\r\n" ]
  @ List.init windows window_response
  @ [ tag (windows + 5) ^ " OK unselected\r\n" ]

let items = [ Imap.Fetch_item.Flags; Imap.Fetch_item.Modseq ]

let () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow = Eio_mock.Flow.make ~pp:(fun _ _ -> ()) "bench" in
  Eio_mock.Flow.on_read flow (List.map (fun s -> `Return s) script);
  Eio_mock.Flow.on_copy_bytes flow [ `Return 65_536 ];
  let auth = Imap_eio.Auth.password ~username:"user" ~password:"pw"
    ~allow_insecure_transport:true () in
  let client = match Imap_eio.Client.of_flow ~sw ~auth flow with
    | Ok c -> c
    | Error e -> failwith (Imap_eio.Client.error_to_string e) in
  let workload () =
    Imap_eio.Client.with_mailbox client ~mode:`Read_only "INBOX"
      (fun selected ->
        let rec go w total =
          if w = windows then Ok total else
          match Imap_eio.Selected.fetch_range selected
                  ~first:(uid (w * window + 1))
                  ~last:(uid ((w + 1) * window)) ~items with
          | Ok rows -> go (w + 1) (total + List.length rows)
          | Error _ as e -> e in
        go 0 0) in
  match Bench_measure.run "session fetch_range 100000 rows" workload with
  | Ok n when n = windows * window -> Imap_eio.Client.close client
  | Ok n -> failwith (Printf.sprintf "%d rows" n)
  | Error e -> failwith (Imap_eio.Client.error_to_string e)
