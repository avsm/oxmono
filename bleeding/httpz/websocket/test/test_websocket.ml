module W = Httpz_websocket
module F = W.Frame

let check label b = Alcotest.(check bool) label true b
let bytes = Bytes.of_string
let key = "dGhlIHNhbXBsZSBub25jZQ=="

let handshake () =
  let headers = ("Host", "server.example.com") ::
    W.Handshake.request ~nonce:"the sample nonce" ~protocols:["chat"] () in
  let response = match W.Handshake.accept ~protocol:"chat" ~meth:"GET"
      ~http_1_1:true headers with
    | Ok h -> h | Error s -> failwith s in
  Alcotest.(check string) "RFC accept value" "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
    (List.assoc "Sec-WebSocket-Accept" response);
  check "client verifies" (W.Handshake.verify ~protocols:["chat"] ~key
    ~status:101 response = Ok (Some "chat"));
  let rejects h = match W.Handshake.accept ~meth:"GET" ~http_1_1:true h with
    | Error _ -> true | Ok _ -> false in
  check "duplicate key" (rejects (("Sec-WebSocket-Key", key) :: headers));
  check "duplicate Host" (rejects (("host", "elsewhere.test") :: headers));
  check "non-canonical key" (rejects (List.map (fun (k, v) ->
    k, if k = "Sec-WebSocket-Key" then "dGhlIHNhbXBsZSBub25jZR==" else v) headers));
  check "invalid protocol offer"
    (rejects (("Sec-WebSocket-Protocol", "a b") :: headers));
  check "extension declined" (not (rejects
    (("Sec-WebSocket-Extensions",
      "permessage-deflate; client_max_window_bits=\"12\"") :: headers)));
  List.iter (fun value -> check "malformed extension rejected" (rejects
    (("Sec-WebSocket-Extensions", value) :: headers)))
    ["permessage-deflate;"; "permessage-deflate,"; "x; a=\"bad space\""; "x; a=\"" ];
  check "POST rejected" (Result.is_error
    (W.Handshake.accept ~meth:"POST" ~http_1_1:true headers));
  check "HTTP/1.0 rejected" (Result.is_error
    (W.Handshake.accept ~meth:"GET" ~http_1_1:false headers));
  List.iter (fun extra ->
    check "unsolicited or duplicate response field"
      (Result.is_error (W.Handshake.verify ~protocols:["chat"] ~key ~status:101
         (extra :: response))))
    ["Sec-WebSocket-Extensions", "permessage-deflate";
     "Sec-WebSocket-Protocol", "chat";
     "Sec-WebSocket-Accept", "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="]

let status ?(role = W.Client) ?(limit = 100000) s =
  let b = bytes s in
  let #(status, _) = F.parse ~role ~max_payload:limit b ~off:0 ~len:(Bytes.length b) in
  status

let frames () =
  check "empty" (status "" = F.Partial);
  check "partial base header" (status "\x81" = F.Partial);
  check "RFC text header" (status "\x81\x05Hello" = F.Complete);
  check "RFC client mask" (status ~role:W.Server
    "\x81\x85\x37\xfa\x21\x3d\x7f\x9f\x4d\x51\x58" = F.Complete);
  List.iter (fun s -> check "invalid frame" (status s = F.Malformed))
    ["\xc1\x00"; "\x83\x00"; "\x09\x00"; "\x89\x7e";
     "\x81\x80\x00\x00\x00\x00";
     "\x82\x7e\x00\x7d";
     "\x82\x7f\x00\x00\x00\x00\x00\x00\xff\xff";
     "\x82\x7f\x80\x00\x00\x00\x00\x00\x00\x00"];
  check "server requires masking" (status ~role:W.Server "\x81\x00" = F.Malformed);
  check "oversized 64-bit length" (status
    "\x82\x7f\x7f\xff\xff\xff\xff\xff\xff\xff" = F.Too_large);
  check "header accepted before payload arrives"
    (status "\x82\x7e\x01\x00" = F.Complete);
  check "application limit" (status ~limit:255 "\x82\x7e\x01\x00" = F.Too_large);
  let b = bytes "Hello" in
  F.mask b ~off:0 ~len:5 ~key:0x37fa213d ~offset:0;
  Alcotest.(check bytes) "RFC masked payload" (bytes "\x7f\x9f\x4d\x51\x58") b;
  F.mask b ~off:0 ~len:2 ~key:0x37fa213d ~offset:0;
  F.mask b ~off:2 ~len:3 ~key:0x37fa213d ~offset:2;
  Alcotest.(check bytes) "mask chunk offset" (bytes "Hello") b;
  let h = Bytes.create 14 in
  let n = F.write h ~off:0 ~fin:true ~opcode:F.Text ~length:5 ~masked:true
      ~mask:0x37fa213d in
  Alcotest.(check string) "RFC client header" "\x81\x85\x37\xfa\x21\x3d"
    (Bytes.sub_string h 0 n);
  List.iter (fun length ->
    ignore (F.write h ~off:0 ~fin:true ~opcode:F.Binary ~length ~masked:false ~mask:0);
    let #(s, parsed) = F.parse ~role:W.Client ~max_payload:max_int h ~off:0 ~len:14 in
    check "length boundary" (s = F.Complete && parsed.#length = length))
    [0; 125; 126; 65535; 65536; max_int];
  let #(s, _) = F.parse ~role:W.Client ~max_payload:1 h ~off:max_int ~len:2 in
  check "invalid range" (s = F.Malformed)

(* The transport deliberately splits every input byte. Atomic cells let the
   compiler check portability of retained callbacks without unsafe casts. *)
let connection ?(role = W.Client) ?max_message ?max_fragments input =
  let pos = Atomic.make 0 in
  let output = Atomic.make [] in
  let random_calls = Atomic.make 0 in
  let random b ~off ~len =
    Atomic.incr random_calls;
    for i = 0 to len - 1 do Bytes.set b (off + i) (Char.chr (i + 1)) done in
  let read b ~off ~len =
    let p = Atomic.get pos in
    let n = min 1 (min len (String.length input - p)) in
    Bytes.blit_string input p b off n;
    Atomic.set pos (p + n);
    n in
  let write b ~off ~len =
    Atomic.set output (Bytes.sub_string b off len :: Atomic.get output) in
  let t = W.create ~role ?max_message ?max_fragments ~random ~read ~write
      ~with_write_lock:(fun f -> f ()) () in
  t, (fun () -> String.concat "" (List.rev (Atomic.get output))), random_calls

let receive t =
  let result = ref None in
  let got = W.receive t ~f:(fun kind b ~off ~len ->
    result := Some (kind, Bytes.sub_string b off len)) in
  got, !result

let messages () =
  let t, out, _ = connection
    "\x01\x02\xe2\x82\x89\x01?\x80\x01\xac\x88\x02\x03\xe8" in
  check "UTF-8 split over fragments, ping interleaved"
    (receive t = (true, Some (W.Text, "\xe2\x82\xac")));
  check "masked pong" (String.sub (out ()) 0 2 = "\x8a\x81");
  check "close received" (receive t = (false, None));
  check "closed connection stays closed" (receive t = (false, None));
  let t, _, _ = connection "\x82\x00\x81\x01x" in
  check "empty binary message" (receive t = (true, Some (W.Binary, "")));
  check "next message" (receive t = (true, Some (W.Text, "x")));
  let t, out, _ = connection ~role:W.Server
    "\x81\x85\x37\xfa\x21\x3d\x7f\x9f\x4d\x51\x58" in
  check "server unmasks RFC frame" (receive t = (true, Some (W.Text, "Hello")));
  W.send t W.Text (bytes "Hello") ~off:0 ~len:5;
  Alcotest.(check string) "server frame" "\x81\x05Hello" (out ())

let rejected () =
  let rejects ?max_message ?max_fragments code input =
    let t, _, _ = connection ?max_message ?max_fragments input in
    match receive t with
    | _ -> Alcotest.fail "invalid message accepted"
    | exception W.Protocol_error (actual, _) ->
        Alcotest.(check int) "close error category" code actual;
        check "connection invalidated"
          (try ignore (receive t); false with Invalid_argument _ -> true)
  in
  List.iter (rejects 1002)
    ["\x80\x00"; "\x01\x00\x81\x00"; "\x88\x01x";
     "\x89\x7f"; "\xc1\x7f"; "\x83\x7f";
     "\x88\x02\x03\xed"; "\x88\x02\x03\xee";
     "\x88\x02\x03\xf2"];
  List.iter (rejects 1007)
    ["\x81\x02\xc0\x80"; "\x81\x03\xed\xa0\x80";
     "\x81\x04\xf4\x90\x80\x80"; "\x81\x01\xff";
     "\x81\x02\xe2\x82"; "\x88\x03\x03\xe8\xff"];
  rejects ~max_message:3 1009 "\x02\x02ab\x80\x02cd";
  rejects ~max_fragments:2 1009 "\x02\x00\x00\x00\x80\x00";
  List.iter (rejects 1006) [""; "\x81"; "\x81\x03ab"; "\x01\x00"]

let writes () =
  let t, out, calls = connection "\x82\x01x\x88\x00" in
  let b = Bytes.make 9000 'x' in
  W.send t W.Binary b ~off:1 ~len:8999;
  W.ping t (bytes "p") ~off:0 ~len:1;
  check "fresh masking key per frame" (Atomic.get calls = 2);
  check "source unchanged" (Bytes.for_all ((=) 'x') b);
  let server, _, _ = connection ~role:W.Server (out ()) in
  check "scratch buffer spans one frame"
    (receive server = (true, Some (W.Binary, String.make 8999 'x')));
  W.close t ();
  W.close t ();
  check "close emitted only once" (Atomic.get calls = 3);
  check "data after sent close is not delivered" (receive t = (false, None));
  let t, _, _ = connection "" in
  check "invalid send text rejected"
    (try W.send t W.Text (bytes "\xff") ~off:0 ~len:1; false
     with Invalid_argument _ -> true);
  check "invalid close reason rejected"
    (try W.close t ~reason:"\xff" (); false with Invalid_argument _ -> true);
  check "reserved close code rejected"
    (try W.close t ~code:1005 (); false with Invalid_argument _ -> true)

let failures () =
  let t = W.create ~role:W.Server
      ~read:(fun _ ~off:_ ~len:_ -> 0)
      ~write:(fun _ ~off:_ ~len:_ -> raise Exit)
      ~with_write_lock:(fun f -> f ()) () in
  check "write exception propagated"
    (try W.send t W.Binary (bytes "x") ~off:0 ~len:1; false with Exit -> true);
  check "partial write poisons connection"
    (try W.ping t (bytes "") ~off:0 ~len:0; false with Invalid_argument _ -> true);
  check "client requires random source"
    (try ignore (W.create ~role:W.Client
      ~read:(fun _ ~off:_ ~len:_ -> 0) ~write:(fun _ ~off:_ ~len:_ -> ())
      ~with_write_lock:(fun f -> f ()) ()); false
     with Invalid_argument _ -> true)

let () = Alcotest.run "WebSocket"
  ["protocol", List.map (fun (name, f) -> Alcotest.test_case name `Quick f)
    ["handshake", handshake; "frame vectors", frames;
     "messages and control", messages; "malformed messages", rejected;
     "writes and close", writes; "transport failures", failures]]
