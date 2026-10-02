open Crowthebot

let check label b = if not b then failwith label
let json s = Result.get_ok (Jsont_bytesrw.decode_string Jsont.json s)
let admin = "@admin:example.org"
let room = "!room:example.org"

let field key codec s =
  Result.get_ok (Jsont_bytesrw.decode_string (Jsont.mem key codec) s)

let contains text part =
  let rec loop i =
    i + String.length part <= String.length text
    && (String.sub text i (String.length part) = part || loop (i + 1))
  in
  loop 0

let () =
  let logs = Buffer.create 4096 in
  let formatter = Format.formatter_of_buffer logs in
  Logs.set_reporter (Logs.format_reporter ~app:formatter ~dst:formatter ());
  Diagnostics.configure ~verbose:false;
  let logged part =
    Format.pp_print_flush formatter ();
    contains (Buffer.contents logs) part
  in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let now = ref 1788912000. in
  let db = Sqlite3_eio.open_memory ~sw () in
  let store = Store.create ~now:(fun () -> !now) db ~admin in
  let state = Store.locations store in
  ignore
    (Location_store.attach state ~actor:admin ~room ~event:"$link"
       ~person:"Alice" ~connection:"home" ~user:"alice"
       ~device:"0b8a58ec-phone");
  (* The Recorder serves whatever fixes the phone has published so far. *)
  let fixes = ref [ !now -. 3600. ] in
  let fetch =
    Fetch_mock.client (fun req ->
        Fetch_mock.respond
          ("["
          ^ String.concat ","
              (List.map
                 (fun t -> Printf.sprintf {|{"lat":51,"lon":0,"tst":%.0f}|} t)
                 !fixes)
          ^ "]")
          req)
  in
  let config =
    Result.get_ok
      (Owntracks_config.of_string ~client_id:"test"
         {|
[owntracks.recorder]
url="https://recorder.example/tracks"
[[owntracks.devices]]
id="0B8A58EC-PHONE"
name="Alice Phone"
|})
  in
  let published = ref [] and answers = ref true in
  let publish _ ~topic payload =
    published := (topic, payload) :: !published;
    if !answers then begin
      now := !now +. 5.;
      fixes := !now :: !fixes
    end
  in
  let locations ?publish () =
    Locations.create ~fresh_wait:0.3 ~state
      ~sources:
        [
          ( "home",
            Owntracks_source.initialize ?publish
              ~load:(fun _ -> config)
              ~fetch ~clock:(Eio.Stdenv.mono_clock env)
              ~now:(fun () -> !now)
              (json
                 {|{"config_file":"/private/fixture.toml","user":"alice","device":"0b8a58ec-phone","allow_http":false,"lookback_days":7}|})
          );
        ]
      ~default:(Some "home") ()
  in
  let get ?publish args =
    Locations.invoke
      (Locations.for_request (locations ?publish ()) ~actor:admin ~room
         ~event:"$q")
      "location_get" args
  in
  let fresh = {|{"person":"Alice","fresh":true}|} in
  let recorded output =
    field "location"
      (Jsont.mem "last_reported_position"
         (Jsont.mem "recorded_at" Jsont.string))
      output
  in
  (match get ~publish fresh with
  | Ok output ->
      check "a fresh fix is requested on the phone's own topic"
        (!published
        = [
            ( "owntracks/alice/0B8A58EC-PHONE/cmd",
              {|{"_type":"cmd","action":"reportLocation"}|} );
          ]);
      check "the new fix is returned and cached"
        (field "fresh_fix" Jsont.string output = "received"
        && recorded output = Store.timestamp !now)
  | Error e -> failwith e);
  answers := false;
  let before = Store.timestamp !now in
  (match get ~publish fresh with
  | Ok output ->
      check "a silent phone leaves the latest fix and says so"
        (String.starts_with ~prefix:"none within 0.3 seconds"
           (field "fresh_fix" Jsont.string output)
        && recorded output = before)
  | Error e -> failwith e);
  (match get fresh with
  | Ok output ->
      check "without a publisher the request is unavailable"
        (String.starts_with ~prefix:"unavailable"
           (field "fresh_fix" Jsont.string output))
  | Error e -> failwith e);
  check "the request, its outcome and the starting fix are logged"
    (logged
       "Location fix requested topic=\"owntracks/alice/0B8A58EC-PHONE/cmd\""
    && logged "Location fix received user=\"alice\""
    && logged "Location fix not received user=\"alice\""
    && logged "Location fresh fix wanted person=\"Alice\""
    && logged "age_min=");
  check "location logs carry no coordinates"
    (not (logged "latitude") && not (logged "lat="));
  let failing _ ~topic:_ _ = failwith "connection refused" in
  (match get ~publish:failing fresh with
  | Ok output ->
      check "a broker failure is reported to the model"
        (field "fresh_fix" Jsont.string output
        = "Could not send the location request over MQTT.")
  | Error e -> failwith e);
  check "a broker failure is logged with its reason"
    (logged "Location fix request failed"
    && logged "connection refused");
  check "a plain get publishes nothing"
    (let n = List.length !published in
     Result.is_ok (get ~publish {|{"person":"Alice"}|})
     && List.length !published = n);
  print_endline "crowthebot: fresh OwnTracks fix requests passed"
