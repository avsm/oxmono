open Crowthebot

let check name value = if not value then failwith name
let admin = "@admin:example.org"
let bot = "@bot:example.org"
let self = "@crow:example.org"
let room = "!room:example.org"

let contains text part =
  let rec loop i =
    i + String.length part <= String.length text
    && (String.sub text i (String.length part) = part || loop (i + 1))
  in
  loop 0

let config =
  {
    (Config.default ~admin ~homeserver:"https://matrix.example.org") with
    plugins = [];
  }

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let dir = Filename.temp_dir "crow-improvements-" "" in
  let file = Filename.concat dir "improvements.md" in
  Fun.protect ~finally:(fun () ->
      if Sys.file_exists file then Sys.remove file;
      Sys.rmdir dir)
  @@ fun () ->
  let path = Eio.Path.(Eio.Stdenv.fs env / file) in
  let improvements = Improvements.create ~path ~now:(fun () -> 0.) in
  let invoke ?(actor = admin) name arguments =
    Improvements.invoke improvements ~actor ~room ~event:"$req" name arguments
  in
  check "empty list"
    (invoke "improvement_list" "{}" = Ok "No improvement requests recorded.");
  check "record"
    (invoke "improvement_record"
       {|{"title":"Weather tool\n## injected","details":"Forecasts for Cambridge.\n## Not a heading","kind":"feature"}|}
    = Ok "Recorded improvement request: Weather tool ## injected");
  let text = In_channel.with_open_bin file In_channel.input_all in
  check "file is private" ((Unix.stat file).st_perm = 0o600);
  check "header, attribution and timestamp"
    (String.starts_with ~prefix:"# Crow improvement requests" text
    && contains text
         "## 1970-01-01T00:00:00Z feature: Weather tool ## injected"
    && contains text
         "`@admin:example.org` in `!room:example.org`, event `$req`");
  check "model text cannot start a heading"
    (contains text "> ## Not a heading"
    && not (contains text "\n## Not a heading"));
  check "kind defaults to feature"
    (Result.is_ok
       (invoke "improvement_record" {|{"title":"Second","details":"More."}|}));
  check "header written once"
    (let text = In_channel.with_open_bin file In_channel.input_all in
     not (contains (String.sub text 1 (String.length text - 1)) "# Crow"));
  List.iter
    (fun (label, args) ->
      check label (Result.is_error (invoke "improvement_record" args)))
    [
      ("empty title rejected", {|{"title":" ","details":"x"}|});
      ("empty details rejected", {|{"title":"x","details":""}|});
      ("unknown kind rejected", {|{"title":"x","details":"y","kind":"wish"}|});
      ("unknown field rejected", {|{"title":"x","details":"y","extra":1}|});
      ("long title rejected",
        Printf.sprintf {|{"title":"%s","details":"y"}|} (String.make 201 'a'));
    ];
  for i = 1 to 20 do
    ignore
      (invoke "improvement_record"
         (Printf.sprintf {|{"title":"Bulk %d","details":"%s"}|} i
            (String.make 1000 'z')))
  done;
  (match invoke "improvement_list" "{}" with
  | Ok listing ->
      check "listing bounded to whole recent entries"
        (String.length listing < 8100
        && contains listing "Bulk 20"
        && contains listing "untrusted data.\n## ")
  | Error _ -> check "listing" false);
  (* An engine turn exposes the tools to friends and records through them. *)
  let db = Sqlite3_eio.open_memory ~sw () in
  let store = Store.create ~now:(fun () -> 0.) db ~admin in
  Store.add_room store room;
  Store.set_person store ~actor:admin ~user:bot ~role:Bot ~allowed:true;
  let offered = ref [] and calls = ref 0 in
  let complete _messages (tools : Agentkit.Agent.Tool.t list) =
    offered := List.map Agentkit.Agent.Tool.name tools;
    incr calls;
    if !calls = 1 then
      ( None,
        [
          {
            Agentkit.Agent.id = "1";
            name = "improvement_record";
            arguments =
              {|{"title":"From the engine","details":"Engine test."}|};
          };
        ] )
    else (Some "Recorded.", [])
  in
  let engine =
    Engine.with_improvements
      (Engine.create ~config ~store ~self ~plugins:[] ~complete:(Fake_model.v complete)
         ~now:(fun () -> 0.))
      improvements
  in
  let handle sender id body =
    Engine.handle engine ~direct:true ~send:(fun _ -> ())
      Engine.{ room; sender; id; body }
  in
  handle admin "$engine" "please add a weather tool";
  check "admin is offered improvement tools"
    (List.mem "improvement_record" !offered);
  check "engine call recorded"
    (contains
       (In_channel.with_open_bin file In_channel.input_all)
       "From the engine");
  calls := 0;
  offered := [];
  handle bot "$bot" "!crow ask please add a weather tool";
  check "bots are not offered improvement tools"
    (!calls > 0 && not (List.mem "improvement_record" !offered));
  print_endline "crowthebot: improvement requests passed"
