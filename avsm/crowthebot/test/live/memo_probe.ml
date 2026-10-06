(* Uses synthetic identities and an in-memory store. No Matrix connection. *)
let contains text part =
  let text = String.lowercase_ascii text in
  let rec loop i =
    i + String.length part <= String.length text
    && (String.sub text i (String.length part) = part || loop (i + 1))
  in
  loop 0

let check name condition = if not condition then failwith name

let () =
  if Array.length Sys.argv <> 2 then invalid_arg "usage: memo_probe MODEL.gguf";
  Eio_main.run @@ fun env ->
  let tmp = Filename.temp_dir "memo-model" "" in
  let fs = Eio.Stdenv.fs env in
  let dir = Eio.Path.(fs / tmp) in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Filename.quote_command "rm" [ "-rf"; tmp ])))
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      let engine =
        Ds4.V4.create ~sw ~cache:dir ~model:Eio.Path.(fs / Sys.argv.(1)) ()
      in
      let complete = Agentkit_ds4.complete engine ~ctx_size:16384 () in
      let open Crowthebot in
      let admin = "@admin:example.org" and room = "!memo:example.org" in
      let store =
        Store.create ~now:(fun () -> 0.) (Sqlite3_eio.open_memory ~sw ()) ~admin
      in
      Store.add_room store room;
      List.iteri
        (fun i body ->
          ignore
            (Store.add_fact store ~actor:admin ~room
               ~event:("$" ^ string_of_int i)
               ~source:"command" ~body))
        [
          "Alice preferred jasmine tea last year.";
          "Correction: Alice now prefers mint tea, replacing jasmine.";
        ];
      let config =
        {
          (Config.default ~admin ~homeserver:"https://matrix.example.org") with
          plugins = [];
          backend = Config.Ds4;
        }
      in
      let bot =
        Engine.create ~config ~store ~self:"@crow:example.org" ~plugins:[]
          ~complete ~now:(fun () -> 0.)
      in
      let replies = ref [] in
      Engine.handle bot
        ~send:(fun text -> replies := text :: !replies)
        {
          room;
          sender = admin;
          id = "$probe";
          body =
            "!crow ask Call memory_overview with {}. I want the bounded \
             hierarchical overview of all memory and its range keys. Then call \
             memory_get for ID 2 and state the corrected preference.";
        };
      let uses =
        Store.tool_uses store ~day:"1970-01-01" ~after:0 ~through:max_int
          ~limit:20
      in
      List.iter
        (fun (u : Store.tool_use) ->
          Printf.printf "Live tool %s status=%s\n" u.tool u.status)
        uses;
      List.iter (fun text -> Printf.printf "Live reply: %s\n" text) !replies;
      List.iter
        (fun name ->
          check ("model invoked " ^ name)
            (List.exists
               (fun (u : Store.tool_use) -> u.tool = name && u.status = "ok")
               uses))
        [ "memory_overview"; "memory_get" ];
      let _, summaries = Store.memory_tree store ~actor:admin in
      check "actual model generated bounded summary"
        (List.length summaries = 1
        && List.for_all
             (fun (_, text) ->
               String.length text <= 512 && contains text "mint")
             summaries);
      check "actual model used corrected memory"
        (List.exists (fun text -> contains text "mint") !replies);
      let matrix =
        Matrix_rooms.create ~store ~self:"@crow:example.org"
          ~state:(fun () -> None)
          ()
      in
      let tool =
        List.find
          (fun t -> Agentkit.Agent.Tool.name t = "matrix_send")
          (Matrix_rooms.tools matrix)
      in
      let posted = ref None in
      let dispatch (call : Agentkit.Agent.tool_call) =
        let codec =
          Jsont.Object.map Fun.id
          |> Jsont.Object.mem "text" Jsont.string ~enc:Fun.id
          |> Jsont.Object.finish
        in
        match Jsont_bytesrw.decode_string codec call.arguments with
        | Error error -> Error error
        | Ok text ->
            posted := Some text;
            Ok "Sent to the synthetic room."
      in
      let guard call =
        if call.Agentkit.Agent.name = "matrix_send" then Ok ()
        else Error "Only the synthetic posting tool is available."
      in
      let tools = Agentkit.Turn.bind ~guard ~dispatch [ tool ] in
      ignore
        (Agentkit.Turn.run ~complete ~tools ~guard ~dispatch
           [
             Agentkit.Chat.System
               (config.system_prompt ^ Matrix_rooms.system_prompt);
             Agentkit.Chat.User
               "This is a DM from avsm. Post https://example.org/paper in \
                !research:example.org as a new paper for discussion. The \
                requester belongs to that room. Use matrix_send.";
           ]);
      let text = Option.get !posted in
      check "shared post is succinct and has no requester narration"
        (String.length text <= 250
        && contains text "https://example.org/paper"
        && (not (contains text "avsm"))
        && (not (contains text "requested"))
        && (not (contains text "on behalf"))
        && not (contains text "dm"));
      Printf.printf "Live synthetic shared-room post: %s\n" text;
      print_endline
        "Live model memory, correction and shared-room style passed.")
