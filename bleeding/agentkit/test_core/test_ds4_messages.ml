module Chat = Agentkit.Chat

let () =
  List.iter
    (fun dialect ->
      let history =
        [
          Chat.System "rules";
          Chat.User "question";
          Chat.Assistant
            {
              text = "looking";
              calls =
                [ { Agentkit.Agent.id = "a"; name = "read"; arguments = "{}" } ];
            };
          Chat.Tool_result { id = "a"; content = "answer" };
        ]
      in
      let actual = Agentkit_ds4.messages ~dialect ~tools:[] history in
      let expected =
        [
          Dsml.system "rules";
          Dsml.user "question";
          Dsml.assistant ~content:"looking"
            ~tool_calls:
              [ Dsml.tool_call ~id:"a" ~name:"read" ~arguments:"{}" () ]
            ();
          Dsml.tool ~id:"a" "answer";
        ]
      in
      assert (
        Dsml.encode_messages ~dialect Dsml.Chat actual
        = Dsml.encode_messages ~dialect Dsml.Chat expected))
    [ Dsml.Deepseek; Dsml.Deepseek41; Dsml.Glm; Dsml.Qwen ];
  assert (Agentkit_ds4.reasoning (Some "high") = (Dsml.Thinking, Some Dsml.High));
  assert (Agentkit_ds4.reasoning (Some "max") = (Dsml.Thinking, Some Dsml.Max));
  (match Agentkit_ds4.reasoning (Some "unsupported") with
  | exception Invalid_argument _ -> ()
  | _ -> assert false);
  let image = { Chat.format = Chat.Png; data = "bytes" } in
  (match
     Agentkit_ds4.messages ~dialect:Dsml.Deepseek41 ~tools:[]
       [ Chat.User_images { text = "describe"; images = [ image ] } ]
   with
  | exception Invalid_argument _ -> ()
  | _ -> assert false);
  let call = Dsml.tool_call ~id:"kept" ~name:"read" ~arguments:"{}" () in
  match Agentkit_ds4.event (Ds4.Agent.Tool_call call) with
  | Agentkit.Agent.Tool_call { id = "kept"; _ } -> ()
  | _ -> assert false
