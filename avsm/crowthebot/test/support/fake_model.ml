let v f (r : Agentkit.Chat.request) =
  let text, calls = f r.messages r.tools in
  Agentkit.Chat.response ~calls text
