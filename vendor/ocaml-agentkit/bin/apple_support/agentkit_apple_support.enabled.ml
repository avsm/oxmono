let available = true

let models () =
  [ { Agentkit.Driver.name = "default"; description = "Apple system model" } ]

let create = Agentkit_apple_tools.create
let context_size = Agentkit_apple_tools.context_size
