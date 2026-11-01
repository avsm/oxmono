let registry ~ds4 ~apple ~openrouter =
  Agentkit.Driver.merge [ ds4; apple; openrouter ]
