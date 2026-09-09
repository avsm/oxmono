val configuration : Tool_config.t

val initialize : fetch:Fetch.plain -> Jsont.json -> Openrouter.t
(** [initialize ~fetch settings] binds a key to its configured model endpoint.
    The returned client has no configuration store capability. *)
