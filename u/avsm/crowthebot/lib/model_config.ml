type settings = { url : string; api_key : string; allow_http : bool }

let jsont =
  Jsont.Object.map (fun url api_key allow_http -> { url; api_key; allow_http })
  |> Jsont.Object.mem "url" Jsont.string ~enc:(fun s -> s.url)
  |> Jsont.Object.mem "api_key" Jsont.string ~enc:(fun s -> s.api_key)
  |> Jsont.Object.mem "allow_http" Jsont.bool ~enc:(fun s -> s.allow_http)
  |> Jsont.Object.finish

let validate s =
  ignore (Tool_config.endpoint ~allow_http:s.allow_http s.url);
  if
    s.api_key = ""
    || String.length s.api_key > 16384
    || String.exists (fun c -> Char.code c <= 32 || Char.code c = 127) s.api_key
  then invalid_arg "Invalid model API key."

let configuration =
  let open Cmdliner in
  Tool_config.v ~name:"openrouter" ~doc:"Named model endpoints and API keys."
    Term.(
      const (fun url api_key_file allow_http () ->
          ignore (Tool_config.endpoint ~allow_http url);
          let api_key =
            Tool_config.secret ~label:"Model API key" api_key_file
          in
          let s = { url; api_key; allow_http } in
          validate s;
          Tool_config.encode jsont s)
      $ Arg.(
          value
          & opt string "https://openrouter.ai/api/v1"
          & info [ "url" ] ~doc:"OpenRouter-compatible API base URL.")
      $ Arg.(
          value
          & opt (some string) None
          & info [ "api-key-file" ]
              ~doc:"0600 key file. Otherwise prompt without echo.")
      $ Arg.(
          value & flag
          & info [ "allow-http" ]
              ~doc:"Allow sending the key to this trusted HTTP endpoint."))

let initialize ~fetch json =
  let s = Tool_config.decode jsont json in
  validate s;
  let fetch = Fetch.restrict ~under:[ s.url ] ~methods:[ `POST ] fetch in
  (* Openrouter intentionally refuses bearer credentials on HTTP. The explicit
     operator setting permits this one endpoint through Fetch instead. *)
  let fetch =
    Fetch.with_credentials ~scope:[ s.url ] ~allow_insecure:s.allow_http
      [ Fetch.Credential.bearer s.api_key ]
      fetch
  in
  Openrouter.of_fetch ~base_url:s.url ~max_response_bytes:(1024 * 1024) fetch
