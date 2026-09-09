type t = { name : string; description : string; run : query:string -> string }

let with_workspace ~sw ~profile_dir build =
  let profile_dir = Eio.Path.open_subtree ~sw profile_dir in
  let path = Eio.Path.(profile_dir / "workspace") in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 path;
  let stat = Eio.Path.stat ~follow:false path in
  if stat.kind <> `Directory || stat.perm land 0o077 <> 0 then
    invalid_arg "tool workspace must be a private directory, not a symlink";
  build (Eio.Path.open_subtree ~sw path)

let clip ~bytes text =
  if String.length text <= bytes then text
  else begin
    let last = ref bytes in
    while !last > 0 && Char.code text.[!last] land 0xc0 = 0x80 do
      decr last
    done;
    String.sub text 0 !last ^ "\n[truncated]"
  end

let query_jsont =
  Jsont.Object.map ~kind:"tool arguments" Fun.id
  |> Jsont.Object.mem "query" Jsont.string ~enc:Fun.id ~dec_absent:(fun () ->
      "")
  |> Jsont.Object.finish

let parameters =
  match
    Jsont_bytesrw.decode_string Jsont.json
      {|{"type":"object","properties":{"query":{"type":"string","description":"Tool-specific query, or empty for its default operation"}},"additionalProperties":false}|}
  with
  | Ok json -> json
  | Error message -> failwith message

let tool t =
  Openrouter.Tool.v ~name:t.name ~description:t.description ~parameters ()

let invoke_result t arguments =
  match Jsont_bytesrw.decode_string query_jsont arguments with
  | Error _ ->
      Error "Invalid tool arguments: expected an object with string query."
  | Ok query when String.length query > 256 -> Error "Query is too long."
  | Ok query -> Ok (clip ~bytes:4096 (t.run ~query))

let invoke t arguments =
  match invoke_result t arguments with Ok result | Error result -> result
