type t = { name : string; description : string; run : query:string -> string }

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
      {|{"type":"object","properties":{"query":{"type":"string","description":"Name or URL substring, or empty to list feeds"}},"additionalProperties":false}|}
  with
  | Ok json -> json
  | Error message -> failwith message

let tool t =
  Openrouter.Tool.v ~name:t.name ~description:t.description ~parameters ()

let invoke t arguments =
  match Jsont_bytesrw.decode_string query_jsont arguments with
  | Error _ -> "Invalid tool arguments: expected an object with string query."
  | Ok query when String.length query > 256 -> "Query is too long."
  | Ok query -> clip ~bytes:4096 (t.run ~query)

let blogroll_url = "https://anil.recoil.org/network/blogroll.opml"

let contains text query =
  let text = String.lowercase_ascii text
  and query = String.lowercase_ascii query in
  let rec loop i =
    i + String.length query <= String.length text
    && (String.sub text i (String.length query) = query || loop (i + 1))
  in
  loop 0

let blogroll ~fetch ~now =
  let cache = ref None in
  let fetch = Fetch.restrict ~under:[ blogroll_url ] ~methods:[ `GET ] fetch in
  let load () =
    match !cache with
    | Some (until, feeds) when now () < until -> feeds
    | _ ->
        let source =
          Fetch.with_response ~redirects:0 fetch `GET blogroll_url
            (fun response ->
              if Fetch.status response <> 200 then
                failwith "blogroll HTTP request failed";
              Eio.Buf_read.parse_exn
                ~max_size:((2 * 1024 * 1024) + 1)
                Eio.Buf_read.take_all (Fetch.body response))
        in
        let feeds =
          match Sortal_feed.Opml.decode source with
          | Ok document -> document.feeds
          | Error message -> failwith message
        in
        cache := Some (now () +. 3600., feeds);
        feeds
  in
  {
    name = "blogroll";
    description =
      "Read Anil's public OPML blogroll. Search by name or URL. Returns feed \
       subscriptions, not article contents.";
    run =
      (fun ~query ->
        let feeds =
          load ()
          |> List.filter (fun (f : Sortal_feed.Opml.feed) ->
              contains f.title query || contains f.xml_url query)
        in
        let rec take n = function
          | [] -> []
          | _ when n = 0 -> []
          | x :: xs -> x :: take (n - 1) xs
        in
        let lines =
          take 20 feeds
          |> List.map (fun (f : Sortal_feed.Opml.feed) ->
              f.title ^ "\n" ^ f.xml_url)
        in
        clip ~bytes:4096
          (Printf.sprintf "%d matching feeds. Showing at most 20.\n%s"
             (List.length feeds)
             (String.concat "\n\n" lines)));
  }
