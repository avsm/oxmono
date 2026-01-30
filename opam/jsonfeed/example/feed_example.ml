(** Example: Creating and serializing a JSON Feed

    This demonstrates:
    - Creating authors
    - Creating items with different content types
    - Creating a complete feed
    - Serializing to JSON string and file *)

open Jsonfeed

(* Helper to write feed to output channel *)
let to_file filename feed =
  match Jsonfeed.to_string feed with
  | Ok s ->
      Out_channel.with_open_gen
        [ Open_wronly; Open_creat; Open_trunc; Open_text ] 0o644 filename
        (fun oc -> Out_channel.output_string oc s)
  | Error e ->
      Printf.eprintf "Error encoding feed: %s\n" (Jsont.Error.to_string e);
      exit 1

let create_blog_feed () =
  (* Create some authors *)
  let jane =
    Author.create ~name:"Jane Doe" ~url:"https://example.com/authors/jane"
      ~avatar:"https://example.com/avatars/jane.png" ()
  in

  let john =
    Author.create ~name:"John Smith" ~url:"https://example.com/authors/john" ()
  in

  (* Create items with different content types *)
  let item1 =
    Item.create ~id:"https://example.com/posts/1"
      ~url:"https://example.com/posts/1" ~title:"Introduction to OCaml"
      ~content:
        (`Both
           ( "<p>OCaml is a powerful functional programming language.</p>",
             "OCaml is a powerful functional programming language." ))
      ~date_published:
        (Jsonfeed.Rfc3339.parse "2024-11-01T10:00:00Z" |> Option.get)
      ~date_modified:
        (Jsonfeed.Rfc3339.parse "2024-11-01T15:30:00Z" |> Option.get)
      ~authors:[ jane ]
      ~tags:[ "ocaml"; "programming"; "functional" ]
      ~summary:"A beginner's guide to OCaml programming" ()
  in

  let item2 =
    Item.create ~id:"https://example.com/posts/2"
      ~url:"https://example.com/posts/2" ~title:"JSON Feed for Syndication"
      ~content:
        (`Html "<p>JSON Feed is a modern alternative to RSS and Atom.</p>")
      ~date_published:
        (Jsonfeed.Rfc3339.parse "2024-11-02T09:00:00Z" |> Option.get)
      ~authors:[ jane; john ]
      ~tags:[ "json"; "syndication"; "web" ]
      ~image:"https://example.com/images/jsonfeed.png" ()
  in

  (* Microblog-style item (text only, no title) *)
  let item3 =
    Item.create ~id:"https://example.com/micro/42"
      ~content:(`Text "Just shipped a new feature! 🚀")
      ~date_published:
        (Jsonfeed.Rfc3339.parse "2024-11-03T08:15:00Z" |> Option.get)
      ~tags:[ "microblog" ] ()
  in

  (* Create the complete feed *)
  let feed =
    Jsonfeed.create ~title:"Example Blog" ~home_page_url:"https://example.com"
      ~feed_url:"https://example.com/feed.json"
      ~description:"A blog about programming, web development, and technology"
      ~icon:"https://example.com/icon-512.png"
      ~favicon:"https://example.com/favicon-64.png" ~authors:[ jane; john ]
      ~language:"en-US" ~items:[ item1; item2; item3 ] ()
  in

  feed

let create_podcast_feed () =
  (* Create podcast author *)
  let host =
    Author.create ~name:"Podcast Host" ~url:"https://podcast.example.com/host"
      ~avatar:"https://podcast.example.com/host-avatar.jpg" ()
  in

  (* Create episode with audio attachment *)
  let attachment =
    Attachment.create ~url:"https://podcast.example.com/episodes/ep1.mp3"
      ~mime_type:"audio/mpeg" ~title:"Episode 1: Introduction"
      ~size_in_bytes:15_728_640L ~duration_in_seconds:1800 ()
  in

  let episode =
    Item.create ~id:"https://podcast.example.com/episodes/1"
      ~url:"https://podcast.example.com/episodes/1"
      ~title:"Episode 1: Introduction"
      ~content:(`Html "<p>Welcome to our first episode!</p>")
      ~date_published:
        (Jsonfeed.Rfc3339.parse "2024-11-01T12:00:00Z" |> Option.get)
      ~attachments:[ attachment ] ~authors:[ host ]
      ~image:"https://podcast.example.com/episodes/ep1-cover.jpg" ()
  in

  (* Create podcast feed with hub for real-time updates *)
  let hub =
    Hub.create ~type_:"WebSub" ~url:"https://pubsubhubbub.appspot.com/" ()
  in

  let feed =
    Jsonfeed.create ~title:"Example Podcast"
      ~home_page_url:"https://podcast.example.com"
      ~feed_url:"https://podcast.example.com/feed.json"
      ~description:"A podcast about interesting topics"
      ~icon:"https://podcast.example.com/icon.png" ~authors:[ host ]
      ~language:"en-US" ~hubs:[ hub ] ~items:[ episode ] ()
  in

  feed

let main () =
  (* Create blog feed *)
  let blog_feed = create_blog_feed () in
  Format.printf "Created blog feed: %a\n\n" Jsonfeed.pp blog_feed;

  (* Serialize to string *)
  (match Jsonfeed.to_string blog_feed with
  | Ok json_string ->
      Format.printf "JSON (first 200 chars): %s...\n\n"
        (String.sub json_string 0 (min 200 (String.length json_string)))
  | Error e ->
      Printf.eprintf "Error serializing to string: %s\n"
        (Jsont.Error.to_string e);
      exit 1);

  (* Serialize to file *)
  to_file "blog-feed.json" blog_feed;
  Format.printf "Wrote blog feed to blog-feed.json\n\n";

  (* Create podcast feed *)
  let podcast_feed = create_podcast_feed () in
  Format.printf "Created podcast feed: %a\n\n" Jsonfeed.pp_summary podcast_feed;

  (* Validate feeds *)
  (match Jsonfeed.validate blog_feed with
  | Ok () -> Format.printf "✓ Blog feed is valid\n"
  | Error errors ->
      Format.printf "✗ Blog feed validation errors:\n";
      List.iter (Format.printf "  - %s\n") errors);

  match Jsonfeed.validate podcast_feed with
  | Ok () -> Format.printf "✓ Podcast feed is valid\n"
  | Error errors ->
      Format.printf "✗ Podcast feed validation errors:\n";
      List.iter (Format.printf "  - %s\n") errors

let () = main ()
