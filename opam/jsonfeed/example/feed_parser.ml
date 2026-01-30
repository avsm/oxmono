(** Example: Parsing and analyzing JSON Feeds

    This demonstrates:
    - Parsing feeds from files
    - Analyzing feed metadata
    - Iterating over items
    - Working with dates and content *)

open Jsonfeed

(* Helper to read feed from file *)
let of_file filename =
  let content = In_channel.with_open_text filename In_channel.input_all in
  Jsonfeed.of_string content

let print_feed_info feed =
  Format.printf "Feed Information:\n";
  Format.printf "  Title: %s\n" (Jsonfeed.title feed);
  Format.printf "  Version: %s\n" (Jsonfeed.version feed);

  (match Jsonfeed.home_page_url feed with
  | Some url -> Format.printf "  Home Page: %s\n" url
  | None -> ());

  (match Jsonfeed.feed_url feed with
  | Some url -> Format.printf "  Feed URL: %s\n" url
  | None -> ());

  (match Jsonfeed.description feed with
  | Some desc -> Format.printf "  Description: %s\n" desc
  | None -> ());

  (match Jsonfeed.language feed with
  | Some lang -> Format.printf "  Language: %s\n" lang
  | None -> ());

  (match Jsonfeed.authors feed with
  | Some authors ->
      Format.printf "  Authors:\n";
      List.iter
        (fun author ->
          match Author.name author with
          | Some name ->
              Format.printf "    - %s" name;
              (match Author.url author with
              | Some url -> Format.printf " (%s)" url
              | None -> ());
              Format.printf "\n"
          | None -> ())
        authors
  | None -> ());

  Format.printf "  Items: %d\n\n" (List.length (Jsonfeed.items feed))

let print_item_details item =
  Format.printf "Item: %s\n" (Item.id item);

  (match Item.title item with
  | Some title -> Format.printf "  Title: %s\n" title
  | None -> Format.printf "  (No title - microblog entry)\n");

  (match Item.url item with
  | Some url -> Format.printf "  URL: %s\n" url
  | None -> ());

  (* Print content info *)
  (match Item.content item with
  | `Html html ->
      Format.printf "  Content: HTML only (%d chars)\n" (String.length html)
  | `Text text ->
      Format.printf "  Content: Text only (%d chars)\n" (String.length text)
  | `Both (html, text) ->
      Format.printf "  Content: Both HTML (%d chars) and Text (%d chars)\n"
        (String.length html) (String.length text));

  (* Print dates *)
  (match Item.date_published item with
  | Some date ->
      Format.printf "  Published: %s\n" (Jsonfeed.Rfc3339.format date)
  | None -> ());

  (match Item.date_modified item with
  | Some date -> Format.printf "  Modified: %s\n" (Jsonfeed.Rfc3339.format date)
  | None -> ());

  (* Print tags *)
  (match Item.tags item with
  | Some tags when tags <> [] ->
      Format.printf "  Tags: %s\n" (String.concat ", " tags)
  | _ -> ());

  (* Print attachments *)
  (match Item.attachments item with
  | Some attachments when attachments <> [] ->
      Format.printf "  Attachments:\n";
      List.iter
        (fun att ->
          Format.printf "    - %s (%s)\n" (Attachment.url att)
            (Attachment.mime_type att);
          (match Attachment.size_in_bytes att with
          | Some size ->
              let mb = Int64.to_float size /. (1024. *. 1024.) in
              Format.printf "      Size: %.2f MB\n" mb
          | None -> ());
          match Attachment.duration_in_seconds att with
          | Some duration ->
              let mins = duration / 60 in
              let secs = duration mod 60 in
              Format.printf "      Duration: %dm%ds\n" mins secs
          | None -> ())
        attachments
  | _ -> ());

  Format.printf "\n"

let analyze_feed feed =
  let items = Jsonfeed.items feed in

  Format.printf "\n=== Feed Analysis ===\n\n";

  (* Count content types *)
  let html_only = ref 0 in
  let text_only = ref 0 in
  let both = ref 0 in

  List.iter
    (fun item ->
      match Item.content item with
      | `Html _ -> incr html_only
      | `Text _ -> incr text_only
      | `Both _ -> incr both)
    items;

  Format.printf "Content Types:\n";
  Format.printf "  HTML only: %d\n" !html_only;
  Format.printf "  Text only: %d\n" !text_only;
  Format.printf "  Both: %d\n\n" !both;

  (* Find items with attachments *)
  let with_attachments =
    List.filter
      (fun item ->
        match Item.attachments item with
        | Some att when att <> [] -> true
        | _ -> false)
      items
  in

  Format.printf "Items with attachments: %d\n\n" (List.length with_attachments);

  (* Collect all unique tags *)
  let all_tags =
    List.fold_left
      (fun acc item ->
        match Item.tags item with Some tags -> acc @ tags | None -> acc)
      [] items
  in
  let unique_tags = List.sort_uniq String.compare all_tags in

  if unique_tags <> [] then
    Format.printf "All tags used: %s\n\n" (String.concat ", " unique_tags)

let main () =
  (* Parse from example_feed.json file *)
  Format.printf "=== Parsing JSON Feed from example_feed.json ===\n\n";

  try
    match of_file "example/example_feed.json" with
    | Ok feed -> (
        print_feed_info feed;

        Format.printf "=== Items ===\n\n";
        List.iter print_item_details (Jsonfeed.items feed);

        analyze_feed feed;

        (* Demonstrate round-trip parsing *)
        Format.printf "\n=== Round-trip Test ===\n\n";
        match Jsonfeed.to_string feed with
        | Error e ->
            Printf.eprintf "Error serializing feed: %s\n"
              (Jsont.Error.to_string e);
            exit 1
        | Ok json -> (
            match Jsonfeed.of_string json with
            | Ok feed2 ->
                if Jsonfeed.equal feed feed2 then
                  Format.printf "✓ Round-trip successful: feeds are equal\n"
                else Format.printf "✗ Round-trip failed: feeds differ\n"
            | Error err ->
                Format.eprintf "✗ Round-trip failed: %s\n"
                  (Jsont.Error.to_string err)))
    | Error err ->
        Format.eprintf "Error parsing feed: %s\n" (Jsont.Error.to_string err)
  with Sys_error msg -> Format.eprintf "Error reading file: %s\n" msg

let () = main ()
