(** Tests for jsonfeed library *)

open Jsonfeed

(* Author tests *)

let test_author_create_with_name () =
  let author = Author.create ~name:"Jane Doe" () in
  Alcotest.(check (option string)) "name" (Some "Jane Doe") (Author.name author);
  Alcotest.(check (option string)) "url" None (Author.url author);
  Alcotest.(check (option string)) "avatar" None (Author.avatar author);
  Alcotest.(check bool) "is_valid" true (Author.is_valid author)

let test_author_create_with_url () =
  let author = Author.create ~url:"https://example.com" () in
  Alcotest.(check (option string)) "name" None (Author.name author);
  Alcotest.(check (option string))
    "url" (Some "https://example.com") (Author.url author);
  Alcotest.(check bool) "is_valid" true (Author.is_valid author)

let test_author_create_with_all_fields () =
  let author =
    Author.create ~name:"Jane Doe" ~url:"https://example.com"
      ~avatar:"https://example.com/avatar.png" ()
  in
  Alcotest.(check (option string)) "name" (Some "Jane Doe") (Author.name author);
  Alcotest.(check (option string))
    "url" (Some "https://example.com") (Author.url author);
  Alcotest.(check (option string))
    "avatar" (Some "https://example.com/avatar.png") (Author.avatar author);
  Alcotest.(check bool) "is_valid" true (Author.is_valid author)

let test_author_create_no_fields_fails () =
  Alcotest.check_raises "no fields"
    (Invalid_argument
       "Author.create: at least one field (name, url, or avatar) must be \
        provided") (fun () -> ignore (Author.create ()))

let test_author_equal () =
  let a1 = Author.create ~name:"Jane Doe" () in
  let a2 = Author.create ~name:"Jane Doe" () in
  let a3 = Author.create ~name:"John Smith" () in
  Alcotest.(check bool) "equal same" true (Author.equal a1 a2);
  Alcotest.(check bool) "equal different" false (Author.equal a1 a3)

let test_author_pp () =
  let author = Author.create ~name:"Jane Doe" ~url:"https://example.com" () in
  let s = Format.asprintf "%a" Author.pp author in
  Alcotest.(check string)
    "pp with name and url" "Jane Doe <https://example.com>" s

let author_tests =
  [
    ("create with name", `Quick, test_author_create_with_name);
    ("create with url", `Quick, test_author_create_with_url);
    ("create with all fields", `Quick, test_author_create_with_all_fields);
    ("create with no fields fails", `Quick, test_author_create_no_fields_fails);
    ("equal", `Quick, test_author_equal);
    ("pp", `Quick, test_author_pp);
  ]

(* Attachment tests *)

let test_attachment_create_minimal () =
  let att =
    Attachment.create ~url:"https://example.com/file.mp3"
      ~mime_type:"audio/mpeg" ()
  in
  Alcotest.(check string)
    "url" "https://example.com/file.mp3" (Attachment.url att);
  Alcotest.(check string) "mime_type" "audio/mpeg" (Attachment.mime_type att);
  Alcotest.(check (option string)) "title" None (Attachment.title att);
  Alcotest.(check (option int64))
    "size_in_bytes" None
    (Attachment.size_in_bytes att);
  Alcotest.(check (option int))
    "duration_in_seconds" None
    (Attachment.duration_in_seconds att)

let test_attachment_create_complete () =
  let att =
    Attachment.create ~url:"https://example.com/episode.mp3"
      ~mime_type:"audio/mpeg" ~title:"Episode 1" ~size_in_bytes:15_728_640L
      ~duration_in_seconds:1800 ()
  in
  Alcotest.(check string)
    "url" "https://example.com/episode.mp3" (Attachment.url att);
  Alcotest.(check string) "mime_type" "audio/mpeg" (Attachment.mime_type att);
  Alcotest.(check (option string))
    "title" (Some "Episode 1") (Attachment.title att);
  Alcotest.(check (option int64))
    "size_in_bytes" (Some 15_728_640L)
    (Attachment.size_in_bytes att);
  Alcotest.(check (option int))
    "duration_in_seconds" (Some 1800)
    (Attachment.duration_in_seconds att)

let test_attachment_equal () =
  let a1 =
    Attachment.create ~url:"https://example.com/file.mp3"
      ~mime_type:"audio/mpeg" ()
  in
  let a2 =
    Attachment.create ~url:"https://example.com/file.mp3"
      ~mime_type:"audio/mpeg" ()
  in
  let a3 =
    Attachment.create ~url:"https://example.com/other.mp3"
      ~mime_type:"audio/mpeg" ()
  in
  Alcotest.(check bool) "equal same" true (Attachment.equal a1 a2);
  Alcotest.(check bool) "equal different" false (Attachment.equal a1 a3)

let attachment_tests =
  [
    ("create minimal", `Quick, test_attachment_create_minimal);
    ("create complete", `Quick, test_attachment_create_complete);
    ("equal", `Quick, test_attachment_equal);
  ]

(* Hub tests *)

let test_hub_create () =
  let hub = Hub.create ~type_:"WebSub" ~url:"https://example.com/hub" () in
  Alcotest.(check string) "type_" "WebSub" (Hub.type_ hub);
  Alcotest.(check string) "url" "https://example.com/hub" (Hub.url hub)

let test_hub_equal () =
  let h1 = Hub.create ~type_:"WebSub" ~url:"https://example.com/hub" () in
  let h2 = Hub.create ~type_:"WebSub" ~url:"https://example.com/hub" () in
  let h3 = Hub.create ~type_:"rssCloud" ~url:"https://example.com/hub" () in
  Alcotest.(check bool) "equal same" true (Hub.equal h1 h2);
  Alcotest.(check bool) "equal different" false (Hub.equal h1 h3)

let hub_tests =
  [ ("create", `Quick, test_hub_create); ("equal", `Quick, test_hub_equal) ]

(* Item tests *)

let test_item_create_html () =
  let item =
    Item.create ~id:"https://example.com/1" ~content:(`Html "<p>Hello</p>") ()
  in
  Alcotest.(check string) "id" "https://example.com/1" (Item.id item);
  Alcotest.(check (option string))
    "content_html" (Some "<p>Hello</p>") (Item.content_html item);
  Alcotest.(check (option string)) "content_text" None (Item.content_text item)

let test_item_create_text () =
  let item =
    Item.create ~id:"https://example.com/2" ~content:(`Text "Hello world") ()
  in
  Alcotest.(check string) "id" "https://example.com/2" (Item.id item);
  Alcotest.(check (option string)) "content_html" None (Item.content_html item);
  Alcotest.(check (option string))
    "content_text" (Some "Hello world") (Item.content_text item)

let test_item_create_both () =
  let item =
    Item.create ~id:"https://example.com/3"
      ~content:(`Both ("<p>Hello</p>", "Hello"))
      ()
  in
  Alcotest.(check string) "id" "https://example.com/3" (Item.id item);
  Alcotest.(check (option string))
    "content_html" (Some "<p>Hello</p>") (Item.content_html item);
  Alcotest.(check (option string))
    "content_text" (Some "Hello") (Item.content_text item)

let test_item_with_metadata () =
  let item =
    Item.create ~id:"https://example.com/4" ~content:(`Html "<p>Test</p>")
      ~title:"Test Post" ~url:"https://example.com/posts/4"
      ~tags:[ "test"; "example" ] ()
  in
  Alcotest.(check (option string)) "title" (Some "Test Post") (Item.title item);
  Alcotest.(check (option string))
    "url" (Some "https://example.com/posts/4") (Item.url item);
  Alcotest.(check (option (list string)))
    "tags"
    (Some [ "test"; "example" ])
    (Item.tags item)

let test_item_equal () =
  let i1 = Item.create ~id:"https://example.com/1" ~content:(`Text "test") () in
  let i2 =
    Item.create ~id:"https://example.com/1" ~content:(`Html "<p>test</p>") ()
  in
  let i3 = Item.create ~id:"https://example.com/2" ~content:(`Text "test") () in
  Alcotest.(check bool) "equal same id" true (Item.equal i1 i2);
  Alcotest.(check bool) "equal different id" false (Item.equal i1 i3)

let item_tests =
  [
    ("create with HTML content", `Quick, test_item_create_html);
    ("create with text content", `Quick, test_item_create_text);
    ("create with both contents", `Quick, test_item_create_both);
    ("create with metadata", `Quick, test_item_with_metadata);
    ("equal", `Quick, test_item_equal);
  ]

(* Jsonfeed tests *)

let test_feed_create_minimal () =
  let feed = Jsonfeed.create ~title:"Test Feed" ~items:[] () in
  Alcotest.(check string) "title" "Test Feed" (Jsonfeed.title feed);
  Alcotest.(check string)
    "version" "https://jsonfeed.org/version/1.1" (Jsonfeed.version feed);
  Alcotest.(check int) "items length" 0 (List.length (Jsonfeed.items feed))

let test_feed_create_with_items () =
  let item =
    Item.create ~id:"https://example.com/1" ~content:(`Text "Hello") ()
  in
  let feed = Jsonfeed.create ~title:"Test Feed" ~items:[ item ] () in
  Alcotest.(check int) "items length" 1 (List.length (Jsonfeed.items feed))

let test_feed_validate_valid () =
  let feed = Jsonfeed.create ~title:"Test" ~items:[] () in
  match Jsonfeed.validate feed with
  | Ok () -> ()
  | Error errors ->
      Alcotest.fail
        (Printf.sprintf "Validation should succeed: %s"
           (String.concat "; " errors))

let test_feed_validate_empty_title () =
  let feed = Jsonfeed.create ~title:"" ~items:[] () in
  match Jsonfeed.validate feed with
  | Ok () -> Alcotest.fail "Should fail validation"
  | Error errors ->
      Alcotest.(check bool)
        "has error" true
        (List.exists (fun s -> String.starts_with ~prefix:"title" s) errors)

let contains_substring s sub =
  try
    let _ = Str.search_forward (Str.regexp_string sub) s 0 in
    true
  with Not_found -> false

let test_feed_to_string () =
  let feed = Jsonfeed.create ~title:"Test Feed" ~items:[] () in
  match Jsonfeed.to_string feed with
  | Ok json ->
      Alcotest.(check bool)
        "contains version" true
        (contains_substring json "version");
      Alcotest.(check bool)
        "contains title" true
        (contains_substring json "Test Feed")
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "Serialization failed: %s" (Jsont.Error.to_string e))

let test_feed_parse_minimal () =
  let json =
    {|{
    "version": "https://jsonfeed.org/version/1.1",
    "title": "Test Feed",
    "items": []
  }|}
  in
  match Jsonfeed.of_string json with
  | Ok feed ->
      Alcotest.(check string) "title" "Test Feed" (Jsonfeed.title feed);
      Alcotest.(check int) "items" 0 (List.length (Jsonfeed.items feed))
  | Error err ->
      Alcotest.fail
        (Printf.sprintf "Parse failed: %s" (Jsont.Error.to_string err))

let test_feed_parse_with_item () =
  let json =
    {|{
    "version": "https://jsonfeed.org/version/1.1",
    "title": "Test Feed",
    "items": [
      {
        "id": "https://example.com/1",
        "content_html": "<p>Hello</p>"
      }
    ]
  }|}
  in
  match Jsonfeed.of_string json with
  | Ok feed -> (
      let items = Jsonfeed.items feed in
      Alcotest.(check int) "items count" 1 (List.length items);
      match items with
      | [ item ] ->
          Alcotest.(check string)
            "item id" "https://example.com/1" (Item.id item);
          Alcotest.(check (option string))
            "content_html" (Some "<p>Hello</p>") (Item.content_html item)
      | _ -> Alcotest.fail "Expected 1 item")
  | Error err ->
      Alcotest.fail
        (Printf.sprintf "Parse failed: %s" (Jsont.Error.to_string err))

let test_feed_roundtrip () =
  let author = Author.create ~name:"Test Author" () in
  let item =
    Item.create ~id:"https://example.com/1" ~title:"Test Item"
      ~content:(`Html "<p>Hello, world!</p>")
      ~date_published:
        (Jsonfeed.Rfc3339.parse "2024-11-01T10:00:00Z" |> Option.get)
      ~tags:[ "test"; "example" ] ()
  in

  let feed1 =
    Jsonfeed.create ~title:"Test Feed" ~home_page_url:"https://example.com"
      ~authors:[ author ] ~items:[ item ] ()
  in

  (* Serialize and parse *)
  match Jsonfeed.to_string feed1 with
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "Serialization failed: %s" (Jsont.Error.to_string e))
  | Ok json -> (
      match Jsonfeed.of_string json with
      | Ok feed2 ->
          Alcotest.(check string)
            "title" (Jsonfeed.title feed1) (Jsonfeed.title feed2);
          Alcotest.(check (option string))
            "home_page_url"
            (Jsonfeed.home_page_url feed1)
            (Jsonfeed.home_page_url feed2);
          Alcotest.(check int)
            "items count"
            (List.length (Jsonfeed.items feed1))
            (List.length (Jsonfeed.items feed2))
      | Error err ->
          Alcotest.fail
            (Printf.sprintf "Round-trip parse failed: %s"
               (Jsont.Error.to_string err)))

let test_feed_parse_invalid_missing_content () =
  let json =
    {|{
    "version": "https://jsonfeed.org/version/1.1",
    "title": "Test",
    "items": [
      {
        "id": "1"
      }
    ]
  }|}
  in
  match Jsonfeed.of_string json with
  | Ok _ -> Alcotest.fail "Should reject item without content"
  | Error err ->
      let err_str = Jsont.Error.to_string err in
      Alcotest.(check bool)
        "has error" true
        (contains_substring err_str "content")

let jsonfeed_tests =
  [
    ("create minimal feed", `Quick, test_feed_create_minimal);
    ("create feed with items", `Quick, test_feed_create_with_items);
    ("validate valid feed", `Quick, test_feed_validate_valid);
    ("validate empty title", `Quick, test_feed_validate_empty_title);
    ("to_string", `Quick, test_feed_to_string);
    ("parse minimal feed", `Quick, test_feed_parse_minimal);
    ("parse feed with item", `Quick, test_feed_parse_with_item);
    ("round-trip", `Quick, test_feed_roundtrip);
    ( "parse invalid missing content",
      `Quick,
      test_feed_parse_invalid_missing_content );
  ]

(* Unknown fields preservation tests *)

let test_author_unknown_roundtrip () =
  let json =
    {|{
    "name": "Test Author",
    "custom_field": "custom value",
    "another_extension": 42
  }|}
  in
  match Jsont_bytesrw.decode_string' Author.jsont json with
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "Parse failed: %s" (Jsont.Error.to_string e))
  | Ok author -> (
      (* Check that unknown fields are preserved *)
      let unknown = Author.unknown author in
      Alcotest.(check bool)
        "has unknown fields" false
        (Jsonfeed.Unknown.is_empty unknown);
      (* Encode and decode again *)
      match Jsont_bytesrw.encode_string' Author.jsont author with
      | Error e ->
          Alcotest.fail
            (Printf.sprintf "Encode failed: %s" (Jsont.Error.to_string e))
      | Ok json2 -> (
          match Jsont_bytesrw.decode_string' Author.jsont json2 with
          | Error e ->
              Alcotest.fail
                (Printf.sprintf "Re-parse failed: %s" (Jsont.Error.to_string e))
          | Ok author2 ->
              (* Verify unknown fields survive roundtrip *)
              let unknown2 = Author.unknown author2 in
              Alcotest.(check bool)
                "unknown fields preserved" false
                (Jsonfeed.Unknown.is_empty unknown2)))

let test_item_unknown_roundtrip () =
  let json =
    {|{
    "id": "https://example.com/1",
    "content_html": "<p>Test</p>",
    "custom_metadata": "some custom data",
    "x_custom_number": 123.45
  }|}
  in
  match Jsont_bytesrw.decode_string' Item.jsont json with
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "Parse failed: %s" (Jsont.Error.to_string e))
  | Ok item -> (
      (* Check that unknown fields are preserved *)
      let unknown = Item.unknown item in
      Alcotest.(check bool)
        "has unknown fields" false
        (Jsonfeed.Unknown.is_empty unknown);
      (* Encode and decode again *)
      match Jsont_bytesrw.encode_string' Item.jsont item with
      | Error e ->
          Alcotest.fail
            (Printf.sprintf "Encode failed: %s" (Jsont.Error.to_string e))
      | Ok json2 -> (
          match Jsont_bytesrw.decode_string' Item.jsont json2 with
          | Error e ->
              Alcotest.fail
                (Printf.sprintf "Re-parse failed: %s" (Jsont.Error.to_string e))
          | Ok item2 ->
              let unknown2 = Item.unknown item2 in
              Alcotest.(check bool)
                "unknown fields preserved" false
                (Jsonfeed.Unknown.is_empty unknown2)))

let test_feed_unknown_roundtrip () =
  let json =
    {|{
    "version": "https://jsonfeed.org/version/1.1",
    "title": "Test Feed",
    "items": [],
    "custom_extension": "custom value",
    "x_another_field": {"nested": "data"}
  }|}
  in
  match Jsonfeed.of_string json with
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "Parse failed: %s" (Jsont.Error.to_string e))
  | Ok feed -> (
      (* Check that unknown fields are preserved *)
      let unknown = Jsonfeed.unknown feed in
      Alcotest.(check bool)
        "has unknown fields" false
        (Jsonfeed.Unknown.is_empty unknown);
      (* Encode and decode again *)
      match Jsonfeed.to_string feed with
      | Error e ->
          Alcotest.fail
            (Printf.sprintf "Encode failed: %s" (Jsont.Error.to_string e))
      | Ok json2 -> (
          match Jsonfeed.of_string json2 with
          | Error e ->
              Alcotest.fail
                (Printf.sprintf "Re-parse failed: %s" (Jsont.Error.to_string e))
          | Ok feed2 ->
              let unknown2 = Jsonfeed.unknown feed2 in
              Alcotest.(check bool)
                "unknown fields preserved" false
                (Jsonfeed.Unknown.is_empty unknown2)))

let test_hub_unknown_roundtrip () =
  let json =
    {|{
    "type": "WebSub",
    "url": "https://example.com/hub",
    "custom_field": "test"
  }|}
  in
  match Jsont_bytesrw.decode_string' Hub.jsont json with
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "Parse failed: %s" (Jsont.Error.to_string e))
  | Ok hub -> (
      let unknown = Hub.unknown hub in
      Alcotest.(check bool)
        "has unknown fields" false
        (Jsonfeed.Unknown.is_empty unknown);
      match Jsont_bytesrw.encode_string' Hub.jsont hub with
      | Error e ->
          Alcotest.fail
            (Printf.sprintf "Encode failed: %s" (Jsont.Error.to_string e))
      | Ok json2 -> (
          match Jsont_bytesrw.decode_string' Hub.jsont json2 with
          | Error e ->
              Alcotest.fail
                (Printf.sprintf "Re-parse failed: %s" (Jsont.Error.to_string e))
          | Ok hub2 ->
              let unknown2 = Hub.unknown hub2 in
              Alcotest.(check bool)
                "unknown fields preserved" false
                (Jsonfeed.Unknown.is_empty unknown2)))

let test_attachment_unknown_roundtrip () =
  let json =
    {|{
    "url": "https://example.com/file.mp3",
    "mime_type": "audio/mpeg",
    "x_custom": "value"
  }|}
  in
  match Jsont_bytesrw.decode_string' Attachment.jsont json with
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "Parse failed: %s" (Jsont.Error.to_string e))
  | Ok att -> (
      let unknown = Attachment.unknown att in
      Alcotest.(check bool)
        "has unknown fields" false
        (Jsonfeed.Unknown.is_empty unknown);
      match Jsont_bytesrw.encode_string' Attachment.jsont att with
      | Error e ->
          Alcotest.fail
            (Printf.sprintf "Encode failed: %s" (Jsont.Error.to_string e))
      | Ok json2 -> (
          match Jsont_bytesrw.decode_string' Attachment.jsont json2 with
          | Error e ->
              Alcotest.fail
                (Printf.sprintf "Re-parse failed: %s" (Jsont.Error.to_string e))
          | Ok att2 ->
              let unknown2 = Attachment.unknown att2 in
              Alcotest.(check bool)
                "unknown fields preserved" false
                (Jsonfeed.Unknown.is_empty unknown2)))

let unknown_fields_tests =
  [
    ("author unknown roundtrip", `Quick, test_author_unknown_roundtrip);
    ("item unknown roundtrip", `Quick, test_item_unknown_roundtrip);
    ("feed unknown roundtrip", `Quick, test_feed_unknown_roundtrip);
    ("hub unknown roundtrip", `Quick, test_hub_unknown_roundtrip);
    ("attachment unknown roundtrip", `Quick, test_attachment_unknown_roundtrip);
  ]

(* Main test suite *)

let () =
  Alcotest.run "jsonfeed"
    [
      ("Author", author_tests);
      ("Attachment", attachment_tests);
      ("Hub", hub_tests);
      ("Item", item_tests);
      ("Jsonfeed", jsonfeed_tests);
      ("Unknown Fields", unknown_fields_tests);
    ]
