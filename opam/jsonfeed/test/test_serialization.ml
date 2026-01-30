(** Simple test to demonstrate JSON serialization works *)

open Jsonfeed

let () =
  (* Create a simple feed *)
  let author = Author.create ~name:"Test Author" () in
  let item =
    Item.create ~id:"https://example.com/1" ~title:"Test Item"
      ~content:(`Html "<p>Hello, world!</p>") ()
  in

  let feed =
    Jsonfeed.create ~title:"Test Feed" ~home_page_url:"https://example.com"
      ~authors:[ author ] ~items:[ item ] ()
  in

  (* Serialize to JSON *)
  let json =
    match Jsonfeed.to_string feed with
    | Ok s -> s
    | Error e -> failwith (Jsont.Error.to_string e)
  in

  (* Print it *)
  Printf.printf "Generated JSON Feed:\n%s\n\n" json;

  (* Validate *)
  match Jsonfeed.validate feed with
  | Ok () -> Printf.printf "✓ Feed is valid\n"
  | Error errors ->
      Printf.printf "✗ Feed has errors:\n";
      List.iter (Printf.printf "  - %s\n") errors
