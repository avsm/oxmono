(** Test executable for verifying jsont location tracking

    Usage: test_location_errors <file> [field]

    Parses JSON feed files and outputs JSON with either:
    - Success: {"status":"ok", "field":"<field>", "value":"<value>"}
    - Error: {"status":"error", "message":"...", "location":{...}, "context":"..."}
*)

open Jsonfeed

(* Helper to format path context *)
let format_context (ctx : Jsont.Error.Context.t) =
  if Jsont.Error.Context.is_empty ctx then "$"
  else
    let indices = ctx in
    let rec format_path acc = function
      | [] -> if acc = "" then "$" else "$" ^ acc
      | ((_kinded_sort, _meta), idx) :: rest ->
          let segment =
            match idx with
            | Jsont.Path.Mem (name, _meta) -> "." ^ name
            | Jsont.Path.Nth (n, _meta) -> "[" ^ string_of_int n ^ "]"
          in
          format_path (acc ^ segment) rest
    in
    format_path "" indices

(* Extract field from successfully parsed feed *)
let extract_field field feed =
  match field with
  | "title" -> Jsonfeed.title feed
  | "version" -> Jsonfeed.version feed
  | "item_count" -> string_of_int (List.length (Jsonfeed.items feed))
  | "first_item_id" -> (
      match Jsonfeed.items feed with
      | [] -> "(no items)"
      | item :: _ -> Item.id item)
  | _ -> "(unknown field)"

(* Escape JSON strings *)
let escape_json_string s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (function
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c when c < ' ' -> Printf.bprintf buf "\\u%04x" (Char.code c)
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

(* Output success as JSON *)
let output_success field value =
  Printf.printf {|{"status":"ok","field":"%s","value":"%s"}|}
    (escape_json_string field) (escape_json_string value);
  print_newline ()

(* Output error as JSON *)
let output_error (ctx, meta, kind) =
  let message = Jsont.Error.kind_to_string kind in
  let textloc = Jsont.Meta.textloc meta in
  let file = Jsont.Textloc.file textloc in
  let first_byte = Jsont.Textloc.first_byte textloc in
  let last_byte = Jsont.Textloc.last_byte textloc in
  let line_num, line_start_byte = Jsont.Textloc.first_line textloc in
  let column = first_byte - line_start_byte + 1 in
  let context = format_context ctx in

  Printf.printf
    {|{"status":"error","message":"%s","location":{"file":"%s","line":%d,"column":%d,"byte_start":%d,"byte_end":%d},"context":"%s"}|}
    (escape_json_string message)
    (escape_json_string file) line_num column first_byte last_byte
    (escape_json_string context);
  print_newline ()

let main () =
  (* Disable ANSI styling in error messages for consistent output *)
  Jsont.Error.disable_ansi_styler ();

  if Array.length Sys.argv < 2 then (
    Printf.eprintf "Usage: %s <file> [field]\n" Sys.argv.(0);
    Printf.eprintf "Fields: title, version, item_count, first_item_id\n";
    exit 1);

  let file = Sys.argv.(1) in
  let field = if Array.length Sys.argv > 2 then Sys.argv.(2) else "title" in

  (* Read file *)
  let content =
    try In_channel.with_open_text file In_channel.input_all
    with Sys_error msg ->
      Printf.printf {|{"status":"error","message":"File error: %s"}|}
        (escape_json_string msg);
      print_newline ();
      exit 1
  in

  (* Parse with location tracking *)
  match Jsonfeed.decode_string ~locs:true ~file content with
  | Ok feed ->
      let value = extract_field field feed in
      output_success field value
  | Error err ->
      output_error err;
      exit 1

let () = main ()
