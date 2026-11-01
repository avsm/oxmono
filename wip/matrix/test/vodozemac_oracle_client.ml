(** Shared client for the line-oriented vodozemac test oracle. *)

module Smap = Map.MakePortable (String)

let json_object_jsont =
  Matrix_proto.Json.Codec.as_string_map Matrix_proto.Json.Codec.json

type response = Jsont.json Smap.t
type oracle = { o_in : in_channel; o_out : out_channel; mutable closed : bool }

let encode codec value =
  match Jsont_bytesrw.encode_string ~format:Jsont.Minify codec value with
  | Ok text -> text
  | Error error -> failwith error

let json_of codec value =
  match Jsont.Json.encode codec value with
  | Ok json -> json
  | Error error -> failwith error

let json_of_text text =
  match Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json text with
  | Ok json -> json
  | Error error -> failwith ("invalid oracle argument JSON: " ^ error)

let parse_object s : response =
  match Jsont_bytesrw.decode_string json_object_jsont s with
  | Ok m -> m
  | Error e -> failwith (Format.asprintf "cannot parse oracle JSON %S: %s" s e)

let field m k =
  match Smap.find_opt k m with
  | Some v -> v
  | None -> failwith (Printf.sprintf "missing oracle JSON field %S" k)

let json_string j =
  match Jsont.Json.decode Matrix_proto.Json.Codec.string j with
  | Ok s -> s
  | Error e -> failwith e

let jstring m k = json_string (field m k)

let jint m k =
  match Jsont.Json.decode Matrix_proto.Json.Codec.int (field m k) with
  | Ok n -> n
  | Error e -> failwith e

let jobject m k =
  match Jsont.Json.decode json_object_jsont (field m k) with
  | Ok v -> v
  | Error e -> failwith e

let jlist m k =
  match Jsont.Json.decode (Jsont.list json_object_jsont) (field m k) with
  | Ok v -> v
  | Error e -> failwith e

let jstring_esc value = encode Matrix_proto.Json.Codec.string value

let oracle_path =
  lazy
    (match Sys.getenv_opt "VODOZEMAC_ORACLE" with
    | Some p when Sys.file_exists p -> Some p
    | Some _ when Sys.getenv_opt "MATRIX_REQUIRE_ORACLE" = Some "1" ->
        failwith "required VODOZEMAC_ORACLE executable is missing"
    | Some _ | None ->
        let rel =
          Filename.concat "test"
            (Filename.concat "vodozemac-oracle"
               (Filename.concat "target"
                  (Filename.concat "release" "vodozemac-oracle")))
        in
        let rec up dir n =
          if n = 0 then None
          else
            let candidate = Filename.concat dir rel in
            if Sys.file_exists candidate then Some candidate
            else
              let parent = Filename.dirname dir in
              if parent = dir then None else up parent (n - 1)
        in
        up (Sys.getcwd ()) 8)

let oracle = ref None

let close o =
  if not o.closed then (
    o.closed <- true;
    ignore (Unix.close_process (o.o_in, o.o_out)))

let () = at_exit (fun () -> match !oracle with Some o -> close o | None -> ())

let get_oracle () =
  match !oracle with
  | Some o when not o.closed -> Some o
  | Some _ -> None
  | None -> (
      match Lazy.force oracle_path with
      | None -> None
      | Some path ->
          let o_in, o_out = Unix.open_process_args path [| path |] in
          let o = { o_in; o_out; closed = false } in
          oracle := Some o;
          Some o)

let call o request =
  output_string o.o_out request;
  output_char o.o_out '\n';
  flush o.o_out;
  let line =
    try input_line o.o_in with End_of_file -> failwith "oracle exited"
  in
  let m = parse_object line in
  match Smap.find_opt "ok" m with
  | Some j when Jsont.Json.decode Jsont.bool j = Ok true -> m
  | _ ->
      failwith
        (Printf.sprintf "oracle error for %s: %s" request
           (match Smap.find_opt "error" m with
           | Some j -> json_string j
           | None -> line))

let cmd name args =
  let fields =
    ("cmd", json_of Matrix_proto.Json.Codec.string name)
    :: List.map (fun (key, value) -> (key, json_of_text value)) args
  in
  encode
    (Matrix_proto.Json.Codec.string_map Matrix_proto.Json.Codec.json)
    fields

let s v = jstring_esc v
let i v = string_of_int v

let with_oracle name f () =
  match get_oracle () with
  | None when Sys.getenv_opt "MATRIX_REQUIRE_ORACLE" = Some "1" ->
      failwith "required vodozemac oracle is not built"
  | None -> Printf.printf "SKIP %s: vodozemac-oracle not built\n%!" name
  | Some o -> f o
