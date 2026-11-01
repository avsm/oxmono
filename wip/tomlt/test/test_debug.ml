open Tomlt

(* Helper to encode TOML to string via writer *)
let toml_to_string value =
  let buf = Buffer.create 256 in
  Tomlt_bytesrw.to_writer (Bytesrw.Bytes.Writer.of_buffer buf) value;
  Buffer.contents buf

type config = { name : string; timeout : int option }

let config_codec =
  Table.(
    obj (fun name timeout -> { name; timeout })
    |> mem "name" string ~enc:(fun c -> c.name)
    |> opt_mem "timeout" int ~enc:(fun c -> c.timeout)
    |> finish
  )

let () =
  (* Test encoding *)
  let c = { name = "app"; timeout = None } in
  let toml = encode config_codec c in
  Printf.printf "Encoded TOML:\n%s\n" (toml_to_string toml);

  (* Show raw structure *)
  Printf.printf "\nRaw structure: %s\n" (match toml with
    | Toml.Table pairs ->
        String.concat ", " (List.map (fun (k, v) ->
          Printf.sprintf "%s=%s" k (match v with
            | Toml.String s -> Printf.sprintf "\"%s\"" s
            | Toml.Bool b -> string_of_bool b
            | Toml.Int i -> Int64.to_string i
            | _ -> "?"
          )
        ) pairs)
    | _ -> "not a table");

  (* Test decoding the encoded value *)
  Printf.printf "\nDecoding...\n";
  match decode config_codec toml with
  | Ok { name; timeout } ->
      Printf.printf "Decoded: name=%s, timeout=%s\n" name
        (match timeout with Some t -> string_of_int t | None -> "None")
  | Error e ->
      Printf.printf "Decode error: %s\n" (Toml.Error.to_string e)
