(* Test runner for toml-test suite *)

let test_dir = "../toml-test/tests"

(* Simple JSON comparison - normalizes whitespace and order *)
let normalize_json s =
  (* Remove all whitespace outside of strings *)
  let buf = Buffer.create (String.length s) in
  let in_string = ref false in
  let escaped = ref false in
  String.iter (fun c ->
    if !escaped then begin
      Buffer.add_char buf c;
      escaped := false
    end else if !in_string then begin
      Buffer.add_char buf c;
      if c = '\\' then escaped := true
      else if c = '"' then in_string := false
    end else begin
      if c = '"' then begin
        in_string := true;
        Buffer.add_char buf c
      end else if c <> ' ' && c <> '\n' && c <> '\r' && c <> '\t' then
        Buffer.add_char buf c
    end
  ) s;
  Buffer.contents buf

let parse_json_string s pos =
  if s.[pos] <> '"' then failwith "Expected string";
  let buf = Buffer.create 64 in
  let p = ref (pos + 1) in
  let len = String.length s in
  while !p < len && s.[!p] <> '"' do
    if s.[!p] = '\\' then begin
      incr p;
      if !p >= len then failwith "Unexpected end in string";
      match s.[!p] with
      | '"' -> Buffer.add_char buf '"'; incr p
      | '\\' -> Buffer.add_char buf '\\'; incr p
      | 'n' -> Buffer.add_char buf '\n'; incr p
      | 'r' -> Buffer.add_char buf '\r'; incr p
      | 't' -> Buffer.add_char buf '\t'; incr p
      | 'b' -> Buffer.add_char buf '\b'; incr p
      | 'f' -> Buffer.add_char buf (Char.chr 0x0C); incr p
      | 'u' ->
          incr p;
          if !p + 4 > len then failwith "Invalid unicode escape";
          let hex = String.sub s !p 4 in
          let cp = int_of_string ("0x" ^ hex) in
          (* Convert codepoint to UTF-8 *)
          if cp <= 0x7F then
            Buffer.add_char buf (Char.chr cp)
          else if cp <= 0x7FF then begin
            Buffer.add_char buf (Char.chr (0xC0 lor (cp lsr 6)));
            Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
          end else begin
            Buffer.add_char buf (Char.chr (0xE0 lor (cp lsr 12)));
            Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
            Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
          end;
          p := !p + 4
      | c -> failwith (Printf.sprintf "Invalid escape: \\%c" c)
    end else begin
      Buffer.add_char buf s.[!p];
      incr p
    end
  done;
  if !p >= len then failwith "Unclosed string";
  (Buffer.contents buf, !p + 1)

(* Semantic comparison for tagged JSON values *)
type json_value =
  | JString of string
  | JNumber of string
  | JBool of bool
  | JNull
  | JArray of json_value list
  | JObject of (string * json_value) list

let rec parse_json_value s pos =
  let len = String.length s in
  let skip_ws pos =
    let p = ref pos in
    while !p < len && (s.[!p] = ' ' || s.[!p] = '\t' || s.[!p] = '\n' || s.[!p] = '\r') do
      incr p
    done;
    !p
  in
  let pos = skip_ws pos in
  if pos >= len then failwith "Unexpected end of JSON";
  match s.[pos] with
  | '{' ->
      let pos = ref (skip_ws (pos + 1)) in
      let pairs = ref [] in
      while !pos < len && s.[!pos] <> '}' do
        if !pairs <> [] then begin
          if s.[!pos] <> ',' then failwith "Expected comma";
          pos := skip_ws (!pos + 1)
        end;
        let (key, p) = parse_json_string s !pos in
        pos := skip_ws p;
        if s.[!pos] <> ':' then failwith "Expected colon";
        pos := skip_ws (!pos + 1);
        let (value, p) = parse_json_value s !pos in
        pairs := (key, value) :: !pairs;
        pos := skip_ws p
      done;
      if !pos >= len then failwith "Unclosed object";
      (JObject (List.rev !pairs), !pos + 1)
  | '[' ->
      let pos = ref (skip_ws (pos + 1)) in
      let items = ref [] in
      while !pos < len && s.[!pos] <> ']' do
        if !items <> [] then begin
          if s.[!pos] <> ',' then failwith "Expected comma";
          pos := skip_ws (!pos + 1)
        end;
        let (value, p) = parse_json_value s !pos in
        items := value :: !items;
        pos := skip_ws p
      done;
      if !pos >= len then failwith "Unclosed array";
      (JArray (List.rev !items), !pos + 1)
  | '"' ->
      let (str, p) = parse_json_string s pos in
      (JString str, p)
  | c when c >= '0' && c <= '9' || c = '-' ->
      let start = pos in
      let p = ref pos in
      while !p < len && (let c = s.[!p] in c >= '0' && c <= '9' || c = '-' || c = '+' || c = '.' || c = 'e' || c = 'E') do
        incr p
      done;
      (JNumber (String.sub s start (!p - start)), !p)
  | 't' ->
      if pos + 4 <= len && String.sub s pos 4 = "true" then (JBool true, pos + 4)
      else failwith "Invalid JSON"
  | 'f' ->
      if pos + 5 <= len && String.sub s pos 5 = "false" then (JBool false, pos + 5)
      else failwith "Invalid JSON"
  | 'n' ->
      if pos + 4 <= len && String.sub s pos 4 = "null" then (JNull, pos + 4)
      else failwith "Invalid JSON"
  | _ -> failwith (Printf.sprintf "Invalid JSON character: %c" s.[pos])

(* Normalize datetime fractional seconds: remove trailing zeros *)
let normalize_datetime_frac s =
  (* Find the fractional part and normalize it *)
  let len = String.length s in
  let buf = Buffer.create len in
  let i = ref 0 in
  while !i < len do
    let c = s.[!i] in
    if c = '.' then begin
      (* Found decimal point - collect digits and normalize *)
      Buffer.add_char buf '.';
      incr i;
      let frac_start = Buffer.length buf in
      while !i < len && s.[!i] >= '0' && s.[!i] <= '9' do
        Buffer.add_char buf s.[!i];
        incr i
      done;
      (* Remove trailing zeros from fractional part *)
      let contents = Buffer.contents buf in
      let frac_end = ref (String.length contents - 1) in
      while !frac_end >= frac_start && contents.[!frac_end] = '0' do
        decr frac_end
      done;
      (* If only the dot remains, remove it too *)
      if !frac_end = frac_start - 1 then
        decr frac_end;
      Buffer.clear buf;
      Buffer.add_substring buf contents 0 (!frac_end + 1);
      (* Add rest of string *)
      while !i < len do
        Buffer.add_char buf s.[!i];
        incr i
      done
    end else begin
      Buffer.add_char buf c;
      incr i
    end
  done;
  Buffer.contents buf

(* Semantic comparison of tagged JSON values *)
let rec json_values_equal expected actual =
  match expected, actual with
  | JNull, JNull -> true
  | JBool a, JBool b -> a = b
  | JNumber a, JNumber b -> a = b
  | JString a, JString b -> a = b
  | JArray a, JArray b ->
      List.length a = List.length b &&
      List.for_all2 json_values_equal a b
  | JObject pairs_e, JObject pairs_a ->
      (* Check if this is a tagged value {"type": ..., "value": ...} *)
      let get_tagged pairs =
        match List.assoc_opt "type" pairs, List.assoc_opt "value" pairs with
        | Some (JString typ), Some (JString value) when List.length pairs = 2 ->
            Some (typ, value)
        | _ -> None
      in
      (match get_tagged pairs_e, get_tagged pairs_a with
      | Some (type_e, value_e), Some (type_a, value_a) ->
          (* Tagged value comparison *)
          if type_e <> type_a then false
          else begin
            match type_e with
            | "float" ->
                (* Compare floats numerically *)
                (try
                  let f_e = float_of_string value_e in
                  let f_a = float_of_string value_a in
                  f_e = f_a || (Float.is_nan f_e && Float.is_nan f_a)
                with _ -> value_e = value_a)
            | "datetime" | "datetime-local" | "date-local" | "time-local" ->
                (* Normalize fractional seconds *)
                normalize_datetime_frac value_e = normalize_datetime_frac value_a
            | _ ->
                (* String comparison for other types *)
                value_e = value_a
          end
      | _ ->
          (* Regular object comparison - sort by keys *)
          let sorted_e = List.sort (fun (a, _) (b, _) -> String.compare a b) pairs_e in
          let sorted_a = List.sort (fun (a, _) (b, _) -> String.compare a b) pairs_a in
          List.length sorted_e = List.length sorted_a &&
          List.for_all2 (fun (ke, ve) (ka, va) -> ke = ka && json_values_equal ve va) sorted_e sorted_a)
  | _ -> false

let json_equal a b =
  try
    let (va, _) = parse_json_value a 0 in
    let (vb, _) = parse_json_value b 0 in
    json_values_equal va vb
  with _ -> false

let run_valid_test toml_file json_file =
  let toml_content = In_channel.with_open_bin toml_file In_channel.input_all in
  match Tomlt_bytesrw.of_string toml_content with
  | Error e -> `Fail (Printf.sprintf "Decode error: %s" (Tomlt.Toml.Error.to_string e))
  | Ok toml ->
      let actual_json = Tomlt_bytesrw.Tagged_json.encode toml in
      let expected_json = In_channel.with_open_bin json_file In_channel.input_all in
      if json_equal actual_json expected_json then
        `Pass
      else
        `Fail (Printf.sprintf "JSON mismatch.\nExpected: %s\nActual: %s"
          (normalize_json expected_json) (normalize_json actual_json))

let run_invalid_test toml_file =
  let toml_content = In_channel.with_open_bin toml_file In_channel.input_all in
  match Tomlt_bytesrw.of_string toml_content with
  | Error _ -> `Pass  (* Should fail *)
  | Ok _ -> `Fail "Should have failed but parsed successfully"

(* Encoder test: JSON -> TOML -> JSON round-trip *)
let run_encoder_test json_file =
  let json_content = In_channel.with_open_bin json_file In_channel.input_all in
  (* First, encode JSON to TOML *)
  match Tomlt_bytesrw.Tagged_json.decode_and_encode_toml json_content with
  | Error msg -> `Fail (Printf.sprintf "Encode error: %s" msg)
  | Ok toml_output ->
      (* Then decode the TOML back to check round-trip *)
      match Tomlt_bytesrw.of_string toml_output with
      | Error e -> `Fail (Printf.sprintf "Round-trip decode error: %s\nTOML was:\n%s" (Tomlt.Toml.Error.to_string e) toml_output)
      | Ok decoded_toml ->
          (* Compare the decoded result with original JSON *)
          let actual_json = Tomlt_bytesrw.Tagged_json.encode decoded_toml in
          if json_equal actual_json json_content then
            `Pass
          else
            `Fail (Printf.sprintf "Round-trip mismatch.\nOriginal JSON: %s\nEncoded TOML:\n%s\nDecoded JSON: %s"
              (normalize_json json_content) toml_output (normalize_json actual_json))

let read_file_list filename =
  let ic = open_in filename in
  let rec loop acc =
    match input_line ic with
    | line -> loop (String.trim line :: acc)
    | exception End_of_file -> close_in ic; List.rev acc
  in
  loop []

let () =
  let valid_passed = ref 0 in
  let valid_failed = ref 0 in
  let encoder_passed = ref 0 in
  let encoder_failed = ref 0 in
  let invalid_passed = ref 0 in
  let invalid_failed = ref 0 in
  let failures = ref [] in

  (* Read the file list for TOML 1.1.0 *)
  let files = read_file_list (test_dir ^ "/files-toml-1.1.0") in

  List.iter (fun file ->
    if String.length file > 0 then begin
      let full_path = test_dir ^ "/" ^ file in
      if Sys.file_exists full_path then begin
        if String.length file >= 6 && String.sub file 0 6 = "valid/" then begin
          (* Valid test - needs both .toml and .json *)
          if Filename.check_suffix file ".toml" then begin
            let json_file = (Filename.chop_suffix full_path ".toml") ^ ".json" in
            if Sys.file_exists json_file then begin
              (* Decoder test: TOML -> JSON *)
              (match run_valid_test full_path json_file with
              | `Pass -> incr valid_passed
              | `Fail msg ->
                  incr valid_failed;
                  failures := (file ^ " (decode)", msg) :: !failures);
              (* Encoder test: JSON -> TOML -> JSON round-trip *)
              (match run_encoder_test json_file with
              | `Pass -> incr encoder_passed
              | `Fail msg ->
                  incr encoder_failed;
                  failures := (file ^ " (encode)", msg) :: !failures)
            end
          end
        end else if String.length file >= 8 && String.sub file 0 8 = "invalid/" then begin
          (* Invalid test - only .toml *)
          if Filename.check_suffix file ".toml" then begin
            match run_invalid_test full_path with
            | `Pass -> incr invalid_passed
            | `Fail msg ->
                incr invalid_failed;
                failures := (file, msg) :: !failures
          end
        end
      end
    end
  ) files;

  Printf.printf "\n=== Test Results ===\n";
  Printf.printf "Decoder tests: %d passed, %d failed\n" !valid_passed !valid_failed;
  Printf.printf "Encoder tests: %d passed, %d failed\n" !encoder_passed !encoder_failed;
  Printf.printf "Invalid tests: %d passed, %d failed\n" !invalid_passed !invalid_failed;
  Printf.printf "Total: %d passed, %d failed\n"
    (!valid_passed + !encoder_passed + !invalid_passed)
    (!valid_failed + !encoder_failed + !invalid_failed);

  if !failures <> [] then begin
    Printf.printf "\n=== Failures (first 30) ===\n";
    List.iter (fun (file, msg) ->
      Printf.printf "\n%s:\n  %s\n" file msg
    ) (List.rev !failures |> List.filteri (fun i _ -> i < 30))
  end;

  (* Show some valid test failures specifically *)
  let valid_failures = List.filter (fun (f, _) -> String.sub f 0 6 = "valid/") (List.rev !failures) in
  if valid_failures <> [] then begin
    Printf.printf "\n=== Valid Test Failures (first 20) ===\n";
    List.iter (fun (file, msg) ->
      Printf.printf "\n%s:\n  %s\n" file (String.sub msg 0 (min 200 (String.length msg)))
    ) (List.filteri (fun i _ -> i < 20) valid_failures)
  end;

  if !valid_failed + !invalid_failed > 0 then exit 1
