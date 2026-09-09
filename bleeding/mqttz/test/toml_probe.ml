let report = function
  | Ok value -> Printf.printf "OK %S\n" value
  | Error message -> Printf.printf "ERROR %S\n" message

let () =
  List.iter
    (fun text ->
      Toml.of_string Toml.Value.codec text
      |> Result.map (Toml.to_string Toml.Value.codec)
      |> Result.map_error Toml.Error.to_string
      |> report)
    [
      "client_id='client'\nport=1883\n";
      "[mqtt]\npassword='binary'\nkeep_alive=60\n";
      "when=2026-09-09T12:34:56Z\n";
      "when=2026-09-09T12:34:56.123+01:30\n";
      "x=[1,2,3]\n";
      "x=1\nx=2";
      "bad=\"\\uD800\"";
      "[";
    ];
  List.iter
    (fun text ->
      Toml.Datetime.of_string text
      |> Result.map Toml.Datetime.to_string
      |> report)
    [
      "2026-09-09T12:34:56Z";
      "2026-09-09 12:34:56.123-01:30";
      "2026-09-09T99:34:56Z";
      "2026-13-09T12:34:56Z";
      "2026-09-09T12:34:56bad";
      "missing";
    ];
  List.iter
    (fun text ->
      Toml.Datetime_local.of_string text
      |> Result.map Toml.Datetime_local.to_string
      |> report)
    [
      "2026-09-09T12:34:56";
      "2026-09-09 12:34:56.123";
      "2026-09-09T99:34:56";
      "2026-13-09T12:34:56";
      "missing";
    ]
