(* brotli_cli.ml - Command-line interface for Brotli compression *)

open Cmdliner

(* Helper to read entire file *)
let read_file path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
    let n = in_channel_length ic in
    really_input_string ic n)

(* Helper to write entire file *)
let write_file path data =
  let oc = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () ->
    output_string oc data)

(* Quality level conversion *)
let quality_of_int = function
  | 0 -> Brotli.Q0 | 1 -> Brotli.Q1 | 2 -> Brotli.Q2 | 3 -> Brotli.Q3
  | 4 -> Brotli.Q4 | 5 -> Brotli.Q5 | 6 -> Brotli.Q6 | 7 -> Brotli.Q7
  | 8 -> Brotli.Q8 | 9 -> Brotli.Q9 | 10 -> Brotli.Q10 | 11 -> Brotli.Q11
  | n -> invalid_arg (Printf.sprintf "invalid quality level: %d" n)

(* Compression implementation *)
let compress_impl ~quality ~keep ~force ~stdout ~verbose input_files =
  let quality = quality_of_int quality in
  let had_error = ref false in
  let process_file path =
    let output_path = path ^ ".br" in
    if not force && not stdout && Sys.file_exists output_path then begin
      Printf.eprintf "brotli: %s: output file exists (use -f to overwrite)\n%!" output_path;
      had_error := true
    end else begin
      try
        let data = read_file path in
        let compressed = Brotli.compress ~quality data in
        if stdout then
          print_string compressed
        else begin
          write_file output_path compressed;
          if verbose then
            Printf.eprintf "%s: %d -> %d bytes (%.1f%%)\n%!"
              path (String.length data) (String.length compressed)
              (100.0 *. float (String.length compressed) /. float (String.length data));
          if not keep then
            Sys.remove path
        end
      with
      | Sys_error msg ->
        Printf.eprintf "brotli: %s\n%!" msg;
        had_error := true
      | exn ->
        Printf.eprintf "brotli: %s: %s\n%!" path (Printexc.to_string exn);
        had_error := true
    end
  in
  List.iter process_file input_files;
  if !had_error then exit 1

(* Decompression implementation *)
let decompress_impl ~keep ~force ~stdout ~verbose input_files =
  let had_error = ref false in
  let process_file path =
    let output_path =
      if String.length path > 3 && String.sub path (String.length path - 3) 3 = ".br" then
        String.sub path 0 (String.length path - 3)
      else
        path ^ ".decompressed"
    in
    if not force && not stdout && Sys.file_exists output_path then begin
      Printf.eprintf "brotli: %s: output file exists (use -f to overwrite)\n%!" output_path;
      had_error := true
    end else begin
      try
        let data = read_file path in
        let decompressed = Brotli.decompress data in
        if stdout then
          print_string decompressed
        else begin
          write_file output_path decompressed;
          if verbose then
            Printf.eprintf "%s: %d -> %d bytes\n%!"
              path (String.length data) (String.length decompressed);
          if not keep then
            Sys.remove path
        end
      with
      | Sys_error msg ->
        Printf.eprintf "brotli: %s\n%!" msg;
        had_error := true
      | Brotli.Brotli_error err ->
        Printf.eprintf "brotli: %s: decompression failed: %s\n%!" path (Brotli.error_to_string err);
        had_error := true
      | exn ->
        Printf.eprintf "brotli: %s: %s\n%!" path (Printexc.to_string exn);
        had_error := true
    end
  in
  List.iter process_file input_files;
  if !had_error then exit 1

(* Common arguments *)
let keep_arg =
  let doc = "Keep input files after processing (do not delete)." in
  Arg.(value & flag & info ["k"; "keep"] ~doc)

let force_arg =
  let doc = "Overwrite existing output files without prompting." in
  Arg.(value & flag & info ["f"; "force"] ~doc)

let stdout_arg =
  let doc = "Write output to standard output instead of a file." in
  Arg.(value & flag & info ["c"; "stdout"] ~doc)

let verbose_arg =
  let doc = "Print compression statistics to standard error." in
  Arg.(value & flag & info ["v"; "verbose"] ~doc)

let input_files_arg =
  let doc = "Input file(s) to process." in
  Arg.(non_empty & pos_all file [] & info [] ~docv:"FILE" ~doc)

(* Compress command *)
let quality_arg =
  let doc =
    "Compression quality level (0-11). Higher values produce smaller output \
     but take longer. Levels 0-4 are fast, 5-9 are balanced, 10-11 use \
     optimal parsing for maximum compression. Default is 4."
  in
  Arg.(value & opt int 4 & info ["q"; "quality"] ~docv:"LEVEL" ~doc)

let compress_term =
  let compress quality keep force stdout verbose input_files =
    if quality < 0 || quality > 11 then begin
      Printf.eprintf "brotli: quality must be between 0 and 11\n%!";
      exit 1
    end else
      compress_impl ~quality ~keep ~force ~stdout ~verbose input_files
  in
  Term.(const compress $ quality_arg $ keep_arg $ force_arg $ stdout_arg $ verbose_arg $ input_files_arg)

let compress_cmd =
  let doc = "Compress files using Brotli." in
  let man = [
    `S Manpage.s_description;
    `P "Compress one or more files using the Brotli algorithm. By default, \
        each input file is replaced with a compressed version having the \
        '.br' extension.";
    `S Manpage.s_examples;
    `Pre "  # Compress a file with default quality (4)";
    `Pre "  brotli compress myfile.txt";
    `Noblank;
    `Pre "  # Compress with maximum compression";
    `Pre "  brotli compress -q 11 myfile.txt";
    `Noblank;
    `Pre "  # Compress to stdout for piping";
    `Pre "  brotli compress -c myfile.txt | wc -c";
    `Noblank;
    `Pre "  # Compress multiple files, keeping originals";
    `Pre "  brotli compress -k *.txt";
  ] in
  let info = Cmd.info "compress" ~doc ~man in
  Cmd.v info compress_term

(* Decompress command *)
let decompress_term =
  let decompress keep force stdout verbose input_files =
    decompress_impl ~keep ~force ~stdout ~verbose input_files
  in
  Term.(const decompress $ keep_arg $ force_arg $ stdout_arg $ verbose_arg $ input_files_arg)

let decompress_cmd =
  let doc = "Decompress Brotli-compressed files." in
  let man = [
    `S Manpage.s_description;
    `P "Decompress one or more Brotli-compressed files. By default, each \
        input file with a '.br' extension is replaced with its decompressed \
        contents (extension removed). Files without '.br' extension produce \
        output with '.decompressed' appended.";
    `S Manpage.s_examples;
    `Pre "  # Decompress a file";
    `Pre "  brotli decompress myfile.txt.br";
    `Noblank;
    `Pre "  # Decompress to stdout";
    `Pre "  brotli decompress -c myfile.txt.br";
    `Noblank;
    `Pre "  # Decompress multiple files, keeping compressed versions";
    `Pre "  brotli decompress -k *.br";
  ] in
  let info = Cmd.info "decompress" ~doc ~man in
  Cmd.v info decompress_term

(* Info command implementation *)
let info_impl ~json input_files =
  let had_error = ref false in
  let first = ref true in
  if json then print_string "[";
  let process_file path =
    try
      let compressed_data = read_file path in
      let compressed_size = String.length compressed_data in
      let decompressed_size =
        try
          let decompressed = Brotli.decompress compressed_data in
          Some (String.length decompressed)
        with
        | Brotli.Brotli_error _ -> None
        | _ -> None  (* Catch any other decompression errors *)
      in
      if json then begin
        if not !first then print_string ",";
        first := false;
        print_string "\n  {";
        Printf.printf "\"file\": %S, " path;
        Printf.printf "\"compressed_size\": %d, " compressed_size;
        (match decompressed_size with
         | Some size ->
           Printf.printf "\"decompressed_size\": %d, " size;
           Printf.printf "\"ratio\": %.2f, " (float compressed_size /. float size *. 100.0);
           Printf.printf "\"valid\": true"
         | None ->
           Printf.printf "\"decompressed_size\": null, ";
           Printf.printf "\"ratio\": null, ";
           Printf.printf "\"valid\": false";
           had_error := true);
        print_string "}"
      end else begin
        Printf.printf "%s:\n" path;
        Printf.printf "  Compressed size:   %d bytes\n" compressed_size;
        (match decompressed_size with
         | Some size ->
           Printf.printf "  Decompressed size: %d bytes\n" size;
           Printf.printf "  Compression ratio: %.1f%%\n" (float compressed_size /. float size *. 100.0);
           Printf.printf "  Space savings:     %.1f%%\n" (100.0 -. float compressed_size /. float size *. 100.0);
           Printf.printf "  Status:            valid\n"
         | None ->
           Printf.printf "  Decompressed size: unknown (invalid data)\n";
           Printf.printf "  Status:            invalid or corrupted\n";
           had_error := true)
      end
    with
    | Sys_error msg ->
      if json then begin
        if not !first then print_string ",";
        first := false;
        Printf.printf "\n  {\"file\": %S, \"error\": %S}" path msg
      end else
        Printf.eprintf "brotli: %s\n%!" msg;
      had_error := true
  in
  List.iter process_file input_files;
  if json then print_string "\n]\n";
  if !had_error then exit 1

let json_arg =
  let doc = "Output information in JSON format for machine parsing." in
  Arg.(value & flag & info ["json"] ~doc)

let info_files_arg =
  let doc = "Brotli-compressed file(s) to inspect." in
  Arg.(non_empty & pos_all file [] & info [] ~docv:"FILE" ~doc)

let info_term =
  let show_info json input_files =
    info_impl ~json input_files
  in
  Term.(const show_info $ json_arg $ info_files_arg)

let info_cmd =
  let doc = "Display information about Brotli-compressed files." in
  let man = [
    `S Manpage.s_description;
    `P "Display compression statistics for one or more Brotli-compressed files, \
        including compressed size, decompressed size, and compression ratio.";
    `P "The command validates each file by attempting to decompress it, \
        reporting whether the data is valid Brotli-compressed content.";
    `S Manpage.s_examples;
    `Pre "  # Show info for a single file";
    `Pre "  brotli info myfile.txt.br";
    `Noblank;
    `Pre "  # Show info for multiple files";
    `Pre "  brotli info *.br";
    `Noblank;
    `Pre "  # Output as JSON for scripting";
    `Pre "  brotli info --json myfile.txt.br";
  ] in
  let cmd_info = Cmd.info "info" ~doc ~man in
  Cmd.v cmd_info info_term

(* Main command *)
let default_cmd =
  let doc = "Brotli compression and decompression tool." in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) compresses and decompresses files using the Brotli algorithm \
        (RFC 7932). It provides high compression ratios with fast decompression, \
        making it ideal for web content and general-purpose compression.";
    `P "Use '$(tname) compress' to compress files, '$(tname) decompress' \
        to decompress them, and '$(tname) info' to inspect compressed files. \
        Quality levels range from 0 (fastest) to 11 (best compression).";
    `S Manpage.s_commands;
    `S "QUALITY LEVELS";
    `P "$(b,0-4): Fast compression, suitable for real-time use.";
    `P "$(b,5-9): Balanced compression and speed.";
    `P "$(b,10-11): Maximum compression using optimal parsing (slow).";
    `S Manpage.s_examples;
    `Pre "  # Compress a file";
    `Pre "  brotli compress myfile.txt";
    `Noblank;
    `Pre "  # Decompress a file";
    `Pre "  brotli decompress myfile.txt.br";
    `Noblank;
    `Pre "  # Compress with high quality";
    `Pre "  brotli compress -q 9 largefile.bin";
    `Noblank;
    `Pre "  # Show compression statistics";
    `Pre "  brotli info myfile.txt.br";
    `S Manpage.s_bugs;
    `P "Report bugs at https://github.com/avsm/eeww/issues";
    `S Manpage.s_see_also;
    `P "gzip(1), zstd(1)";
  ] in
  let cmd_info = Cmd.info "brotli" ~version:"1.0.0" ~doc ~man in
  Cmd.group cmd_info [compress_cmd; decompress_cmd; info_cmd]

let () = exit (Cmd.eval default_cmd)
