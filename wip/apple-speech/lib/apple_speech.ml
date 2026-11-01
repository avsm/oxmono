type error =
  | Unavailable
  | Unsupported_locale of string
  | Assets_missing of string
  | Unreadable of string
  | Failed of string

exception Error of error

let pp_error ppf = function
  | Unavailable ->
      Format.pp_print_string ppf
        "Apple speech is unavailable on this device or platform"
  | Unsupported_locale m | Assets_missing m | Unreadable m | Failed m ->
      Format.pp_print_string ppf m

let () =
  Printexc.Safe.register_printer (function
    | Error e -> Some (Format.asprintf "Apple_speech.Error: %a" pp_error e)
    | _ -> None)

external available : unit -> bool = "caml_apple_speech_available"
external is_macos : unit -> bool = "caml_apple_speech_is_macos"

let require_macos () = if not (is_macos ()) then raise (Error Unavailable)

external raw_locales : bool -> int * string = "caml_apple_speech_locales"
external raw_status : string option -> int * string = "caml_apple_speech_status"

external raw_install : string option -> int * string
  = "caml_apple_speech_install"

external raw_transcribe : string -> string option -> bool -> int * string
  = "caml_apple_speech_transcribe"

external raw_duration : string -> int * string = "caml_apple_speech_duration"

(* The codes match AppleSpeechBridge.swift. *)
let check (code, text) =
  match code with
  | 0 -> text
  | 1 -> raise (Error Unavailable)
  | 2 -> raise (Error (Unsupported_locale text))
  | 3 -> raise (Error (Assets_missing text))
  | 4 -> raise (Error (Unreadable text))
  | _ -> raise (Error (Failed text))

let blocking label f =
  require_macos ();
  check (Eio_unix.run_in_systhread ~label:("apple-speech." ^ label) f)

let decode codec text =
  match Jsont_bytesrw.decode_string codec text with
  | Ok value -> value
  | Error e -> raise (Error (Failed ("invalid bridge JSON: " ^ e)))

let locales installed =
  decode (Jsont.list Jsont.string)
    (blocking "locales" (fun () -> raw_locales installed))

let supported_locales () = locales false
let installed_locales () = locales true

type status = Unsupported | Supported | Downloading | Installed

let status ?locale () =
  match blocking "status" (fun () -> raw_status locale) with
  | "unsupported" -> Unsupported
  | "supported" -> Supported
  | "downloading" -> Downloading
  | "installed" -> Installed
  | other -> raise (Error (Failed ("unknown asset status " ^ other)))

let install ?locale () = blocking "install" (fun () -> raw_install locale)

type segment = { start : float; duration : float; text : string }

let segment =
  Jsont.Object.map (fun start duration text -> { start; duration; text })
  |> Jsont.Object.mem "start" Jsont.number ~enc:(fun s -> s.start)
  |> Jsont.Object.mem "duration" Jsont.number ~enc:(fun s -> s.duration)
  |> Jsont.Object.mem "text" Jsont.string ~enc:(fun s -> s.text)
  |> Jsont.Object.finish

let transcribe ?locale ?(install = true) path =
  decode (Jsont.list segment)
    (blocking "transcribe" (fun () -> raw_transcribe path locale install))

let duration path =
  let text = blocking "duration" (fun () -> raw_duration path) in
  match float_of_string_opt text with
  | Some seconds -> seconds
  | None -> raise (Error (Failed ("invalid duration " ^ text)))

let text segments =
  segments
  |> List.map (fun s -> String.trim s.text)
  |> List.filter (fun s -> s <> "")
  |> String.concat " "

(* Synthesis runs [say] in its own process. AVSpeechSynthesizer delivers audio
   only through the main thread's run loop, which an OCaml host does not run. *)

type voice = { name : string; locale : string; sample : string }

let say = "/usr/bin/say"

let run mgr ?(stdin = "") args =
  require_macos ();
  let out = Buffer.create 4096 and err = Buffer.create 256 in
  match
    Eio.Process.run mgr
      ~stdin:(Eio.Flow.string_source stdin)
      ~stdout:(Eio.Flow.buffer_sink out)
      ~stderr:(Eio.Flow.buffer_sink err)
      (say :: args)
  with
  | () -> Buffer.contents out
  | exception Eio.Io _ ->
      let detail = String.trim (Buffer.contents err) in
      raise
        (Error
           (Failed (if detail = "" then "speech synthesis failed" else detail)))

(* A line reads "Name (Variant)   en_GB    # Sample text". The locale is the
   first token after the name that looks like one. *)
let parse_voice line =
  match String.index_opt line '#' with
  | None -> None
  | Some hash -> (
      let sample =
        String.trim
          (String.sub line (hash + 1) (String.length line - hash - 1))
      in
      let words =
        String.split_on_char ' ' (String.sub line 0 hash)
        |> List.filter (fun w -> w <> "")
      in
      match List.rev words with
      | locale :: rev_name when String.contains locale '_' && rev_name <> [] ->
          Some { name = String.concat " " (List.rev rev_name); locale; sample }
      | _ -> None)

let voices mgr =
  run mgr [ "-v"; "?" ]
  |> String.split_on_char '\n'
  |> List.filter_map parse_voice

type format = M4a | Wav | Aiff

let format_args = function
  | M4a -> [ "--file-format=m4af"; "--data-format=aac" ]
  | Wav -> [ "--file-format=WAVE"; "--data-format=LEI16@22050" ]
  | Aiff -> [ "--file-format=AIFF" ]

(* [say] reads [[...]] as embedded commands that change voice, rate or
   pronunciation, so they cannot come from the text. *)
let plain text =
  String.map (function '[' -> '(' | ']' -> ')' | c -> c) text

let synthesize mgr ?voice ?rate ?(format = M4a) ~text path =
  require_macos ();
  if String.trim text = "" then
    invalid_arg "Apple_speech.synthesize: empty text";
  Option.iter
    (fun r ->
      if r < 50 || r > 700 then
        invalid_arg "Apple_speech.synthesize: rate must be 50 to 700")
    rate;
  Option.iter
    (fun v ->
      if not (List.exists (fun (x : voice) -> x.name = v) (voices mgr)) then
        raise (Error (Failed ("unknown voice " ^ v))))
    voice;
  let args =
    (match voice with None -> [] | Some v -> [ "-v"; v ])
    @ (match rate with None -> [] | Some r -> [ "-r"; string_of_int r ])
    @ format_args format
    @ [ "-o"; path; "-f"; "-" ]
  in
  ignore (run mgr ~stdin:(plain text) args)
