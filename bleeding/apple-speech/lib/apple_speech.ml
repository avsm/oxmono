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
        "speech transcription is unavailable on this device"
  | Unsupported_locale m | Assets_missing m | Unreadable m | Failed m ->
      Format.pp_print_string ppf m

let () =
  Printexc.Safe.register_printer (function
    | Error e -> Some (Format.asprintf "Apple_speech.Error: %a" pp_error e)
    | _ -> None)

external available : unit -> bool = "caml_apple_speech_available"
external raw_locales : bool -> int * string = "caml_apple_speech_locales"
external raw_status : string option -> int * string = "caml_apple_speech_status"

external raw_install : string option -> int * string
  = "caml_apple_speech_install"

external raw_transcribe : string -> string option -> bool -> int * string
  = "caml_apple_speech_transcribe"

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

let text segments =
  segments
  |> List.map (fun s -> String.trim s.text)
  |> List.filter (fun s -> s <> "")
  |> String.concat " "
