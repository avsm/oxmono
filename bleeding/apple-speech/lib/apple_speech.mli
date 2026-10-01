(** On-device speech transcription with Apple's Speech framework.

    Transcription uses [SpeechAnalyzer] with a [SpeechTranscriber] module on
    macOS 26 or later. Audio never leaves the machine. Any file that
    [AVAudioFile] reads is accepted, including WAV, AAC in MP4 or M4A, and Opus
    in Ogg, which is what Matrix voice messages use.

    Each language needs a model that the system downloads once and shares
    between applications. {!install} fetches it, and {!transcribe} can fetch it
    on first use.

    Every function except {!available} blocks on a system thread, so it must run
    inside Eio. Locales are identifiers such as ["en-GB"] or ["fr_FR"]. Omitting
    one uses the user's current locale. A locale resolves to the closest one the
    system supports. *)

type error =
  | Unavailable  (** this device cannot transcribe speech *)
  | Unsupported_locale of string  (** no transcription model for the locale *)
  | Assets_missing of string
      (** the locale's model is missing and installation was not allowed *)
  | Unreadable of string  (** the file is missing or not audio *)
  | Failed of string  (** the framework reported another failure *)

exception Error of error

val pp_error : Format.formatter -> error -> unit

val available : unit -> bool
(** [available ()] is [true] when this device supports speech transcription. *)

val supported_locales : unit -> string list
(** [supported_locales ()] lists the locales that have a transcription model,
    installed or not. *)

val installed_locales : unit -> string list
(** [installed_locales ()] lists the locales whose model is installed. *)

type status =
  | Unsupported
  | Supported  (** a model exists but is not installed *)
  | Downloading
  | Installed

val status : ?locale:string -> unit -> status
(** [status ?locale ()] is the state of the locale's model. Raises {!Error}
    with {!Unsupported_locale} when no model matches. *)

val install : ?locale:string -> unit -> string
(** [install ?locale ()] downloads the locale's model if it is missing and
    returns the resolved locale identifier. Raises {!Error}. *)

type segment = {
  start : float;  (** seconds from the start of the audio *)
  duration : float;  (** seconds *)
  text : string;
}

val transcribe : ?locale:string -> ?install:bool -> string -> segment list
(** [transcribe ?locale ?install path] transcribes the audio file at [path] in
    order. Silence gives [[]]. With [install] [true], the default, a missing
    model is downloaded first. Otherwise a missing model raises {!Error} with
    {!Assets_missing}. Raises {!Error} for any other failure. *)

val text : segment list -> string
(** [text segments] joins the segments' text with single spaces. *)
