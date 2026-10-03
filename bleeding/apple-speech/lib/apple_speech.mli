(** On-device speech transcription and synthesis on macOS.

    On other platforms, {!available} returns [false] and operations that use
    Apple frameworks or [say] raise [Error Unavailable]. {!text} remains usable.

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
  | Unavailable  (** speech is unavailable on this device or platform *)
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

val duration : string -> float
(** [duration path] is the length in seconds of the audio file at [path].
    Raises {!Error} with {!Unreadable} when it is not audio. *)

val text : segment list -> string
(** [text segments] joins the segments' text with single spaces. *)

(** {1 Synthesis}

    Speech synthesis runs the system's [/usr/bin/say], which uses the same
    voices as the rest of macOS, including any enhanced or premium voices
    downloaded in System Settings. A separate process is needed because
    Apple's synthesis APIs deliver audio only on the main thread's run loop,
    which an OCaml program does not run. *)

type voice = {
  name : string;  (** such as ["Daniel"] or ["Eddy (English (UK))"] *)
  locale : string;  (** such as ["en_GB"] *)
  sample : string;  (** the voice's sample sentence *)
}

val voices : _ Eio.Process.mgr -> voice list
(** [voices mgr] lists the installed voices. *)

type format =
  | M4a  (** AAC in MPEG-4, which Matrix clients play *)
  | Wav  (** 16-bit PCM at 22.05 kHz *)
  | Aiff

val synthesize :
  _ Eio.Process.mgr ->
  ?voice:string ->
  ?rate:int ->
  ?format:format ->
  text:string ->
  string ->
  unit
(** [synthesize mgr ~text path] speaks [text] into the audio file [path],
    replacing it. [voice] is a name from {!voices} and defaults to the system
    voice. [rate] is in words per minute, 50 to 700. [format] defaults to
    {!M4a}. Square brackets in [text] become parentheses, because [say] reads
    [[[...]]] as embedded commands. Raises [Invalid_argument] for blank text or
    a rate out of range, and {!Error} for an unknown voice or a failed
    synthesis. *)
