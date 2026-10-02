(** Voice messages transcribed on this machine.

    A Matrix [m.audio] message, including an encrypted one, is downloaded,
    transcribed with Apple's on-device speech recogniser and returned as text
    for the ordinary request path. Audio never leaves the machine. *)

type source = Matrix_eio.Media.source

val source : Jsont.json -> (source, string) result
(** [source content] is where the audio of the [m.room.message] [content]
    lives, plain or encrypted. Its claimed size and duration must be within the
    limits: at most 20 MiB and ten minutes. *)

val marker : string
(** [marker] begins every transcript, ["[voice message] "]. *)

val transcribe :
  download:(source -> string) ->
  ?locale:string ->
  Jsont.json ->
  (string, string) result
(** [transcribe ~download ?locale content] downloads the audio with [download],
    transcribes it and returns the text marked as a voice message, as in
    ["[voice message] Remind me at nine."]. The downloaded body must also be
    within 20 MiB. A recording with no speech is an [Error]. The audio is held
    in a private temporary file that is removed afterwards. It must run inside
    Eio. *)

type note = {
  audio : string;
  content_type : string;  (** ["audio/ogg"], or ["audio/mp4"] *)
  filename : string;
  duration : int;  (** milliseconds *)
  waveform : int list;  (** at most 100 levels from 0 to 1024 *)
}
(** A spoken voice note ready to upload. *)

val speak : process_mgr:_ Eio.Process.mgr -> voice:string -> string -> note
(** [speak ~process_mgr ~voice text] synthesises [text] with [voice]. The audio
    is Opus in Ogg, which Matrix clients expect for a voice message, when
    [ffmpeg] is installed, and AAC in MPEG-4 otherwise. The duration and
    waveform come from the synthesised samples. Intermediate files are private
    and removed. Raises [Failure] if synthesis or encoding fails or the voice is
    unknown. *)

val image :
  download:(source -> string) ->
  Jsont.json ->
  (Agentkit.Chat.image, string) result
(** [image ~download content] fetches the image of an [m.image] message,
    within 20 MiB, and recognises its format from its bytes rather than the
    sender's claimed type. *)

val download : Matrix_eio.Client.t -> source -> string
(** [download client source] fetches and, for encrypted media, decrypts and
    verifies the attachment. Raises [Failure] on any error. *)
