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

val download : Matrix_eio.Client.t -> source -> string
(** [download client source] fetches and, for encrypted media, decrypts and
    verifies the attachment. Raises [Failure] on any error. *)
