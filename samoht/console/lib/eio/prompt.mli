(** Interactive prompts over Eio flows.

    A prompt writes a question to an output flow and reads the answer as one
    line from an input flow. Both are Eio capabilities, so a fiber waiting for
    an answer suspends instead of parking the whole domain in a blocking read,
    and a test drives the very same code from a string source. Every question is
    result-typed: a prompt with nobody at the other end refuses rather than
    waiting for input that will never be typed.

    One question does not fit that shape. {!secret} reads the local terminal
    through Unix termios, because echo is that terminal's own line discipline
    and masking what is typed means taking it over. It is host-only, and takes
    no {!type-t}. *)

type t
(** A prompt: where questions go, and the buffered input answers come from. *)

val v :
  ?interactive:bool ->
  stdin:_ Eio.Flow.source ->
  stdout:_ Eio.Flow.sink ->
  unit ->
  t
(** [v ~stdin ~stdout ()] is a prompt reading answers from [stdin] and writing
    questions to [stdout].

    [interactive] defaults to whether [stdin] is a terminal: an Eio flow backed
    by a Unix descriptor for which [isatty] holds. A prompt that is not
    interactive answers [Error] to every question and writes nothing, rather
    than blocking on input nobody will type.

    One {!type-t} is created per program and reused: it owns the read buffer, so
    two questions in a row do not lose the input between them. *)

val interactive : t -> bool
(** [interactive t] is [true] iff [t] has someone to ask. *)

val line : t -> string -> (string, [ `Msg of string ]) result
(** [line t question] writes [question] verbatim, flushed and with no newline
    added, and is the next line of input without its terminator (LF or CRLF).
    The answer is returned as typed, spaces included. This is [Error] when [t]
    is not interactive or the input ends. *)

val confirm : t -> string -> (bool, [ `Msg of string ]) result
(** [confirm t label] asks [label ^ " [y/N] "] and is [true] for an answer of
    ["y"] or ["yes"], compared after trimming surrounding whitespace and
    ignoring case, and [false] for any other line, an empty one included: the
    capital in [N] is the promise that only an explicit yes goes ahead. This is
    [Error] when [t] is not interactive or the input ends. *)

val expect : t -> string -> string -> (unit, [ `Msg of string ]) result
(** [expect t question word] asks [question] and is [Ok ()] iff the answer is
    exactly [word], byte for byte. It is the gate that makes an operator type
    the name of what is about to be destroyed, so a different answer is [Error]
    saying the answer did not match, and the error never spells [word] out: a
    message that repeats the word hands over the very answer the gate exists to
    ask for. This is [Error] too when [t] is not interactive or the input ends.
*)

val secret : ?prompt:string -> unit -> (string, [ `Msg of string ]) result
(** [secret ?prompt ()] reads one line from the local terminal with echo
    disabled and writes one [*] for every entered character. It reads that
    terminal through Unix termios rather than through the flows a {!type-t} was
    given: echo is the terminal's own line discipline, so masking what is typed
    means taking the terminal over. The returned secret is never written or
    retained in input history. Terminal attributes are restored on success,
    error, exception, and SIGINT.

    The prompt and masks use stderr so stdout remains available for command
    output. Returns [Error] when stdin is not a terminal or reaches EOF. *)
