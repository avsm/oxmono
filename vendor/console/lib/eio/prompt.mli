(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Questions asked on the terminal.

    A prompt writes a question on a formatter and reads the answer, one line,
    from an Eio flow, so a fiber waiting for an answer lets the others run.

    The terminal edits the line as the user types it, except under {!secret},
    which switches the terminal's echo off and edits the line with a
    {!Console_input} editor that shows a [*] for each grapheme cluster typed. A
    prompt with nobody to type an answer says so with an [Error] instead of
    waiting for ever.

    While a display that [Console_eio.run] opened runs, a question takes its
    progress rows off the screen for the time it is asked. What the asking fiber
    prints meanwhile goes straight out, and what other fibers print is taken by
    the display as usual. *)

type t
(** The type for prompts. *)

val v : ?interactive:bool -> ?ppf:Format.formatter -> _ Eio.Flow.source -> t
(** [v stdin] is a prompt that reads answers from [stdin].

    - [ppf] is where it writes questions (defaults to {!Fmt.stderr}, so that
      standard output stays the program's).
    - [interactive] says whether someone can answer (defaults to whether [stdin]
      is a terminal: an Eio flow backed by a Unix descriptor for which [isatty]
      holds). A prompt that is not interactive answers [Error] to every question
      and writes nothing, rather than block on input nobody will type.

    A program creates one {!type-t} and reuses it: it owns the read buffer, so
    two questions in a row do not lose the input between them. *)

val interactive : t -> bool
(** [interactive t] is [true] iff [t] has someone to ask. *)

type error = [ `End_of_input | `Msg of string ]
(** The type for the errors of a question. [`End_of_input] is the end of the
    input before an answer, such as Ctrl-D on a terminal; [`Msg] is any other
    error, a prompt that is not {!interactive} among them. *)

val keys : t -> (Eio.Buf_read.t -> 'a) -> ('a, error) result
(** [keys t f] is [Ok (f input)], with [input] the bytes of the keys typed on
    the terminal behind [t]'s input, each readable as soon as it is typed and
    not shown, such as a game reads them. A key that sends an escape sequence,
    an arrow for example, is its bytes, which [f] decodes. Ctrl-C and Ctrl-Z
    still send their signals.

    The terminal is given back as it was found when [f] returns or raises, when
    its fiber is cancelled and on SIGINT or SIGTERM, and while SIGTSTP stops the
    program. The rows of a display stay on the screen while [f] runs. This is
    [Error `End_of_input] when [f] raises [End_of_file], as its reads do at the
    end of the input, and [Error (`Msg _)] when the input is not a terminal,
    when [t] is not {!interactive} or on a [Unix.Unix_error]. Any other
    exception of [f] is raised again. *)

val pp_error : error Fmt.t
(** [pp_error] formats an error, [`End_of_input] as ["end of input"] and
    [`Msg m] as [m]. *)

val line : t -> string -> (string, error) result
(** [line t question] is the next line of input, without its terminator (LF or
    CRLF), after writing [question]. The question is flushed, with no newline
    added, and each control character in it but a line feed is a space. The
    answer is returned as typed, spaces included. This is [Error `End_of_input]
    when the input ends before a line feed, and [Error (`Msg _)] when [t] is not
    interactive or when the line is longer than 4096 bytes, which is then
    skipped up to its line feed. *)

val confirm : t -> string -> (bool, error) result
(** [confirm t question] asks [question ^ " [y/N] "] and is [true] for an answer
    of ["y"] or ["yes"], compared after trimming surrounding whitespace and
    ignoring case, and [false] for any other line, an empty one included. Its
    errors are those of {!line}. *)

val secret : t -> string -> (string, error) result
(** [secret t question] is like {!line} but the answer is not shown. One [*] is
    written for each grapheme cluster typed instead, then a line feed when the
    answer ends. The answer is the bytes typed, as they were typed, less the
    keys that edit the line ({!Console_input.feed}), and it is never retained.
    As with {!line}, more than 4096 bytes typed before the line ends is
    [Error (`Msg _)], and the rest of the line is skipped.

    Since masking switches echo off on the terminal behind [t]'s input, this is
    also [Error (`Msg _)] when that input is not a terminal. [question] is
    written first, and echo goes off after it: a key typed before then, which
    the terminal has already shown, is discarded, as getpass(3) does. The
    terminal is given back as it was found on return, on an exception and on
    SIGINT or SIGTERM. *)
