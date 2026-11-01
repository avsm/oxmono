(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** A line editor that does no input or output.

    An editor turns the bytes a user types into what to echo and the lines
    entered. It keeps the line being typed, and:
    - inserts and erases at a cursor that the left and right arrows move;
    - recalls earlier lines with the up and down arrows;
    - completes the current word on Tab.

    It reads and writes nothing itself, so a network channel, a serial console
    or a terminal in raw mode drives it alike. The program reads bytes, passes
    them to {!feed} and writes the echo back. To ask a question on the program's
    own terminal, use [Console_eio.Prompt], which reads the answer and uses an
    editor to hide a password.

    Editing works on grapheme clusters (UAX #29), each a character with its
    marks:
    - A printable character is inserted at the cursor, even one whose bytes
      arrive across separate {!feed} calls. A mark joins the cluster before the
      cursor.
    - Backspace erases the cluster before the cursor.
    - The left and right arrows step over one cluster.

    A cluster takes the cells a terminal draws it in, two for a wide character
    or an emoji, so cursor motion is exact for them.

    The keys, the history and the completion follow those of the
    {{:https://erratique.ch/software/down}down} toplevel line editor. *)

type t
(** The type for line editors: the current line and the history of submitted
    ones. *)

val v :
  ?prompt:string ->
  ?history:string list ->
  ?complete:(string -> string list) ->
  ?mask:string ->
  unit ->
  t
(** [v ()] is a line editor with an empty line, such that:

    - [prompt] (defaults to [""]) is written again before the current line after
      Tab lists several completions, so that the line is redrawn behind its
      prompt. It stays empty when the caller writes no prompt;
    - [history] are the lines the up arrow recalls, oldest first (defaults to
      [[]]);
    - [complete word] is the list of completions of [word], the last word of the
      line, when Tab is pressed. The editor extends [word] to the longest prefix
      the completions share, and lists them when more than one is left. Without
      [complete], Tab does nothing;
    - [mask], if given, is echoed once for each cluster typed, in place of the
      cluster.

    A control character in [prompt], [history], [mask] or a completion is echoed
    as a space, so none of them can move the cursor or change colours.

    With [mask], {!pending} and the lines of {!feed} still hold the bytes typed
    as they are, other than the keys. A byte above [0x7F] that is no character
    is kept. The replacement character, the space for a C1 control and the
    dropped mark of {!feed} are only in the echo. A masked editor, for a
    password or a token, completes nothing and recalls nothing, and its
    {!history} stays empty because it drops the [history] seed and keeps no line
    it submits. *)

val feed : t -> string -> string * string list
(** [feed t input] is [(echo, lines)] once each byte of [input] is read in turn,
    where [echo] is the bytes to write back to the terminal and [lines] are the
    lines [input] completes, in order, each without its terminator. The bytes
    act as follows:

    - a printable character is inserted at the cursor and echoed. It is ASCII
      \[[0x20];[0x7E]\], or a UTF-8 character of several bytes, which may arrive
      across several [feed] calls. A mark of no width with nothing before it in
      the line to join is dropped, as a terminal drops it in the first column.
      Bytes that decode to no character become replacement characters U+FFFD,
      one for each maximal part of a character, as {!Console_text.sanitize}
      shows them. A C1 control (U+0080 to U+009F), which a terminal would act
      on, becomes a space, so what is echoed and kept only prints;
    - [\127] (DEL) and [\008] (BS) erase the cluster before the cursor, echoing
      ["\b \b"] at the end of the line;
    - ['\r'] and ['\n'] submit the line, but a ['\n'] right after a ['\r'], the
      second half of a CRLF, submits nothing;
    - [\t] completes the current word ({!v});
    - [ESC \[ A] and [ESC \[ B] recall the previous and the next line of the
      history;
    - [ESC \[ C] and [ESC \[ D] move the cursor right and left over one cluster.
      A terminal in application cursor mode sends [ESC O A] to [ESC O D], which
      act the same;
    - any other byte is ignored.

    The cursor always stands between two grapheme clusters of the line. When a
    character typed joins the cluster after the cursor, as a ZWJ typed before an
    emoji does, or an erased cluster lets the clusters on either side join, the
    cursor goes after the joined cluster.

    Feeding [input] at once or in pieces gives the same echo and lines, and the
    echo holds no escape code, since the cursor moves by backspaces. *)

val pending : t -> string
(** [pending t] is the current line, the bytes typed since the last submitted
    line. *)

val history : t -> string list
(** [history t] is the submitted lines, oldest first, after the [history] seed
    and with consecutive duplicates dropped. *)
