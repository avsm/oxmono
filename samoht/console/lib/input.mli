(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Cooked terminal input: a line editor.

    It accumulates printable input into a line, echoes what was typed, erases on
    backspace, recalls earlier lines with the up and down arrows, completes the
    current word on Tab, and moves a cursor within the line with the left and
    right arrows. A line is submitted on carriage return or line feed. Pure and
    transport-independent: a network channel, a serial console or stdin drives
    it the same way, and the echo it returns is what to write back to the
    terminal.

    Editing is by whole UTF-8 character at the cursor: a printable character (or
    a multi-byte one, even when its bytes arrive across separate {!feed} calls)
    is inserted, backspace erases the character before the cursor, and the left
    and right arrows step over one character. A character's width in cells comes
    from the [char_width] hint (see {!v}), so cursor motion is exact for wide
    CJK and emoji when one is supplied. {!crlf} is the matching output-side
    translation a terminal expects.

    The key bindings and the history and completion model follow Daniel
    Buenzli's [down] toplevel line editor (ISC licensed). *)

type t
(** A line editor holding the current line and the history of submitted ones. *)

val v :
  ?prompt:string ->
  ?history:string list ->
  ?complete:(string -> string list) ->
  ?char_width:(Uchar.t -> int) ->
  ?mask:string ->
  unit ->
  t
(** [v ?prompt ?history ?complete ?char_width ?mask ()] is a line editor.

    [prompt] (default empty) is reprinted before the current line after Tab
    lists several completion candidates, so the redrawn line keeps its prompt;
    leave it empty when the caller writes no prompt.

    [history] seeds the recallable lines, oldest first.

    [complete word] returns the candidate completions of the last whitespace-
    delimited [word] of the line when Tab is pressed. The editor extends the
    word to the candidates' longest common prefix and, when more than one
    remains, lists them. With no [complete], Tab does nothing.

    [char_width] gives the display cells a character occupies, used to place the
    cursor and redraw the line; it defaults to the Unicode-aware
    {!Console.Width.default_char_width}. Override it when the target terminal
    uses a different width policy.

    [mask], when provided, is echoed once for each typed character while the
    real line remains available through {!pending} and {!feed}. Masked editors
    disable completion and history navigation, and never retain submitted lines
    in {!history}; this mode is intended for passwords and tokens. *)

val feed : t -> string -> string * string list
(** [feed t input] processes [input] one byte at a time and returns
    [(echo, lines)]. [echo] is the bytes to write back to the terminal and
    [lines] are the lines completed by this input, in order, each without its
    terminator.

    A printable character -- ASCII [0x20..0x7e], or a multi-byte UTF-8 character
    whose lead and continuation bytes may span several [feed] calls -- is
    inserted at the cursor and echoed. [\127] (DEL) and [\008] (BS) erase the
    character before the cursor, echoing ["\b \b"] at end of line; ['\r'] and
    ['\n'] submit the line (a ['\n'] right after a ['\r'] is the trailing half
    of a CRLF and submits nothing); [\t] runs completion. The arrows ([ESC \[ A]
    / [ESC \[ B]) recall the previous and next history entries and ([ESC \[ C] /
    [ESC \[ D]) move the cursor right and left over one character. Any other
    byte is ignored. *)

val crlf : string -> string
(** [crlf s] is [s] with every line feed turned into a carriage-return line feed
    -- the output-side companion to {!feed}, since a terminal in raw mode needs
    ["\r\n"] line endings (the termios [ONLCR] translation). Output produced
    with ['\n'] line endings passes through it on the way to the terminal. *)

val pending : t -> string
(** [pending t] is the current line, the bytes typed since the last submitted
    line. *)

val history : t -> string list
(** [history t] is the submitted lines, oldest first, after the [history] seed
    and with consecutive duplicates dropped. *)
