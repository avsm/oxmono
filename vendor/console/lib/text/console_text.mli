(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Untrusted text made safe for a terminal.

    Text from outside the program, such as a subprocess's output, a name read
    off the network or a file name, can hold bytes that a terminal acts on:
    escape codes that move the cursor, clear the screen or change colours. These
    functions return text that only prints.

    This library is [console.text]. It needs only
    {{:https://erratique.ch/software/uucp}uucp}, not the rest of [console], so
    code that must stay small can use it alone. *)

val sanitize : ?keep_newlines:bool -> string -> string
(** [sanitize s] is [s] made safe to print, with an escape code in it kept
    visible as text.

    - Each control character (C0, DEL and C1) is a space.
    - Each maximal subpart of an ill-formed UTF-8 sequence, as
      [String.get_utf_8_uchar] decodes it, is the replacement character U+FFFD:
      a lone byte, or the start of a character that its next byte does not
      continue, such as ["\xf0\x9f"], which is one U+FFFD.
    - The characters a terminal draws as nothing, which can hide text or reorder
      the text around them, are removed: those of Unicode's
      Default_Ignorable_Code_Point property, such as the bidirectional controls,
      the zero-width space U+200B, the word joiner U+2060, the zero-width
      no-break space U+FEFF, the Hangul fillers and the tag characters U+E0000
      to U+E007F, and the interlinear annotation characters U+FFF9 to U+FFFB.
      The soft hyphen U+00AD is one of them: a terminal draws it as a hyphen, as
      a space or as nothing, so it is removed too, and a word it split reads
      whole.
    - The flags of England, Scotland and Wales keep their tags: the black flag
      U+1F3F4, then the tag characters of [gbeng], [gbsct] or [gbwls], then the
      cancel tag U+E007F, the emoji tag sequences of Unicode's
      emoji-sequences.txt. Any other run of tags is removed, so that no hidden
      text rides on a flag, and the black flag stays alone.
    - A variation selector, U+FE00 to U+FE0F, U+E0100 to U+E01EF or a Mongolian
      one, is kept after a base and removed after no base: after a space, a
      control, a line end, a joiner or another selector. At the start of [s] it
      is kept, since its base may end the text before [s]. A run of selectors
      thus keeps only its first, and cannot hide text.
    - The joiners U+200C and U+200D, which some scripts and emoji need, are
      kept.
    - A line ends at a line feed, a carriage return, or the two together. VT, FF
      and NEL (U+0085), which a terminal reads as a line feed, end a line too.
      Each line end is one line feed, or one space if [keep_newlines] is [false]
      (defaults to [true]).

    The result is valid UTF-8, with no control character but line feeds, and
    [sanitize (sanitize s) = sanitize s]. *)

val strip : ?keep_newlines:bool -> ?keep_styles:bool -> string -> string
(** [strip s] is the text a terminal shows of [s]: the {!sanitize} of [s] once
    each escape code is removed whole.

    - A hyperlink's target and a window title go with their escape code.
    - [ESC E], the 7-bit next line, ends a line instead.
    - DEL and the C0 controls that a terminal acts on without printing are
      removed too, all but a tab, ESC and the line ends: [strip "a\007b"] is
      ["ab"].
    - Each line end is one line feed, or one space if [keep_newlines] is [false]
      (defaults to [true]), as with {!sanitize}.

    Without [keep_styles], the result holds no escape and
    [strip (strip s) = strip s].

    If [keep_styles] (defaults to [false]), the escape codes that only set
    colours and text attributes are kept, to relay the output of a tool that
    coloured it. A colour left on at the end of [s] is turned off, so [s] cannot
    change the look of what is printed after it. *)

val boundary : string -> int
(** [boundary s] is where [s] can be cut when the text goes on after it, neither
    inside a UTF-8 character nor inside an escape code: before the escape code
    under way at the end of [s], if one is and does not take all of [s], else
    before the last character of [s] if that one is not valid UTF-8, which the
    bytes after [s] may complete, else at the end of [s]. Text read in pieces,
    such as a child process's output, cut there, gives each piece to {!strip}
    with no escape code split between two. *)
