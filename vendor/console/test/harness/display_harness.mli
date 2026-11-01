(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tasks shown on a captured surface, for the {!Console.Task},
    {!Console.Progress} and {!Console.Display} tests. *)

open Console

module Driver = Task_driver
(** How [Console_eio.run] drives tasks: their scope, its [Logs] messages and the
    current task. *)

val contains : string -> string -> bool
(** [contains affix s] is [true] iff [affix] occurs in [s]. *)

val glyphs : string -> Uchar.t array
(** [glyphs s] are the characters of [s], in order. *)

val bar_span : string -> (int * int) option
(** [bar_span row] is the cells of the first and the last block or shade glyph
    of [row], each glyph one cell: the ends of a bar in {!Console.Theme.dos}, if
    [row] has one. *)

val clock : unit -> (unit -> float) * (float -> unit)
(** [clock ()] is a clock reading zero and a function that advances it. *)

val plain :
  ?label:string ->
  ?layout:Progress.Layout.t ->
  ?theme:Theme.t ->
  ?progress:Progress.t ->
  ?view:(Task.t -> View.t) ->
  ?keep_ended:bool ->
  (Task.t -> (float -> unit) -> 'a) ->
  'a * string
(** [plain f] is the value of [f root advance] for a [root] whose tasks show on
    a formatter that is not a terminal, in [progress] (defaults to [`Plain]),
    and what it wrote once the work succeeded, unless [f] ended it, and the
    display closed, as under [Console_eio.run]. [theme], if given, is set on the
    formatter. *)

type term = private {
  vte : Console_vte.t;
  root : Task.t;
  display : Display.t;
  advance : float -> unit;
  size : (int * int) ref;
  chunks : string list ref;  (** Newest first. *)
}
(** The type for displays on an emulated terminal: every write the display makes
    is one chunk fed to [vte]. *)

val terminal :
  ?width:int ->
  ?height:int ->
  ?label:string ->
  ?layout:Progress.Layout.t ->
  ?theme:Theme.t ->
  ?styled:bool ->
  ?raw:bool ->
  ?before:string ->
  ?view:(Task.t -> View.t) ->
  ?keep_ended:bool ->
  ?commit:(Task.t -> View.t option) ->
  ?plain:(Task.Event.t -> View.t option) ->
  ?delay:float ->
  unit ->
  term
(** [terminal ()] is a root whose tasks show ({!Progress.show}) on a terminal of
    [width] by [height] (defaults to 40 by 8), in colour iff [styled] (defaults
    to [false]). If [raw] (defaults to [false]) the terminal's line discipline
    is in raw mode: a line feed moves down and keeps the column. [before]
    (defaults to [""]) is what the host wrote on the terminal before the display
    started. [theme], if given, is set on the display's formatter. *)

val close : term -> unit
(** [close t] ends the work as a success, unless it ended, and closes the
    display, as [Console_eio.run] does once its function returns. *)

val written : term -> string
(** [written t] is every byte the display wrote, in order. *)

val resize : term -> width:int -> height:int -> unit
(** [resize t ~width ~height] resizes the terminal and its emulator. *)

val screen : term -> string list
(** [screen t] are the rows of the screen, trailing blanks and blank rows at the
    bottom removed. *)

val lines : term -> string list
(** [lines t] are the scrollback and screen rows, trailing blanks and blank rows
    at the bottom removed. *)

val check_invalid : string -> (unit -> unit) -> unit
(** [check_invalid msg f] checks that [f ()] raises [Invalid_argument msg]. *)

(** {1:matrix Tests matrix 0.1.0 fails}

    The emulator of matrix 0.1.0 has defects that invariant-hq/mosaic has fixed
    since. The tests that meet them are skipped until console moves to matrix
    0.2.0. *)

val skip_on_matrix_0_1 : string -> ('a -> unit) -> 'a -> unit
(** [skip_on_matrix_0_1 fix test] is [test], skipped. [fix] names the upstream
    change it needs. *)
