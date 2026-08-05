(* Copyright (c) 2024, Anil Madhavapeddy <anil@recoil.org>

   Permission to use, copy, modify, and/or distribute this software for
   any purpose with or without fee is hereby granted, provided that the
   above copyright notice and this permission notice appear in all
   copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
   WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
   WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
   AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
   DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
   OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
   TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
   PERFORMANCE OF THIS SOFTWARE. *)

(** Command-line image processing operations for srcsetter.

    This module provides the core image processing pipeline including
    file discovery, image conversion, and progress reporting.

    {1 High-Level Pipeline}

    The simplest way to use this module is via {!run}, which executes
    the complete pipeline:

    {[
      Srcsetter_cmd.run
        ~proc_mgr:(Eio.Stdenv.process_mgr env)
        ~src_dir:Eio.Path.(fs / "images/originals")
        ~dst_dir:Eio.Path.(fs / "images/output")
        ()
    ]}

    {1 Configuration} *)

(** Configuration for the image processing pipeline. *)
type ('a, 'b) config = {
  dummy : bool;  (** When true, skip actual image conversion (dry run) *)
  preserve : bool;  (** When true, skip conversion if destination exists *)
  proc_mgr : 'a Eio.Process.mgr;  (** Eio process manager for running ImageMagick *)
  src_dir : 'b Eio.Path.t;  (** Source directory containing original images *)
  dst_dir : 'b Eio.Path.t;  (** Destination directory for generated images *)
  img_widths : int list;  (** List of target widths for responsive variants *)
  img_exts : string list;  (** File extensions to process (e.g., ["jpg"; "png"]) *)
  idx_file : string;  (** Name of the JSON index file to generate *)
  max_fibers : int;  (** Maximum concurrent conversion operations *)
}

(** {1 File Operations} *)

val file_seq :
  filter:(string -> bool) ->
  ([> Eio.Fs.dir_ty ] as 'a) Eio.Path.t ->
  'a Eio.Path.t Seq.t
(** [file_seq ~filter path] recursively enumerates files in [path].

    Returns a sequence of file paths where [filter filename] is true.
    Directories are traversed depth-first. *)

val iter_seq_p : ?max_fibers:int -> ('a -> unit) -> 'a Seq.t -> unit
(** [iter_seq_p ?max_fibers fn seq] iterates [fn] over [seq] in parallel.

    @param max_fibers Optional limit on concurrent fibers. Must be positive.
    @raise Invalid_argument if [max_fibers] is not positive. *)

(** {1 Image Operations} *)

val dims : ('a, 'b) config -> 'b Eio.Path.t -> int * int
(** [dims cfg path] returns the [(width, height)] dimensions of an image.

    Uses ImageMagick's [identify] command to read image metadata. *)

val convert : ('a, 'b) config -> string * string * int -> unit
(** [convert cfg (src, dst, size)] converts an image to WebP format.

    Creates the destination directory if needed, then uses ImageMagick
    to resize and convert the image with auto-orientation.

    @param src Source filename relative to [cfg.src_dir]
    @param dst Destination filename relative to [cfg.dst_dir]
    @param size Target width in pixels *)

val convert_pdf :
  ('a, 'b) config ->
  size:string ->
  dst:'b Eio.Path.t ->
  src:'b Eio.Path.t ->
  unit
(** [convert_pdf cfg ~size ~dst ~src] converts a PDF's first page to an image.

    Renders at 300 DPI, crops the top half, and resizes to the target width. *)

(** {1 Pipeline Execution} *)

val run :
  proc_mgr:'a Eio.Process.mgr ->
  src_dir:'b Eio.Path.t ->
  dst_dir:'b Eio.Path.t ->
  ?idx_file:string ->
  ?img_widths:int list ->
  ?img_exts:string list ->
  ?max_fibers:int ->
  ?dummy:bool ->
  ?preserve:bool ->
  unit ->
  Srcsetter.t list
(** [run ~proc_mgr ~src_dir ~dst_dir ()] runs the full srcsetter pipeline.

    Scans [src_dir] for images, converts them to WebP format at multiple
    responsive sizes, and writes an index file to [dst_dir].

    @param proc_mgr Eio process manager for running ImageMagick
    @param src_dir Source directory containing original images
    @param dst_dir Destination directory for generated images
    @param idx_file Name of the index file (default ["index.json"])
    @param img_widths List of target widths (default common responsive breakpoints)
    @param img_exts List of extensions to process (default common image formats)
    @param max_fibers Maximum concurrent operations (default 8)
    @param dummy When true, skip actual conversions (default false)
    @param preserve When true, skip existing files (default true)
    @return List of {!Srcsetter.t} entries describing generated images *)
