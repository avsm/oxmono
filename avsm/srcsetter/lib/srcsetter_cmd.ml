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
    file discovery, image conversion, and progress reporting. *)

open Eio

(** Configuration for the image processing pipeline.

    @param dummy When true, skip actual image conversion (dry run)
    @param preserve When true, skip conversion if destination exists
    @param proc_mgr Eio process manager for running ImageMagick
    @param src_dir Source directory containing original images
    @param dst_dir Destination directory for generated images
    @param img_widths List of target widths for responsive variants
    @param img_exts File extensions to process (e.g., ["jpg"; "png"])
    @param idx_file Name of the JSON index file to generate
    @param max_fibers Maximum concurrent conversion operations *)
type ('a, 'b) config = {
  dummy : bool;
  preserve : bool;
  proc_mgr : 'a Eio.Process.mgr;
  src_dir : 'b Path.t;
  dst_dir : 'b Path.t;
  img_widths : int list;
  img_exts : string list;
  idx_file : string;
  max_fibers : int;
}

(** [file_seq ~filter path] recursively enumerates files in [path].

    Returns a sequence of file paths where [filter filename] is true.
    Directories are traversed depth-first. *)
let rec file_seq ~filter path =
  let dirs, files =
    Path.with_open_dir path Path.read_dir
    |> List.fold_left
         (fun (dirs, files) f ->
           let fp = Path.(path / f) in
           match Path.kind ~follow:false fp with
           | `Regular_file when filter f -> (dirs, fp :: files)
           | `Directory -> (f :: dirs, files)
           | _ -> (dirs, files))
         ([], [])
  in
  Seq.append (List.to_seq files)
    (Seq.flat_map (fun f -> file_seq ~filter Path.(path / f)) (List.to_seq dirs))

(** [iter_seq_p ?max_fibers fn seq] iterates [fn] over [seq] in parallel.

    @param max_fibers Optional limit on concurrent fibers. Must be positive.
    @raise Invalid_argument if [max_fibers] is not positive. *)
let iter_seq_p ?max_fibers fn seq =
  Eio.Switch.run ~name:"iter_seq_p" @@ fun sw ->
  match max_fibers with
  | None -> Seq.iter (fun v -> Fiber.fork ~sw @@ fun () -> fn v) seq
  | Some mf when mf <= 0 -> invalid_arg "iter_seq_p: max_fibers must be positive"
  | Some mf ->
      let sem = Semaphore.make mf in
      Seq.iter
        (fun v ->
          Semaphore.acquire sem;
          Fiber.fork ~sw @@ fun () ->
          Fun.protect ~finally:(fun () -> Semaphore.release sem) @@ fun () ->
          fn v)
        seq

(** [relativize_path dir path] returns [path] relative to [dir].

    @raise Failure if [path] is not under [dir]. *)
let relativize_path dir path =
  let dir = Path.native_exn dir in
  let path = Path.native_exn path in
  match Fpath.(rem_prefix (v dir) (v path)) with
  | None -> failwith "relativize_path: path is not under directory"
  | Some rel -> Fpath.to_string rel

(** [dims cfg path] returns the [(width, height)] dimensions of an image.

    Uses ImageMagick's [identify] command to read image metadata. *)
let dims { proc_mgr; _ } path =
  let path = Path.native_exn path in
  let args = [ "identify"; "-ping"; "-format"; "%w %h"; path ] in
  let output = Process.parse_out proc_mgr Buf_read.take_all args in
  Scanf.sscanf output "%d %d" (fun w h -> (w, h))

(** [try_dims cfg path] returns [Some (w, h)] if identify succeeds, [None] otherwise. *)
let try_dims cfg path =
  try Some (dims cfg path)
  with _ -> None

(** [file_size path] returns the size of the file in bytes. *)
let file_size path =
  let stat = Path.stat ~follow:true path in
  Optint.Int63.to_int stat.size

(** [is_valid_image cfg path] returns true if the file exists, has non-zero size,
    and identify can read its dimensions. *)
let is_valid_image cfg path =
  Path.is_file path &&
  file_size path > 0 &&
  Option.is_some (try_dims cfg path)

(** [width_from_variant_name name] extracts the width from a variant filename.

    Variant filenames have the form "path/name.WIDTH.webp". Returns [None] for
    base images (no width suffix). *)
let width_from_variant_name name =
  let base = Filename.chop_extension name in (* remove .webp *)
  let parts = String.split_on_char '.' base in
  match List.rev parts with
  | last :: _ -> (
      match int_of_string_opt last with
      | Some w -> Some w
      | None -> None)
  | [] -> None

(** [run cfg args] executes a shell command unless in dummy mode. *)
let run { dummy; proc_mgr; _ } args =
  if not dummy then Process.run proc_mgr args

(** [convert cfg (src, dst, size)] converts an image to WebP format.

    Creates the destination directory if needed, then uses ImageMagick
    to resize and convert the image with auto-orientation. *)
let convert ({ src_dir; dst_dir; dummy; _ } as cfg) (src, dst, size) =
  if dummy then ()
  else begin
    let dir =
      if Filename.dirname dst = "." then dst_dir
      else Path.(dst_dir / Filename.dirname dst)
    in
    Path.mkdirs ~exists_ok:true ~perm:0o755 dir;
    let src_path = Path.(native_exn (src_dir / src)) in
    let dst_path = Path.(native_exn (dst_dir / dst)) in
    let sz = Printf.sprintf "%dx" size in
    run cfg
      [
        "magick"; src_path;
        "-auto-orient"; "-thumbnail"; sz;
        "-quality"; "100";
        "-gravity"; "center"; "-extent"; sz;
        dst_path;
      ]
  end

(** [convert_pdf cfg ~size ~dst ~src] converts a PDF's first page to an image.

    Renders at 300 DPI, crops the top half, and resizes to the target width. *)
let convert_pdf cfg ~size ~dst ~src =
  let src_path = Path.native_exn src in
  let dst_path = Path.native_exn dst in
  let sz = Printf.sprintf "%sx" size in
  run cfg
    [
      "magick"; "-density"; "300"; "-quality"; "100";
      src_path ^ "[0]";
      "-gravity"; "North"; "-crop"; "100%x50%+0+0";
      "-resize"; sz;
      dst_path;
    ]

(** [needed_sizes ~img_widths ~w] returns widths from [img_widths] that are <= [w]. *)
let needed_sizes ~img_widths ~w = List.filter (fun tw -> tw <= w) img_widths

(** [needs_conversion ~preserve dst] returns true if [dst] should be generated.

    When [preserve] is true, existing files are skipped. *)
let needs_conversion ~preserve dst =
  not (preserve && Path.is_file dst)

(** [translate cfg ?w src] computes source and destination paths for conversion.

    Returns [(src_file, dst_file, width_opt, needs_work)] where [needs_work]
    indicates whether the conversion should be performed. *)
let translate { src_dir; dst_dir; preserve; _ } ?w src =
  let src_file = relativize_path src_dir src in
  let width_suffix = Option.fold ~none:"" ~some:(fun w -> "." ^ string_of_int w) w in
  let dst_file = Printf.sprintf "%s%s.webp" (Filename.chop_extension src_file) width_suffix in
  let dst = Path.(dst_dir / dst_file) in
  (src_file, dst_file, w, needs_conversion ~preserve dst)

(** {1 Progress Bar Rendering} *)

(** [main_bar total] creates a progress bar for [total] items. *)
let main_bar total =
  let open Progress.Line in
  let style =
    let open Bar_style in
    let open Progress.Color in
    v ~delims:("|", "|") ~color:(hex "#FFBA08") [ "█"; "▓"; "▒"; "░"; " " ]
  in
  list [ bar ~style:(`Custom style) total; ticker_to total ]

(** [main_bar_heading head total] creates a labeled progress display. *)
let main_bar_heading head total =
  let open Progress.Multi in
  line (Progress.Line.const head) ++ line (main_bar total) ++ blank

(** [one_bar total] creates a compact progress bar for individual file processing. *)
let one_bar total =
  let open Progress.Line in
  let style =
    let open Bar_style in
    let open Progress.Color in
    v ~delims:("{", "}") ~color:(ansi `blue) [ "="; ">"; " " ]
  in
  let left = list [ spinner (); bar ~style:(`Custom style) ~width:(`Fixed 12) total; const " " ] in
  pair left string

(** {1 Image Processing} *)

(** [truncate_string str max_len] truncates [str] to [max_len] chars with ellipsis. *)
let truncate_string str max_len =
  if String.length str <= max_len then str
  else if max_len <= 3 then String.sub "..." 0 max_len
  else String.sub str 0 (max_len - 3) ^ "..."

(** [process_file cfg (display, main_rep) src] processes a single source image.

    Converts the image to WebP format at multiple responsive sizes.
    Shows a nested progress bar for files requiring many conversions.

    @return An {!Srcsetter.t} entry with metadata about the generated images. *)
let process_file cfg (display, main_rep) src =
  let w, h = dims cfg src in
  let needed_w = needed_sizes ~img_widths:cfg.img_widths ~w in
  let base_src, base_dst, _, _ as base = translate cfg src in
  let needed = List.map (fun w -> translate cfg ~w src) needed_w in
  let variants =
    needed
    |> List.map (fun (_, dst, _, _) -> (dst, (0, 0)))
    |> Srcsetter.MS.of_list
  in
  let slug = Filename.basename base_dst |> Filename.chop_extension in
  let ent = Srcsetter.v base_dst slug base_src variants (w, h) in
  let todo =
    List.filter_map
      (fun (src, dst, sz, needs_work) ->
        if needs_work then Some (src, dst, Option.value sz ~default:w) else None)
      (base :: needed)
  in
  let num_todo = List.length todo in
  if num_todo > 3 then begin
    let line = one_bar num_todo in
    let reporter = Progress.Display.add_line display line in
    let completed = ref [] in
    let report_progress sz =
      if sz > 0 then completed := sz :: !completed;
      let sizes_str = String.concat "," (List.map string_of_int !completed) in
      let basename = Path.native_exn src |> Filename.basename |> Filename.chop_extension in
      let label = Printf.sprintf "%25s -> %s" (truncate_string basename 25) sizes_str in
      Progress.Reporter.report reporter (1, label)
    in
    report_progress 0;
    List.iter (fun (_, _, sz as job) -> report_progress sz; convert cfg job) todo;
    main_rep 1;
    Progress.Display.remove_line display reporter
  end
  else begin
    List.iter (convert cfg) todo;
    main_rep 1
  end;
  ent

(** {1 Pipeline Execution} *)

let min_interval = Some (Mtime.Span.of_uint64_ns 1000L)

(** [stage1 cfg] scans for images in the source directory.

    Returns a sequence of file paths matching the configured extensions. *)
let stage1 { img_exts; src_dir; _ } =
  let filter f = List.exists (Filename.check_suffix ("." ^ f)) img_exts in
  let fs = file_seq ~filter src_dir in
  let total = Seq.length fs in
  Format.printf "[1/3] Scanned %d images from %a.\n%!" total Path.pp src_dir;
  fs

(** [stage2 cfg fs] processes images, converting to WebP at multiple sizes.

    @return List of {!Srcsetter.t} entries with placeholder dimensions. *)
let stage2 ({ max_fibers; dst_dir; _ } as cfg) fs =
  let display =
    Progress.Display.start
      ~config:(Progress.Config.v ~persistent:false ~min_interval ())
      (main_bar_heading "[2/3] Processing images..." (Seq.length fs))
  in
  let [ _; main_rep ] = Progress.Display.reporters display in
  let ents = ref [] in
  iter_seq_p ~max_fibers
    (fun src ->
      let ent = process_file cfg (display, main_rep) src in
      ents := ent :: !ents)
    fs;
  Progress.Display.finalise display;
  Format.printf "[2/3] Processed %d images to %a.\n%!" (List.length !ents)
    Path.pp dst_dir;
  !ents

(** [stage3 cfg ents] verifies generated images and records their dimensions.

    Regenerates any images that have zero length or fail identify validation.

    @return List of {!Srcsetter.t} entries with actual dimensions. *)
let stage3 ({ src_dir; dst_dir; max_fibers; _ } as cfg) ents =
  let ents_seq = List.to_seq ents in
  let oents = ref [] in
  let regenerated = ref 0 in
  let display =
    Progress.Display.start
      ~config:(Progress.Config.v ~persistent:false ~min_interval ())
      (main_bar_heading "[3/3] Verifying images..." (List.length ents))
  in
  let [ _; rep ] = Progress.Display.reporters display in
  iter_seq_p ~max_fibers
    (fun ent ->
      let src_path = Path.(src_dir / Srcsetter.origin ent) in
      let orig_w, _ = dims cfg src_path in
      (* Verify and regenerate base image if needed *)
      let base_path = Path.(dst_dir / Srcsetter.name ent) in
      if not (is_valid_image cfg base_path) then begin
        incr regenerated;
        convert cfg (Srcsetter.origin ent, Srcsetter.name ent, orig_w)
      end;
      let w, h = dims cfg base_path in
      (* Verify and regenerate variants if needed *)
      let variants =
        Srcsetter.MS.bindings ent.variants
        |> List.map (fun (k, _) ->
            let variant_path = Path.(dst_dir / k) in
            if not (is_valid_image cfg variant_path) then begin
              incr regenerated;
              let target_w = Option.value (width_from_variant_name k) ~default:orig_w in
              convert cfg (Srcsetter.origin ent, k, target_w)
            end;
            (k, dims cfg variant_path))
        |> Srcsetter.MS.of_list
      in
      rep 1;
      oents := { ent with Srcsetter.dims = (w, h); variants } :: !oents)
    ents_seq;
  Progress.Display.finalise display;
  if !regenerated > 0 then
    Printf.printf "[3/3] Verified %d images, regenerated %d invalid outputs.\n%!"
      (List.length ents) !regenerated
  else
    Printf.printf "[3/3] Verified %d generated image sizes.\n%!"
      (List.length ents);
  !oents

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
let run
    ~proc_mgr
    ~src_dir
    ~dst_dir
    ?(idx_file = "index.json")
    ?(img_widths = [ 320; 480; 640; 768; 1024; 1280; 1440; 1600; 1920; 2560; 3840 ])
    ?(img_exts = [ "png"; "webp"; "jpeg"; "jpg"; "bmp"; "heic"; "gif" ])
    ?(max_fibers = 8)
    ?(dummy = false)
    ?(preserve = true)
    ()
  =
  let img_widths = List.sort (fun a b -> compare b a) img_widths in
  let cfg =
    {
      dummy;
      preserve;
      proc_mgr;
      src_dir;
      dst_dir;
      idx_file;
      img_widths;
      img_exts;
      max_fibers;
    }
  in
  let fs = stage1 cfg in
  let ents = stage2 cfg fs in
  let oents = stage3 cfg ents in
  let j = Srcsetter.list_to_json oents |> Result.get_ok in
  let idx = Path.(dst_dir / idx_file) in
  Path.save ~append:false ~create:(`Or_truncate 0o644) idx j;
  oents
