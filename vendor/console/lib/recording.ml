(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type output = {
  ppf : Format.formatter;
  out : Format.formatter_out_functions;
  pending : Buffer.t;
}

type t = {
  file : string;
  oc : out_channel;
  mutable outputs : output list;
  mutable time : unit -> float;
  mutable last : float;
  mutable cr : bool;
}

let current = ref None

(* No terminal has more than the 4096 columns or rows a display takes. *)
let dimension name ~default =
  match Option.bind (Sys.getenv_opt name) int_of_string_opt with
  | Some n when n > 0 && n <= 4096 -> n
  | _ -> default

let size () = (dimension "COLUMNS" ~default:80, dimension "LINES" ~default:24)

(* [data] as a terminal shows it: its driver puts a carriage return before
   each line feed that has none. *)
let shown t data =
  let b = Buffer.create (String.length data + 16) in
  String.iter
    (fun c ->
      if c = '\n' && not t.cr then Buffer.add_char b '\r';
      Buffer.add_char b c;
      t.cr <- c = '\r')
    data;
  Buffer.contents b

(* The time never goes back, though the clock of a later run starts again
   at zero. *)
let emit t data =
  let time = Float.max t.last (t.time ()) in
  t.last <- time;
  let data = shown t data in
  output_string t.oc (Cast.to_line Cast.event { time; data });
  flush t.oc

(* Each flush is a write, cut between two characters: the bytes of one cut
   short wait for those that end it. *)
let written t o =
  let all = Buffer.contents o.pending in
  match Console_text.boundary all with
  | 0 -> ()
  | k ->
      Buffer.clear o.pending;
      Buffer.add_substring o.pending all k (String.length all - k);
      emit t (String.sub all 0 k)

let record t ppf =
  if not (List.exists (fun o -> o.ppf == ppf) t.outputs) then begin
    Format.pp_print_flush ppf ();
    let o =
      {
        ppf;
        out = Format.pp_get_formatter_out_functions ppf ();
        pending = Buffer.create 256;
      }
    in
    let blank n =
      if n > 0 then Buffer.add_string o.pending (String.make n ' ')
    in
    Format.pp_set_formatter_out_functions ppf
      {
        o.out with
        out_string = Buffer.add_substring o.pending;
        out_flush = (fun () -> written t o);
        out_newline = (fun () -> Buffer.add_char o.pending '\n');
        out_spaces = blank;
        out_indent = blank;
      };
    t.outputs <- o :: t.outputs
  end

let stop t =
  List.iter
    (fun o ->
      Format.pp_print_flush o.ppf ();
      if Buffer.length o.pending > 0 then emit t (Buffer.contents o.pending);
      Format.pp_set_formatter_out_functions o.ppf o.out)
    t.outputs;
  close_out t.oc

(* A recording asked for and impossible ends the program before it runs,
   as a bad option would. *)
let open_cast file =
  let oc =
    try open_out_bin file
    with Sys_error e ->
      Printf.eprintf "%s: CONSOLE_CAST: cannot record: %s\n%!"
        (Filename.basename Sys.executable_name)
        e;
      exit 1
  in
  let width, height = size () in
  let term = match Sys.getenv_opt "TERM" with Some "" -> None | t -> t in
  output_string oc (Cast.to_line Cast.header { width; height; term });
  flush oc;
  { file; oc; outputs = []; time = (fun () -> 0.); last = 0.; cr = false }

let start ~time ppf =
  let file = Cast.file () in
  (match !current with
  | Some t when Some t.file = file -> ()
  | last ->
      Option.iter stop last;
      current := Option.map open_cast file);
  match !current with
  | None -> false
  | Some t ->
      t.time <- time;
      let std = ppf == Fmt.stdout || ppf == Fmt.stderr in
      List.iter (record t) (if std then [ Fmt.stdout; Fmt.stderr ] else [ ppf ]);
      true
