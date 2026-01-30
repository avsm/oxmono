(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Error = Tomlt.Toml.Error

type Eio.Exn.err += E of Error.t

let err e = Eio.Exn.create (E e)

let () =
  Eio.Exn.register_pp (fun f -> function
    | E e ->
      Format.fprintf f "Toml %a" Error.pp e;
      true
    | _ -> false
  )

let wrap_error f =
  try f ()
  with Error.Error e ->
    raise (err e)

let parse ?file input =
  try Tomlt_bytesrw.parse input
  with Error.Error e ->
    let bt = Printexc.get_raw_backtrace () in
    let eio_exn = err e in
    let eio_exn = match file with
      | Some f -> Eio.Exn.add_context eio_exn "parsing %s" f
      | None -> eio_exn
    in
    Printexc.raise_with_backtrace eio_exn bt

let of_flow ?file flow =
  let input = Eio.Flow.read_all flow in
  parse ?file input

let of_path ~fs path =
  let file = Eio.Path.(/) fs path |> Eio.Path.native_exn in
  Eio.Path.load (Eio.Path.(/) fs path)
  |> parse ~file

let of_file path =
  let file = Eio.Path.native_exn path in
  Eio.Path.load path |> parse ~file

let to_flow flow value =
  let buf = Buffer.create 256 in
  Tomlt_bytesrw.to_writer (Bytesrw.Bytes.Writer.of_buffer buf) value;
  Eio.Flow.copy_string (Buffer.contents buf) flow

let to_path ~fs path value =
  Eio.Path.save ~create:(`Or_truncate 0o644) (Eio.Path.(/) fs path)
    (let buf = Buffer.create 256 in
     Tomlt_bytesrw.to_writer (Bytesrw.Bytes.Writer.of_buffer buf) value;
     Buffer.contents buf)

let to_file path value =
  Eio.Path.save ~create:(`Or_truncate 0o644) path
    (let buf = Buffer.create 256 in
     Tomlt_bytesrw.to_writer (Bytesrw.Bytes.Writer.of_buffer buf) value;
     Buffer.contents buf)

(* Codec-based operations *)
let decode_flow ?file codec flow =
  let toml = of_flow ?file flow in
  Tomlt.decode codec toml

let decode_flow_exn ?file codec flow =
  let toml = of_flow ?file flow in
  wrap_error (fun () -> Tomlt.decode_exn codec toml)

let decode_path codec ~fs path =
  let toml = of_path ~fs path in
  Tomlt.decode codec toml

let decode_path_exn codec ~fs path =
  let toml = of_path ~fs path in
  wrap_error (fun () -> Tomlt.decode_exn codec toml)

let decode_file codec path =
  let toml = of_file path in
  Tomlt.decode codec toml

let decode_file_exn codec path =
  let toml = of_file path in
  wrap_error (fun () -> Tomlt.decode_exn codec toml)

let encode_flow codec value flow =
  let toml = Tomlt.encode codec value in
  to_flow flow toml

let encode_path codec value ~fs path =
  let toml = Tomlt.encode codec value in
  to_path ~fs path toml

let encode_file codec value path =
  let toml = Tomlt.encode codec value in
  to_file path toml

(* Time utilities *)
let current_tz_offset_s = Ptime_clock.current_tz_offset_s
let now = Ptime_clock.now

let today_date ?tz_offset_s () =
  let tz_offset_s =
    tz_offset_s
    |> Option.fold ~none:(current_tz_offset_s ()) ~some:Option.some
    |> Option.value ~default:0
  in
  Ptime.to_date ~tz_offset_s (now ())

(* Pre-configured ptime codecs with system timezone *)
let ptime ?frac_s () =
  Tomlt.ptime ?frac_s ~get_tz:current_tz_offset_s ~now ()

let ptime_full () =
  Tomlt.ptime_full ~get_tz:current_tz_offset_s ()
