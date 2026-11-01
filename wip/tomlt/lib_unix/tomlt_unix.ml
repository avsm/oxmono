(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Bytes = Bytesrw.Bytes

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

(* Channel-based I/O *)
let of_channel ?file ic =
  let r = Bytes.Reader.of_in_channel ic in
  Tomlt_bytesrw.parse_reader ?file r

let to_channel oc value =
  let w = Bytes.Writer.of_out_channel oc in
  Tomlt_bytesrw.to_writer w value

(* File-based I/O *)
let of_file path =
  let ic = open_in path in
  Fun.protect ~finally:(fun () -> close_in ic)
    (fun () -> of_channel ~file:path ic)

let to_file path value =
  let oc = open_out path in
  Fun.protect ~finally:(fun () -> close_out oc)
    (fun () -> to_channel oc value)

(* Codec-based file operations *)
let decode_file codec path =
  let toml = of_file path in
  Tomlt.decode codec toml

let decode_file_exn codec path =
  let toml = of_file path in
  Tomlt.decode_exn codec toml

let encode_file codec value path =
  let toml = Tomlt.encode codec value in
  to_file path toml

(* Pre-configured ptime codecs with system timezone *)
let ptime ?frac_s () =
  Tomlt.ptime ?frac_s ~get_tz:current_tz_offset_s ~now ()

let ptime_full () =
  Tomlt.ptime_full ~get_tz:current_tz_offset_s ()
