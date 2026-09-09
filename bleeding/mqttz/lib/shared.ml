(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Shared MQTT types used by both V3 and V5 implementations.

    This is an internal module. Use {!Mqttz} for the public API. *)

module Qos = struct
  type t = [ `At_most_once | `At_least_once | `Exactly_once ]

  let to_int = function
    | `At_most_once -> 0
    | `At_least_once -> 1
    | `Exactly_once -> 2

  let of_int = function
    | 0 -> `At_most_once
    | 1 -> `At_least_once
    | 2 -> `Exactly_once
    | n -> invalid_arg (Printf.sprintf "Qos.of_int: invalid value %d" n)

  let to_string = function
    | `At_most_once -> "at_most_once"
    | `At_least_once -> "at_least_once"
    | `Exactly_once -> "exactly_once"

  let pp ppf t = Format.fprintf ppf "%s" (to_string t)
end

module Protocol_version = struct
  type t = [ `V3_1_1 | `V5_0 ]

  let to_int = function `V3_1_1 -> 4 | `V5_0 -> 5
  let to_string = function `V3_1_1 -> "3.1.1" | `V5_0 -> "5.0"
  let pp ppf t = Format.fprintf ppf "MQTT %s" (to_string t)
end

module Credentials = struct
  type t = [ `Username of string | `Password of string | `Username_password of string * string ]

  let pp ppf = function
    | `Password _ -> Format.fprintf ppf "Password(<hidden>)"
    | `Username u -> Format.fprintf ppf "Username(%s)" u
    | `Username_password (u, _) ->
        Format.fprintf ppf "Username_password(%s, <hidden>)" u
end

module Will = struct
  type t = { topic : string; payload : string; qos : Qos.t; retain : bool }

  let create ~topic ~payload ~qos ~retain = { topic; payload; qos; retain }
  let topic t = t.topic
  let payload t = t.payload
  let qos t = t.qos
  let retain t = t.retain

  let pp ppf t =
    Format.fprintf ppf "Will{topic=%s; qos=%a; retain=%b}" t.topic Qos.pp t.qos
      t.retain
end

module Packet_id = struct
  type t = int

  let pp ppf t = Format.fprintf ppf "%d" t
end

module Topic = struct
  module Name = struct
    type t = string

    let pp ppf t = Format.fprintf ppf "%s" t

    let[@zero_alloc] validate t =
      Utf8.valid t
      && String.length t > 0
      && String.length t <= 65535
      && not (String.contains t '#' || String.contains t '+')
  end

  module Filter = struct
    type t = string

    let pp ppf t = Format.fprintf ppf "%s" t

    let[@zero_alloc] validate t =
      let len = String.length t in
      let rec scan t len i =
        if i = len then true
        else match t.[i] with
          | '#' -> i = len - 1 && (i = 0 || t.[i - 1] = '/')
          | '+' ->
              (i = 0 || t.[i - 1] = '/')
              && (i = len - 1 || t.[i + 1] = '/')
              && scan t len (i + 1)
          | _ -> scan t len (i + 1)
      in
      len > 0 && len <= 65535 && Utf8.valid t && scan t len 0

    let[@zero_alloc] shared_start filter =
      let len = String.length filter in
      let rec group filter len i =
        if i = len then -1
        else match filter.[i] with
          | '+' | '#' -> -1
          | '/' -> if i > 7 && i + 1 < len then i + 1 else -1
          | _ -> group filter len (i + 1)
      in
      if String.starts_with ~prefix:"$share/" filter then group filter len 7
      else 0

    let[@zero_alloc] validate_shared filter =
      validate filter && shared_start filter >= 0

    let[@zero_alloc] matches ~filter ~topic =
      let flen = String.length filter and tlen = String.length topic in
      let rec level_end text len i =
        if i = len || text.[i] = '/' then i else level_end text len (i + 1)
      in
      let rec equal_level filter topic fi ti count =
        count = 0 || (filter.[fi] = topic.[ti]
          && equal_level filter topic (fi + 1) (ti + 1) (count - 1))
      in
      let rec levels filter topic flen tlen fi ti =
        if fi = flen then ti = tlen
        else if filter.[fi] = '#' then true
        else
          let fend = level_end filter flen fi in
          let tend = level_end topic tlen ti in
          let matches = filter.[fi] = '+' ||
            (fend - fi = tend - ti && equal_level filter topic fi ti (fend - fi)) in
          matches &&
          if fend = flen then tend = tlen
          else if tend = tlen then
            fend + 2 = flen && filter.[fend + 1] = '#'
          else levels filter topic flen tlen (fend + 1) (tend + 1)
      in
      if not (validate_shared filter && Name.validate topic) then false
      else
        let start = shared_start filter in
        if topic.[0] = '$' && (filter.[start] = '+' || filter.[start] = '#') then false
        else levels filter topic flen tlen start 0

  end
end

(** {1 Formatting Helpers} *)

let pp_semi ppf () = Format.fprintf ppf "; "

module Packet_type = struct
  type t =
    [ `RESERVED
    | `CONNECT
    | `CONNACK
    | `PUBLISH
    | `PUBACK
    | `PUBREC
    | `PUBREL
    | `PUBCOMP
    | `SUBSCRIBE
    | `SUBACK
    | `UNSUBSCRIBE
    | `UNSUBACK
    | `PINGREQ
    | `PINGRESP
    | `DISCONNECT
    | `AUTH ]

  let to_int = function
    | `RESERVED -> 0
    | `CONNECT -> 1
    | `CONNACK -> 2
    | `PUBLISH -> 3
    | `PUBACK -> 4
    | `PUBREC -> 5
    | `PUBREL -> 6
    | `PUBCOMP -> 7
    | `SUBSCRIBE -> 8
    | `SUBACK -> 9
    | `UNSUBSCRIBE -> 10
    | `UNSUBACK -> 11
    | `PINGREQ -> 12
    | `PINGRESP -> 13
    | `DISCONNECT -> 14
    | `AUTH -> 15

  let of_int = function
    | 0 -> `RESERVED
    | 1 -> `CONNECT
    | 2 -> `CONNACK
    | 3 -> `PUBLISH
    | 4 -> `PUBACK
    | 5 -> `PUBREC
    | 6 -> `PUBREL
    | 7 -> `PUBCOMP
    | 8 -> `SUBSCRIBE
    | 9 -> `SUBACK
    | 10 -> `UNSUBSCRIBE
    | 11 -> `UNSUBACK
    | 12 -> `PINGREQ
    | 13 -> `PINGRESP
    | 14 -> `DISCONNECT
    | 15 -> `AUTH
    | n -> invalid_arg (Printf.sprintf "Packet_type.of_int: invalid value %d" n)

  let to_string = function
    | `RESERVED -> "RESERVED"
    | `CONNECT -> "CONNECT"
    | `CONNACK -> "CONNACK"
    | `PUBLISH -> "PUBLISH"
    | `PUBACK -> "PUBACK"
    | `PUBREC -> "PUBREC"
    | `PUBREL -> "PUBREL"
    | `PUBCOMP -> "PUBCOMP"
    | `SUBSCRIBE -> "SUBSCRIBE"
    | `SUBACK -> "SUBACK"
    | `UNSUBSCRIBE -> "UNSUBSCRIBE"
    | `UNSUBACK -> "UNSUBACK"
    | `PINGREQ -> "PINGREQ"
    | `PINGRESP -> "PINGRESP"
    | `DISCONNECT -> "DISCONNECT"
    | `AUTH -> "AUTH"

  let pp ppf t = Format.fprintf ppf "%s" (to_string t)
end
