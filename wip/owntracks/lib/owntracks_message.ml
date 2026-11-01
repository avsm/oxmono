(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t =
  | Location of Owntracks_location.t
  | Transition of Owntracks_transition.t
  | Waypoint of Owntracks_waypoint.t
  | Card of Owntracks_card.t
  | Lwt of Owntracks_lwt.t
  | Waypoints of Owntracks_waypoint.t list

let location l = Location l
let transition t = Transition t
let waypoint w = Waypoint w
let card c = Card c
let lwt l = Lwt l
let waypoints ws = Waypoints ws

let jsont : t Jsont.t =
  let case_location =
    Jsont.Object.Case.map "location" Owntracks_location.jsont_bare ~dec:location
  in
  let case_transition =
    Jsont.Object.Case.map "transition" Owntracks_transition.jsont_bare
      ~dec:transition
  in
  let case_waypoint =
    Jsont.Object.Case.map "waypoint" Owntracks_waypoint.jsont_bare ~dec:waypoint
  in
  let waypoints_body =
    Jsont.Object.map ~kind:"waypoints" Fun.id
    |> Jsont.Object.mem "waypoints"
         (Jsont.list Owntracks_waypoint.jsont)
         ~enc:Fun.id
    |> Jsont.Object.skip_unknown |> Jsont.Object.finish
  in
  let case_waypoints =
    Jsont.Object.Case.map "waypoints" waypoints_body ~dec:waypoints
  in
  let case_card =
    Jsont.Object.Case.map "card" Owntracks_card.jsont_bare ~dec:card
  in
  let case_lwt =
    Jsont.Object.Case.map "lwt" Owntracks_lwt.jsont_bare ~dec:lwt
  in
  let enc_case = function
    | Location l -> Jsont.Object.Case.value case_location l
    | Transition t -> Jsont.Object.Case.value case_transition t
    | Waypoint w -> Jsont.Object.Case.value case_waypoint w
    | Card c -> Jsont.Object.Case.value case_card c
    | Lwt l -> Jsont.Object.Case.value case_lwt l
    | Waypoints ws -> Jsont.Object.Case.value case_waypoints ws
  in
  let cases =
    Jsont.Object.Case.
      [
        make case_location;
        make case_transition;
        make case_waypoint;
        make case_waypoints;
        make case_card;
        make case_lwt;
      ]
  in
  Jsont.Object.map ~kind:"message" Fun.id
  |> Jsont.Object.case_mem "_type" Jsont.string ~enc:Fun.id ~enc_case cases
  |> Jsont.Object.skip_unknown |> Jsont.Object.finish

let pp ppf = function
  | Location loc -> Owntracks_location.pp ppf loc
  | Transition tr -> Owntracks_transition.pp ppf tr
  | Waypoint wp -> Owntracks_waypoint.pp ppf wp
  | Card c -> Owntracks_card.pp ppf c
  | Lwt l -> Owntracks_lwt.pp ppf l
  | Waypoints ws -> Format.fprintf ppf "%d waypoints" (List.length ws)

let of_string s = Jsont_bytesrw.decode_string jsont s
let to_string t = Jsont_bytesrw.encode_string ~buf:(Bytes.create 1024) jsont t

let decode (payload : Mqttz.Slice.t @ local) =
  let reader =
    if payload.len = 0 then Bytesrw.Bytes.Reader.of_string ""
    else
      Bytesrw.Bytes.Slice.make payload.bytes ~first:payload.off
        ~length:payload.len
      |> Bytesrw.Bytes.Reader.of_slice
  in
  Jsont_bytesrw.decode jsont reader

let encode value =
  let buffer = Buffer.create 256 in
  let writer = Bytesrw.Bytes.Writer.of_buffer ~slice_length:1024 buffer in
  match Jsont_bytesrw.encode jsont value ~eod:true writer with
  | Error _ as e -> e
  | Ok () -> Ok (Mqttz.Slice.make (Buffer.to_bytes buffer))
