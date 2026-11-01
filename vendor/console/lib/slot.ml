(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type 'a op = Get of 'a option ref | Set of 'a

type 'a t = {
  wrap : 'a op -> Format.stag;
  unwrap : Format.stag -> 'a op option;
  setting : bool;
  watched : bool;
}

let v (type a) ?(setting = true) ?(watched = false) () : a t =
  let module M = struct
    type Format.stag += Op of a op
  end in
  {
    wrap = (fun op -> M.Op op);
    unwrap = (function M.Op op -> Some op | _ -> None);
    setting;
    watched;
  }

type mark = Format.stag -> string

(* A slot's tag function answers a probe with itself and whether nothing
   but slots that are not settings lies under it. *)
type Format.stag += Probe of (mark * bool) option ref

let format_marks =
  (Format.pp_get_formatter_stag_functions
     (Format.formatter_of_buffer (Buffer.create 0))
     ())
    .mark_open_stag

let unset_marks (f : mark) =
  f == format_marks
  ||
  let r = ref None in
  ignore (f (Probe r));
  match !r with Some (g, unset) -> g == f && unset | None -> false

let unset ppf =
  unset_marks (Format.pp_get_formatter_stag_functions ppf ()).mark_open_stag

(* What the tag function of a slot answers, which no other answers. *)
let ack = "\000console.slot"

let ask s ppf op =
  let funs = Format.pp_get_formatter_stag_functions ppf () in
  String.equal (funs.mark_open_stag (s.wrap op)) ack

let find s ppf =
  let r = ref None in
  ignore (ask s ppf (Get r));
  !r

let sets = Atomic.make 0
let generation () = Atomic.get sets

let set s ppf v =
  if s.watched then Atomic.incr sets;
  if not (ask s ppf (Set v)) then begin
    let current = ref v in
    let funs = Format.pp_get_formatter_stag_functions ppf () in
    let other = funs.mark_open_stag in
    let unset = (not s.setting) && unset_marks other in
    let rec mark_open_stag stag =
      match stag with
      | Probe r ->
          r := Some (mark_open_stag, unset);
          ack
      | stag -> (
          match s.unwrap stag with
          | Some (Get r) ->
              r := Some !current;
              ack
          | Some (Set v) ->
              current := v;
              ack
          | None -> other stag)
    in
    Format.pp_set_formatter_stag_functions ppf { funs with mark_open_stag }
  end
