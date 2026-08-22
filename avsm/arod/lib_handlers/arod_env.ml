(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = {
  ctx : Arod.Ctx.t;
  config : Arod.Config.t;
  cache : Proffer.Cache.t;
  now : unit -> float;
  feed : Arod_render.feed -> string;
  pagination :
    collection:string option ->
    offset:int ->
    limit:int ->
    types:string list ->
    (Proffer.Body.Sink.t -> unit);
  search : q:string -> limit:int -> (Proffer.Body.Sink.t -> unit) * int;
  log_search : query:string -> limit:int -> results:int option -> unit;
  read_image : string list -> string option;
  read_paper : string -> string option;
  report : Arod_render.report -> range:string -> string;
}

let create ~ctx ~cache ~search ~log_search ~read_image ~read_paper ~reader ~now
    =
  {
    ctx;
    config = Arod.Ctx.config ctx;
    cache;
    now;
    feed = (fun which -> Arod_render.feed ~ctx which);
    pagination =
      (fun ~collection ~offset ~limit ~types ->
        Arod_render.pagination ~ctx ~collection ~offset ~limit ~types);
    search =
      (fun ~q ~limit ->
        if String.equal q "" then
          ((fun sink -> Proffer.Body.Sink.write sink {|{"results":[]}|}), 0)
        else
          let results = search ~limit q in
          (Arod_render.search ~ctx results, List.length results));
    log_search;
    read_image;
    read_paper;
    report =
      (fun which ~range -> Arod_render.report ~db:(reader ()) which ~range);
  }
