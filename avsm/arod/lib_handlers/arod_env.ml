(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = {
  config : Arod.Config.t;
  cache : Proffer.Cache.t;
  now : unit -> float;
  listing : Arod_render.listing -> Arod_render.flavour -> string;
  entry : Arod_render.entry_kind -> string -> Arod_render.flavour -> string;
  entry_markdown : string -> string option;
  paper_bib : string -> string option;
  feed : Arod_render.feed -> string;
  sitemap : unit -> string;
  blogroll : unit -> string;
  pagination :
    collection:string option ->
    offset:int ->
    limit:int ->
    types:string list ->
    string;
  search : q:string -> limit:int -> string;
  read_image : string list -> string option;
  read_paper : string -> string option;
  report : Arod_render.report -> range:string -> string;
}

let create ~ctx ~cache ~search ~read_image ~read_paper ~reader ~now =
  {
    config = Arod.Ctx.config ctx;
    cache;
    now;
    listing = (fun which flavour -> Arod_render.listing ~ctx which flavour);
    entry = (fun kind slug flavour -> Arod_render.entry ~ctx kind slug flavour);
    entry_markdown = (fun slug -> Arod_render.entry_markdown ~ctx slug);
    paper_bib = (fun slug -> Arod_render.paper_bib ~ctx slug);
    feed = (fun which -> Arod_render.feed ~ctx which);
    sitemap = (fun () -> Arod_render.sitemap ~ctx);
    blogroll = (fun () -> Arod_render.blogroll ~ctx);
    pagination =
      (fun ~collection ~offset ~limit ~types ->
        Arod_render.pagination ~ctx ~collection ~offset ~limit ~types);
    search =
      (fun ~q ~limit ->
        if String.equal q "" then {|{"results":[]}|}
        else Arod_render.search ~ctx (search ~limit q));
    read_image;
    read_paper;
    report =
      (fun which ~range -> Arod_render.report ~db:(reader ()) which ~range);
  }
