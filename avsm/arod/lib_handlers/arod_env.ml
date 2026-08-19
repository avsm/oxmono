(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = {
  ctx : Arod.Ctx.t;
  cache : Proffer.Cache.t;
  search : limit:int -> string -> Arod_search.result list;
  read_image : string list -> string option;
  read_paper : string -> string option;
  reader : unit -> Sqlite3_eio.t;
  now : unit -> float;
}
