(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Theme bootstrap for the Arod site.

    The stylesheet itself is built from [tailwind/site.css] into
    [assets/tw.css] by [tailwind/regen.sh]. This module holds only the
    script that applies the stored colour scheme before first paint. *)

(** [theme_init_js] adds the [dark] class to the document element when the
    stored or preferred scheme is dark. It runs synchronously in [<head>],
    before the stylesheet, so a dark page never flashes light. *)

let theme_init_js = {|
(function(){
  var t = localStorage.getItem('theme');
  if (t === 'dark' || (!t && matchMedia('(prefers-color-scheme:dark)').matches)) {
    document.documentElement.classList.add('dark');
  }
})();
|}
