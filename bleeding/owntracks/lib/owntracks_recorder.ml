(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let locations_jsont : Owntracks_location.t list Jsont.t =
  Jsont.list Owntracks_location.jsont_bare

let locations_data_jsont : Owntracks_location.t list Jsont.t =
  let make data = data in
  Jsont.Object.map ~kind:"data_response" make
  |> Jsont.Object.mem "data" locations_jsont ~enc:Fun.id
  |> Jsont.Object.skip_unknown |> Jsont.Object.finish

let string_list_jsont : string list Jsont.t = Jsont.list Jsont.string

let string_list_results_jsont : string list Jsont.t =
  let make results = results in
  Jsont.Object.map ~kind:"results_response" make
  |> Jsont.Object.mem "results" string_list_jsont ~enc:Fun.id
  |> Jsont.Object.skip_unknown |> Jsont.Object.finish

let locations_response_jsont =
  Jsont.any ~kind:"Recorder locations" ~dec_array:locations_jsont
    ~dec_object:locations_data_jsont
    ~enc:(fun _ -> locations_data_jsont)
    ()

let list_response_jsont =
  Jsont.any ~kind:"Recorder list" ~dec_array:string_list_jsont
    ~dec_object:string_list_results_jsont
    ~enc:(fun _ -> string_list_results_jsont)
    ()

let decode_locations reader =
  Jsont_bytesrw.decode locations_response_jsont reader

let decode_list reader = Jsont_bytesrw.decode list_response_jsont reader
