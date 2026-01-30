(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Immich API client for contact face thumbnails *)

let src = Logs.Src.create "bushel.immich" ~doc:"Immich face thumbnails"
module Log = (val Logs.src_log src : Logs.LOG)

(** {1 Types} *)

type person = {
  id : string;
  name : string;
  thumbnail_path : string option;
}

type fetch_result =
  | Ok of string  (** Saved to path *)
  | Skipped of string  (** Already exists *)
  | NotFound of string  (** No match found *)
  | Error of string  (** Error message *)

(** {1 Jsont Codecs} *)

let person_jsont : person Jsont.t =
  let open Jsont in
  let open Object in
  map ~kind:"person" (fun id name thumbnail_path -> { id; name; thumbnail_path })
  |> mem "id" string ~enc:(fun p -> p.id)
  |> mem "name" string ~enc:(fun p -> p.name)
  |> mem "thumbnailPath" (some string) ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun p -> p.thumbnail_path)
  |> finish

let people_jsont = Jsont.list person_jsont

let decode_people json_str =
  match Jsont_bytesrw.decode_string people_jsont json_str with
  | Ok people -> Result.Ok people
  | Error e -> Result.Error e

(** {1 Immich API} *)

let search_person ~http ~endpoint ~api_key name =
  let encoded_name = Uri.pct_encode name in
  let url = Printf.sprintf "%s/api/search/person?name=%s" endpoint encoded_name in
  let header = "X-Api-Key: " ^ api_key in

  match Bushel_http.get_with_header ~http ~header url with
  | Result.Error e -> Result.Error e
  | Result.Ok body -> decode_people body

let download_thumbnail ~http ~endpoint ~api_key person_id output_path =
  let url = Printf.sprintf "%s/api/people/%s/thumbnail" endpoint person_id in
  let header = "X-Api-Key: " ^ api_key in

  match Bushel_http.get_with_header ~http ~header url with
  | Result.Error e -> Result.Error e
  | Result.Ok body ->
    try
      (* Ensure output directory exists *)
      let dir = Filename.dirname output_path in
      if not (Sys.file_exists dir) then
        Unix.mkdir dir 0o755;
      let oc = open_out_bin output_path in
      output_string oc body;
      close_out oc;
      Result.Ok output_path
    with e ->
      Result.Error (Printf.sprintf "Failed to write file: %s" (Printexc.to_string e))

(** {1 Contact Face Fetching} *)

let fetch_face_for_contact ~http ~endpoint ~api_key ~output_dir contact =
  let names = Sortal_schema.Contact.names contact in
  let handle = Sortal_schema.Contact.handle contact in
  let output_path = Filename.concat output_dir (handle ^ ".jpg") in

  (* Skip if already exists *)
  if Sys.file_exists output_path then begin
    Log.debug (fun m -> m "Skipping %s: thumbnail already exists" handle);
    Skipped output_path
  end else begin
    Log.info (fun m -> m "Fetching face for contact: %s" handle);

    (* Try each name until we find a match *)
    let rec try_names = function
      | [] ->
        Log.warn (fun m -> m "No person found for contact %s" handle);
        NotFound handle
      | name :: rest ->
        Log.debug (fun m -> m "Trying name: %s" name);
        match search_person ~http ~endpoint ~api_key name with
        | Result.Error e ->
          Log.err (fun m -> m "Search error for %s: %s" name e);
          Error e
        | Result.Ok [] ->
          Log.debug (fun m -> m "No results for %s, trying next name" name);
          try_names rest
        | Result.Ok (person :: _) ->
          Log.info (fun m -> m "Found match for %s: %s" name person.name);
          match download_thumbnail ~http ~endpoint ~api_key person.id output_path with
          | Result.Ok path -> Ok path
          | Result.Error e -> Error e
    in
    try_names names
  end

let fetch_all_faces ~http ~endpoint ~api_key ~output_dir contacts =
  (* Ensure output directory exists *)
  if not (Sys.file_exists output_dir) then
    Unix.mkdir output_dir 0o755;

  let results = List.map (fun contact ->
    let handle = Sortal_schema.Contact.handle contact in
    let result = fetch_face_for_contact ~http ~endpoint ~api_key ~output_dir contact in
    (handle, result)
  ) contacts in

  (* Summary *)
  let ok_count = List.length (List.filter (fun (_, r) -> match r with Ok _ -> true | _ -> false) results) in
  let skipped_count = List.length (List.filter (fun (_, r) -> match r with Skipped _ -> true | _ -> false) results) in
  let not_found_count = List.length (List.filter (fun (_, r) -> match r with NotFound _ -> true | _ -> false) results) in
  let error_count = List.length (List.filter (fun (_, r) -> match r with Error _ -> true | _ -> false) results) in

  Log.info (fun m -> m "Face sync complete: %d ok, %d skipped, %d not found, %d errors"
    ok_count skipped_count not_found_count error_count);

  results
