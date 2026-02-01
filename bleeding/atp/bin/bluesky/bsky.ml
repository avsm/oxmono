(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Bluesky CLI using cmdliner with xrpc_auth for session management. *)

open Cmdliner
module CreateRecord = Atp_lexicon_atproto.Com.Atproto.Repo.CreateRecord
module Richtext = Atp_lexicon_bsky.App.Bsky.Richtext
module Embed = Atp_lexicon_bsky.App.Bsky.Embed
module Post = Atp_lexicon_bsky.App.Bsky.Feed.Post

let app_name = "bsky"
let version = "0.1.0"

(* Blob upload response *)

type blob_response = { blob : Atp.Blob_ref.t }

let blob_response_jsont =
  Jsont.Object.map ~kind:"BlobResponse" (fun blob -> { blob })
  |> Jsont.Object.mem "blob" Atp.Blob_ref.jsont ~enc:(fun r -> r.blob)
  |> Jsont.Object.finish

(* Upload image and return blob ref *)
let upload_image client ~image_path =
  let content = Eio.Path.load image_path in
  let content_type =
    let ext = Filename.extension (snd image_path) |> String.lowercase_ascii in
    match ext with
    | ".jpg" | ".jpeg" -> "image/jpeg"
    | ".png" -> "image/png"
    | ".gif" -> "image/gif"
    | ".webp" -> "image/webp"
    | _ -> "application/octet-stream"
  in
  let response =
    Xrpc.Client.procedure_blob client ~nsid:"com.atproto.repo.uploadBlob"
      ~params:[] ~blob:content ~content_type ~decoder:blob_response_jsont
  in
  response.blob

(* Helper to encode a typed value to Jsont.json *)
let to_json jsont value =
  match Jsont_bytesrw.encode_string jsont value with
  | Ok s -> (
      match Jsont_bytesrw.decode_string Jsont.json s with
      | Ok json -> json
      | Error e -> failwith ("Failed to decode JSON: " ^ e))
  | Error e -> failwith ("Failed to encode JSON: " ^ e)

(* Facet helpers using generated lexicon types *)

let make_link_feature uri : Jsont.json =
  to_json Richtext.Facet.link_jsont { uri }

let make_tag_feature tag : Jsont.json = to_json Richtext.Facet.tag_jsont { tag }

let make_facet ~byte_start ~byte_end features : Richtext.Facet.main =
  let index : Richtext.Facet.byte_slice = { byte_start; byte_end } in
  { index; features }

(* Image embed helpers using generated lexicon types *)

let make_image_embed ~blob_ref ~alt : Jsont.json =
  let image : Embed.Images.image =
    { image = blob_ref; alt; aspect_ratio = None }
  in
  to_json Embed.Images.image_jsont image

let make_images_embed images : Jsont.json =
  let embed : Embed.Images.main = { images } in
  to_json Embed.Images.main_jsont embed

(* Post creation using generated lexicon types *)

let create_post_record ~text ~langs ~facets ~embed : Jsont.json =
  let now = Unix.gettimeofday () in
  let tm = Unix.gmtime now in
  let created_at =
    Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d.000Z" (1900 + tm.tm_year)
      (1 + tm.tm_mon) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec
  in
  let post : Post.main =
    {
      text;
      entities = None;
      facets;
      reply = None;
      embed;
      langs;
      labels = None;
      tags = None;
      created_at;
    }
  in
  to_json Post.main_jsont post

(* Find URL in text and return byte positions *)
let find_url_in_text text url =
  let re = Re.str url |> Re.compile in
  match Re.exec_opt re text with
  | Some group ->
      let byte_start, byte_end = Re.Group.offset group 0 in
      Some (byte_start, byte_end)
  | None -> None

(* Helper to load session and create API *)
let with_api ?profile env f =
  Eio.Switch.run @@ fun sw ->
  let fs = env#fs in
  match Xrpc_auth.Session.load fs ~app_name ?profile () with
  | None ->
      let profile_name =
        match profile with
        | Some p -> p
        | None -> Xrpc_auth.Session.get_current_profile fs ~app_name
      in
      Fmt.epr "Not logged in (profile: %s). Use 'bsky auth login' first.@."
        profile_name;
      exit 1
  | Some session ->
      let api =
        Xrpc_auth.Client.create ~sw ~env ~app_name ?profile ~pds:session.pds ()
      in
      Xrpc_auth.Client.resume api ~session;
      f api

let do_post ~api ~text ~langs ~link ~image ~image_alt ~hashtags =
  let client = Xrpc_auth.Client.get_client api in
  let fs = Xrpc_auth.Client.get_fs api in
  let did = Xrpc_auth.Client.get_did api in

  (* Build the final text with hashtags appended *)
  let hashtag_suffix =
    match hashtags with
    | [] -> ""
    | tags ->
        let tag_strings =
          List.map
            (fun t -> if String.starts_with ~prefix:"#" t then t else "#" ^ t)
            tags
        in
        " " ^ String.concat " " tag_strings
  in
  let full_text = text ^ hashtag_suffix in

  (* Build facets *)
  let facets = ref [] in

  (* Add link facet if URL is in text *)
  (match link with
  | Some url -> (
      match find_url_in_text full_text url with
      | Some (byte_start, byte_end) ->
          let feature = make_link_feature url in
          let facet = make_facet ~byte_start ~byte_end [ feature ] in
          facets := facet :: !facets
      | None ->
          (* URL not in text - add it at the end *)
          Fmt.pr "Note: Link not found in text, won't be clickable.@.")
  | None -> ());

  (* Add hashtag facets *)
  let base_len = String.length text in
  let current_pos =
    ref (if hashtag_suffix = "" then base_len else base_len + 1)
  in
  List.iter
    (fun tag ->
      let tag_with_hash =
        if String.starts_with ~prefix:"#" tag then tag else "#" ^ tag
      in
      let tag_without_hash =
        if String.starts_with ~prefix:"#" tag then
          String.sub tag 1 (String.length tag - 1)
        else tag
      in
      let byte_start = !current_pos in
      let byte_end = byte_start + String.length tag_with_hash in
      let feature = make_tag_feature tag_without_hash in
      let facet = make_facet ~byte_start ~byte_end [ feature ] in
      facets := facet :: !facets;
      current_pos := byte_end + 1 (* +1 for space *))
    hashtags;

  let facets_opt = match !facets with [] -> None | f -> Some (List.rev f) in

  (* Upload image if provided *)
  let embed =
    match image with
    | Some image_path ->
        let path = Eio.Path.(fs / image_path) in
        let blob_ref = upload_image client ~image_path:path in
        let alt = Option.value ~default:"" image_alt in
        let img = make_image_embed ~blob_ref ~alt in
        Some (make_images_embed [ img ])
    | None -> None
  in

  let langs_opt = match langs with [] -> None | l -> Some l in
  let record =
    create_post_record ~text:full_text ~langs:langs_opt ~facets:facets_opt
      ~embed
  in
  let input =
    {
      CreateRecord.repo = did;
      collection = "app.bsky.feed.post";
      rkey = None;
      validate = Some true;
      record;
      swap_commit = None;
    }
  in
  let output =
    Xrpc.Client.procedure client ~nsid:"com.atproto.repo.createRecord"
      ~params:[] ~input:(Some CreateRecord.input_jsont) ~input_data:(Some input)
      ~decoder:CreateRecord.output_jsont
  in
  Fmt.pr "Post created successfully!@.";
  Fmt.pr "  URI: %s@." output.uri;
  Fmt.pr "  CID: %s@." output.cid;
  output

(* CLI arguments *)

let text_arg =
  let doc = "Text content of the post (max 300 graphemes)." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"TEXT" ~doc)

let langs_arg =
  let doc = "Language codes for the post (e.g., en, ja)." in
  Arg.(value & opt_all string [] & info [ "l"; "lang" ] ~docv:"LANG" ~doc)

let link_arg =
  let doc =
    "URL to make clickable in the post. The URL must appear in the text."
  in
  Arg.(value & opt (some string) None & info [ "link" ] ~docv:"URL" ~doc)

let image_arg =
  let doc =
    "Path to an image file to attach to the post (jpg, png, gif, webp)."
  in
  Arg.(value & opt (some string) None & info [ "image"; "i" ] ~docv:"FILE" ~doc)

let image_alt_arg =
  let doc = "Alt text for the image (for accessibility)." in
  Arg.(
    value & opt (some string) None & info [ "image-alt"; "a" ] ~docv:"TEXT" ~doc)

let hashtag_arg =
  let doc =
    "Hashtag to append to the post (can be specified multiple times). Will be \
     appended to the end of the text."
  in
  Arg.(value & opt_all string [] & info [ "tag"; "t" ] ~docv:"TAG" ~doc)

(* Post command *)

let post_action ~text ~langs ~link ~image ~image_alt ~hashtags ~profile env =
  with_api ?profile env @@ fun api ->
  let _ = do_post ~api ~text ~langs ~link ~image ~image_alt ~hashtags in
  ()

let post_cmd =
  let doc = "Post to Bluesky." in
  let info = Cmd.info "post" ~doc in
  let post' text langs link image image_alt hashtags profile =
    Eio_main.run @@ fun env ->
    post_action ~text ~langs ~link ~image ~image_alt ~hashtags ~profile env
  in
  Cmd.v info
    Term.(
      const post' $ text_arg $ langs_arg $ link_arg $ image_arg $ image_alt_arg
      $ hashtag_arg $ Xrpc_auth.Cmd.profile_arg)

(* Main command *)

let main_cmd =
  let doc = "Bluesky CLI - post and interact with Bluesky" in
  let man =
    [
      `S Manpage.s_description;
      `P "A command-line interface for Bluesky using AT Protocol.";
      `P "Configuration is stored in ~/.config/bsky/ with profile support.";
      `P
        "Sessions are stored per-profile in \
         ~/.config/bsky/profiles/<profile>/session.json.";
      `S "EXAMPLES";
      `P "Login:";
      `Pre "  bsky auth login alice.bsky.social";
      `P "Post a simple message:";
      `Pre "  bsky post \"Hello, Bluesky!\"";
      `P "Post with a clickable link:";
      `Pre
        "  bsky post \"Check out https://example.com\" --link \
         https://example.com";
      `P "Post with an image:";
      `Pre
        "  bsky post \"Look at this photo\" --image photo.jpg --image-alt \"A \
         nice photo\"";
      `P "Post with hashtags:";
      `Pre
        "  bsky post \"My thoughts on OCaml\" --tag OCaml --tag \
         FunctionalProgramming";
      `P "Work with multiple accounts:";
      `Pre "  bsky auth login alice.bsky.social";
      `Pre "  bsky auth login bob.example.com";
      `Pre "  bsky auth profile list";
      `Pre "  bsky auth profile switch bob.example.com";
      `Pre "  bsky post \"From my other account\" --profile alice.bsky.social";
      `S Manpage.s_bugs;
      `P "Report bugs at https://github.com/avsm/ocaml-atp/issues";
    ]
  in
  let info = Cmd.info "bsky" ~version ~doc ~man in
  let default_action text langs link image image_alt hashtags profile =
    Eio_main.run @@ fun env ->
    post_action ~text ~langs ~link ~image ~image_alt ~hashtags ~profile env
  in
  let default =
    Term.(
      const default_action $ text_arg $ langs_arg $ link_arg $ image_arg
      $ image_alt_arg $ hashtag_arg $ Xrpc_auth.Cmd.profile_arg)
  in
  Cmd.group info ~default [ post_cmd; Xrpc_auth.Cmd.auth_cmd ~app_name () ]

let () =
  Fmt_tty.setup_std_outputs ();
  exit (Cmd.eval main_cmd)
