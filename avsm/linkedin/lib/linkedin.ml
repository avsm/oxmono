(*---------------------------------------------------------------------------
  Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let src = Logs.Src.create "linkedin" ~doc:"LinkedIn API client"
module Log = (val Logs.src_log src : Logs.LOG)

(* The REST Posts API endpoint (replaces the older v2/ugcPosts) *)
let posts_url = "https://api.linkedin.com/rest/posts"
let userinfo_url = "https://api.linkedin.com/v2/userinfo"

type t = {
  session : Requests.t;
}

let create ~session ~access_token =
  let session = Requests.set_auth session
    (Requests.Auth.bearer ~token:access_token) in
  let session = Requests.set_default_header session
    "LinkedIn-Version" "202603" in
  let session = Requests.set_default_header session
    "X-Restli-Protocol-Version" "2.0.0" in
  { session }

(* JSON helpers using Jsont.Json constructors *)

module J = struct
  let n s = Jsont.Json.name s
  let s v = Jsont.Json.string v
  let b v = Jsont.Json.bool v
  let obj mems = Jsont.Json.object' mems
  let mem k v = Jsont.Json.mem (n k) v
  let list_of vs = Jsont.Json.list vs

  let encode json =
    match Jsont_bytesrw.encode_string Jsont.json json with
    | Ok s -> s
    | Error e -> failwith e

  let decode s =
    match Jsont_bytesrw.decode_string Jsont.json s with
    | Ok j -> j
    | Error e -> failwith e

  let get_string name json =
    match json with
    | Jsont.Object (mems, _) ->
      (match List.find_opt (fun ((k, _), _) -> k = name) mems with
       | Some (_, Jsont.String (v, _)) -> Some v
       | _ -> None)
    | _ -> None

  let get_string_exn name json =
    match get_string name json with
    | Some v -> v
    | None -> failwith (Fmt.str "Missing field %S in JSON response" name)
end

(* --- User Info --- *)

type userinfo = {
  sub : string;
  name : string;
  given_name : string;
  family_name : string;
  picture : string option;
  email : string option;
  person_urn : string;
}

let userinfo t =
  Log.debug (fun m -> m "GET %s" userinfo_url);
  let resp = Requests.get t.session userinfo_url in
  let status = Requests.Response.status_code resp in
  let body = Requests.Response.text resp in
  Log.debug (fun m -> m "Response %d: %s" status body);
  if status < 200 || status >= 300 then
    failwith (Fmt.str "LinkedIn userinfo error (HTTP %d): %s" status body);
  let json = J.decode body in
  let sub = J.get_string_exn "sub" json in
  let name = J.get_string_exn "name" json in
  let given_name = J.get_string_exn "given_name" json in
  let family_name = J.get_string_exn "family_name" json in
  let picture = J.get_string "picture" json in
  let email = J.get_string "email" json in
  let person_urn = "urn:li:person:" ^ sub in
  { sub; name; given_name; family_name; picture; email; person_urn }

(* --- Posting --- *)

type visibility = Connections | Public

type feed_distribution = Main_feed | None_

type post_result = {
  id : string;
}

let visibility_str = function
  | Connections -> "CONNECTIONS"
  | Public -> "PUBLIC"

let feed_distribution_str = function
  | Main_feed -> "MAIN_FEED"
  | None_ -> "NONE"

(* Build the common base fields for any post *)
let base_fields ~author ?(visibility = Public)
    ?(distribution = Main_feed) ?(reshare_disabled = false) commentary =
  [ J.mem "author" (J.s author);
    J.mem "commentary" (J.s commentary);
    J.mem "visibility" (J.s (visibility_str visibility));
    J.mem "distribution" (J.obj [
      J.mem "feedDistribution" (J.s (feed_distribution_str distribution));
      J.mem "targetEntities" (J.list_of []);
      J.mem "thirdPartyDistributionChannels" (J.list_of []);
    ]);
    J.mem "lifecycleState" (J.s "PUBLISHED");
    J.mem "isReshareDisabledByAuthor" (J.b reshare_disabled);
  ]

let text_body ~author ?visibility ?distribution ?reshare_disabled commentary =
  J.obj (base_fields ~author ?visibility ?distribution
           ?reshare_disabled commentary)

let article_body ~author ~url ?title ?description ?thumbnail
    ?visibility ?distribution ?reshare_disabled commentary =
  let article_mems =
    [ J.mem "source" (J.s url) ]
    @ (match title with
       | Some t -> [J.mem "title" (J.s t)]
       | None -> [])
    @ (match description with
       | Some d -> [J.mem "description" (J.s d)]
       | None -> [])
    @ (match thumbnail with
       | Some t -> [J.mem "thumbnail" (J.s t)]
       | None -> [])
  in
  let fields = base_fields ~author ?visibility ?distribution
    ?reshare_disabled commentary in
  let content = J.mem "content" (J.obj [
    J.mem "article" (J.obj article_mems);
  ]) in
  J.obj (fields @ [content])

let do_post t json =
  let body_str = J.encode json in
  Log.debug (fun m -> m "POST %s@.Body: %s" posts_url body_str);
  let resp = Requests.post t.session
    ~body:(Requests.Body.of_string Requests.Mime.json body_str)
    posts_url
  in
  let status = Requests.Response.status_code resp in
  let body = Requests.Response.text resp in
  Log.debug (fun m -> m "Response %d: %s" status body);
  if status = 201 then begin
    let id = match Requests.Response.header_string "x-restli-id" resp with
      | Some id -> id
      | None -> "(created, no id returned)"
    in
    Ok { id }
  end else
    Error (Fmt.str "LinkedIn API error (HTTP %d): %s" status body)

let post_text t ~author ?visibility ?distribution ?reshare_disabled
    commentary =
  let json = text_body ~author ?visibility ?distribution
    ?reshare_disabled commentary in
  do_post t json

let post_article t ~author ~url ?title ?description ?thumbnail
    ?visibility ?distribution ?reshare_disabled commentary =
  let json = article_body ~author ~url ?title ?description ?thumbnail
    ?visibility ?distribution ?reshare_disabled commentary in
  do_post t json

(* --- Dry-Run / Inspection --- *)

let post_text_json ~author ?visibility ?distribution ?reshare_disabled
    commentary =
  let json = text_body ~author ?visibility ?distribution
    ?reshare_disabled commentary in
  J.encode json

let post_article_json ~author ~url ?title ?description ?thumbnail
    ?visibility ?distribution ?reshare_disabled commentary =
  let json = article_body ~author ~url ?title ?description ?thumbnail
    ?visibility ?distribution ?reshare_disabled commentary in
  J.encode json
