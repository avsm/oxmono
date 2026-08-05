(*---------------------------------------------------------------------------
  Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** LinkedIn Posts API client.

    Communicates with the LinkedIn REST Posts API
    ([POST https://api.linkedin.com/rest/posts]) for creating shares
    on behalf of a person or organization (page).

    {2 Authentication}

    Obtain an access token via LinkedIn's OAuth 2.0 flow.
    - For personal posts: scopes [openid], [profile], [w_member_social]
    - For organization/page posts: scope [w_organization_social]
      (requires ADMINISTRATOR or CONTENT_ADMIN role on the page)

    {2 Usage}

    {[
      Eio.Switch.run @@ fun sw ->
      let session = Requests.create ~sw env in
      let li = Linkedin.create ~session ~access_token:"..." in

      (* Post as a person *)
      let me = Linkedin.userinfo li in
      Linkedin.post_text li ~author:me.person_urn "Hello LinkedIn!";

      (* Post as an organization page *)
      let org = "urn:li:organization:12345" in
      Linkedin.post_article li ~author:org
        ~url:"https://example.com"
        ~title:"My Article"
        ~description:"Description for the link card"
        "Check out this article!";
    ]}

    {2 Rate Limits}

    - 150 requests/day per member
    - 100,000 requests/day per application *)

type t
(** A LinkedIn API client with authentication. *)

(** {1 Creation} *)

val create : session:Requests.t -> access_token:string -> t
(** [create ~session ~access_token] creates a LinkedIn API client.
    The [session] handles HTTP connections and the [access_token]
    is a valid OAuth 2.0 bearer token.

    Sets the required headers:
    - [LinkedIn-Version: 202603]
    - [X-Restli-Protocol-Version: 2.0.0]
    - [Authorization: Bearer ...] *)

(** {1 User Info} *)

type userinfo = {
  sub : string;
  (** Subject identifier. *)
  name : string;
  given_name : string;
  family_name : string;
  picture : string option;
  email : string option;
  person_urn : string;
  (** Convenience: [urn:li:person:{sub}] *)
}

val userinfo : t -> userinfo
(** [userinfo t] retrieves the authenticated member's profile via the
    OpenID Connect userinfo endpoint.
    @raise Failure on HTTP or parse errors. *)

(** {1 Posting}

    The [author] parameter is a URN string:
    - Person: [urn:li:person:{id}] (get via {!userinfo})
    - Organization: [urn:li:organization:{id}] *)

type visibility = Connections | Public

type feed_distribution = Main_feed | None_

type post_result = {
  id : string;
  (** The URN of the created post (from [x-restli-id] header). *)
}

val post_text :
  t ->
  author:string ->
  ?visibility:visibility ->
  ?distribution:feed_distribution ->
  ?reshare_disabled:bool ->
  string ->
  (post_result, string) result
(** [post_text t ~author commentary] creates a text-only post.
    Default visibility is {!Public}, distribution is {!Main_feed}. *)

val post_article :
  t ->
  author:string ->
  url:string ->
  ?title:string ->
  ?description:string ->
  ?thumbnail:string ->
  ?visibility:visibility ->
  ?distribution:feed_distribution ->
  ?reshare_disabled:bool ->
  string ->
  (post_result, string) result
(** [post_article t ~author ~url commentary] creates a post with an
    article/link card. The [url] is the article source URL. [title] and
    [description] customise the link card. [thumbnail] is an optional
    image URN ([urn:li:image:{id}]) for the card image.
    Default visibility is {!Public}. *)

(** {1 Dry-Run / Inspection} *)

val post_text_json :
  author:string ->
  ?visibility:visibility ->
  ?distribution:feed_distribution ->
  ?reshare_disabled:bool ->
  string ->
  string
(** Returns the JSON request body for a text post without sending. *)

val post_article_json :
  author:string ->
  url:string ->
  ?title:string ->
  ?description:string ->
  ?thumbnail:string ->
  ?visibility:visibility ->
  ?distribution:feed_distribution ->
  ?reshare_disabled:bool ->
  string ->
  string
(** Returns the JSON request body for an article post without sending. *)
