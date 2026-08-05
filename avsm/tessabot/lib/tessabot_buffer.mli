(*---------------------------------------------------------------------------
  Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Buffer GraphQL API client.

    Communicates with the Buffer API at [https://api.buffer.com]
    using bearer token authentication and GraphQL queries/mutations. *)

type scheduling_type = Automatic | Notification

type post_mode = Share_now | Custom_schedule | Share_next

type create_post_input = {
  text : string;
  channel_id : string;
  service : string;
  (** Channel service type (e.g. "bluesky", "linkedin", "threads").
      Used to send the correct per-network metadata key. *)
  scheduling_type : scheduling_type;
  mode : post_mode;
  due_at : Ptime.t option;
  image_urls : string list;
  link_attachment : string option;
  (** URL for social card / link preview. Sent as [linkAttachment] in
      per-network metadata matching the channel's service type. *)
}

type post_result = {
  id : string;
  text : string;
}

type channel = {
  id : string;
  name : string;
  service : string;
}

type organization = {
  id : string;
}

(** {1 GraphQL Query Building}

    These functions build the GraphQL query strings for dry-run display. *)

val create_post_graphql : create_post_input -> string * string
(** [create_post_graphql input] returns [(query, variables)] as JSON strings
    for the createPost mutation. *)

val organizations_graphql : unit -> string
(** Returns the GraphQL query string for fetching organizations. *)

val channels_graphql : string -> string * string
(** [channels_graphql org_id] returns [(query, variables)] for fetching
    channels in an organization. *)

(** {1 API Calls} *)

val create_post :
  session:Requests.t ->
  create_post_input ->
  (post_result, string) result

val list_organizations :
  session:Requests.t ->
  (organization list, string) result

val list_channels :
  session:Requests.t ->
  org_id:string ->
  (channel list, string) result
