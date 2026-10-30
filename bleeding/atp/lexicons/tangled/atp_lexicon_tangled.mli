(* Atp_lexicon_tangled - generated from atproto lexicons *)

(** AT Protocol lexicon types and Jsont codecs for Atp_lexicon_tangled. *)

(** Utility functions for resilient parsing. *)
module Filter : sig
  val filter_list : 'a Jsont.t -> Jsont.json list -> 'a list
  (** [filter_list jsont json_list] parses each element with [jsont],
      returning only successfully parsed elements. Non-compliant records
      are silently skipped. *)
end

module Org : sig
  module Tangled : sig
    module Webhook : sig
      module RetryWebhookDelivery : sig
(** Re-send a specific webhook delivery. *)


type input = {
  delivery : string;  (** Delivery ID to retry. *)
  repo : string;  (** DID of the repository as minted by the knot. *)
  webhook : string;  (** Webhook ID that owns the delivery. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module DeleteWebhook : sig
(** Delete a webhook from a repository. *)


type input = {
  repo : string;  (** DID of the repository as minted by the knot. *)
  webhook : string;  (** Webhook ID to delete. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module Defs : sig

type event_type = string
val event_type_jsont : event_type Jsont.t


type delivery = {
  created_at : string;
  event : Jsont.json;  (** Event type that triggered the delivery. *)
  id : string;  (** Unique ID for tracking this delivery attempt. *)
  request_body : string option;
  response_body : string option;
  response_code : int option;
  success : bool;
  url : string;
  webhook : string;  (** ID of the webhook this delivery belongs to. *)
}

(** Jsont codec for {!type:delivery}. *)
val delivery_jsont : delivery Jsont.t


type webhook = {
  active : bool;  (** Whether the webhook is currently enabled. *)
  created_at : string;
  events : Jsont.json list;  (** Event types this webhook is subscribed to. *)
  id : string;  (** Webhook identifier. *)
  updated_at : string option;
  url : string;  (** Endpoint URL that receives webhook payloads. *)
}

(** Jsont codec for {!type:webhook}. *)
val webhook_jsont : webhook Jsont.t

      end
      module UpdateWebhook : sig
(** Update an existing webhook. Only supplied fields are changed. *)


type input = {
  active : bool option;
  events : Jsont.json list option;
  repo : string;  (** DID of the repository as minted by the knot. *)
  secret : string option;
  url : string option;
  webhook : string;  (** Webhook ID to update. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module ListWebhooks : sig
(** List webhooks configured for a repository. *)

(** Query/procedure parameters. *)
type params = {
  repo : string;  (** DID of the repository as minted by the knot. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  webhooks : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListWebhookDeliveries : sig
(** List recent delivery attempts for a webhook. *)

(** Query/procedure parameters. *)
type params = {
  limit : int option;
  repo : string;  (** DID of the repository as minted by the knot. *)
  webhooks : string list;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  deliveries : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CreateWebhook : sig
(** Create a new webhook for a repository. *)


type input = {
  active : bool option;  (** Whether the webhook should be active immediately. *)
  events : Jsont.json list;  (** Event types to subscribe to. *)
  repo : string;  (** DID of the repository as minted by the knot. *)
  secret : string option;  (** Optional HMAC secret used to sign payloads. If omitted, payloads are not signed. *)
  url : string;  (** Endpoint URL that will receive webhook payloads. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  id : string;  (** ID of the newly created webhook. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module String : sig
      module ListStrings : sig

type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.string record *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Scope identifier whose string records to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetString : sig

(** Query/procedure parameters. *)
type params = {
  uri : string;  (** AT-URI of the sh.tangled.string record to fetch. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.string record. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CountStrings : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Scope identifier whose string records to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module Spindle : sig
      module DescribeSpindle : sig

type contact = {
  email : string option;
}

(** Jsont codec for {!type:contact}. *)
val contact_jsont : contact Jsont.t


type links = {
  privacy_policy : string option;
  terms_of_service : string option;
}

(** Jsont codec for {!type:links}. *)
val links_jsont : links Jsont.t



type output = {
  capabilities : string list option;  (** Protocol capability tokens this spindle implements. Spindle that omit this field are treated as legacy. *)
  contact : Jsont.json option;  (** Contact information *)
  did : string;
  links : Jsont.json option;  (** URLs of service policy documents. *)
  version : string;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module Snippet : sig
      module String : sig

type text_file = {
  content : Atp.Blob_ref.t;
  name : string;  (** file name *)
}

(** Jsont codec for {!type:text_file}. *)
val text_file_jsont : text_file Jsont.t

(** TID is preferred for record-key, but not enforced. *)

type main = {
  created_at : string;
  description : string option;
  snippet : Jsont.json;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
    end
    module Secret : sig
      module RemoveSecret : sig
(** Remove a CI secret *)


type input = {
  key : string;
  repo : string;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module ListSecrets : sig

type secret = {
  created_at : string;
  created_by : string;
  key : string;
}

(** Jsont codec for {!type:secret}. *)
val secret_jsont : secret Jsont.t


(** Query/procedure parameters. *)
type params = {
  repo : string;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  secrets : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module AddSecret : sig
(** Add a CI secret *)


type input = {
  key : string;
  repo : string;
  value : string;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
    end
    module Query : sig
      module EnrichResponse : sig
(** Moderation labels the configured labeler applied. Only !hide and !warn are served, takedowns leave subjects out instead. Use collection:. for labels on the record, collection:.repo for labels on its author's account. Absent when there are none. *)

type labels = {
  labels : Jsont.json list;
}

(** Jsont codec for {!type:labels}. *)
val labels_jsont : labels Jsont.t

(** One sidecar payload, computed from the records at a link source whose reference equals the found reference. *)

type link_descriptor = {
  source : string;  (** Collection whose records are linked, a colon, then an index-backed path (subject or .repo), or . for the record the reference names, which hydrates into that collection's view. *)
  targets : string list option;  (** Record paths into the inner response selecting references for this payload. A path can end in an envelope: path:.repo for a ref's author, path:.rkey for the did its rkey names, path:. for the ref itself. All references are considered when omitted. *)
  type_ : string;  (** NSID of the payload type to produce, which also defines the payload's shape. Results land at data\[ref\]\[source\]\[type\]. *)
}

(** Jsont codec for {!type:link_descriptor}. *)
val link_descriptor_jsont : link_descriptor Jsont.t

(** Run an XRPC query and hydrate its response with a data sidecar: typed payloads for every did / at-uri / strong-ref found in the response. *)


type input = {
  enrich : Jsont.json list;  (** Payloads to compute for each found reference. *)
  params : Jsont.json option;  (** Parameters for the inner query, exactly as it declares them. *)
  viewer : string option;  (** DID whose own relation to each reference is looked up for viewer payloads and views. Required when any descriptor requests a viewer payload. *)
  xrpc : string;  (** NSID of the inner query to run, e.g. org.tangled.repo.listRepos. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  data : Jsont.json;  (** Reference (did or at-uri) -> link source -> payload type nsid -> payload. References no descriptor applies to are absent. *)
  output : Jsont.json;  (** The inner query's response, unmodified. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module PublicKey : sig
      module ListKeys : sig

type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.publicKey record *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Owner DID whose public-key records to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetPublicKey : sig

(** Query/procedure parameters. *)
type params = {
  public_key : string;  (** AT-URI of the sh.tangled.publicKey record to fetch. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.publicKey record. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CountKeys : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Owner DID whose public-key records to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module Markup : sig
      module Plaintext : sig

type main = {
  text : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module Embed : sig
        module Video : sig

type main = {
  video : Atp.Blob_ref.t;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

        end
        module Image : sig

type main = {
  image : Atp.Blob_ref.t;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

        end
      end
      module Markdown : sig
(** Tangled Flavored Markdown format text (inline) *)

type inline = {
  text : string;  (** Final post-processed markdown content that will be rendered *)
}

(** Jsont codec for {!type:inline}. *)
val inline_jsont : inline Jsont.t

(** Tangled Flavored Markdown format text *)

type main = {
  blobs : Jsont.json list option;  (** list of blobs referenced in markdown *)
  text : string;  (** Final post-processed markdown content that will be rendered *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
    end
    module Label : sig
      module ListDefinitions : sig

type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded org.tangled.label.definition record *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** DID of the repo whose label definitions to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Defs : sig

type label_token = {
  key : string;  (** Record-key of org.tangled.label.definition owned by this repository. *)
}

(** Jsont codec for {!type:label_token}. *)
val label_token_jsont : label_token Jsont.t

      end
      module Definition : sig

type main = {
  color : string option;  (** The hex value for the background color for the label. Appviews may choose to respect this. *)
  created_at : string;
  name : string;  (** The display name of this label. *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module CountDefinitions : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** DID of the repo whose label definitions to count. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module Knot : sig
      module Member : sig
(** Offer of membership on this knot, w/ member-DID as rkey. *)

type main = {
  created_at : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module DescribeKnot : sig

type contact = {
  email : string option;
}

(** Jsont codec for {!type:contact}. *)
val contact_jsont : contact Jsont.t


type links = {
  privacy_policy : string option;
  terms_of_service : string option;
}

(** Jsont codec for {!type:links}. *)
val links_jsont : links Jsont.t



type output = {
  capabilities : string list option;  (** Protocol capability tokens this knot implements. Knots that omit this field are treated as legacy. *)
  contact : Jsont.json option;  (** Contact information *)
  did : string;
  links : Jsont.json option;  (** URLs of service policy documents. *)
  version : string;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Block : sig
(** Ban of an account from this knot, w/ the banned DID as rkey. *)

type main = {
  created_at : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
    end
    module Key : sig
      module Unspecced : sig
        module GpgKey : sig
(** gpg public key *)

type main = {
  created_at : string;
  name : string;  (** human-readable name for this key *)
  public_key : string;  (** ASCII-armored public key block *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

        end
      end
      module SshKey : sig
(** ssh public key *)

type main = {
  created_at : string;
  name : string;  (** human-readable name for this key *)
  public_key : string;  (** public key in OpenSSH authorized_keys format *)
  usages : string list option;  (** default to `\["auth", "sign"\]` when omitted. *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
    end
    module Graph : sig
      module Vouch : sig
(** normalized subject-DID as rkey. *)

type main = {
  created_at : string;
  evidences : Jsont.json list option;  (** Optional list of ATURIs serving as evidence for this vouch (ex. issues, PRs) *)
  kind : string;  (** Whether this user is being vouched for or denounced *)
  reason : string option;  (** The reason for this vouch/denouncement *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module ListNetworkVouches : sig

type list_item = {
  actor : string;  (** The requested actor this vouch involves. *)
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.graph.vouch record *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t

(** List vouches involving one or more actors as seen through a viewer's vouch network: vouches the viewer's network placed on an actor, and vouches an actor placed on the viewer's network. Requires authentication. *)

(** Query/procedure parameters. *)
type params = {
  actors : string list;  (** DIDs of the profiles whose network vouches to list. At most 50 per request. *)
  cursor : string option;  (** Pagination cursor. Only accepted when a single actor is requested. *)
  limit : int option;  (** Maximum number of vouches per actor. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListFollows : sig

type list_item = {
  cid : string option;
  subject : string;  (** The followed account, which the record only names by its rkey *)
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.graph.follow record *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Followee DID whose inbound follows to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetFollow : sig

(** Query/procedure parameters. *)
type params = {
  actor : string;  (** DID of the potential follower. *)
  subject : string;  (** Followee DID to check. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  uri : string;  (** Uri of actor's follow record for subject. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Follow : sig
(** Record declaring a social 'follow' relationship of another account, w/ normalized subject-DID as rkey. *)

type main = {
  created_at : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module CountFollowsBy : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Actor DID whose follow authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CountFollows : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Followee DID whose inbound follows to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Collaborator : sig
(** Offer of collaboration on this repo, w/ normalized invitee-DID as rkey. *)

type main = {
  created_at : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module Attestation : sig
(** Attesting that I the user claim a given record over on a knot! A knot will claim that I wrote something, but it's only really tied to me if I too say that I wrote it. The 'change' field is literally the Git Object ID of the Collaborative Object (COB) change that made the knot-side record. *)

type main = {
  change : string option;  (** The Git Object ID of the COB change that made the knot-side record. *)
  created_at : string;
  subject : Jsont.json;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module ListFollowsBy : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Actor DID whose follow authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module Git : sig
      module SetDefaultBranch : sig
(** Set the default branch of this repository *)


type input = {
  branch : string;  (** Branch name without the refs/heads/ prefix *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  cid : string;  (** Version of the refs record after the move *)
  uri : string;  (** The repository's org.tangled.git.refs record *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Refs : sig

type main = {
  default_branch : string;  (** Default branch name without refs/heads/ prefix. *)
  refs : string;  (** List of git refs in `git show-ref` format. *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module ReceivePack : sig
(** No-op xrpc endpoint for `git-push` permission scope. *)

      end
      module PushEvent : sig
(** A git ref edit. *)

type ref_edit = {
  name : string;  (** Full git reference name, ie refs/heads/main. *)
  new_ : string option;  (** Full hex object id the ref now points at. *)
  old : string option;  (** Full hex object id the ref pointed at before. *)
}

(** Jsont codec for {!type:ref_edit}. *)
val ref_edit_jsont : ref_edit Jsont.t

(** Represents git-push operation to target repository. *)

type main = {
  edits : Jsont.json list;
  options : string list option;  (** Git push options. *)
  pusher : string;  (** The account that performed this push. *)
  repo : string;  (** The repo this event comes from. *)
  seq : int;  (** The stream sequence number of this message. *)
  sig_ : string;  (** Signature over the DAG-CBOR of this message with the seq and sig keys absent (repo/pusher DID, edits, time, options), by the repo's atproto signing key. seq is excluded so a relay can reassign it without invalidating the signature. *)
  time : string;  (** Timestamp of when this message was originally broadcast. *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module Oid : sig

type main = {
  oid : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module ListRefs : sig

type default_branch = {
  head : string option;  (** Commit SHA at the tip of the default branch, for reconciling against a last-known state. Width depends on the repo's git object-format. *)
  ref_ : string;  (** Default branch ref name that HEAD points at, eg. refs/heads/main. *)
}

(** Jsont codec for {!type:default_branch}. *)
val default_branch_jsont : default_branch Jsont.t


type ref_ = {
  ref_ : string;  (** Full ref name, eg. refs/heads/main or refs/tags/v1.0 *)
  sha : string;  (** Object SHA the ref points at. Width depends on the repo's git object-format. *)
}

(** Jsont codec for {!type:ref_}. *)
val ref__jsont : ref_ Jsont.t

(** List every ref & its commit SHA for a git repo, equivalent to git ls-remote. Gives the full ref state of a repo. *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;  (** Maximum number of refs to return in this page *)
  repo : string;  (** DID of the git repo as minted by the knot *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;  (** Cursor for the next page, absent when the last page is reached *)
  default_branch : Jsont.json option;
  refs : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListLanguages : sig

type language = {
  color : string option;  (** Render color for this language *)
  extensions : string list option;  (** File extensions recognized as this language *)
  file_count : int option;  (** Files counted for this language *)
  name : string;  (** Language name *)
  percentage : int;  (** Share of total size, 0-100 *)
  size : int;  (** Bytes counted for this language *)
}

(** Jsont codec for {!type:language}. *)
val language_jsont : language Jsont.t

(** Language breakdown of a repository's tree at a ref. *)

(** Query/procedure parameters. *)
type params = {
  ref_ : string option;  (** Git revision (branch, tag, or commit Object ID) *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  languages : Jsont.json list;
  ref_ : string;  (** The ref the breakdown was computed at *)
  total_files : int option;  (** Total counted files *)
  total_size : int option;  (** Total bytes across counted files *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetBlob : sig

(** Query/procedure parameters. *)
type params = {
  path : string;  (** Path within the repository tree *)
  ref_ : string option;  (** Git reference (branch, tag, or commit SHA) *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t

(** raw blob served in octet-stream *)

type output = unit
val output_jsont : output Jsont.t

      end
      module GetBlame : sig

type author = {
  email : string;
  name : string;
}

(** Jsont codec for {!type:author}. *)
val author_jsont : author Jsont.t


type run = {
  commit : string;  (** The commit Object ID of the entry in commits this run is attributed to *)
  count : int;  (** Number of lines in the run *)
  start : int;  (** First line of the run, one-based *)
}

(** Jsont codec for {!type:run}. *)
val run_jsont : run Jsont.t


type commit = {
  author : Jsont.json;
  authored_at : string;  (** When the change was authored *)
  sha : string;  (** Commit Object ID *)
  subject : string;  (** First line of the commit message *)
}

(** Jsont codec for {!type:commit}. *)
val commit_jsont : commit Jsont.t

(** Line-by-line attribution of a file at a ref. *)

(** Query/procedure parameters. *)
type params = {
  path : string;  (** Path of the file to blame *)
  ref_ : string;  (** Git revision (branch, tag, or commit Object ID) *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  commit : string;  (** The resolved commit Object ID *)
  commits : Jsont.json list;  (** Unique commits referenced by blame runs *)
  runs : Jsont.json list;  (** Complete, gap-free one-based line ranges sorted by start *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetArchive : sig

(** Query/procedure parameters. *)
type params = {
  format : string option;  (** Archive format *)
  prefix : string;  (** Prefix for files in the archive *)
  ref_ : string;  (** Git reference (branch, tag, or commit SHA) *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t

(** Binary archive data *)

type output = unit
val output_jsont : output Jsont.t

      end
      module DeleteBranch : sig
(** Delete a branch on this repository *)


type input = {
  branch : string;
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module Defs : sig

type byte_changes = {
  lhs : int;
  rhs : int;
}

(** Jsont codec for {!type:byte_changes}. *)
val byte_changes_jsont : byte_changes Jsont.t

(** One side of a file diff. *)

type diff_src = {
  content : string option;  (** Inlined content. Used on interdiff's LHS. *)
  is_binary : bool;
  is_submodule : bool;
  oid : string;  (** Blob oid, or the commit oid for a submodule *)
  path : string;
  size : int;
}

(** Jsont codec for {!type:diff_src}. *)
val diff_src_jsont : diff_src Jsont.t


type diff_stat = {
  deletions : int;
  files_changed : int;
  insertions : int;
}

(** Jsont codec for {!type:diff_stat}. *)
val diff_stat_jsont : diff_stat Jsont.t

(** One aligned line pair; a side is absent when that side has no line. *)

type line_pair = {
  lhs : int option;
  rhs : int option;
}

(** Jsont codec for {!type:line_pair}. *)
val line_pair_jsont : line_pair Jsont.t


type signature = {
  email : string;  (** Person email *)
  name : string;  (** Person name *)
  when_ : string;  (** Timestamp of the signature *)
}

(** Jsont codec for {!type:signature}. *)
val signature_jsont : signature Jsont.t


type submodule = {
  branch : string option;  (** Branch to track in the submodule *)
  name : string;  (** Submodule name *)
  url : string;  (** Submodule repository URL *)
}

(** Jsont codec for {!type:submodule}. *)
val submodule_jsont : submodule Jsont.t


type commit = {
  author : Jsont.json;
  committer : Jsont.json;
  hash : string;  (** Commit Object ID *)
  message : string;
  tree : string;  (** Tree Object ID *)
}

(** Jsont codec for {!type:commit}. *)
val commit_jsont : commit Jsont.t


type diff_hunk = {
  lines : Jsont.json list;
  novel_lhs : int list;  (** 0-based line numbers holding novel content on the left side *)
  novel_rhs : int list;  (** 0-based line numbers holding novel content on the right side *)
}

(** Jsont codec for {!type:diff_hunk}. *)
val diff_hunk_jsont : diff_hunk Jsont.t


type file_diff = {
  has_byte_changes : Jsont.json option;  (** Byte count per side, present only when the two sides differ *)
  has_syntactic_changes : bool;
  hunks : Jsont.json list;
  lhs_src : Jsont.json;
  rhs_src : Jsont.json;
}

(** Jsont codec for {!type:file_diff}. *)
val file_diff_jsont : file_diff Jsont.t

      end
      module ListTags : sig

type annotation = {
  message : string;  (** Tag message *)
  tagger : Jsont.json option;
}

(** Jsont codec for {!type:annotation}. *)
val annotation_jsont : annotation Jsont.t


type tag_info = {
  annotation : Jsont.json option;  (** Present only for annotated tags *)
  author : Jsont.json;
  message : string;  (** Latest commit message *)
  name : string;  (** Tag name *)
  oid : string;  (** Latest commit oid on this tag *)
}

(** Jsont codec for {!type:tag_info}. *)
val tag_info_jsont : tag_info Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;  (** Maximum number of tags to return *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  tags : Jsont.json list;
  total : int;  (** Total number of tags in the repository *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListCommits : sig
(** List commit history reachable from a ref, newest first. The ref may be any full ref name, including refs/cobs/*. *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Commit Object ID of the last commit in the previous page *)
  limit : int option;  (** Maximum number of commits to return in this page *)
  path : string option;  (** Only list commits touching this path *)
  ref_ : string;  (** Git revision (branch, tag, commit Object ID, or full ref name) *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  commits : Jsont.json list;
  cursor : string option;  (** Cursor for the next page, absent when the last page is reached *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListBranches : sig

type branch_info = {
  author : Jsont.json;
  committer : Jsont.json;
  message : string;  (** Latest commit message *)
  name : string;  (** Branch name *)
  oid : string;  (** Latest commit oid on this branch *)
}

(** Jsont codec for {!type:branch_info}. *)
val branch_info_jsont : branch_info Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;  (** Maximum number of branches to return *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  branches : Jsont.json list;
  cursor : string option;
  default_branch : string option;  (** Name of the default branch *)
  total : int;  (** Total number of branches in the repository *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetTree : sig

type tree_entry = {
  last_commit : Jsont.json option;
  mode : string;  (** File mode *)
  name : string;  (** Relative file or directory name *)
  size : int;  (** File size in bytes *)
}

(** Jsont codec for {!type:tree_entry}. *)
val tree_entry_jsont : tree_entry Jsont.t


(** Query/procedure parameters. *)
type params = {
  path : string option;  (** Path within the repository tree *)
  ref_ : string option;  (** Git revision (branch, tag, or commit Object ID) *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  entries : Jsont.json list;
  last_commit : Jsont.json option;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetEntry : sig
(** get metadata of blob by ref and path *)

(** Query/procedure parameters. *)
type params = {
  path : string;  (** path of the entity *)
  ref_ : string option;  (** Git revision (branch, tag, or commit Object ID) *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  last_commit : Jsont.json option;
  mode : string;
  name : string;  (** The file name *)
  oid : string;
  size : int;  (** Blob size *)
  submodule : Jsont.json option;  (** Submodule information if path is a submodule *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetDiff : sig
(** Full diff between two revisions of a repository. *)

(** Query/procedure parameters. *)
type params = {
  base : string;  (** Base revision (branch, tag, or commit Object ID) *)
  head : string;  (** Head revision (branch, tag, or commit Object ID) *)
  paths : string option;  (** Only include diffs touching these paths, comma-separated *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  diffs : Jsont.json list;
  stats : Jsont.json option;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetBranch : sig

(** Query/procedure parameters. *)
type params = {
  name : string;  (** Branch name to get information for *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  author : Jsont.json;
  hash : string;  (** Latest commit hash on this branch *)
  message : string;  (** Latest commit message *)
  name : string;  (** Branch name *)
  total_commits : int;  (** Number of commits reachable from this branch *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetTag : sig

(** Query/procedure parameters. *)
type params = {
  repo : string;  (** DID of the repository *)
  tag : string;  (** Name of tag, such as v1.3.0 *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  annotation : Jsont.json option;  (** Present only for annotated tags *)
  author : Jsont.json;
  message : string;  (** Latest commit message *)
  name : string;  (** Tag name *)
  oid : string;  (** Latest commit oid on this tag *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module Event : sig
      module Push : sig
(** When a repository ref is updated. *)

type main = {
  actor : string option;  (** The account that pushed. *)
  new_sha : string;
  old_sha : string;
  push_options : string list option;  (** The push options sent with the push, omitted when there were none. *)
  ref_ : string;
  repo : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module PullRequest : sig
(** When pull request is created or resubmitted. *)

type main = {
  action : string option;  (** The pull request lifecycle action that produced this trigger. *)
  pull : string option;  (** AT-URI of the pull request record this run belongs to. *)
  source_branch : string option;
  source_repo : string option;  (** Repository DID to check out code and workflow definitions from, if different from the target repo. *)
  source_sha : string option;  (** Commit id of the pull request head. *)
  target_branch : string option;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module NotifyEvent : sig

type ci_output = {
  message : string option;  (** Text returned by the server for this push, like how to follow a started CI. *)
  pipelines : string list option;  (** IDs of the scheduled pipelines *)
}

(** Jsont codec for {!type:ci_output}. *)
val ci_output_jsont : ci_output Jsont.t

(** notify an event from external source *)


type input = {
  event : Jsont.json;  (** Tangled event object *)
  repo : string;  (** Repository DID *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  ci : Jsont.json option;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module Ci : sig
      module Trigger : sig
        module Schedule : sig
(** A pipeline the spindle started on a workflow's cron schedule. *)

type main = {
  commit : string;  (** Commit id the pipeline ran on, the head of ref at dispatch. *)
  ref_ : string;  (** Branch the commit was resolved from, the repository's default branch. *)
  repo : string;
  scheduled_at : string;  (** The schedule tick this run is for, to the minute. *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

        end
        module Manual : sig

type pair = {
  key : string;
  value : string;
}

(** Jsont codec for {!type:pair}. *)
val pair_jsont : pair Jsont.t


type main = {
  commit : string;  (** Commit id the pipeline will run on. *)
  inputs : Jsont.json list option;
  ref_ : string option;  (** Optional ref the commit was resolved from. *)
  repo : string;
  workflows : string list option;  (** Workflow names to run. When not provided, every dispatchable workflow is run. *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

        end
      end
      module SubscribePipelineLogs : sig

type control = {
  command : string option;  (** Step command *)
  content : string;
  kind : string option;  (** Step kind *)
  outcome : string option;  (** How the step ended, only set on end frames for steps that can fail *)
  status : string option;  (** Step status *)
  step : int;  (** Step ID *)
  time : string;
  workflow : string;  (** workflow name *)
}

(** Jsont codec for {!type:control}. *)
val control_jsont : control Jsont.t


type data = {
  content : string;
  step : int;  (** Step ID *)
  stream : string;
  time : string;
  workflow : string;  (** workflow name *)
}

(** Jsont codec for {!type:data}. *)
val data_jsont : data Jsont.t

(** Pipeline logs stream *)

(** Query/procedure parameters. *)
type params = {
  pipeline : string;  (** Pipeline ID *)
  repo : string;  (** Repository DID *)
  workflows : string list option;  (** filter logs by specific workflows *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type message = Jsont.json

(** Jsont codec for {!type:message}. *)
val message_jsont : message Jsont.t

      end
      module RetryPipeline : sig
(** Duplicate existing pipeline for rerun. *)


type input = {
  pipeline : string;  (** pipeline id *)
  repo : string;  (** git repository DID *)
  workflows : string list option;  (** Workflow names to filter. When not provided, entire pipeline will be duplicated. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  pipeline : string;  (** Scheduled pipeline id *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CancelPipeline : sig
(** Cancel running pipeline or specific workflows *)


type input = {
  pipeline : string;  (** Spindle-local pipeline id *)
  repo : string;  (** git repository DID *)
  workflows : string list option;  (** Workflow names to filter. When not provided, entire pipeline will be canceled. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module ApprovePipeline : sig
(** Approve blocked pipeline. *)


type input = {
  pipeline : string;  (** pipeline id *)
  repo : string;  (** git repository DID *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module TriggerPipeline : sig
(** Trigger a pipeline at an explicit commit. Runs the named workflows, or every workflow defined in the repo when none are named. *)


type input = {
  repo : string;  (** Target repository DID. Auth is checked against this repo. *)
  trigger : Jsont.json;  (** Trigger metadata for this dispatch. *)
  workflows : string list option;  (** Workflow names to run. When not provided, every dispatchable workflow is run. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  pipeline : string;  (** Spindle-local id of the created pipeline *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Defs : sig

type external_source = {
  link : string;
  name : string option;  (** human-readable name of external source *)
}

(** Jsont codec for {!type:external_source}. *)
val external_source_jsont : external_source Jsont.t


type line_range = {
  end_ : int;
  start : int;
}

(** Jsont codec for {!type:line_range}. *)
val line_range_jsont : line_range Jsont.t

(** A workflow definition *)

type workflow = {
  hash : string option;  (** Fingerprint of the workflow definition as resolved by the spindle. *)
  id : string;
  name : string option;  (** Human-readable name of the workflow *)
  source : Jsont.json;
  triggers : string list;  (** Event types that triggers this workflow. *)
}

(** Jsont codec for {!type:workflow}. *)
val workflow_jsont : workflow Jsont.t


type file_source = {
  commit : string;
  lines : Jsont.json option;
  path : string;
  repo : string;
}

(** Jsont codec for {!type:file_source}. *)
val file_source_jsont : file_source Jsont.t


type workflow_run = {
  definition : Jsont.json;  (** Workflow definition where this workflow run is originated from. *)
  error : string option;
  finished_at : string option;
  id : string;  (** Pipeline-local workflow id, usually same as name. *)
  name : string option;  (** Human-readable name of the workflow *)
  started_at : string option;
  status : string;  (** Workflow status *)
}

(** Jsont codec for {!type:workflow_run}. *)
val workflow_run_jsont : workflow_run Jsont.t

(** A CI pipeline. Record-like, but owned by the spindle rather than a PDS. *)

type pipeline = {
  commit : string;  (** Commit Id this pipeline is running on *)
  created_at : string option;
  id : string;  (** Spindle-local pipeline id *)
  repo : string;  (** Repository DID *)
  source_repo : string option;  (** Repository DID that the commit was checked out from, if different from repo (e.g. a fork for a fork-based pull request) *)
  trigger : Jsont.json;  (** Trigger event that triggered this pipeline. *)
  workflows : Jsont.json list;  (** Triggered workflows *)
}

(** Jsont codec for {!type:pipeline}. *)
val pipeline_jsont : pipeline Jsont.t

      end
      module QueryPipelines : sig
(** Query pipelines in a git repository *)

(** Query/procedure parameters. *)
type params = {
  commits : string list option;  (** Filter pipelines by commit ids. *)
  cursor : string option;  (** Pagination cursor *)
  limit : int option;  (** Maximum number of pipelines to return *)
  repo : string;  (** DID of the repository *)
  triggers : string list option;  (** Filter pipelines by trigger kind. When provided, pipelines matching any listed kind are returned; when omitted, every kind is returned. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  pipelines : Jsont.json list;
  total : int;  (** Number of matching pipelines *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListWorkflowDefinitions : sig

(** Query/procedure parameters. *)
type params = {
  ref_ : string option;
  repo : string;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  workflows : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetWorkflowDefinition : sig

(** Query/procedure parameters. *)
type params = {
  repo : string;
  workflow : string;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  workflow : Jsont.json;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetPipeline : sig

(** Query/procedure parameters. *)
type params = {
  pipeline : string;  (** Spindle-local pipeline id *)
  repo : string;  (** Repository DID *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = Jsont.json

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module Bobbin : sig
      module AwaitRecord : sig
(** Wait for a record written to a PDS to reach this bobbin instance, then report what happened to it. *)

(** Query/procedure parameters. *)
type params = {
  cid : string option;  (** CID of the revision that was written, as returned by createRecord, putRecord or applyWrites. Omit it only when awaiting a delete, which has no CID. any other responds with whatever revision of the record settles first. *)
  uri : string;  (** AT-URI of the record that was written. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cid : string option;  (** CID of the revision this result is about. *)
  detail : string option;  (** Opaque error message which can be more detailed than the reason. *)
  reason : string option;  (** Why a rejected record will not appear. *)
  status : string;  (** What bobbin did with the record. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module Actor : sig
      module SearchActorsTypeahead : sig

type actor = {
  did : string;
  handle : string;
  uri : string;  (** AT-URI for the actor's Tangled profile. *)
}

(** Jsont codec for {!type:actor}. *)
val actor_jsont : actor Jsont.t

(** Find actors by their verified handle. *)

(** Query/procedure parameters. *)
type params = {
  limit : int option;
  q : string;  (** The beginning of a handle. A leading \@ is optional. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  actors : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Profile : sig
(** A declaration of a Tangled account profile. *)

type main = {
  avatar : Atp.Blob_ref.t option;  (** Small image to be displayed next to posts from account. AKA, 'profile picture' *)
  description : string option;  (** Free-form profile description text. *)
  is_organization : bool option;
  labels : Jsont.json option;  (** Self-label values for this record. Most common use-case is 'bot'. *)
  links : string list option;
  location : string option;  (** Free-form location text. *)
  pinned_repositories : string list option;  (** Pinned repositories. Values are repo DIDs for repos that have them, or AT-URIs for legacy repos. *)
  preferred_handle : string option;  (** A handle the user prefers to be displayed as. *)
  pronouns : string option;  (** Preferred gender pronouns. *)
  stats : string list option;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module GetProfiles : sig

type record_view = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.actor.profile record. *)
}

(** Jsont codec for {!type:record_view}. *)
val record_view_jsont : record_view Jsont.t


(** Query/procedure parameters. *)
type params = {
  actors : string list;  (** AT-URIs of the sh.tangled.actor.profile records to fetch. At most 50 per request. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  items : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetProfileActivity : sig

type repo_ref = {
  did : string option;
  name : string;
  owner_did : string;
  owner_handle : string;
  uri : string;
}

(** Jsont codec for {!type:repo_ref}. *)
val repo_ref_jsont : repo_ref Jsont.t


type issue_activity = {
  repo : Jsont.json;
  state : string;
  title : string;
  uri : string;
}

(** Jsont codec for {!type:issue_activity}. *)
val issue_activity_jsont : issue_activity Jsont.t


type pull_activity = {
  repo : Jsont.json;
  state : string;
  title : string;
  uri : string;
}

(** Jsont codec for {!type:pull_activity}. *)
val pull_activity_jsont : pull_activity Jsont.t


type repo_activity = {
  kind : string;
  repo : Jsont.json;
}

(** Jsont codec for {!type:repo_activity}. *)
val repo_activity_jsont : repo_activity Jsont.t


type activity_month = {
  commits : int;
  issues : Jsont.json list;
  pulls : Jsont.json list;
  repos : Jsont.json list;
  start : string;  (** Start of this UTC calendar month. *)
}

(** Jsont codec for {!type:activity_month}. *)
val activity_month_jsont : activity_month Jsont.t

(** Get recent Tangled activity for an actor, grouped by calendar month. *)

(** Query/procedure parameters. *)
type params = {
  actor : string;  (** Actor whose activity to return. *)
  months : int option;  (** Number of UTC calendar months to include, including the current month. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  months : Jsont.json list;
  truncated : bool;  (** True when the result reached the server activity-item safety limit. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetProfile : sig

(** Query/procedure parameters. *)
type params = {
  actor : string;  (** AT-URI of the sh.tangled.actor.profile record to fetch. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.actor.profile record. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Defs : sig
(** Metadata about the requesting account's relationship with the subject account. Only has meaningful content for authed requests. *)

type viewer_state = {
  followed_by : string option;
  following : string option;
}

(** Jsont codec for {!type:viewer_state}. *)
val viewer_state_jsont : viewer_state Jsont.t


type profile_view_basic = {
  avatar : string option;
  avatar_cid : string option;  (** The cid of the avatar blob in the account's profile record. Absent when the record has no avatar or bobbin couldn't read it. *)
  did : string;
  handle : string;
  viewer : Jsont.json option;
}

(** Jsont codec for {!type:profile_view_basic}. *)
val profile_view_basic_jsont : profile_view_basic Jsont.t

(** profileViewBasic + follow counts *)

type profile_view_detailed = {
  avatar : string option;
  avatar_cid : string option;  (** The cid of the avatar blob in the account's profile record. Absent when the record has no avatar or bobbin couldn't read it. *)
  did : string;
  followers_count : int option;
  follows_count : int option;
  handle : string;
  viewer : Jsont.json option;
}

(** Jsont codec for {!type:profile_view_detailed}. *)
val profile_view_detailed_jsont : profile_view_detailed Jsont.t

      end
      module GetTrending : sig
(** Get trending people and repositories. *)


type output = {
  people : Jsont.json list;
  repos : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module Review : sig
      module Unspecced : sig
        module Defs : sig

type review_state = string
val review_state_jsont : review_state Jsont.t

        end
      end
      module SubmitPatch : sig

type patch = {
  base : Jsont.json;
  head : Jsont.json;
  source_branch : string option;
  source_repo : string;  (** DID of the repository to fetch `head` from. *)
  ticket : string;  (** ticket-id *)
}

(** Jsont codec for {!type:patch}. *)
val patch_jsont : patch Jsont.t

(** Create a patch record, fetching its head from the source repository first. The only way to write org.tangled.review.patch records. *)


type input = {
  patch : Jsont.json;
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  cid : string;  (** CID of created org.tangled.review.patch record. *)
  uri : string;  (** AT-URI of created org.tangled.review.patch record. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Patch : sig

type source = {
  branch : string;
  repo : string;
}

(** Jsont codec for {!type:source}. *)
val source_jsont : source Jsont.t

(** Immutable patch attached to the ticket. Record-key must be CID of this record. *)

type main = {
  base : Jsont.json;
  created_at : string;
  head : Jsont.json;
  source : Jsont.json option;
  ticket : string;  (** ticket-id *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
    end
    module Embed : sig
      module Unspecced : sig
        module File : sig

type main = {
  commit : Jsont.json;
  path : string;
  repo : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

        end
      end
      module Commit : sig

type main = {
  commit : Jsont.json;
  repo : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t


type view = {
  change : string option;
  commit : string;
  repo : string;
}

(** Jsont codec for {!type:view}. *)
val view_jsont : view Jsont.t

      end
    end
    module Repo : sig
      module Review : sig
        module Unspecced : sig
          module Round : sig
(** Review round created by a reviewer. *)

type main = {
  commit : Jsont.json;  (** ID of commit that is getting reviewed. *)
  created_at : string;
  labels : Jsont.json option;  (** Self-label values for this record. Most common use-case is 'bot'. *)
  state : Jsont.json;  (** The reviewed state of that commit. This is one and only meaningful content of this record. *)
  subject : string;  (** ticket-id *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

          end
        end
      end
      module ReserveKey : sig
(** Reserve a repo identity under a DID the caller brings. The knot seals and holds a signing key for the repo DID; the caller publishes the matching public key in that DID's document before calling org.tangled.repo.createRepo with the same DID. *)


type input = {
  repo : string;  (** The caller-minted DID to reserve *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  key : string;  (** Multibase-encoded public signing key the knot holds for this repo identity *)
  repo : string;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module MintToken : sig
(** Mint a service-auth JWT whose issuer is this repo's DID, signed by the knot that owns the repo's signing key. When calling this, you'll need the same perms as git push, ie. being repo owner/collaborator/knot-admin. *)


type input = {
  aud : string;  (** Audience of the minted token, a DID service ref for the service, that the token is addressed to. *)
  exp : int;  (** Unix seconds. Must be in the future, and at most 60s after mint time. *)
  lxm : string;  (** Lexicon method to which minted token binds. *)
  repo : string;  (** DID of the repo that becomes the token's issuer. Must be the audience of a given calling session. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  token : string;  (** The signed service-auth JWT. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListRepos : sig

type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.repo record *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Owner DID whose repo records to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListRecordVersions : sig

type version = {
  change_id : string;  (** Object ID of the git change that produced this version *)
  cid : string option;  (** Content id of this version. Missing when the version was removed or the change is a deletion. *)
  created_at : string;  (** When this knot wrote the commit this version rides in. Not the record's own `createdAt`. *)
  editor : string;  (** The account that authored the change *)
}

(** Jsont codec for {!type:version}. *)
val version_jsont : version Jsont.t

(** Every version of a record this knot hosts, oldest first unless order says otherwise. A version's bytes are fetched by content id through com.atproto.sync.getBlocks. *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  order : string option;  (** asc lists the oldest version first, desc the newest. *)
  uri : string;  (** Record in a repo this knot hosts *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;  (** Cursor for the next page, left out when the page already reaches the newest version *)
  uri : string;
  versions : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListPulls : sig

type pull_list_item = {
  author : string option;  (** The account that opened the record, the editor of its first version. Left out when the author can't be told. *)
  cid : string option;
  comment_count : int;  (** Count of sh.tangled.repo.pull.comment records targeting this pull. *)
  state : string;  (** Latest derived state. *)
  state_updated_at : string option;  (** TID-derived timestamp of the latest pull status record. *)
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.repo.pull record *)
}

(** Jsont codec for {!type:pull_list_item}. *)
val pull_list_item_jsont : pull_list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  author : string option;  (** Restrict to pulls authored by this user DID. *)
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  status : string option;  (** Restrict to pulls whose latest derived status matches. *)
  subject : string;  (** Repo DID to list pulls for *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListIssues : sig

type issue_list_item = {
  author : string option;  (** The account that opened the record, the editor of its first version. Left out when the author can't be told. *)
  cid : string option;
  comment_count : int;  (** Count of sh.tangled.repo.issue.comment records targeting this issue. *)
  state : string;  (** Latest derived state. *)
  state_updated_at : string option;  (** TID-derived timestamp of the latest state record. *)
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.repo.issue record *)
}

(** Jsont codec for {!type:issue_list_item}. *)
val issue_list_item_jsont : issue_list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  author : string option;  (** Restrict to issues authored by this user DID. *)
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  state : string option;  (** Restrict to issues whose latest derived state matches. *)
  subject : string;  (** Repo DID to list issues for *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListForks : sig

type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded org.tangled.repo.manifest record of the fork *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Repo DID to list forks of. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListCollaborators : sig

type list_item = {
  added_by : string;  (** DID that added this collaborator *)
  cid : string option;  (** Optional record CID for record-backed indexers *)
  created_at : string;  (** When the repository offered collaboration *)
  effective_since : string;  (** When the collaboration took effect. This listing sorts and pages on it. *)
  subject : string;  (** DID of the collaborator *)
  uri : string option;  (** Optional record AT-URI for record-backed indexers *)
  verified_at : string option;  (** When the knot resolved the collaborator's own acceptance record. Absent on a grant that predates the consent model. *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by effectiveSince. *)
  subject : string;  (** Repo DID whose collaborator records to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListCollaboratorInvitesBy : sig
(** An offer awaiting the subject's acceptance. *)

type invite_item = {
  added_by : string;  (** DID that made the offer. *)
  created_at : string;  (** When the repository made the offer. This listing sorts it by newest first. *)
  knot : string;  (** did:web of knot hosting this repo. *)
  repo : string;  (** DID of repo that made the offer. *)
  uri : string;  (** Invite record on the repository. *)
}

(** Jsont codec for {!type:invite_item}. *)
val invite_item_jsont : invite_item Jsont.t

(** Collaboration offers an actor DID has waiting for them. The inverse of sh.tangled.repo.listCollaboratorInvites, which knots serve only for their own repo. *)

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Actor DID offered collaboration. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  items : Jsont.json list;
  pending : string list;  (** Knots that this index hasn't read yet *)
  truncated : bool;  (** Per-subject dropped offers from items. If truncated is true, missing offers won't say if they were withdrawn, same with pending knots. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListArtifacts : sig

type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded org.tangled.repo.artifact record *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Repo or release identifier whose artifacts to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetReposByRepoDids : sig

type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** sh.tangled.repo record *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  dids : string list;  (** Repo DIDs to fetch. DIDs that resolve to none are omitted from the response. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  items : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetRepos : sig

type record_view = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.repo record. *)
}

(** Jsont codec for {!type:record_view}. *)
val record_view_jsont : record_view Jsont.t


(** Query/procedure parameters. *)
type params = {
  repos : string list;  (** AT-URIs of the sh.tangled.repo records to fetch. At most 50 per request. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  items : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetRepoByRepoDid : sig

(** Query/procedure parameters. *)
type params = {
  repo_did : string;  (** Repo DID whose sh.tangled.repo record to fetch. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cid : string option;
  knot : string option;  (** Host of the knot named by the #tangled_knot service in the repo's DID document. Absent when it names none. *)
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.repo record. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetRepoByName : sig

(** Query/procedure parameters. *)
type params = {
  name : string;  (** Name of the repo as it appears in its url. *)
  owner : string;  (** DID of the account that owns the repo. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cid : string option;
  knot : string option;  (** Host of the knot named by the #tangled_knot service in the repo's DID document. Absent when it names none. *)
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.repo record. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetRepo : sig

(** Query/procedure parameters. *)
type params = {
  repo : string;  (** AT-URI of the sh.tangled.repo record to fetch. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cid : string option;
  knot : string option;  (** Host of the knot named by the #tangled_knot service in the repo's DID document. Absent when it names none. *)
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.repo record. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetImportStatus : sig
(** How far the knot got importing a repository from its source. Only the repository owner and knot admins see the real reason; everyone else gets a generic one. *)

(** Query/procedure parameters. *)
type params = {
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  reason : string option;  (** Why a partial or failed import ended that way *)
  status : string option;  (** Where the import from the source is. Omitted when the repository wasn't created from a source. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ForkSync : sig
(** Fetch the upstream of a fork into this repository, updating the remote-tracking ref for the given branch. *)


type input = {
  branch : string;  (** Branch to fetch from the upstream *)
  repo : string;  (** DID of the fork repository *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module DeleteRepo : sig
(** Delete a repository *)


type input = {
  force : bool option;  (** Skip the check for a declaration record on the owner's PDS. Knot admins only. *)
  repo : string;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module Defs : sig

type language = {
  name : string;
  size : int;  (** Total size of files in this language, in bytes. *)
}

(** Jsont codec for {!type:language}. *)
val language_jsont : language Jsont.t


type source_external = {
  uri : string;  (** git-clone uri of the repo *)
}

(** Jsont codec for {!type:source_external}. *)
val source_external_jsont : source_external Jsont.t


type source_tangled = {
  did : string;  (** DID of the repo identity *)
}

(** Jsont codec for {!type:source_tangled}. *)
val source_tangled_jsont : source_tangled Jsont.t

(** Metadata about the requesting account's relationship with the subject content. Only has meaningful content for authed requests. *)

type viewer_state = {
  fork : string option;
  star : string option;
}

(** Jsont codec for {!type:viewer_state}. *)
val viewer_state_jsont : viewer_state Jsont.t


type repo_view_basic = {
  created_at : string;
  description : string option;
  did : string;
  fork_count : int option;
  is_starred : bool option;
  open_issue_count : int option;
  open_pull_count : int option;
  owner : Jsont.json;
  primary_language : string option;  (** Largest language on the default branch, by bytes. *)
  slug : string;
  star_count : int option;
  viewer : Jsont.json option;
}

(** Jsont codec for {!type:repo_view_basic}. *)
val repo_view_basic_jsont : repo_view_basic Jsont.t

      end
      module Declaration : sig
(** Git repository declaration. Record is owned by the repository owner and record-key is slug of the git repository. *)

type main = {
  created_at : string;
  repo : string;  (** DID of the repo *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module CountRepos : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Owner DID whose repo records to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CountPulls : sig

(** Query/procedure parameters. *)
type params = {
  author : string option;  (** Restrict to pulls authored by this user DID. *)
  status : string option;  (** Restrict to pulls whose latest derived status matches. *)
  subject : string;  (** Repo DID to list pulls for *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CountIssues : sig

(** Query/procedure parameters. *)
type params = {
  author : string option;  (** Restrict to issues authored by this user DID. *)
  state : string option;  (** Restrict to issues whose latest derived state matches. *)
  subject : string;  (** Repo DID to list issues for *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CountForks : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Repo DID to count forks of. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CountCollaboratorsBy : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Actor DID whose collaborator authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CountCollaborators : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Repo DID whose collaborator records to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CountArtifactsBy : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Actor DID whose artifact authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CountArtifacts : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Repo or release identifier whose artifacts to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Artifact : sig

type main = {
  artifact : Atp.Blob_ref.t;  (** the artifact *)
  created_at : string;  (** time of creation of this artifact *)
  name : string;  (** name of the artifact *)
  subject : Jsont.json;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module Manifest : sig

type declaration = {
  name : string;  (** repository name; the slug (rkey of org.tangled.repo.declaration) is lowercase(name) *)
  owner : string;
}

(** Jsont codec for {!type:declaration}. *)
val declaration_jsont : declaration Jsont.t

(** Git repository manifest record. This record is used for git repository discovery like how `org.tangled.actor.profile` is used to find users. *)

type main = {
  contribution_policy : string option;  (** Who may open tickets and patches on this repo. anyone = any DID, members = the knot's member roll, collaborators = the owner plus repo collaborators. Absent means the knot-wide default. *)
  created_at : string;
  declaration : Jsont.json;
  description : string option;
  source : Jsont.json option;  (** source of the repo *)
  spindle : string option;
  topics : string list option;  (** Topics related to the repo *)
  website : string option;  (** Any URI related to the repo *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module ListPullsBy : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  status : string option;  (** Restrict to pulls whose latest derived status matches. *)
  subject : string;  (** Actor DID whose pull authorings to list *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListIssuesBy : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  state : string option;  (** Restrict to issues whose latest derived state matches. *)
  subject : string;  (** Actor DID whose issue authorings to list *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListCollaboratorsBy : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Actor DID whose collaborator authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListArtifactsBy : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Actor DID whose artifact authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CreateRepo : sig
(** Create a new repository *)


type input = {
  created_at : string option;
  default_branch : string option;  (** Default branch of the repository. *)
  description : string option;
  did : string option;  (** Optional user-provided DID to use as the repo identity instead of minting one. *)
  name : string;  (** Name of the repository. *)
  source : Jsont.json option;  (** A source to clone from, populate this when forking or importing a repository. *)
  spindle : string option;
  topics : string list option;  (** Topics related to the repo *)
  website : string option;  (** Any URI related to the repo *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  did : string option;
  key : string option;  (** Multibase-encoded public signing key the knot holds for this repository *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module Track : sig
      module Unspecced : sig
        module Milestone : sig
(** TBD *)

type main = unit

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

        end
      end
      module Defs : sig

(** Enum values: ["open"], ["closed"], ["completed"] *)
type ticket_state = string
val ticket_state_jsont : ticket_state Jsont.t


type ticket_view_detailed = {
  author : Jsont.json;  (** The account that opened the ticket, the editor of its first version. Not the account that last edited or closed it. *)
  body : Jsont.json option;
  cid : string;
  created_at : string;
  repo : Jsont.json;
  state : Jsont.json;
  tangled_labels : Jsont.json list option;
  target_branch : string option;  (** Present when the ticket is a pull request. *)
  title : Jsont.json;
  uri : string;
}

(** Jsont codec for {!type:ticket_view_detailed}. *)
val ticket_view_detailed_jsont : ticket_view_detailed Jsont.t

      end
      module Ticket : sig

type main = {
  assignees : string list option;
  body : Jsont.json option;
  created_at : string;
  labels : Jsont.json option;  (** Self-label values for this record. *)
  state : Jsont.json;
  tangled_labels : Jsont.json list option;
  target_branch : string option;  (** Target branch name. Must not include the `refs/heads/` prefix. *)
  title : Jsont.json;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module GetTicketView : sig

(** Query/procedure parameters. *)
type params = {
  uri : string;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = Jsont.json

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module Temp : sig
      module Webhook : sig
        module UpdateWebhook : sig
(** Update an existing webhook. Only supplied fields are changed. *)


type input = {
  active : bool option;
  events : string list option;
  id : int;  (** Webhook ID to update. *)
  repo_did : string;  (** DID of the repository as minted by the knot. *)
  secret : string option;
  url : string option;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

        end
        module ToggleWebhook : sig
(** Toggle the active state of a webhook. *)


type input = {
  id : int;  (** Webhook ID to toggle. *)
  repo_did : string;  (** DID of the repository as minted by the knot. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  active : bool;  (** New active state of the webhook. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module RetryDelivery : sig
(** Re-send a specific webhook delivery. *)


type input = {
  delivery_id : string;  (** UUID of the delivery to retry. *)
  repo_did : string;  (** DID of the repository as minted by the knot. *)
  webhook_id : int;  (** Webhook ID that owns the delivery. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

        end
        module ListWebhooks : sig

type webhook = {
  active : bool;  (** Whether the webhook is currently enabled. *)
  created_at : string;
  events : string list;  (** Event types this webhook is subscribed to (e.g. 'push', 'repository:renamed'). *)
  id : int;  (** Webhook identifier. *)
  updated_at : string option;
  url : string;  (** Endpoint URL that receives webhook payloads. *)
}

(** Jsont codec for {!type:webhook}. *)
val webhook_jsont : webhook Jsont.t

(** List webhooks configured for a repository. *)

(** Query/procedure parameters. *)
type params = {
  repo_did : string;  (** DID of the repository as minted by the knot. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  webhooks : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module GetDeliveriesForWebhooks : sig

type delivery = {
  created_at : string;
  delivery_id : string;  (** UUID for tracking this delivery attempt. *)
  event : string;  (** Event type that triggered the delivery. *)
  id : int;
  request_body : string option;
  response_body : string option;
  response_code : int option;
  success : bool;
  url : string;
  webhook_id : int;
}

(** Jsont codec for {!type:delivery}. *)
val delivery_jsont : delivery Jsont.t

(** List recent delivery attempts for the given webhooks. *)

(** Query/procedure parameters. *)
type params = {
  ids : int list;
  limit : int option;
  repo_did : string;  (** DID of the repository as minted by the knot. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  deliveries : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module DeleteWebhook : sig
(** Delete a webhook from a repository. *)


type input = {
  id : int;  (** Webhook ID to delete. *)
  repo_did : string;  (** DID of the repository as minted by the knot. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

        end
        module CreateWebhook : sig
(** Create a new webhook for a repository. *)


type input = {
  active : bool option;  (** Whether the webhook should be active immediately. Defaults to true. *)
  events : string list;  (** Event types to subscribe to (e.g. 'push', 'repository:renamed'). *)
  repo_did : string;  (** DID of the repository as minted by the knot. *)
  secret : string option;  (** Optional HMAC secret used to sign payloads. If omitted, payloads are not signed. *)
  url : string;  (** Endpoint URL that will receive webhook payloads. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  id : int;  (** ID of the newly created webhook. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
      end
      module Spindle : sig
        module Quota : sig
          module Unset : sig
(** remove the quota override for a repository or account owner and resource *)


type input = {
  did : string;  (** DID of the repository or account owner *)
  resource : string;  (** quota resource whose override to remove *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

          end
          module Set : sig
(** set a quota override for a repository or account owner; values use the resource's native units *)


type input = {
  did : string;  (** DID of the repository or account owner *)
  limit : int option;  (** positive maximum in the resource's native units; cannot be combined with unlimited *)
  resource : string;  (** quota resource name, such as workflows, vcpus, memory_mib, disk_mib, or cache_storage_bytes *)
  unlimited : bool option;  (** store an unlimited override; cannot be combined with limit *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

          end
          module ReleaseNars : sig
(** give back the cache storage quota of nars an operator removed from the nix cache. admin only and idempotent, unknown nars are reported and not an error *)


type input = {
  nars : string list;  (** nars to release, as cache keys like nar/<hash>.nar.xz (a bare file name works too) or as nix store paths like /nix/store/<hash>-name *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  bytes_ : int;  (** cache storage bytes given back *)
  released : int;  (** how many allocations were deleted *)
  unmatched : string list;  (** the inputs that matched no allocation *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

          end
          module Defs : sig
(** a per-subject override for one quota resource *)

type limit = {
  did : string;  (** DID whose quota is overridden, either a repository or an account owner *)
  limit : int;  (** maximum amount in the resource's native units; -1 means unlimited *)
  resource : string;  (** quota resource name *)
}

(** Jsont codec for {!type:limit}. *)
val limit_jsont : limit Jsont.t

(** current usage aggregated for one subject and resource *)

type usage = {
  did : string;  (** DID for the user or repository scope *)
  resource : string;  (** quota resource name *)
  scope : string;  (** aggregation axis for this row: user or repo *)
  used : int;  (** amount currently allocated or reserved in native resource units *)
}

(** Jsont codec for {!type:usage}. *)
val usage_jsont : usage Jsont.t

          end
          module Usage : sig
(** list non-zero quota usage aggregated by repository and account owner *)

(** Query/procedure parameters. *)
type params = {
  did : string option;  (** limit results to rows for this subject DID *)
  scope : string option;  (** limit results to one aggregation axis: user or repo *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  usages : Jsont.json list;  (** non-zero usage rows, including active reservations and committed allocations *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

          end
          module List : sig
(** list all per-subject quota overrides *)


type output = {
  limits : Jsont.json list;  (** quota overrides ordered by DID and resource *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

          end
          module Get : sig
(** return a per-subject quota override for one resource *)

(** Query/procedure parameters. *)
type params = {
  did : string;  (** DID of the repository or account owner *)
  resource : string;  (** quota resource to look up *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  limit : Jsont.json;  (** quota override for the requested DID and resource *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

          end
        end
        module Moderation : sig
          module Unban : sig
(** remove an existing ban for a repository or account owner *)


type input = {
  did : string;  (** DID of the repository or account owner to unban *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

          end
          module Defs : sig
(** an active ban for a repository or account owner *)

type ban = {
  created_at : string;  (** time at which the ban was created *)
  did : string;  (** DID of the banned subject, either a repository or its owner *)
}

(** Jsont codec for {!type:ban}. *)
val ban_jsont : ban Jsont.t

          end
          module Ban : sig
(** ban a repository or account owner and immediately wipe matching spindle state *)


type input = {
  did : string;  (** DID of the repository or account owner to ban *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

          end
          module ListBans : sig
(** list all active repository and account-owner bans *)


type output = {
  bans : Jsont.json list;  (** active bans ordered by subject DID *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

          end
          module GetBan : sig
(** return the active ban for a repository or account owner DID *)

(** Query/procedure parameters. *)
type params = {
  did : string;  (** DID of the repository or account owner to look up *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  ban : Jsont.json;  (** active ban matching the requested DID *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

          end
        end
        module AllowList : sig
          module Remove : sig
(** Remove a DID from this spindle's allow-list. Only callable by a spindle admin. *)


type input = {
  did : string;  (** DID to remove from allow-list. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

          end
          module List : sig
(** Read this spindle's allow-list. Only callable by a spindle admin. *)


type output = {
  dids : string list;  (** Allow-list entries. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

          end
          module Add : sig
(** Add a DID to this spindle's allow-list, so the spindle runs CI for its repos while admission is closed. Only callable by a spindle admin. *)


type input = {
  did : string;  (** DID of account that should be allowed to enroll repos on this here spindle. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

          end
        end
      end
      module Site : sig
        module ReleaseDomain : sig
(** Release the authenticated user's active domain claim, removing all associated site data. *)


type input = {
  domain : string;  (** Full domain to release (must match the user's active claim). *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

        end
        module GetDomainClaim : sig
(** Get the active sites domain or a verified signup handle for the authenticated user, if any. A signup handle may not have a D1 claim row until the first site write. *)


type output = {
  domain : string option;  (** the site domain (e.g. 'alice.tngl.io') or a verified signup handle, absent if neither exists *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module ClaimDomain : sig
(** Claim a full sites domain (e.g. alice.tngl.io) for the authenticated user. A claim is one per account. tngl.sh handles require confirmation from the handle authority and cannot be released. *)


type input = {
  domain : string;  (** Full domain to claim, e.g. alice.tngl.io. Label rules: 1-63 lowercase letters, digits and hyphens, no leading/trailing hyphen. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

        end
      end
      module Server : sig
        module UploadBlob : sig
(** Upload a blob into a fostered repo on this knot. This endpoint is possible only when legacy admin secret is in use. *)

(** Query/procedure parameters. *)
type params = {
  repo_did : string;  (** Repo DID hosted on this knot that the blob belongs to. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type input = unit
val input_jsont : input Jsont.t


type output = {
  blob : Atp.Blob_ref.t;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module SeedRepo : sig

type record_result = {
  collection : string;
  error : string option;  (** Reason that a record wasn't written. *)
  rkey : string;
}

(** Jsont codec for {!type:record_result}. *)
val record_result_jsont : record_result Jsont.t


type seed_record = {
  author : string;  (** DID which will be attributed to the record as the author. *)
  collection : string;
  created_at : int;  (** Creation time in Unix seconds. *)
  record : Jsont.json;  (** Record body, DAG-CBOR w/o lex validation. Max 1,000,000 bytes post-encode. *)
  rkey : string;
}

(** Jsont codec for {!type:seed_record}. *)
val seed_record_jsont : seed_record Jsont.t

(** Foster an empty git repo TEMPORARILY for an owner from outside the knot, and write the given manifest and records on this knot without lex validation. This endpoint is possible only when legacy admin secret is in use. *)


type input = {
  manifest : Jsont.json;  (** Raw `org.tangled.repo.manifest` record body *)
  records : Jsont.json list;  (** Records to write besides the manifest. Max 200 per req. *)
  repo_did : string;  (** Existing repo DID which our COB records belong to. The foster knot will serve this DID as a shadow, and will give the record (authority) back to the principal knot once that given knot that *actually* owns the repo DID upgrades! *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  did : string;
  key : string;  (** Multibase-encoded public signing key that the knot has for this repo. *)
  results : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module RedeemInviteCode : sig
(** Clarifies the membership in server via invite code. *)


type input = {
  code : string;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

        end
        module HandoverRepo : sig
(** Pull a hosted repo's shadowed COB records back from a given foster knot, rebuild the record chain under this knot's own authority, confirm record CIDs against foster knot's state, finally ack the handover. Since it may take a bit, the endpoint will already give a response as soon as the job is staged up with state "running". Then the next steps run in background finish into the sidecar + logs. Only a knot admin can call this! *)


type input = {
  foster_url : string;
  repo_did : string;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  repo_did : string;
  state : string;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module CreateInviteCode : sig
(** Create an invite code. Similar to `com.atproto.server.createInviteCode` but used for Knot/Spindle membership. *)


type input = {
  for_account : string option;
  use_count : int;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  code : string;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module CancelHandover : sig
(** Abandoning a repo's handover job mid-way; sidecar is removed, boot replay will stop retrying sidecarring. Only a knot admin should/could ever call this. *)


type input = {
  repo_did : string;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  repo_did : string;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module AckHandover : sig
(** Upgrading knot sends this once it is ready to serve *every* fostered record of a repo - at exact CIDs the foster knot was giving out at ingestion time. The signature thingy is this 3rd party requesting-knot's key over dag-cbor of the same fields using the context org.tangled.temp.handover.ack.v1, such that the foster knot can verify it against the requester's did:web doc. *)


type input = {
  acked_at : int;  (** Ack time in Unix seconds. *)
  records : int;
  repo_did : string;
  signature : string;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

        end
      end
      module Search : sig
        module SearchCode : sig

type highlight = {
  end_ : int;  (** End byte offset (exclusive) within the chunk content string. *)
  start : int;  (** Start byte offset within the chunk content string. *)
}

(** Jsont codec for {!type:highlight}. *)
val highlight_jsont : highlight Jsont.t


type chunk = {
  content : string;  (** Source lines for this match chunk. *)
  highlights : Jsont.json list option;  (** Byte-offset ranges within content that match the query. *)
  line_start : int;  (** 1-based line number of the first line in content. *)
}

(** Jsont codec for {!type:chunk}. *)
val chunk_jsont : chunk Jsont.t


type file_result = {
  branches : string list option;
  chunks : Jsont.json list option;  (** set for content matches *)
  commit : string;
  language : string option;  (** Detected programming language. *)
  path : string;  (** File path relative to repository root. *)
  ranges : Jsont.json list option;  (** set for a filename match *)
  repo : Jsont.json;
}

(** Jsont codec for {!type:file_result}. *)
val file_result_jsont : file_result Jsont.t

(** Full-text code search across indexed repositories, backed by Zoekt. *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;
  limit : int option;
  q : string;  (** Search query string. *)
  repo : string option;  (** Restrict search to a specific repository, by its DID. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  results : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
      end
      module Review : sig
        module ListPatches : sig

type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded org.tangled.review.patch record *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  ticket : string;  (** AT-URI of the org.tangled.track.ticket the patches belong to. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
      end
      module Repo : sig
        module UpdateSiteConfig : sig
(** Create or update the site configuration for a repository owned by the authenticated user. Writing the config schedules a deploy attempt; the deploy outcome is recorded in history. If the deploy service is disabled, the config is saved but WorkerUnavailable is returned. *)


type input = {
  branch : string;  (** Branch to deploy from. *)
  dir : string;  (** Directory within the repository to deploy (e.g. '/' or '/docs'). *)
  is_index : bool option;  (** Whether this repo should serve as the index (root) for the claimed domain. *)
  knot : string;  (** Knot host that hosts the repository. *)
  name : string;  (** Repository name as its knot reports it (the first path segment its sub-path site is served under). *)
  repo_did : string;  (** DID of the repository as minted by the knot. *)
  rkey : string;  (** Atproto record key of the repository; identifies the \{did\}/\{rkey\}/ prefix in R2. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

        end
        module ListRecordOrigins : sig

type origin = {
  author : string;  (** The DID of the account that opened the record, the editor of its first version. *)
  change_id : string;  (** Git Object ID of the change that opened the record. *)
  created_at : string;  (** When the knot wrote the opening change. Not the record's own `createdAt`. *)
  rkey : string;
}

(** Jsont codec for {!type:origin}. *)
val origin_jsont : origin Jsont.t

(** Who opened each COB record of one collection in a repository, and when. Gives the same author as the first version in `org.tangled.repo.listRecordVersions`, for every record in one walk, sorted by record key. *)

(** Query/procedure parameters. *)
type params = {
  collection : string;  (** A versioned collection, such as `org.tangled.track.ticket`. *)
  cursor : string option;  (** Pass the cursor from one page into the next query to keep walking records. *)
  limit : int option;
  repo : string;  (** DID of the repository. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  collection : string;
  cursor : string option;  (** Left out when the page already reaches the last record. *)
  origins : Jsont.json list;
  repo : string;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module GetSiteConfig : sig

type site_config = {
  branch : string;  (** Branch to deploy from. *)
  dir : string;  (** Directory within the repository to deploy (e.g. '/' or '/docs'). *)
  is_index : bool;  (** Whether this repo serves as the index (root) for the domain. *)
}

(** Jsont codec for {!type:site_config}. *)
val site_config_jsont : site_config Jsont.t

(** Get the site configuration for a repository, if one exists. *)

(** Query/procedure parameters. *)
type params = {
  repo_did : string;  (** DID of the repository as minted by the knot. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  config : Jsont.json option;  (** Site config for the repository. Absent if no site is configured. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module GetRepoByName : sig

type git_info = {
  default_branch : string;  (** Default branch name from `org.tangled.git.refs/self` record. *)
  is_empty : bool;  (** True if `refs` in `org.tangled.git.refs/self` record is empty. *)
  languages : Jsont.json list option;  (** Languages on the default branch ordered by size, largest first. Absent until the mirror has indexed the repo. *)
}

(** Jsont codec for {!type:git_info}. *)
val git_info_jsont : git_info Jsont.t


(** Query/procedure parameters. *)
type params = {
  name : string;  (** Name of the repo as it appears in its url. *)
  owner : string;  (** DID of the account that owns the repo. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cid : string option;
  git : Jsont.json option;
  knot : string option;  (** Host of the knot named by the #tangled_knot service in the repo's DID document. Absent when it names none. *)
  uri : string;
  value : Jsont.json;  (** Embedded org.tangled.repo.manifest record. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module GetDeployHistory : sig

type deploy = {
  branch : string;  (** Branch the deploy built. *)
  commit_sha : string;  (** Commit the deploy built. Empty for a config-change deploy, which builds the branch tip at deploy time. *)
  created_at : string;  (** When the deploy finished. *)
  dir : string;  (** Directory within the repository the deploy built (e.g. '/' or '/docs'). *)
  error : string;  (** Why the deploy failed. Empty when it succeeded. *)
  status : string;  (** Outcome of the deploy. *)
  trigger : string;  (** What asked for the deploy: a site config change, a push to the deployed branch, or a manual redeploy. *)
}

(** Jsont codec for {!type:deploy}. *)
val deploy_jsont : deploy Jsont.t

(** Get the recent deploy history for a repository, newest first. Authenticated with the caller's serviceauth token (audience = the sites worker's DID); only the repository owner may read it. *)

(** Query/procedure parameters. *)
type params = {
  repo_did : string;  (** DID of the repository as minted by the knot. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  deploys : Jsont.json list;  (** The 20 most recent deploys, newest first. Empty when the repository has never deployed. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module DisableSite : sig
(** Remove the site configuration for a repository, deleting its deployed files. *)


type input = {
  repo_did : string;  (** DID of the repository as minted by the knot. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

        end
        module DeploySite : sig
(** Manually deploy a repository's site now. Authenticated with the caller's serviceauth token (audience = the sites worker's DID); only the repository owner may trigger. Queues the deploy; the outcome is recorded in the repository's deploy history. *)


type input = {
  repo_did : string;  (** DID of the repository to deploy. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

        end
      end
      module Notification : sig
        module UpdateSeen : sig
(** Mark a single notification as read or unread. *)


type input = {
  read : bool;
  uri : string;  (** notification key as returned by listNotifications; pass it back unchanged. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

        end
        module UpdatePreferences : sig
(** Update notification preferences for the authenticated user. Only provided fields are updated. *)


type input = {
  email_notifications : bool option;
  followed : bool option;
  issue_closed : bool option;
  issue_commented : bool option;
  issue_created : bool option;
  pull_commented : bool option;
  pull_created : bool option;
  pull_merged : bool option;
  repo_starred : bool option;
  user_mentioned : bool option;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

        end
        module MarkEntityRead : sig
(** Mark all of the caller's notifications for a given issue or pull as read. Used when the user visits the entity directly, rather than through a notification. *)


type input = {
  entity_at : string;  (** at-uri of the issue or pull whose notifications should be marked read. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

        end
        module MarkAllRead : sig
(** Mark all of the authenticated user's notifications as read. *)

        end
        module ListRecipients : sig
(** Service-internal. Hosted by bobbin. Given an entity at-uri or repo DID, returns the dids subscribed to it. deliberi calls this while ingesting the firehose to fan a notification out to subscribers. *)

(** Query/procedure parameters. *)
type params = {
  collection : string option;  (** Optional collection NSID to filter subscribers by subscription scope. Only subscribers with no collection filter or a filter containing this NSID are returned. *)
  subject : string;  (** at-uri of the entity (for entity-level subscribers) or a repo DID (for repo-level subscribers). *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  dids : string list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module ListNotifications : sig

type notification = {
  actor_did : string;  (** DID of the user who triggered this notification. *)
  category : string;  (** Broad category: 'social' or 'work'. *)
  created_at : string;
  issue_at : string option;  (** AT-URI of the related org.tangled.issue.issue record, if applicable. *)
  pull_at : string option;  (** AT-URI of the related org.tangled.pulls.pull record, if applicable. *)
  read : bool;
  repo_did : string option;  (** DID of the related repository, if applicable. *)
  type_ : string;  (** Notification type: repo_starred, issue_created, issue_commented, issue_closed, issue_reopen, issue_assigned, issue_unassigned, pull_created, pull_commented, pull_merged, pull_closed, pull_reopen, pull_assigned, pull_unassigned, followed, user_mentioned, knot_invited, collaborator_invited. *)
  uri : string;  (** notification key; pass it back to updateSeen and deleteNotification. *)
}

(** Jsont codec for {!type:notification}. *)
val notification_jsont : notification Jsont.t

(** List notifications for the authenticated user, split by category. *)

(** Query/procedure parameters. *)
type params = {
  category : string option;  (** Filter by category: 'all', 'social', or 'work'. *)
  cursor : string option;
  limit : int option;  (** Max notifications per category to return. *)
  read : string option;  (** Filter by read state: 'all' or 'unread'. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  notifications : Jsont.json list;
  social_unread_count : int;
  work_unread_count : int;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module GetUnreadCount : sig
(** Get the total number of unread notifications for the authenticated user. Used to drive the bell badge. *)


type output = {
  count : int;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module GetPreferences : sig

type preferences = {
  email_notifications : bool;
  followed : bool;
  issue_closed : bool;
  issue_commented : bool;
  issue_created : bool;
  pull_commented : bool;
  pull_created : bool;
  pull_merged : bool;
  repo_starred : bool;
  user_mentioned : bool;
}

(** Jsont codec for {!type:preferences}. *)
val preferences_jsont : preferences Jsont.t

(** Get notification preferences for the authenticated user. *)


type output = Jsont.json

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module DeleteNotification : sig
(** Delete a notification. *)


type input = {
  uri : string;  (** notification key as returned by listNotifications; pass it back unchanged. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

        end
      end
      module Newsletter : sig
        module Signup : sig
(** Add an email address to the newsletter. Unauthenticated; signed-in accounts should use org.tangled.temp.account.subscribeNewsletter so the choice is remembered. *)


type input = {
  email : string;  (** Email address to subscribe. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

        end
      end
      module Migrator : sig
        module DescribeSource : sig
(** Describe the git repository at a source URL before migrating it. *)

(** Query/procedure parameters. *)
type params = {
  source_url : string;  (** HTTPS clone URL of the source repository. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  default_branch : string;  (** Branch HEAD points at on the remote. *)
  name : string;  (** Repository name derived from the URL. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module Defs : sig
(** Migration status of a repository. *)

type job = {
  attempts : int;  (** Number of migration attempts. *)
  error : string option;  (** Error message from the last failed attempt. *)
  id : string;  (** Unique job ID. *)
  knot_did : string;
  name : string;
  private_ : bool;
  repo_did : string option;  (** DID of the created repository, once created on the knot. *)
  source_url : string;
  status : string;  (** Current status of the migration job. *)
}

(** Jsont codec for {!type:job}. *)
val job_jsont : job Jsont.t

(** A repository to migrate. *)

type new_job = {
  description : string option;  (** Repository description. *)
  knot_did : string;  (** DID of the destination knot. *)
  name : string;  (** Repository name reserved on the destination knot. *)
  private_ : bool option;  (** Whether the source repository is private. *)
  source_url : string;  (** HTTPS URL of the source repository. *)
}

(** Jsont codec for {!type:new_job}. *)
val new_job_jsont : new_job Jsont.t

(** A migration task containing repository jobs. *)

type task = {
  created_at : string;
  id : string;
  jobs : Jsont.json list;
  owner_did : string;  (** DID of the user who created the task. *)
  updated_at : string option;
}

(** Jsont codec for {!type:task}. *)
val task_jsont : task Jsont.t

        end
        module RetryJob : sig
(** Retry a failed or unauthorized migration job. *)


type input = {
  github_token : string option;  (** GitHub token for retrying a private repository. *)
  job_id : string;  (** ID of the job to retry. *)
  task_id : string;  (** ID of the task the job belongs to. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = Jsont.json

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module ListTasks : sig
(** List migration tasks for the caller. *)

(** Query/procedure parameters. *)
type params = {
  limit : int option;  (** Maximum number of tasks to return. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  tasks : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module GetTask : sig
(** Fetch a migration task and its jobs. *)

(** Query/procedure parameters. *)
type params = {
  task_id : string;  (** ID of the task to fetch. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = Jsont.json

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module CreateTask : sig
(** Create a task to migrate repositories into knots. *)


type input = {
  github_token : string option;  (** GitHub token for reading private source repositories. *)
  jobs : Jsont.json list;
  request_id : string;  (** Idempotency key for this task. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = Jsont.json

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
      end
      module Git : sig
        module SyncFork : sig
(** Update a branch on a fork to match the same branch on its upstream *)


type input = {
  branch : string;
  repo : string;  (** DID of the fork repository *)
  source : string;  (** DID of the upstream repository *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

        end
        module MergeCommit : sig
(** Merge commit content. Used when merge commit is created. *)

type merge_commit_content = {
  author : Jsont.json option;
  message : string option;
}

(** Jsont codec for {!type:merge_commit_content}. *)
val merge_commit_content_jsont : merge_commit_content Jsont.t


type source = {
  commit : string;  (** Head commit ID to merge *)
  repo : string;  (** DID of source git repository *)
}

(** Jsont codec for {!type:source}. *)
val source_jsont : source Jsont.t


type target = {
  branch : string;  (** Target branch to merge into *)
  commit : string;  (** Head commit ID of target branch *)
  repo : string;  (** DID of target git repository *)
}

(** Jsont codec for {!type:target}. *)
val target_jsont : target Jsont.t

(** Merge a commit into repository's branch *)


type input = {
  merge_commit : Jsont.json option;  (** Merge commit content. Used when merge commit is created. *)
  source : Jsont.json;
  style : string;
  target : Jsont.json;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

        end
        module ListTags : sig

type annotation = {
  message : string;  (** Tag message *)
  tagger : Jsont.json option;
}

(** Jsont codec for {!type:annotation}. *)
val annotation_jsont : annotation Jsont.t


type tag_info = {
  annotation : Jsont.json option;  (** Present only for annotated tags *)
  artifacts : Jsont.json list option;  (** org.tangled.repo.artifact records whose subject is this tag's oid *)
  author : Jsont.json;
  message : string;  (** Latest commit message *)
  name : string;  (** Tag name *)
  oid : string;  (** Latest commit oid on this tag *)
}

(** Jsont codec for {!type:tag_info}. *)
val tag_info_jsont : tag_info Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;  (** Maximum number of tags to return *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  tags : Jsont.json list;
  total : int;  (** Total number of tags in the repository *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module ListCommits : sig

type header = {
  key : string;
  value : string;
}

(** Jsont codec for {!type:header}. *)
val header_jsont : header Jsont.t


type commit = {
  author : Jsont.json;
  committer : Jsont.json;
  extra_headers : Jsont.json list;
  message : string;
  oid : string;
  parents : string list;
  stat : Jsont.json option;
  tree : string;
}

(** Jsont codec for {!type:commit}. *)
val commit_jsont : commit Jsont.t


(** Query/procedure parameters. *)
type params = {
  include_stats : bool option;  (** Diff each commit against its first parent and include a line stat *)
  limit : int option;  (** Maximum number of commits to return *)
  ranges : string list option;  (** Commit ids or `<commit>..<commit>` ranges *)
  repos : string list;  (** DIDs of the repositories to read objects from *)
  skip : int option;  (** Number of matching commits to skip. A poor man's pagination. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  commits : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module ListBranches : sig

type branch_info = {
  author : Jsont.json;
  committer : Jsont.json;
  message : string;  (** Latest commit message *)
  name : string;  (** Branch name *)
  oid : string;  (** Latest commit oid on this branch *)
}

(** Jsont codec for {!type:branch_info}. *)
val branch_info_jsont : branch_info Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;  (** Maximum number of branches to return *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  branches : Jsont.json list;
  cursor : string option;
  default_branch : string option;  (** Name of the default branch *)
  total : int;  (** Total number of branches in the repository *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module GetTree : sig

type tree_entry = {
  last_commit : Jsont.json option;
  mode : string;  (** File mode *)
  name : string;  (** Relative file or directory name *)
  size : int;  (** File size in bytes *)
}

(** Jsont codec for {!type:tree_entry}. *)
val tree_entry_jsont : tree_entry Jsont.t


(** Query/procedure parameters. *)
type params = {
  path : string option;  (** Path within the repository tree *)
  ref_ : string option;  (** Git revision (branch, tag, or commit id) *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  entries : Jsont.json list;
  last_commit : Jsont.json option;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module GetMergeBase : sig

(** Query/procedure parameters. *)
type params = {
  base : string;  (** Commit used to find the common base. *)
  head : string;  (** Commit to list history for. *)
  repos : string list;  (** DIDs of the repositories to read objects from *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  commit : string;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module GetEntry : sig
(** get metadata of blob by ref and path *)

(** Query/procedure parameters. *)
type params = {
  path : string;  (** path of the entity *)
  ref_ : string option;  (** Git revision (branch, tag, or commit id) *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  last_commit : Jsont.json option;
  mode : string;
  name : string;  (** The file name *)
  oid : string;
  size : int;  (** Blob size *)
  submodule : Jsont.json option;  (** Submodule information if path is a submodule *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module GetBranch : sig

(** Query/procedure parameters. *)
type params = {
  name : string;  (** Branch name to get information for *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  author : Jsont.json;
  hash : string;  (** Latest commit hash on this branch *)
  message : string;  (** Latest commit message *)
  name : string;  (** Branch name *)
  total_commits : int;  (** Number of commits reachable from this branch *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module GetBlob : sig

(** Query/procedure parameters. *)
type params = {
  download : bool option;  (** Serve the blob as an attachment named after the file *)
  path : string;  (** Path within the repository tree *)
  ref_ : string option;  (** Git reference (branch, tag, or commit SHA) *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t

(** raw blob served in octet-stream *)

type output = unit
val output_jsont : output Jsont.t

        end
        module GetArchive : sig

(** Query/procedure parameters. *)
type params = {
  format : string option;  (** Archive format *)
  prefix : string;  (** Prefix for files in the archive *)
  ref_ : string;  (** Git reference (branch, tag, or commit SHA) *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t

(** Binary archive data *)

type output = unit
val output_jsont : output Jsont.t

        end
        module FormatPatch : sig
(** Serve the commits reachable from 'to' but not from 'from' as a git format-patch mailbox, oldest first. *)

(** Query/procedure parameters. *)
type params = {
  from : string;  (** Revision the series starts after (exclusive) *)
  repo : string;  (** DID of the repository *)
  to_ : string;  (** Revision the series ends at (inclusive) *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t

(** Patch series as text/plain *)

type output = unit
val output_jsont : output Jsont.t

        end
        module DeleteBranch : sig
(** Delete a branch on this repository *)


type input = {
  branch : string;
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

        end
        module ListCommitsByRef : sig

(** Query/procedure parameters. *)
type params = {
  include_stats : bool option;  (** Diff each commit against its first parent and include a line stat *)
  limit : int option;  (** Maximum number of commits to return *)
  ranges : string list option;
  repo : string;  (** DID of the repository *)
  skip : int option;  (** Number of matching commits to skip. A poor man's pagination. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  commits : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module GetTag : sig

(** Query/procedure parameters. *)
type params = {
  repo : string;  (** DID of the repository *)
  tag : string;  (** Name of tag, such as v1.3.0 *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  annotation : Jsont.json option;  (** Present only for annotated tags *)
  author : Jsont.json;
  message : string;  (** Latest commit message *)
  name : string;  (** Tag name *)
  oid : string;  (** Latest commit oid on this tag *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module GetRef : sig

(** Query/procedure parameters. *)
type params = {
  ref_ : string;  (** Reference name. Such as master, refs/heads/master, or v1.16.0-alpha. *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  annotation : Jsont.json option;  (** Present only for annotated tags *)
  author : Jsont.json;
  message : string;  (** Latest commit message *)
  name : string;  (** Full reference name. Example: refs/heads/master *)
  oid : string;  (** Latest commit oid on this reference. *)
  total_commits : int;  (** Number of commits reachable from this tip *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
      end
      module Focus : sig
        module NextItem : sig
(** Mark the current focus notification as read and return the next one. Ends focus mode and returns no item when the queue is exhausted. *)


type input = {
  current_id : int;  (** ID of the notification just addressed, to be marked read. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  issue_at : string option;  (** AT-URI of the issue to navigate to, if the item is an issue. *)
  notification_id : int option;  (** ID of the next notification to address. Absent when focus mode has ended. *)
  pull_at : string option;  (** AT-URI of the pull to navigate to, if the item is a pull. *)
  repo_did : string option;  (** DID of the repository the next item belongs to. Absent when focus mode has ended. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module EndSession : sig
(** Deactivate focus mode. *)

        end
        module BeginSession : sig
(** Activate focus mode. Returns the oldest unread work notification so the client can navigate to it. *)


type output = {
  issue_at : string option;  (** AT-URI of the issue to navigate to, if the item is an issue. *)
  notification_id : int option;  (** ID of the first notification to address. Absent if the queue is empty. *)
  pull_at : string option;  (** AT-URI of the pull to navigate to, if the item is a pull. *)
  repo_did : string option;  (** DID of the repository the item belongs to. Absent if the queue is empty. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
      end
      module Actor : sig
        module Defs : sig

type onboarding_pref = {
  completed_at : string option;
  nudge_dismissed_at : string option;
  started_at : string;
}

(** Jsont codec for {!type:onboarding_pref}. *)
val onboarding_pref_jsont : onboarding_pref Jsont.t


type recent_issue = {
  open_ : bool;
  owner_handle : string;
  repo_name : string;
  rkey : string;
  title : string;
  visited : string;
}

(** Jsont codec for {!type:recent_issue}. *)
val recent_issue_jsont : recent_issue Jsont.t


type recent_pull = {
  owner_handle : string;
  repo_name : string;
  rkey : string;
  state : string;
  title : string;
  visited : string;
}

(** Jsont codec for {!type:recent_pull}. *)
val recent_pull_jsont : recent_pull Jsont.t


type recent_repo = {
  name : string;
  owner_did : string option;
  owner_handle : string;
  visited : string;
}

(** Jsont codec for {!type:recent_repo}. *)
val recent_repo_jsont : recent_repo Jsont.t


type recents_pref = {
  items : Jsont.json list;
}

(** Jsont codec for {!type:recents_pref}. *)
val recents_pref_jsont : recents_pref Jsont.t


type saved_service = {
  id : string;
  value : string;
}

(** Jsont codec for {!type:saved_service}. *)
val saved_service_jsont : saved_service Jsont.t


type saved_knot_pref = {
  items : Jsont.json list;
}

(** Jsont codec for {!type:saved_knot_pref}. *)
val saved_knot_pref_jsont : saved_knot_pref Jsont.t


type saved_spindle_pref = {
  items : Jsont.json list;
}

(** Jsont codec for {!type:saved_spindle_pref}. *)
val saved_spindle_pref_jsont : saved_spindle_pref Jsont.t

        end
      end
      module Account : sig
        module VerifyEmail : sig
(** Verify an email address using the token from the verification link emailed by addEmail. The address becomes verified, and is made primary if it is the account's first verified address. Tokens are single-use. *)


type input = {
  token : string;  (** Verification token from the emailed link. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  email : string;  (** The address that was verified. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module SubscribeNewsletter : sig
(** Opt the authenticated account into newsletter / announcement emails. *)

        end
        module SetPrimaryEmail : sig
(** Set an email address as the primary address for the authenticated account. The address must already be verified. *)


type input = {
  email : string;  (** Email address to promote to primary. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

        end
        module ResendSignup : sig
(** Re-send the signup verification link for a pending signup. Re-mails the same live link (same token, no new invite code) for a signup begun but not yet completed; fails when nothing is in flight for the email or the link has expired. *)


type input = {
  email : string;  (** Email address used to begin the signup. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

        end
        module ListEmails : sig

type email = {
  address : string;  (** Email address. *)
  created_at : string;
  primary : bool;  (** Whether this is the primary email address. *)
  verified : bool;  (** Whether the address has been verified. *)
}

(** Jsont codec for {!type:email}. *)
val email_jsont : email Jsont.t

(** List all email addresses associated with the authenticated account. *)


type output = {
  emails : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module GetSignup : sig
(** Look up a pending signup by its link token, so the completion form can reject a dead link before the user fills it in. *)

(** Query/procedure parameters. *)
type params = {
  token : string;  (** Verification token from the emailed link. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  email : string;  (** Email address the signup was started with. *)
  pds : string;  (** PDS the account will be created on, where the browser starts the OAuth request it passes to completeSignup. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module GetNewsletterPreference : sig
(** Get the authenticated account's newsletter choice. status is absent when the account has neither subscribed nor dismissed the prompt. *)


type output = {
  status : string option;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module DismissNewsletter : sig
(** Dismiss the newsletter sign-up prompt without subscribing. *)

        end
        module DeleteEmail : sig
(** Remove an email address from the authenticated account. Cannot remove the primary address. *)


type input = {
  email : string;  (** Email address to remove. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

        end
        module CompleteSignup : sig
(** The OAuth authorization response for requestUri, as the PDS would have put it in the redirect. *)

type authorization = {
  code : string;
  iss : string;
  state : string;
}

(** Jsont codec for {!type:authorization}. *)
val authorization_jsont : authorization Jsont.t

(** Complete signup with the link token, choosing a username and password. Deliberi provisions the PDS account internally using a pre-minted invite code. *)


type input = {
  password : string;  (** Password for the new account. *)
  request_uri : string option;  (** request_uri of the browser's pushed OAuth authorization request at the PDS. When the PDS supports it, the new account authorizes that request without the PDS sign-in and consent screens. *)
  token : string;  (** Verification token from the emailed link. *)
  username : string;  (** Desired username; the handle is <username>.<pds domain>. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  authorization : Jsont.json option;  (** Present when the PDS authorized requestUri for the new account. Without it the browser signs in through the PDS. *)
  did : string;  (** DID of the newly provisioned account. *)
  handle : string;  (** The assigned handle, <username>.<pds domain>. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module BeginSignup : sig
(** Begin signup: validate the Turnstile token and email a verification code to the address. *)


type input = {
  email : string;  (** Email address for the new account. *)
  turnstile_token : string;  (** Cloudflare Turnstile challenge response token. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

        end
        module AddEmail : sig
(** Add an email address to the authenticated account. Sends a verification code; the address remains unverified until the code is submitted via verifyEmail. Calling again for an already-added unverified address resends a fresh code. *)


type input = {
  email : string;  (** Email address to add. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

        end
      end
    end
    module Feed : sig
      module Subscription : sig

type record = {
  uri : string;
}

(** Jsont codec for {!type:record}. *)
val record_jsont : record Jsont.t


type repo = {
  did : string;
}

(** Jsont codec for {!type:repo}. *)
val repo_jsont : repo Jsont.t


type main = {
  created_at : string;
  ignore : bool option;  (** mute this thread *)
  subject : Jsont.json;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module Star : sig

type record = {
  record : Jsont.json;
}

(** Jsont codec for {!type:record}. *)
val record_jsont : record Jsont.t


type repo = {
  identity : string;
}

(** Jsont codec for {!type:repo}. *)
val repo_jsont : repo Jsont.t


type main = {
  created_at : string;
  subject : Jsont.json;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module Reaction : sig

type main = {
  created_at : string;
  reaction : string;
  subject : Jsont.json;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module ListStars : sig

type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.feed.star record *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Repo DID to list star edges for. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListReactions : sig

type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded org.tangled.feed.reaction record *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Record AT-URI the reactions target. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetTimeline : sig
(** A user followed another user. *)

type follow_event = {
  actor : Jsont.json;
  cid : string;
  subject : Jsont.json;
  uri : string;
}

(** Jsont codec for {!type:follow_event}. *)
val follow_event_jsont : follow_event Jsont.t

(** A repository was created (or forked). The actor is repo.owner. *)

type repo_event = {
  cid : string;
  repo : Jsont.json;
  source : Jsont.json option;  (** Fork parent, present only if this repo is a fork. *)
  uri : string;
}

(** Jsont codec for {!type:repo_event}. *)
val repo_event_jsont : repo_event Jsont.t

(** A repository was starred. *)

type star_event = {
  actor : Jsont.json;  (** Who starred. *)
  cid : string;
  repo : Jsont.json;  (** The starred repo. *)
  uri : string;
}

(** Jsont codec for {!type:star_event}. *)
val star_event_jsont : star_event Jsont.t


type timeline_item = {
  event : Jsont.json;
  event_at : string;
}

(** Jsont codec for {!type:timeline_item}. *)
val timeline_item_jsont : timeline_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;
  following_only : bool option;
  limit : int option;
  viewer : string option;  (** TODO: deprecate this and use auth instead. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  feed : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetStar : sig

(** Query/procedure parameters. *)
type params = {
  actor : string;  (** DID of the potential stargazer. *)
  subject : string;  (** Repo DID to check. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  uri : string;  (** Uri of actor's star record for subject. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Defs : sig

type comment_view = {
  body : Jsont.json;
  cid : string;
  created_at : string;
  editor : string option;  (** Who wrote this version (`x-tngl-editor`). A knot-hosted comment's uri names the repo, not the writer. *)
  embed : Jsont.json option;
  reply_to : Jsont.json option;
  subject : Jsont.json;
  uri : string;
}

(** Jsont codec for {!type:comment_view}. *)
val comment_view_jsont : comment_view Jsont.t

      end
      module CountStarsBy : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Actor DID whose star authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CountStars : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Repo DID to list star edges for. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CountReactionsBy : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Actor DID whose reaction authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CountReactions : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Record AT-URI the reactions target. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CountCommentsBy : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Actor DID whose comment authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CountComments : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Record AT-URI the comments attach to. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Comment : sig

type main = {
  body : Jsont.json;
  created_at : string;
  embed : Jsont.json option;
  reply_to : Jsont.json option;
  subject : Jsont.json;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module ListStarsBy : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Actor DID whose star authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListReactionsBy : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Actor DID whose reaction authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListComments : sig
(** DEPRECATED: only org.tangled.feed.listCommentsBy still answers with this shape. Use #commentView. *)

type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded org.tangled.feed.comment record *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Record AT-URI the comments attach to. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListCommentsBy : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Actor DID whose comment authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module Search : sig
      module Query : sig

type hit = {
  author : string option;  (** Present for tickets. The account that opened the ticket, the editor of its first version. *)
  cid : string option;
  comment_count : int option;  (** Present for tickets. *)
  nsid : string;  (** Collection of the matched record. *)
  score : Jsont.json;  (** Relevance score of the hit, a floating-point number where higher ranks first. *)
  state : Jsont.json option;  (** Present for tickets. *)
  uri : string;
  value : Jsont.json;  (** Embedded matched record. *)
}

(** Jsont codec for {!type:hit}. *)
val hit_jsont : hit Jsont.t


type state_counts = {
  closed : int;
  completed : int;
  open_ : int;
}

(** Jsont codec for {!type:state_counts}. *)
val state_counts_jsont : state_counts Jsont.t

(** Full-text and faceted search over indexed records. *)

(** Query/procedure parameters. *)
type params = {
  author : string option;  (** Restrict to records authored by this DID. *)
  cursor : string option;  (** Pagination cursor *)
  kind : string option;  (** Restrict tickets to issues or pull requests. *)
  label : string list option;  (** Restrict tickets to those with all of these label names. Requires repo. *)
  limit : int option;
  nsid : string option;  (** Restrict to records of this collection. *)
  offset : int option;  (** Absolute offset for numbered pages. Mutually exclusive with cursor. *)
  q : string option;  (** Search query: free text plus key:value qualifiers. See the docs for more information. *)
  repo : string option;  (** Restrict to records under this repo DID. *)
  since : string option;  (** Restrict to records created at or after this time. *)
  sort : string option;  (** Result order. *)
  until : string option;  (** Restrict to records created at or before this time. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  hits : Jsont.json list;
  state_counts : Jsont.json option;  (** Matches per ticket state, without the state filters. *)
  total : int option;  (** Number of matches across all pages. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
  end
end
module Com : sig
  module Bad_example : sig
    module Pocket : sig
      module Defs : sig

type preferences = Jsont.json list
val preferences_jsont : preferences Jsont.t

      end
      module PutPreferences : sig


type input = {
  preferences : Jsont.json;
  swap_rev : string option option;  (** write only if the stored rev is still this one, or only if nothing is stored yet when null. absent means a blind write *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module GetPreferences : sig

(** Query/procedure parameters. *)
type params = unit

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  preferences : Jsont.json;
  rev : string option;  (** opaque revision of the stored preferences, pass as swapRev. pockets without compare-and-swap leave it out *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
  end
  module Atproto : sig
    module Sync : sig
      module SubscribeRepos : sig
(** Represents a change to an account's status on a host (eg, PDS or Relay). The semantics of this event are that the status is at the host which emitted the event, not necessarily that at the currently active PDS. Eg, a Relay takedown would emit a takedown with active=false, even if the PDS is still active. *)

type account = {
  active : bool;  (** Indicates that the account has a repository which can be fetched from the host that emitted this event. *)
  did : string;
  seq : int;
  status : string option;  (** If active=false, this optional field indicates a reason for why the account is not active. *)
  time : string;
}

(** Jsont codec for {!type:account}. *)
val account_jsont : account Jsont.t

(** Represents a change to an account's identity. Could be an updated handle, signing key, or pds hosting endpoint. Serves as a prod to all downstream services to refresh their identity cache. *)

type identity = {
  did : string;
  handle : string option;  (** The current handle for the account, or 'handle.invalid' if validation fails. This field is optional, might have been validated or passed-through from an upstream source. Semantics and behaviors for PDS vs Relay may evolve in the future; see atproto specs for more details. *)
  seq : int;
  time : string;
}

(** Jsont codec for {!type:identity}. *)
val identity_jsont : identity Jsont.t


type info = {
  message : string option;
  name : string;
}

(** Jsont codec for {!type:info}. *)
val info_jsont : info Jsont.t

(** A repo operation, ie a mutation of a single record. *)

type repo_op = {
  action : string;
  cid : Atp.Cid.t option;  (** For creates and updates, the new record CID. For deletions, null. *)
  path : string;
  prev : Atp.Cid.t option;  (** For updates and deletes, the previous record CID (required for inductive firehose). For creations, field should not be defined. *)
}

(** Jsont codec for {!type:repo_op}. *)
val repo_op_jsont : repo_op Jsont.t

(** Updates the repo to a new state, without necessarily including that state on the firehose. Used to recover from broken commit streams, data loss incidents, or in situations where upstream host does not know recent state of the repository. *)

type sync = {
  blocks : string;  (** CAR file containing the commit, as a block. The CAR header must include the commit block CID as the first 'root'. *)
  did : string;  (** The account this repo event corresponds to. Must match that in the commit object. *)
  rev : string;  (** The rev of the commit. This value must match that in the commit object. *)
  seq : int;  (** The stream sequence number of this message. *)
  time : string;  (** Timestamp of when this message was originally broadcast. *)
}

(** Jsont codec for {!type:sync}. *)
val sync_jsont : sync Jsont.t

(** Represents an update of repository state. Note that empty commits are allowed, which include no repo data changes, but an update to rev and signature. *)

type commit = {
  blobs : Atp.Cid.t list;
  blocks : string;  (** CAR file containing relevant blocks, as a diff since the previous repo state. The commit must be included as a block, and the commit block CID must be the first entry in the CAR header 'roots' list. *)
  commit : Atp.Cid.t;  (** Repo commit object CID. *)
  ops : Jsont.json list;
  prev_data : Atp.Cid.t option;  (** The root CID of the MST tree for the previous commit from this repo (indicated by the 'since' revision field in this message). Corresponds to the 'data' field in the repo commit object. NOTE: this field is effectively required for the 'inductive' version of firehose. *)
  rebase : bool;  (** DEPRECATED -- unused *)
  repo : string;  (** The repo this event comes from. Note that all other message types name this field 'did'. *)
  rev : string;  (** The rev of the emitted commit. Note that this information is also in the commit object included in blocks, unless this is a tooBig event. *)
  seq : int;  (** The stream sequence number of this message. *)
  since : string option;  (** The rev of the last emitted commit from this repo (if any). *)
  time : string;  (** Timestamp of when this message was originally broadcast. *)
  too_big : bool;  (** DEPRECATED -- replaced by #sync event and data limits. Indicates that this commit contained too many ops, or data size was too large. Consumers will need to make a separate request to get missing data. *)
}

(** Jsont codec for {!type:commit}. *)
val commit_jsont : commit Jsont.t

(** Repository event stream, aka Firehose endpoint. Outputs repo commits with diff data, and identity update events, for all repositories on the current server. See the atproto specifications for details around stream sequencing, repo versioning, CAR diff format, and more. Public and does not require auth; implemented by PDS and Relay. *)

(** Query/procedure parameters. *)
type params = {
  cursor : int option;  (** The last known event seq number to backfill from. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type message = Jsont.json

(** Jsont codec for {!type:message}. *)
val message_jsont : message Jsont.t

      end
    end
    module Repo : sig
      module StrongRef : sig

type main = {
  cid : string;
  uri : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
    end
    module Label : sig
      module Defs : sig
(** Metadata tag on an atproto resource (eg, repo or record). *)

type label = {
  cid : string option;  (** Optionally, CID specifying the specific version of 'uri' resource this label applies to. Upstream marks this `format: cid`; dropped here because jacquard decodes that format as a dag-cbor cid-link (tag 42), while labels carry it as a plain string. *)
  cts : string;  (** Timestamp when this label was created. *)
  exp : string option;  (** Timestamp at which this label expires (no longer applies). *)
  neg : bool option;  (** If true, this is a negation label, overwriting a previous label. *)
  sig_ : string option;  (** Signature of dag-cbor encoded label. *)
  src : string;  (** DID of the actor who created this label. *)
  uri : string;  (** AT URI of the record, repository (account), or other resource that this label applies to. *)
  val_ : string;  (** The short string name of the value or type of this label. *)
  ver : int option;  (** The AT Protocol version of the label object. *)
}

(** Jsont codec for {!type:label}. *)
val label_jsont : label Jsont.t


type label_value = string
val label_value_jsont : label_value Jsont.t

(** Strings which describe the label in the UI, localized into a specific language. *)

type label_value_definition_strings = {
  description : string;  (** A longer description of what the label means and why it might be applied. *)
  lang : string;  (** The code of the language these strings are written in. *)
  name : string;  (** A short human-readable name for the label. *)
}

(** Jsont codec for {!type:label_value_definition_strings}. *)
val label_value_definition_strings_jsont : label_value_definition_strings Jsont.t

(** Metadata tag on an atproto record, published by the author within the record. Note that schemas should use #selfLabels, not #selfLabel. *)

type self_label = {
  val_ : string;  (** The short string name of the value or type of this label. *)
}

(** Jsont codec for {!type:self_label}. *)
val self_label_jsont : self_label Jsont.t

(** Declares a label value and its expected interpretations and behaviors. *)

type label_value_definition = {
  adult_only : bool option;  (** Does the user need to have adult content enabled in order to configure this label? *)
  blurs : string;  (** What should this label hide in the UI, if applied? 'content' hides all of the target; 'media' hides the images/video/audio; 'none' hides nothing. *)
  default_setting : string option;  (** The default setting for this label. *)
  identifier : string;  (** The value of the label being defined. Must only include lowercase ascii and the '-' character (\[a-z-\]+). *)
  locales : Jsont.json list;
  severity : string;  (** How should a client visually convey this label? 'inform' means neutral and informational; 'alert' means negative and warning; 'none' means show nothing. *)
}

(** Jsont codec for {!type:label_value_definition}. *)
val label_value_definition_jsont : label_value_definition Jsont.t

(** Metadata tags on an atproto record, published by the author within the record. *)

type self_labels = {
  values : Jsont.json list;
}

(** Jsont codec for {!type:self_labels}. *)
val self_labels_jsont : self_labels Jsont.t

      end
      module SubscribeLabels : sig

type info = {
  message : string option;
  name : string;
}

(** Jsont codec for {!type:info}. *)
val info_jsont : info Jsont.t


type labels = {
  labels : Jsont.json list;
  seq : int;
}

(** Jsont codec for {!type:labels}. *)
val labels_jsont : labels Jsont.t

(** Subscribe to stream of labels (and negations). Public endpoint implemented by mod services. Uses same sequencing scheme as repo event stream. *)

(** Query/procedure parameters. *)
type params = {
  cursor : int option;  (** The last known event seq number to backfill from. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type message = Jsont.json

(** Jsont codec for {!type:message}. *)
val message_jsont : message Jsont.t

      end
    end
  end
end
module Sh : sig
  module Tangled : sig
    module Sync : sig
      module RequestCrawl : sig
(** Request a service to persistently crawl hosted repos. Does not require auth. *)


type input = {
  ensure_repo : string option;  (** specific repository to ensure crawling *)
  hostname : string;  (** Hostname of the current service (eg, Knot) that is requesting to be crawled. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module ListRepos : sig

type default_branch = {
  head : string option;  (** Commit SHA at the tip of the default branch, for reconciling against a last-known state. Width depends on the repo's git object-format. *)
  ref_ : string;  (** Default branch ref name, eg. refs/heads/main. *)
}

(** Jsont codec for {!type:default_branch}. *)
val default_branch_jsont : default_branch Jsont.t


type repo = {
  default_branch : default_branch option;
  repo : string;  (** DID of the git repo as minted by the knot *)
  status : string;  (** Serving status of the repo according to the knot. *)
}

(** Jsont codec for {!type:repo}. *)
val repo_jsont : repo Jsont.t

(** List git repos hosted on this service. *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  order : string option;  (** Sort direction over the service's repo listing order. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  repos : repo list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module String : sig

type main = {
  contents : string;
  created_at : string;
  description : string;
  filename : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

    end
    module Spindle : sig

type main = {
  created_at : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      module Member : sig

type main = {
  created_at : string;
  instance : string;  (** spindle instance that the subject is now a member of *)
  subject : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module ListSpindles : sig

type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.spindle record *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Owner DID whose spindle records to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : list_item list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListMembers : sig

type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.spindle.member record *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Spindle identifier whose member records to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : list_item list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CountSpindles : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Owner DID whose spindle records to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CountMembersBy : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Actor DID whose spindle-member authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CountMembers : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Spindle identifier whose member records to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListMembersBy : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Actor DID whose spindle-member authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : ListMembers.list_item list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module PublicKey : sig

type main = {
  created_at : string;  (** key upload timestamp *)
  key : string;  (** public key contents *)
  name : string;  (** human-readable name for this key *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

    end
    module Pipeline : sig

type clone_opts = {
  depth : int;
  skip : bool;
  submodules : bool;
  tags : bool;
}

(** Jsont codec for {!type:clone_opts}. *)
val clone_opts_jsont : clone_opts Jsont.t


type pair = {
  key : string;
  value : string;
}

(** Jsont codec for {!type:pair}. *)
val pair_jsont : pair Jsont.t


type pull_request_trigger_data = {
  action : string option;  (** the pull request lifecycle action that produced this trigger *)
  pull : string option;  (** AT-URI of the sh.tangled.repo.pull record this run belongs to *)
  source_branch : string;
  source_sha : string;
  target_branch : string;
}

(** Jsont codec for {!type:pull_request_trigger_data}. *)
val pull_request_trigger_data_jsont : pull_request_trigger_data Jsont.t


type push_trigger_data = {
  new_sha : string;
  old_sha : string;
  ref_ : string;
}

(** Jsont codec for {!type:push_trigger_data}. *)
val push_trigger_data_jsont : push_trigger_data Jsont.t


type schedule_trigger_data = {
  ref_ : string;  (** default branch ref resolved for the scheduled run *)
  scheduled_at : string;  (** UTC minute selected by the cron schedule *)
  sha : string;  (** commit SHA the scheduled run targets *)
}

(** Jsont codec for {!type:schedule_trigger_data}. *)
val schedule_trigger_data_jsont : schedule_trigger_data Jsont.t


type trigger_repo = {
  default_branch : string;
  did : string;
  knot : string;
  repo : string option;
  repo_did : string option;  (** DID of the repo itself *)
}

(** Jsont codec for {!type:trigger_repo}. *)
val trigger_repo_jsont : trigger_repo Jsont.t


type manual_trigger_data = {
  inputs : pair list option;
  ref_ : string option;  (** optional ref the SHA was resolved from, for display and TANGLED_REF *)
  sha : string;  (** commit SHA the manual run targets *)
}

(** Jsont codec for {!type:manual_trigger_data}. *)
val manual_trigger_data_jsont : manual_trigger_data Jsont.t


type workflow = {
  audience : string option;  (** OIDC audience (aud claim) of the ID token minted for this workflow. omit to run without a token. *)
  clone : clone_opts;
  engine : string;
  name : string;
  raw : string;
  runs_on : string list option;
}

(** Jsont codec for {!type:workflow}. *)
val workflow_jsont : workflow Jsont.t


type trigger_metadata = {
  actor : string option;  (** DID of the account that caused this run: the pushing account, the pull request author, the manual triggerer, or the repo owner for scheduled runs. *)
  kind : string;
  manual : manual_trigger_data option;
  pull_request : pull_request_trigger_data option;
  push : push_trigger_data option;
  repo : trigger_repo;
  schedule : schedule_trigger_data option;
  source_repo : string option;  (** Repository DID that code and workflow definitions are checked out from, when different from repo (e.g. a fork's commit for a fork-based manual trigger). If absent, source uses repo itself. *)
}

(** Jsont codec for {!type:trigger_metadata}. *)
val trigger_metadata_jsont : trigger_metadata Jsont.t


type main = {
  trigger_metadata : trigger_metadata;
  workflows : workflow list;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      module Status : sig

type main = {
  created_at : string;  (** time of creation of this status update *)
  error : string option;  (** error message if failed *)
  exit_code : int option;  (** exit code if failed *)
  pipeline : string;  (** ATURI of the pipeline *)
  status : string;  (** status of the workflow *)
  workflow : string;  (** name of the workflow within this pipeline *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module ListStatuses : sig

type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.pipeline.status record *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Pipeline AT-URI whose status records to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : list_item list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListPipelines : sig

type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.pipeline record *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Repo or spindle identifier whose pipeline records to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : list_item list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CountStatusesBy : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Actor DID whose pipeline-status authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CountStatuses : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Pipeline AT-URI whose status records to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CountPipelinesBy : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Actor DID whose pipeline authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CountPipelines : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Repo or spindle identifier whose pipeline records to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CancelPipeline : sig
(** DEPRECATED: use sh.tangled.ci.cancelPipeline instead - Cancel a running pipeline *)


type input = {
  pipeline : string;  (** pipeline at-uri *)
  repo : string;  (** repo at-uri, spindle can't resolve repo from pipeline at-uri yet *)
  workflow : string;  (** workflow name *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module ListStatusesBy : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Actor DID whose pipeline-status authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : ListStatuses.list_item list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListPipelinesBy : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Actor DID whose pipeline authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : ListPipelines.list_item list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module Owner : sig
(** Get the owner of a service *)


type output = {
  owner : string;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

    end
    module Markup : sig
      module Markdown : sig
(** Tangled Flavored Markdown format text *)

type main = {
  blobs : Atp.Blob_ref.t list option;  (** list of blobs referenced in markdown *)
  original : string option;  (** Original Markdown before post-processing. Used to restore original input on edit. *)
  text : string;  (** Final post-processed markdown content that will be rendered *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
    end
    module Label : sig
      module Op : sig

type operand = {
  key : string;  (** ATURI to the label definition *)
  value : string;  (** Stringified value of the label. This is first unstringed by appviews and then interpreted as a concrete value. *)
}

(** Jsont codec for {!type:operand}. *)
val operand_jsont : operand Jsont.t


type main = {
  add : operand list;
  delete : operand list;
  performed_at : string;
  subject : string;  (** The subject (task, pull or discussion) of this label. Appviews may apply a `scope` check and refuse this op. *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module ListOps : sig

type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.label.op record *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Scope identifier whose label op records to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : list_item list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Definition : sig

type value_type = {
  enum : string list option;  (** Closed set of values that this label can take. *)
  format : string;  (** An optional constraint that can be applied on string concrete types. *)
  type_ : string;  (** The concrete type of this label's value. *)
}

(** Jsont codec for {!type:value_type}. *)
val value_type_jsont : value_type Jsont.t


type main = {
  color : string option;  (** The hex value for the background color for the label. Appviews may choose to respect this. *)
  created_at : string;
  multiple : bool option;  (** Whether this label can be repeated for a given entity, eg.: \[reviewer:foo, reviewer:bar\] *)
  name : string;  (** The display name of this label. *)
  scope : string list;  (** The areas of the repo this label may apply to, eg.: sh.tangled.repo.issue. Appviews may choose to respect this. *)
  value_type : value_type;  (** The type definition of this label. Appviews may allow sorting for certain types. *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module CountOpsBy : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Actor DID whose label-op authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CountOps : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Scope identifier whose label op records to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListOpsBy : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Actor DID whose label-op authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : ListOps.list_item list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module Knot : sig

type main = {
  created_at : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      module Version : sig
(** Get the version of a knot *)


type output = {
  capabilities : string list option;  (** Protocol capability tokens this knot implements, such as knot-acl. Knots that omit this field are treated as legacy. *)
  version : string;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Unban : sig
(** Lift a block previously placed on an account on this knot *)


type input = {
  subject : string;  (** DID of the account to unblock *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module RemoveMember : sig
(** Remove a member from this knot *)


type input = {
  subject : string;  (** DID of the member to remove *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module MemberInvite : sig
(** DEPRECATED: legacy consent flow for the old knotserver; knot2 grants membership on addMember alone. Offer of membership on this knot, w/ invitee-DID as rkey *)

type main = {
  created_at : string;  (** Invite issue timestamp *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module MemberAcceptance : sig
(** DEPRECATED: legacy consent flow for the old knotserver; knot2 grants membership on addMember alone. Acceptance of a knot's membership offer, w/ knot-DID as rkey *)

type main = {
  created_at : string;  (** Invite acceptance timestamp *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module Member : sig
(** DEPRECATED: replaced by org.tangled.knot.member. *)

type main = {
  created_at : string;
  domain : string;  (** domain that this member now belongs to *)
  subject : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module ListMembers : sig

type list_item = {
  added_by : string;  (** DID that added this member *)
  cid : string option;  (** Optional record CID for record-backed indexers *)
  created_at : string;  (** When the member was added. This listing sorts and pages on it. *)
  subject : string;  (** DID of the member *)
  uri : string option;  (** Optional record AT-URI for record-backed indexers *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Knot identifier whose member records to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : list_item list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListKnots : sig

type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.knot record *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Owner DID whose knot records to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : list_item list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListKeys : sig

type public_key = {
  created_at : string;  (** Key upload timestamp *)
  did : string;  (** DID associated with the public key *)
  key : string;  (** Public key contents *)
}

(** Jsont codec for {!type:public_key}. *)
val public_key_jsont : public_key Jsont.t

(** List all public keys stored in the knot server *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;  (** Maximum number of keys to return *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;  (** Pagination cursor for next page *)
  keys : public_key list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CountMembersBy : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Actor DID whose knot-member authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CountMembers : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Knot identifier whose member records to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CountKnots : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Owner DID whose knot records to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Ban : sig
(** Block an account from creating repositories or pushing on this knot *)


type input = {
  subject : string;  (** DID of the account to block *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module AddMember : sig
(** Add a member to this knot *)


type input = {
  subject : string;  (** DID of the member to add *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module AcceptMembership : sig
(** DEPRECATED: legacy consent flow for the old knotserver; knot2 grants membership on addMember alone. Accept this knot's membership offer. The knot refuses the call unless the acceptance record is already in the actor's own repository. *)


type input = {
  acceptance : string;  (** AT-URI of the sh.tangled.knot.memberAcceptance record, w/ knot-DID as rkey *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
    end
    module Identity : sig
      module ResolveCommitters : sig
(** Resolve an actor DID to its verified Git committer identities. *)


type input = {
  actor : string;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  committers : string list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module Graph : sig
      module Vouch : sig

type main = {
  created_at : string;
  evidences : string list option;  (** Optional list of ATURIs serving as evidence for this vouch (ex. issues, PRs) *)
  kind : string;  (** Whether this user is being vouched for or denounced *)
  reason : string option;  (** The reason for this vouch/denouncement *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module ListVouches : sig

type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.graph.vouch record *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Vouchee DID whose inbound vouches to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : list_item list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Follow : sig

type main = {
  created_at : string;
  subject : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module CountVouchesBy : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Actor DID whose vouch authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CountVouches : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Vouchee DID whose inbound vouches to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListVouchesBy : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Actor DID whose vouch authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : ListVouches.list_item list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module Git : sig
      module Temp : sig
        module ListTags : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;  (** Maximum number of tags to return *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = unit
val output_jsont : output Jsont.t

        end
        module ListLanguages : sig

type language = {
  name : string;  (** Programming language name *)
  size : int;  (** Total size of files in this language (bytes) *)
}

(** Jsont codec for {!type:language}. *)
val language_jsont : language Jsont.t


(** Query/procedure parameters. *)
type params = {
  indexed_only : bool option;  (** Only serve stored languages, failing with NotIndexed rather than analyzing the commit on demand *)
  ref_ : string option;  (** Git reference (branch, tag, or commit SHA) *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  languages : language list;
  ref_ : string;  (** The git reference used *)
  total : int;  (** Total size of all analyzed files in bytes *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module ListCommits : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor (commit SHA) *)
  limit : int option;  (** Maximum number of commits to return *)
  ref_ : string option;  (** Git reference (branch, tag, or commit SHA) *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = unit
val output_jsont : output Jsont.t

        end
        module ListBranches : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;  (** Maximum number of branches to return *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = unit
val output_jsont : output Jsont.t

        end
        module GetTree : sig

type readme = {
  contents : string;  (** Contents of the readme file *)
  filename : string;  (** Name of the readme file *)
}

(** Jsont codec for {!type:readme}. *)
val readme_jsont : readme Jsont.t


type signature = {
  email : string;  (** Author email *)
  name : string;  (** Author name *)
  when_ : string;  (** Author timestamp *)
}

(** Jsont codec for {!type:signature}. *)
val signature_jsont : signature Jsont.t


type last_commit = {
  author : signature option;
  hash : string;  (** Commit hash *)
  message : string;  (** Commit message *)
  when_ : string;  (** Commit timestamp *)
}

(** Jsont codec for {!type:last_commit}. *)
val last_commit_jsont : last_commit Jsont.t


type tree_entry = {
  last_commit : last_commit option;
  mode : string;  (** File mode *)
  name : string;  (** Relative file or directory name *)
  size : int;  (** File size in bytes *)
}

(** Jsont codec for {!type:tree_entry}. *)
val tree_entry_jsont : tree_entry Jsont.t


(** Query/procedure parameters. *)
type params = {
  path : string option;  (** Path within the repository tree *)
  ref_ : string;  (** Git reference (branch, tag, or commit SHA) *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  dotdot : string option;  (** Parent directory path *)
  files : tree_entry list;
  last_commit : last_commit option;
  parent : string option;  (** The parent path in the tree *)
  readme : readme option;  (** Readme for this file tree *)
  ref_ : string;  (** The git reference used *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module GetTag : sig

(** Query/procedure parameters. *)
type params = {
  repo : string;  (** DID of the repository *)
  tag : string;  (** Name of tag, such as v1.3.0 *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = unit
val output_jsont : output Jsont.t

        end
        module GetMergeBase : sig

(** Query/procedure parameters. *)
type params = {
  base : string;  (** Ref used to find the common base. *)
  head : string;  (** Ref to list history for. *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  commit : string;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module GetDiff : sig

(** Query/procedure parameters. *)
type params = {
  repo : string;  (** DID of the repository *)
  rev1 : string;  (** First revision (commit, branch, or tag) *)
  rev2 : string;  (** Second revision (commit, branch, or tag) *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t

(** Compare output in application/json *)

type output = unit
val output_jsont : output Jsont.t

        end
        module GetBlob : sig

(** Query/procedure parameters. *)
type params = {
  path : string;  (** Path within the repository tree *)
  ref_ : string option;  (** Git reference (branch, tag, or commit SHA) *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t

(** raw blob served in octet-stream *)

type output = unit
val output_jsont : output Jsont.t

        end
        module GetArchive : sig

(** Query/procedure parameters. *)
type params = {
  format : string option;  (** Archive format *)
  prefix : string option;  (** Prefix for files in the archive *)
  ref_ : string;  (** Git reference (branch, tag, or commit SHA) *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t

(** Binary archive data *)

type output = unit
val output_jsont : output Jsont.t

        end
        module FormatPatch : sig

(** Query/procedure parameters. *)
type params = {
  from : string;
  repo : string;  (** DID of the repository *)
  to_ : string;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = unit
val output_jsont : output Jsont.t

        end
        module Defs : sig

type hash = string
val hash_jsont : hash Jsont.t


type signature = {
  email : string;  (** Person email *)
  name : string;  (** Person name *)
  when_ : string;  (** Timestamp of the signature *)
}

(** Jsont codec for {!type:signature}. *)
val signature_jsont : signature Jsont.t


type submodule = {
  branch : string option;  (** Branch to track in the submodule *)
  name : string;  (** Submodule name *)
  url : string;  (** Submodule repository URL *)
}

(** Jsont codec for {!type:submodule}. *)
val submodule_jsont : submodule Jsont.t


type commit = {
  author : signature;
  committer : signature;
  hash : hash;
  message : string;
  tree : hash;
}

(** Jsont codec for {!type:commit}. *)
val commit_jsont : commit Jsont.t


type tag = {
  message : string option;
  name : string;  (** tag name *)
  tagger : signature;
  target : Jsont.json;
}

(** Jsont codec for {!type:tag}. *)
val tag_jsont : tag Jsont.t


type branch = {
  commit : commit;  (** hydrated commit object *)
  name : string;  (** branch name *)
}

(** Jsont codec for {!type:branch}. *)
val branch_jsont : branch Jsont.t

        end
        module AnalyzeMerge : sig

type conflict_info = {
  filename : string;  (** Name of the conflicted file *)
  reason : string;  (** Reason for the conflict *)
}

(** Jsont codec for {!type:conflict_info}. *)
val conflict_info_jsont : conflict_info Jsont.t

(** Check if a merge is possible between two branches *)

(** Query/procedure parameters. *)
type params = {
  branch : string;  (** Target branch to merge into *)
  patch : string;  (** Patch or pull request to check for merge conflicts *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  conflicts : conflict_info list option;  (** List of files with merge conflicts *)
  is_conflicted : bool;  (** Whether the merge has conflicts *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module ListIndexedLanguages : sig

type repo_languages = {
  commit : string;  (** Default-branch commit the languages were indexed from *)
  languages : ListLanguages.language list;  (** Languages ordered by size, largest first *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:repo_languages}. *)
val repo_languages_jsont : repo_languages Jsont.t

(** Page through the stored default-branch language breakdown of every indexed repository. *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;  (** Maximum number of repositories to return *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  repos : repo_languages list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module GetHead : sig

(** Query/procedure parameters. *)
type params = {
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = Defs.branch

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module GetEntry : sig
(** get metadata of blob by ref and path *)

(** Query/procedure parameters. *)
type params = {
  path : string;  (** path of the entity *)
  ref_ : string option;  (** Git revision (branch, tag, or commit id) *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  last_commit : Defs.commit option;
  mode : string;
  name : string;  (** The file name *)
  oid : string;
  size : int;  (** Blob size *)
  submodule : Defs.submodule option;  (** Submodule information if path is a submodule *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module GetCommit : sig
(** resolve commit from given ref *)

(** Query/procedure parameters. *)
type params = {
  ref_ : string;  (** reference name to resolve *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = Defs.commit

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module GetBranch : sig

(** Query/procedure parameters. *)
type params = {
  name : string;  (** Branch name to get information for *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  author : Defs.signature option;
  hash : string;  (** Latest commit hash on this branch *)
  message : string option;  (** Latest commit message *)
  name : string;  (** Branch name *)
  when_ : string;  (** Timestamp of latest commit *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
      end
      module RefUpdate : sig

type individual_email_commit_count = {
  count : int;
  email : string;
}

(** Jsont codec for {!type:individual_email_commit_count}. *)
val individual_email_commit_count_jsont : individual_email_commit_count Jsont.t


type individual_language_size = {
  lang : string;
  size : int;
}

(** Jsont codec for {!type:individual_language_size}. *)
val individual_language_size_jsont : individual_language_size Jsont.t


type commit_count_breakdown = {
  by_email : individual_email_commit_count list option;
}

(** Jsont codec for {!type:commit_count_breakdown}. *)
val commit_count_breakdown_jsont : commit_count_breakdown Jsont.t


type lang_breakdown = {
  inputs : individual_language_size list option;
}

(** Jsont codec for {!type:lang_breakdown}. *)
val lang_breakdown_jsont : lang_breakdown Jsont.t


type meta = {
  commit_count : commit_count_breakdown;
  is_default_ref : bool;
  lang_breakdown : lang_breakdown option;
}

(** Jsont codec for {!type:meta}. *)
val meta_jsont : meta Jsont.t

(** An event record representing git-push operation to git repository, emitted by knots. *)

type main = {
  changed_files : string list option;  (** files changed between commits *)
  committer_did : string;  (** did of the user that pushed this ref *)
  meta : meta;
  new_sha : string;  (** new SHA of this ref *)
  old_sha : string;  (** old SHA of this ref *)
  owner_did : string option;  (** did of the owner of the repo *)
  push_options : string list option;  (** push options passed on git-push *)
  ref_ : string;  (** Ref being updated *)
  repo : string;  (** DID of the repo itself *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module Ref : sig
(** Present state of one git ref on this repository, w/ the escaped refname as rkey (every byte outside \[A-Za-z0-9_-\] as ~xx in hex, ~ itself as ~7e; refs/heads/main is refs~2fheads~2fmain) *)

type main = {
  sha : string;  (** Object the ref points at *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module Oid : sig

type main = {
  oid : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module MergeCheck : sig

type conflict_info = {
  filename : string;  (** Name of the conflicted file *)
  reason : string;  (** Reason for the conflict *)
}

(** Jsont codec for {!type:conflict_info}. *)
val conflict_info_jsont : conflict_info Jsont.t


type source = {
  commit : string;  (** Head commit ID of PR to merge *)
  repo : string;  (** DID of source git repository *)
}

(** Jsont codec for {!type:source}. *)
val source_jsont : source Jsont.t

(** Check if a merge is possible between two branches. Performs 3-way merge check *)


type input = {
  branch : string;  (** Target branch to merge into *)
  repo : string;  (** DID of Git repository *)
  source : source;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  conflicts : conflict_info list option;  (** List of files with merge conflicts *)
  is_conflicted : bool;  (** Whether the merge has conflicts *)
  message : string option;  (** Additional message about the merge check *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListRefs : sig

type default_branch = {
  head : string option;  (** Commit SHA at the tip of the default branch, for reconciling against a last-known state. Width depends on the repo's git object-format. *)
  ref_ : string;  (** Default branch ref name that HEAD points at, eg. refs/heads/main. *)
}

(** Jsont codec for {!type:default_branch}. *)
val default_branch_jsont : default_branch Jsont.t


type ref_ = {
  ref_ : string;  (** Full ref name, eg. refs/heads/main or refs/tags/v1.0 *)
  sha : string;  (** Object SHA the ref points at. Width depends on the repo's git object-format. *)
}

(** Jsont codec for {!type:ref_}. *)
val ref__jsont : ref_ Jsont.t

(** List every ref & its commit SHA for a git repo, equivalent to git ls-remote. Gives the full ref state of a repo. *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;  (** Maximum number of refs to return in this page *)
  repo : string;  (** DID of the git repo as minted by the knot *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;  (** Cursor for the next page, absent when the last page is reached *)
  default_branch : default_branch option;
  refs : ref_ list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module KeepCommit : sig

type commit = {
  oid : string;
  repo : string;
}

(** Jsont codec for {!type:commit}. *)
val commit_jsont : commit Jsont.t


type patches = {
  patches : string list;
}

(** Jsont codec for {!type:patches}. *)
val patches_jsont : patches Jsont.t

(** Keep commit for an atproto record. *)


type input = {
  record : string;  (** at-uri of target record. rkey is required *)
  repo : string;
  source : Jsont.json;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  commit : string;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Defs : sig

type byte_changes = {
  lhs : int;
  rhs : int;
}

(** Jsont codec for {!type:byte_changes}. *)
val byte_changes_jsont : byte_changes Jsont.t

(** One side of a file diff. *)

type diff_src = {
  content : string option;  (** Inlined content. Used on interdiff's LHS. *)
  is_binary : bool;
  is_submodule : bool;
  oid : string;  (** Blob oid, or the commit oid for a submodule *)
  path : string;
  size : int;
}

(** Jsont codec for {!type:diff_src}. *)
val diff_src_jsont : diff_src Jsont.t


type diff_stat = {
  deletions : int;
  files_changed : int;
  insertions : int;
}

(** Jsont codec for {!type:diff_stat}. *)
val diff_stat_jsont : diff_stat Jsont.t

(** One aligned line pair; a side is absent when that side has no line. *)

type line_pair = {
  lhs : int option;
  rhs : int option;
}

(** Jsont codec for {!type:line_pair}. *)
val line_pair_jsont : line_pair Jsont.t


type signature = {
  email : string;  (** Person email *)
  name : string;  (** Person name *)
  when_ : string;  (** Timestamp of the signature *)
}

(** Jsont codec for {!type:signature}. *)
val signature_jsont : signature Jsont.t


type diff_hunk = {
  lines : line_pair list;
  novel_lhs : int list;  (** 0-based line numbers holding novel content on the left side *)
  novel_rhs : int list;  (** 0-based line numbers holding novel content on the right side *)
}

(** Jsont codec for {!type:diff_hunk}. *)
val diff_hunk_jsont : diff_hunk Jsont.t


type file_diff = {
  has_byte_changes : byte_changes option;  (** Byte count per side, present only when the two sides differ *)
  has_syntactic_changes : bool;
  hunks : diff_hunk list;
  lhs_src : diff_src;
  rhs_src : diff_src;
}

(** Jsont codec for {!type:file_diff}. *)
val file_diff_jsont : file_diff Jsont.t

      end
      module Temp2 : sig
        module MergeCheck : sig

type conflict = {
  filename : string;  (** Path of the conflicted file *)
  reason : string;  (** Short classification of the conflict *)
}

(** Jsont codec for {!type:conflict}. *)
val conflict_jsont : conflict Jsont.t

(** 3-way merge check between a target branch tip (ours) and a source commit (theirs). Read-only: the merged tree is never persisted. *)

(** Query/procedure parameters. *)
type params = {
  source_commit : string;  (** Source commit to merge (theirs) *)
  source_repo : string;
  target_commit : string;  (** Target branch tip to merge into (ours) *)
  target_repo : string;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  conflicts : conflict list;
  is_conflicted : bool;  (** Whether the merge has unresolved conflicts *)
  message : string option;  (** Additional detail, e.g. unrelated histories *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module ListCommits : sig

type header = {
  key : string;
  value : string;
}

(** Jsont codec for {!type:header}. *)
val header_jsont : header Jsont.t


type commit = {
  author : Defs.signature;
  committer : Defs.signature;
  extra_headers : header list;
  message : string;
  oid : string;
  parents : string list;
  stat : Defs.diff_stat option;
  tree : string;
}

(** Jsont codec for {!type:commit}. *)
val commit_jsont : commit Jsont.t


(** Query/procedure parameters. *)
type params = {
  after : string option;
  all_refs : bool option;
  before : string option;
  include_stats : bool option;  (** Diff each commit against its first parent and include a line stat *)
  limit : int option;  (** Maximum number of commits to return *)
  ranges : string list option;
  repo : string;  (** DID of the repository *)
  skip : int option;  (** Number of matching commits to skip. A poor man's pagination. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  commits : commit list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module GetInterdiff : sig

(** Query/procedure parameters. *)
type params = {
  base_commit1 : string;
  base_commit2 : string;
  base_repo : string;
  head_commit1 : string;
  head_commit2 : string;
  head_repo : string;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  diffs : Defs.file_diff list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module GetDiff : sig

(** Query/procedure parameters. *)
type params = {
  base_commit : string;
  base_repo : string;
  head_commit : string;
  head_repo : string;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t

(** TBD *)

type output = {
  diffs : Defs.file_diff list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module GetCommitStats : sig

type month = {
  commits : int;  (** Commits attributed to the actor during this month. *)
  start : string;  (** Start of this UTC calendar month. *)
}

(** Jsont codec for {!type:month}. *)
val month_jsont : month Jsont.t

(** Get an actor's monthly commit counts. *)

(** Query/procedure parameters. *)
type params = {
  actor : string;  (** Actor whose commits to count. *)
  months : int option;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  months : month list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module GetBlame : sig

type commit = {
  author : Defs.signature;
  message : string;  (** Full commit message *)
  oid : string;
}

(** Jsont codec for {!type:commit}. *)
val commit_jsont : commit Jsont.t


type run = {
  commit : string;  (** oid of a commit in the output commits array *)
  count : int;  (** Number of consecutive lines in this run *)
  start : int;  (** First one-based line in this run *)
}

(** Jsont codec for {!type:run}. *)
val run_jsont : run Jsont.t


(** Query/procedure parameters. *)
type params = {
  path : string;  (** Path to the file within the repository *)
  ref_ : string;  (** Revspec to blame at: a branch, tag, or commit sha *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  commit : string;  (** The resolved git commit object id *)
  commits : commit list;  (** Unique commits referenced by blame runs *)
  runs : run list;  (** Complete, gap-free one-based line ranges sorted by start *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
      end
      module MergeCommit : sig
(** Merge commit content. Used when merge commit is created. *)

type merge_commit_content = {
  author : Defs.signature option;
  message : string option;
}

(** Jsont codec for {!type:merge_commit_content}. *)
val merge_commit_content_jsont : merge_commit_content Jsont.t


type source = {
  commit : string;  (** Head commit ID to merge *)
  repo : string;  (** DID of source git repository *)
}

(** Jsont codec for {!type:source}. *)
val source_jsont : source Jsont.t


type target = {
  branch : string;  (** Target branch to merge into *)
  commit : string;  (** Head commit ID of target branch *)
  repo : string;  (** DID of target git repository *)
}

(** Jsont codec for {!type:target}. *)
val target_jsont : target Jsont.t

(** DEPRECATED: use `org.tangled.temp.git.mergeCommit` *)


type input = {
  merge_commit : merge_commit_content option;  (** Merge commit content. Used when merge commit is created. *)
  source : source;
  style : string;
  target : target;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
    end
    module Ci : sig
      module Trigger : sig

type pair = {
  key : string;
  value : string;
}

(** Jsont codec for {!type:pair}. *)
val pair_jsont : pair Jsont.t

(** TODO: reference PR record with strongRef instead of embedding raw values *)

type pull_request = {
  action : string option;  (** the pull request lifecycle action that produced this trigger *)
  pull : string option;  (** AT-URI of the sh.tangled.repo.pull record this run belongs to *)
  source_branch : string option;
  source_repo : string option;  (** Repository DID to check out code and workflow definitions from, if different from the target repo. *)
  source_sha : string;
  target_branch : string;
}

(** Jsont codec for {!type:pull_request}. *)
val pull_request_jsont : pull_request Jsont.t


type push = {
  new_sha : string;
  old_sha : string;
  ref_ : string;
}

(** Jsont codec for {!type:push}. *)
val push_jsont : push Jsont.t


type schedule = {
  ref_ : string;  (** default branch ref resolved for the scheduled run *)
  scheduled_at : string;  (** UTC minute selected by the cron schedule *)
  sha : string;  (** commit SHA the scheduled run targets *)
}

(** Jsont codec for {!type:schedule}. *)
val schedule_jsont : schedule Jsont.t


type manual = {
  inputs : pair list option;
  ref_ : string option;  (** optional ref the SHA was resolved from, for display and TANGLED_REF *)
  sha : string;  (** commit SHA the manual run targets *)
  source_repo : string option;  (** Repository DID to check out code and workflow definitions from, if different from the target repo. *)
}

(** Jsont codec for {!type:manual}. *)
val manual_jsont : manual Jsont.t

      end
      module SubscribePipelineLogs : sig

type control = {
  command : string option;  (** Step command *)
  content : string;
  kind : string option;  (** Step kind *)
  outcome : string option;  (** How the step ended, only set on end frames for steps that can fail *)
  status : string option;  (** Step status *)
  step : int;  (** Step ID *)
  time : string;
  workflow : string;  (** workflow name *)
}

(** Jsont codec for {!type:control}. *)
val control_jsont : control Jsont.t


type data = {
  content : string;
  step : int;  (** Step ID *)
  stream : string;
  time : string;
  workflow : string;  (** workflow name *)
}

(** Jsont codec for {!type:data}. *)
val data_jsont : data Jsont.t

(** Pipeline logs stream *)

(** Query/procedure parameters. *)
type params = {
  pipeline : string;  (** Pipeline ID *)
  workflows : string list option;  (** filter logs by specific workflows *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type message = Jsont.json

(** Jsont codec for {!type:message}. *)
val message_jsont : message Jsont.t

      end
      module DescribeWorkflowDefinition : sig
(** Resolve the workflow definition a pipeline would use at a given commit and return a fingerprint of it. *)

(** Query/procedure parameters. *)
type params = {
  repo : string;  (** Target repository DID the workflow definition belongs to. *)
  sha : string;  (** Commit SHA to resolve the workflow definition at *)
  source_repo : string option;  (** Repository DID to resolve workflow definitions from, if different from the target repo *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  derived : bool;  (** Whether the workflow definition is derived from this repository at all. When false, no commit-to-commit comparison is meaningful (e.g. definitions managed externally), and callers should not surface change warnings. *)
  hash : string option;  (** Fingerprint of the workflow definition as resolved by the spindle. Absent when derived is false. *)
  workflows : string list option;  (** Names or paths of the effective workflow files that produced the hash. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CancelPipeline : sig
(** Cancel running pipeline or specific workflows *)


type input = {
  pipeline : string;  (** pipeline ID *)
  repo : string;  (** git repository DID *)
  workflows : string list option;  (** Workflow names to filter. When not provided, entire pipeline will be canceled. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module TriggerPipeline : sig
(** Trigger a pipeline at an explicit commit. Runs the named workflows, or every workflow defined in the repo when none are named. *)


type input = {
  repo : string;  (** Target repository DID. Auth is checked against this repo. *)
  trigger : Jsont.json;  (** Trigger metadata for this dispatch. *)
  workflows : string list option;  (** Workflow names to run. When not provided, every dispatchable workflow is run. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  pipeline : string;  (** AT-URI of the created pipeline *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Pipeline : sig

type workflow = {
  error : string option;
  finished_at : string option;
  id : string;  (** Spindle-local workflow id. Unique per pipeline, usually same as name. *)
  name : string;  (** Name of the workflow *)
  started_at : string option;
  status : string;  (** Workflow status *)
}

(** Jsont codec for {!type:workflow}. *)
val workflow_jsont : workflow Jsont.t

(** A CI pipeline. Record-like, but owned by the spindle rather than a PDS. *)

type main = {
  commit : string;  (** Commit Id this pipeline is running on *)
  created_at : string option;
  id : string;  (** Spindle-local pipeline id *)
  repo : string;  (** Repository DID *)
  source_repo : string option;  (** Repository DID that the commit was checked out from, if different from repo (e.g. a fork for a fork-based pull request) *)
  trigger : Jsont.json;  (** Trigger event metadata *)
  workflows : workflow list;  (** Triggered workflows *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module QueryPipelines : sig
(** Query pipelines in git repository *)

(** Query/procedure parameters. *)
type params = {
  commits : string list option;  (** Filter pipelines by commits. When provided, maximum one pipeline per commit id will be returned. *)
  cursor : string option;  (** Pagination cursor *)
  kinds : string list option;  (** Filter pipelines by trigger kind. When provided, pipelines matching any listed kind are returned; when omitted, every kind is returned. *)
  limit : int option;  (** Maximum number of pipelines to return *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  pipelines : Pipeline.main list;
  total : int;  (** Maximum number of pipelines *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetPipeline : sig

(** Query/procedure parameters. *)
type params = {
  pipeline : string;  (** Spindle-local pipeline id *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = Pipeline.main

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module Actor : sig
      module Profile : sig
(** A declaration of a Tangled account profile. *)

type main = {
  avatar : Atp.Blob_ref.t option;  (** Small image to be displayed next to posts from account. AKA, 'profile picture' *)
  bluesky : bool;  (** Include link to this account on Bluesky. *)
  description : string option;  (** Free-form profile description text. *)
  is_organization : bool option;  (** Temporary flag indicating that this profile belongs to an organization. *)
  links : string list option;
  location : string option;  (** Free-form location text. *)
  pinned_repositories : string list option;  (** Pinned repositories. Values are repo DIDs for repos that have them, or AT-URIs for legacy repos. *)
  preferred_handle : string option;  (** A handle the user prefers to be displayed as. *)
  pronouns : string option;  (** Preferred gender pronouns. *)
  stats : string list option;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module Defs : sig
(** Metadata about the requesting account's relationship with the subject account. Only has meaningful content for authed requests. *)

type viewer_state = {
  followed_by : string option;
  following : string option;
}

(** Jsont codec for {!type:viewer_state}. *)
val viewer_state_jsont : viewer_state Jsont.t


type profile_view_basic = {
  avatar : string option;
  did : string;
  handle : string;
  viewer : viewer_state option;
}

(** Jsont codec for {!type:profile_view_basic}. *)
val profile_view_basic_jsont : profile_view_basic Jsont.t

(** profileViewBaisc + follow counts *)

type profile_view_detailed = {
  avatar : string option;
  did : string;
  followers_count : int option;
  follows_count : int option;
  handle : string;
  viewer : viewer_state option;
}

(** Jsont codec for {!type:profile_view_detailed}. *)
val profile_view_detailed_jsont : profile_view_detailed Jsont.t

      end
    end
    module Repo : sig

type main = {
  created_at : string;
  description : string option;
  knot : string;  (** knot where the repo was created *)
  labels : string list option;  (** List of labels that this repo subscribes to *)
  name : string option;  (** Cosmetic name of the repo. *)
  repo_did : string option;  (** DID of the repo itself, if assigned *)
  source : string option;  (** source of the repo *)
  spindle : string option;  (** CI runner to send jobs to and receive results from *)
  topics : string list option;  (** Topics related to the repo *)
  website : string option;  (** Any URI related to the repo *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      module Tree : sig

type readme = {
  contents : string;  (** Contents of the readme file *)
  filename : string;  (** Name of the readme file *)
}

(** Jsont codec for {!type:readme}. *)
val readme_jsont : readme Jsont.t


type signature = {
  email : string;  (** Author email *)
  name : string;  (** Author name *)
  when_ : string;  (** Author timestamp *)
}

(** Jsont codec for {!type:signature}. *)
val signature_jsont : signature Jsont.t


type last_commit = {
  author : signature option;
  hash : string;  (** Commit hash *)
  message : string;  (** Commit message *)
  when_ : string;  (** Commit timestamp *)
}

(** Jsont codec for {!type:last_commit}. *)
val last_commit_jsont : last_commit Jsont.t


type tree_entry = {
  last_commit : last_commit option;
  mode : string;  (** File mode *)
  name : string;  (** Relative file or directory name *)
  size : int;  (** File size in bytes *)
}

(** Jsont codec for {!type:tree_entry}. *)
val tree_entry_jsont : tree_entry Jsont.t


(** Query/procedure parameters. *)
type params = {
  path : string option;  (** Path within the repository tree *)
  ref_ : string;  (** Git reference (branch, tag, or commit SHA) *)
  repo : string;  (** Repository identifier in format 'did:plc:.../repoName' *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  dotdot : string option;  (** Parent directory path *)
  files : tree_entry list;
  last_commit : last_commit option;
  parent : string option;  (** The parent path in the tree *)
  readme : readme option;  (** Readme for this file tree *)
  ref_ : string;  (** The git reference used *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Tags : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;  (** Maximum number of tags to return *)
  repo : string;  (** Repository identifier in format 'did:plc:.../repoName' *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = unit
val output_jsont : output Jsont.t

      end
      module Tag : sig

(** Query/procedure parameters. *)
type params = {
  repo : string;  (** Repository identifier in format 'did:plc:.../repoName' *)
  tag : string;  (** Name of tag, such as v1.3.0 *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = unit
val output_jsont : output Jsont.t

      end
      module SetDefaultBranch : sig
(** Set the default branch for a repository *)


type input = {
  default_branch : string;
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module SetContributionPolicy : sig
(** Set who can contribute issues, comments, labels, reactions and pulls to a repository this knot hosts. Only the repository owner and the knot's admins can call it. A repository starts from the knot-wide default at creation. *)


type input = {
  contribution_policy : string;  (** anyone: any account with a DID can contribute. Members: accounts on this knot's member grant list. Collaborators: the owner and the repository's collaborators. *)
  repo : string;  (** DID of the repository this policy change is for *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module ReserveKey : sig
(** Reserve a signing key for a bring-your-own DID repository identity. The knot seals the key and returns its multibase encoding so the owner can publish it in their DID document before calling sh.tangled.repo.create. *)


type input = {
  repo_did : string;  (** The DID this repository will use. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  key : string;  (** Multibase-encoded public key to publish in the DID document. *)
  repo_did : string;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Rename : sig
(** Rename a repository on this knot. The given rkey becomes the canonical record key. Prior rkeys keep resolving as aliases until the repository is deleted. *)


type input = {
  name : string;  (** New display name of the repository *)
  repo : string;  (** DID of the repository to rename *)
  rkey : string;  (** Rkey of the sh.tangled.repo record written for the new name *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module RemoveSecret : sig
(** Remove a CI secret *)


type input = {
  key : string;
  repo : string;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module RemoveCollaborator : sig
(** Remove a collaborator from a repository on this knot *)


type input = {
  repo : string;  (** DID of the repository to remove the collaborator from *)
  subject : string;  (** DID of the collaborator to remove *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module Push : sig
(** Authorizes a git push to a knot over HTTP. A client mints a service-auth token with this method with the knot as its audience, then gives it on git-receive-pack & LFS object requests! *)

      end
      module Pull : sig
(** DEPRECATED: revisions of this pull request, newer rounds are appended to this array. appviews may reject records do not treat this field as append-only. the blob format is gzipped text-based git-format-patches. *)

type round = {
  created_at : string;
  patch_blob : Atp.Blob_ref.t;
}

(** Jsont codec for {!type:round}. *)
val round_jsont : round Jsont.t


type source = {
  branch : string;
  repo : string option;
}

(** Jsont codec for {!type:source}. *)
val source_jsont : source Jsont.t


type target = {
  branch : string;
  repo : string;
}

(** Jsont codec for {!type:target}. *)
val target_jsont : target Jsont.t


type version = {
  base : string;  (** merge-base commit ID for combined interdiff *)
  created_at : string;
  head : string;  (** HEAD commit ID *)
}

(** Jsont codec for {!type:version}. *)
val version_jsont : version Jsont.t


type main = {
  blobs : Atp.Blob_ref.t list option;
  body : string option;
  created_at : string;
  dependent_on : string option;
  mentions : string list option;
  references : string list option;
  rounds : round list;  (** DEPRECATED: always empty *)
  source : source option;
  target : target;
  title : string;
  versions : version list;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

        module Status : sig

type main = {
  created_at : string;
  pull : string;
  status : string;  (** status of the pull request *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

          module Open : sig
(** open pull request *)

type main = string
val main_jsont : main Jsont.t

          end
          module Merged : sig
(** merged pull request *)

type main = string
val main_jsont : main Jsont.t

          end
          module Closed : sig
(** closed pull request *)

type main = string
val main_jsont : main Jsont.t

          end
        end
        module Merge : sig
(** Merge a pull request by commit. The knot merges the pull's latest version onto its target branch, merging the stack below first, and writes the merged status record in the same critical section as the branch update. *)


type input = {
  repo : string;  (** DID of the repository the pull lives on *)
  rkey : string;  (** Record key of the pull to merge *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  merged : string list;  (** At-uris of the pulls this call marked merged, bottom of the stack first *)
  tip : string;  (** Commit id the target branch ended on; the standing tip when the merge needed no move *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module ListStatuses : sig

type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.repo.pull.status record *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Pull AT-URI whose status records to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : list_item list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module CountStatusesBy : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Actor DID whose pull-status authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module CountStatuses : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Pull AT-URI whose status records to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module Comment : sig

type main = {
  body : string;
  created_at : string;
  mentions : string list option;
  pull : string;
  references : string list option;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

        end
        module ListStatusesBy : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Actor DID whose pull-status authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : ListStatuses.list_item list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
      end
      module MergeCheck : sig

type conflict_info = {
  filename : string;  (** Name of the conflicted file *)
  reason : string;  (** Reason for the conflict *)
}

(** Jsont codec for {!type:conflict_info}. *)
val conflict_info_jsont : conflict_info Jsont.t

(** Check if a merge is possible between two branches *)


type input = {
  branch : string;  (** Target branch to merge into *)
  did : string option;  (** DID of the repository owner. A knot without the repo-did-input capability reads this and name in place of repo. *)
  name : string option;  (** Name of the repository. A knot without the repo-did-input capability reads this and DID in place of repo. *)
  patch : string;  (** Patch or pull request to check for merge conflicts *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  conflicts : conflict_info list option;  (** List of files with merge conflicts *)
  error : string option;  (** Error message if check failed *)
  is_conflicted : bool;  (** Whether the merge has conflicts *)
  message : string option;  (** Additional message about the merge check *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Merge : sig
(** DEPRECATED: Merge a patch into a repository branch *)


type input = {
  author_email : string option;  (** Author email for the merge commit *)
  author_name : string option;  (** Author name for the merge commit *)
  branch : string;  (** Target branch to merge into *)
  commit_body : string option;  (** Additional commit message body *)
  commit_message : string option;  (** Merge commit message *)
  did : string option;  (** DID of the repository owner. A knot without the repo-did-input capability reads this and name in place of repo. *)
  name : string option;  (** Name of the repository. A knot without the repo-did-input capability reads this and DID in place of repo. *)
  patch : string;  (** Patch content to merge *)
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module Log : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor (commit SHA) *)
  limit : int option;  (** Maximum number of commits to return *)
  path : string option;  (** Path to filter commits by *)
  ref_ : string;  (** Git reference (branch, tag, or commit SHA) *)
  repo : string;  (** Repository identifier in format 'did:plc:.../repoName' *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = unit
val output_jsont : output Jsont.t

      end
      module ListSecrets : sig

type secret = {
  created_at : string;
  created_by : string;
  key : string;
  repo : string;
}

(** Jsont codec for {!type:secret}. *)
val secret_jsont : secret Jsont.t


(** Query/procedure parameters. *)
type params = {
  repo : string;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  secrets : secret list option;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListCollaborators : sig

type list_item = {
  added_by : string;  (** DID that added this collaborator *)
  cid : string option;  (** Optional record CID for record-backed indexers *)
  created_at : string;  (** When the repository offered collaboration *)
  effective_since : string;  (** When the collaboration took effect. This listing sorts and pages on it. *)
  subject : string;  (** DID of the collaborator *)
  uri : string option;  (** Optional record AT-URI for record-backed indexers *)
  verified_at : string option;  (** When the knot resolved the collaborator's own acceptance record. Absent on a grant that predates the consent model. *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  offset : int option;  (** Absolute offset for random-access pagination. Mutually exclusive with cursor; offsets drift under concurrent writes, so follow up with the returned cursor. *)
  order : string option;  (** Sort direction by effectiveSince. *)
  subject : string;  (** Repo DID whose collaborator records to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : list_item list;
  total : int option;  (** Total items in the full list; omitted for filtered or merged views *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListCollaboratorInvites : sig
(** An offer awaiting the subject's acceptance. effectiveSince and access both arrive once the acceptance resolves. *)

type invite_item = {
  added_by : string;  (** DID that made the offer *)
  created_at : string;  (** When the repository made the offer. This listing sorts and pages on it. *)
  subject : string;  (** DID the repository offered collaboration to *)
}

(** Jsont codec for {!type:invite_item}. *)
val invite_item_jsont : invite_item Jsont.t

(** List this repository's outstanding collaboration offers. An offer leaves this listing once acceptCollaboration resolves the subject's record, or removeCollaborator withdraws it. *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Repo DID whose outstanding collaboration offers to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : invite_item list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListCobVersions : sig

type version = {
  change_id : string;  (** Git Object ID of the change associated with this version. *)
  cid : string option;  (** Missing once the version is removed, or when the change is a deletion. *)
  created_at : string;  (** When this knot wrote the commit this version rides in. Not the record's own `createdAt`. *)
  editor : string;  (** The DID of the account that made this version. *)
}

(** Jsont codec for {!type:version}. *)
val version_jsont : version Jsont.t

(** Every version of a COB record! Give a record's URI and get back every saved version of that record, oldest to newest unless `order` says otherwise. Fetch a version's CID through `com.atproto.sync.getBlocks` for as long as the knot still has it. *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pass the cursor from one page into the next query to keep walking versions. *)
  limit : int option;
  order : string option;  (** Which end the walk starts from. `desc` puts the newest version first, which is where the version you just wrote is. *)
  uri : string;  (** AT-URI of a record in a repository this knot hosts, its repository authority a DID. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;  (** Left out when the page already reaches the last version in the requested order. *)
  total : int option;  (** How many versions the record has, whatever the page. *)
  uri : string;
  versions : version list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Languages : sig

type language = {
  color : string option;  (** Hex color code for this language *)
  extensions : string list option;  (** File extensions associated with this language *)
  file_count : int option;  (** Number of files in this language *)
  name : string;  (** Programming language name *)
  percentage : int;  (** Percentage of total codebase (0-100) *)
  size : int;  (** Total size of files in this language (bytes) *)
}

(** Jsont codec for {!type:language}. *)
val language_jsont : language Jsont.t


(** Query/procedure parameters. *)
type params = {
  ref_ : string option;  (** Git reference (branch, tag, or commit SHA) *)
  repo : string;  (** Repository identifier in format 'did:plc:.../repoName' *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  languages : language list option;
  ref_ : string;  (** The git reference used *)
  total_files : int option;  (** Total number of files analyzed *)
  total_size : int option;  (** Total size of all analyzed files in bytes *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Issue : sig

type main = {
  blobs : Atp.Blob_ref.t list option;
  body : string option;
  created_at : string;
  mentions : string list option;
  references : string list option;
  repo : string;
  title : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

        module State : sig

type main = {
  created_at : string;
  issue : string;
  state : string;  (** state of the issue *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

          module Open : sig
(** open issue *)

type main = string
val main_jsont : main Jsont.t

          end
          module Closed : sig
(** closed issue *)

type main = string
val main_jsont : main Jsont.t

          end
        end
        module ListStates : sig

type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.repo.issue.state record *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Issue AT-URI whose state records to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : list_item list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module CountStatesBy : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Actor DID whose issue-state authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module CountStates : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Issue AT-URI whose state records to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module Comment : sig

type main = {
  body : string;
  created_at : string;
  issue : string;
  mentions : string list option;
  references : string list option;
  reply_to : string option;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

        end
        module ListStatesBy : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Actor DID whose issue-state authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : ListStates.list_item list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
      end
      module HiddenRef : sig
(** Create a hidden ref in a repository *)


type input = {
  fork_ref : string;  (** Fork reference name *)
  remote_ref : string;  (** Remote reference name *)
  repo : string;  (** DID of the fork that the hidden ref belongs to *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  error : string option;  (** Error message if creation failed *)
  ref_ : string option;  (** The created hidden ref name *)
  success : bool;  (** Whether the hidden ref was created successfully *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetPulls : sig

type record_view = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.repo.pull record. *)
}

(** Jsont codec for {!type:record_view}. *)
val record_view_jsont : record_view Jsont.t


(** Query/procedure parameters. *)
type params = {
  pulls : string list;  (** AT-URIs of the sh.tangled.repo.pull records to fetch. At most 50 per request. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  items : record_view list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetPull : sig

(** Query/procedure parameters. *)
type params = {
  pull : string;  (** AT-URI of the sh.tangled.repo.pull record to fetch. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.repo.pull record. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetIssues : sig

type record_view = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.repo.issue record. *)
}

(** Jsont codec for {!type:record_view}. *)
val record_view_jsont : record_view Jsont.t


(** Query/procedure parameters. *)
type params = {
  issues : string list;  (** AT-URIs of the sh.tangled.repo.issue records to fetch. At most 50 per request. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  items : record_view list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetIssue : sig

(** Query/procedure parameters. *)
type params = {
  issue : string;  (** AT-URI of the sh.tangled.repo.issue record to fetch. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.repo.issue record. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetDefaultBranch : sig

type signature = {
  email : string;  (** Author email *)
  name : string;  (** Author name *)
  when_ : string;  (** Author timestamp *)
}

(** Jsont codec for {!type:signature}. *)
val signature_jsont : signature Jsont.t


(** Query/procedure parameters. *)
type params = {
  repo : string;  (** Repository identifier in format 'did:plc:.../repoName' *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  author : signature option;
  hash : string;  (** Latest commit hash on default branch *)
  message : string option;  (** Latest commit message *)
  name : string;  (** Default branch name *)
  short_hash : string option;  (** Short commit hash *)
  when_ : string;  (** Timestamp of latest commit *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ForkSync : sig
(** Sync a forked repository with its upstream source *)


type input = {
  branch : string;  (** Branch to sync *)
  did : string option;  (** DID of the fork owner. A knot without the repo-did-input capability reads this and name in place of repo. *)
  name : string option;  (** Name of the forked repository. A knot without the repo-did-input capability reads this and DID in place of repo. *)
  repo : string;  (** DID of the fork to sync *)
  source : string option;  (** AT-URI of the source repository. A knot without the repo-did-input capability requires this field without reading it. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module Diff : sig

(** Query/procedure parameters. *)
type params = {
  ref_ : string;  (** Git reference (branch, tag, or commit SHA) *)
  repo : string;  (** Repository identifier in format 'did:plc:.../repoName' *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = unit
val output_jsont : output Jsont.t

      end
      module DescribeRepo : sig
(** Fetch the knot's authoritative metadata for a git repo DID. *)

(** Query/procedure parameters. *)
type params = {
  repo_did : string;  (** DID of the git repo as minted by the knot *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  content : string option;  (** Status of the clone from source. Omitted if not created from a source. *)
  owner_did : string;  (** DID of the current owner according to the knot. *)
  reason : string option;  (** Reason for partial or failed content. *)
  repo_did : string;
  rkey : string;  (** Current rkey of the sh.tangled.repo record tracked by this knot *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module DeleteBranch : sig
(** DEPRECATED: use `org.tangled.temp.git.deleteBranch` *)


type input = {
  branch : string;
  repo : string;  (** DID of the repository *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module Delete : sig
(** Delete a repository *)


type input = {
  did : string option;  (** DID of the repository owner. A knot without the repo-did-input capability reads this and name in place of repo. *)
  force : bool option;  (** Admin-only. Delete even though the repository record still exists on the owner's PDS. *)
  name : string option;  (** Name of the repository to delete. A knot without the repo-did-input capability reads this and DID in place of repo. *)
  repo : string;  (** DID of the repository to delete *)
  rkey : string option;  (** Rkey of the repository record. A knot without the repo-did-input capability checks this against the owner's PDS. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module Defs : sig
(** Metadata about the requesting account's relationship with the subject content. Only has meaningful content for authed requests. *)

type viewer_state = {
  fork : string option;
  star : string option;
}

(** Jsont codec for {!type:viewer_state}. *)
val viewer_state_jsont : viewer_state Jsont.t


type repo_view_basic = {
  created_at : string;
  description : string option;
  did : string;
  fork_count : int option;
  is_starred : bool option;
  open_issue_count : int option;
  open_pull_count : int option;
  owner : Actor.Defs.profile_view_basic;
  primary_language : string option;  (** Largest language on the default branch, by bytes. *)
  slug : string;
  star_count : int option;
  viewer : viewer_state option;
}

(** Jsont codec for {!type:repo_view_basic}. *)
val repo_view_basic_jsont : repo_view_basic Jsont.t

      end
      module Create : sig

type source = {
  kind : string;  (** fork tracks the source as upstream; import is a one-time copy. *)
  url : string;  (** Git URL to clone from *)
}

(** Jsont codec for {!type:source}. *)
val source_jsont : source Jsont.t

(** Create a new repository *)


type input = {
  default_branch : string option;  (** Default branch to push to *)
  name : string;  (** Name of the repository *)
  repo_did : string option;  (** Optional user-provided DID to use as the repo identity instead of minting one. *)
  rkey : string;  (** Rkey of the repository record *)
  source : source option;  (** A source to clone from, populate this when forking or importing a repository. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  key : string option;  (** Multibase-encoded public signing key the knot holds for this repository *)
  repo_did : string option;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CountPullsBy : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Actor DID whose pull authorings to list *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CountIssuesBy : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Actor DID whose issue authorings to list *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;  (** Total number of matching records. *)
  distinct_authors : int;  (** Number of distinct authors among the matching records. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Compare : sig

(** Query/procedure parameters. *)
type params = {
  repo : string;  (** Repository identifier in format 'did:plc:.../repoName' *)
  rev1 : string;  (** First revision (commit, branch, or tag) *)
  rev2 : string;  (** Second revision (commit, branch, or tag) *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t

(** Compare output in application/json *)

type output = unit
val output_jsont : output Jsont.t

      end
      module CollaboratorInvite : sig
(** Offer of collaboration on this repo, w/ invitee-DID as rkey *)

type main = {
  created_at : string;  (** Invite issue timestamp *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module CollaboratorAcceptance : sig
(** Acceptance of a repo's collaboration offer, w/ repo-DID as rkey *)

type main = {
  created_at : string;  (** Invite acceptance timestamp *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module Collaborator : sig
(** DEPRECATED: use sh.tangled.repo.collaboratorInvite plus sh.tangled.repo.collaboratorAcceptance instead. Existing records keep their meaning and this NSID is never reused, so indexers shouldn't migrate what they have already stored. *)

type main = {
  created_at : string;
  repo : string;  (** repo DID to add this user to *)
  subject : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module CheckPushAllowed : sig
(** Check whether the holder of a public key is allowed to push to a repo. *)

(** Query/procedure parameters. *)
type params = {
  key : string;  (** Public key in OpenSSH authorized_keys format. *)
  repo : string;  (** A repo DID. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  allowed : bool;  (** Whether the key's owner may push to the repo. *)
  did : string option;  (** DID the key resolved to, if a match was found. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Branches : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;  (** Maximum number of branches to return *)
  repo : string;  (** Repository identifier in format 'did:plc:.../repoName' *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = unit
val output_jsont : output Jsont.t

      end
      module Branch : sig

type signature = {
  email : string;  (** Author email *)
  name : string;  (** Author name *)
  when_ : string;  (** Author timestamp *)
}

(** Jsont codec for {!type:signature}. *)
val signature_jsont : signature Jsont.t


(** Query/procedure parameters. *)
type params = {
  name : string;  (** Branch name to get information for *)
  repo : string;  (** Repository identifier in format 'did:plc:.../repoName' *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  author : signature option;
  hash : string;  (** Latest commit hash on this branch *)
  is_default : bool option;  (** Whether this is the default branch *)
  message : string option;  (** Latest commit message *)
  name : string;  (** Branch name *)
  short_hash : string option;  (** Short commit hash *)
  when_ : string;  (** Timestamp of latest commit *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Blob : sig

type signature = {
  email : string;  (** Author email *)
  name : string;  (** Author name *)
  when_ : string;  (** Author timestamp *)
}

(** Jsont codec for {!type:signature}. *)
val signature_jsont : signature Jsont.t


type submodule = {
  branch : string option;  (** Branch to track in the submodule *)
  name : string;  (** Submodule name *)
  url : string;  (** Submodule repository URL *)
}

(** Jsont codec for {!type:submodule}. *)
val submodule_jsont : submodule Jsont.t


type last_commit = {
  author : signature option;
  hash : string;  (** Commit hash *)
  message : string;  (** Commit message *)
  when_ : string;  (** Commit timestamp *)
}

(** Jsont codec for {!type:last_commit}. *)
val last_commit_jsont : last_commit Jsont.t


(** Query/procedure parameters. *)
type params = {
  path : string;  (** Path to the file within the repository *)
  raw : bool option;  (** Return raw file content instead of JSON response *)
  ref_ : string;  (** Git reference (branch, tag, or commit SHA) *)
  repo : string;  (** AT-URI of the sh.tangled.repo record *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  content : string option;  (** File content (base64 encoded for binary files) *)
  encoding : string option;  (** Content encoding *)
  file_too_large : bool option;
  is_binary : bool option;  (** Whether the file is binary *)
  last_commit : last_commit option;
  mime_type : string option;  (** MIME type of the file *)
  path : string;  (** The file path *)
  ref_ : string;  (** The git reference used *)
  size : int option;  (** File size in bytes *)
  submodule : submodule option;  (** Submodule information if path is a submodule *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Blame : sig

type author = {
  email : string;
  name : string;
}

(** Jsont codec for {!type:author}. *)
val author_jsont : author Jsont.t


type run = {
  commit : string;  (** SHA of the commit in the output commits array *)
  count : int;  (** Number of consecutive lines in this run *)
  start : int;  (** First one-based line in this run *)
}

(** Jsont codec for {!type:run}. *)
val run_jsont : run Jsont.t


type commit = {
  author : author;
  authored_at : string;  (** Git author timestamp *)
  sha : string;
  subject : string;
}

(** Jsont codec for {!type:commit}. *)
val commit_jsont : commit Jsont.t


(** Query/procedure parameters. *)
type params = {
  path : string;  (** Path to the file within the repository *)
  ref_ : string;  (** Git reference (branch, tag, or commit SHA) *)
  repo : string;  (** AT-URI of the sh.tangled.repo record *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  commit : string;  (** The resolved git commit object ID *)
  commits : commit list;  (** Unique commits referenced by blame runs *)
  runs : run list;  (** Complete, gap-free one-based line ranges sorted by start *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Artifact : sig

type main = {
  artifact : Atp.Blob_ref.t;  (** the artifact *)
  created_at : string;  (** time of creation of this artifact *)
  name : string;  (** name of the artifact *)
  repo : string option;  (** repo that this artifact is being uploaded to *)
  repo_did : string option;
  tag : string;  (** hash of the tag object that this artifact is attached to (only annotated tags are supported) *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module Archive : sig

(** Query/procedure parameters. *)
type params = {
  format : string option;  (** Archive format *)
  prefix : string option;  (** Prefix for files in the archive *)
  ref_ : string;  (** Git reference (branch, tag, or commit SHA) *)
  repo : string;  (** Repository identifier in format 'did:plc:.../repoName' *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t

(** Binary archive data *)

type output = unit
val output_jsont : output Jsont.t

      end
      module AddSecret : sig
(** Add a CI secret *)


type input = {
  key : string;
  repo : string;
  value : string;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module AddCollaborator : sig
(** Add a collaborator to a repository on this knot *)


type input = {
  repo : string;  (** DID of the repository to add the collaborator to *)
  subject : string;  (** DID of the collaborator to add *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module AcceptCollaboration : sig
(** Accept a repository's collaboration offer. The knot refuses the call unless the acceptance record is already in the actor's own repository. *)


type input = {
  acceptance : string;  (** AT-URI of the sh.tangled.repo.collaboratorAcceptance record, w/ repo-DID as rkey *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
    end
    module Embed : sig
      module Commit : sig

type main = {
  commit : Git.Oid.main;
  repo : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t


type view = {
  change : string option;
  commit : string;
  repo : string;
}

(** Jsont codec for {!type:view}. *)
val view_jsont : view Jsont.t

      end
    end
    module Pull : sig
      module Defs : sig

type patch_view = {
  base : string;
  created_at : string;
  head : string;
}

(** Jsont codec for {!type:patch_view}. *)
val patch_view_jsont : patch_view Jsont.t


type pull_view = {
  author : Actor.Defs.profile_view_basic;
  cid : string;
  created_at : string;
  repo : Repo.Defs.repo_view_basic;
  state : string option;
  target_branch : string;
  title : string;
  uri : string;
}

(** Jsont codec for {!type:pull_view}. *)
val pull_view_jsont : pull_view Jsont.t


type source = {
  branch : string;
  repo : Repo.Defs.repo_view_basic option;
}

(** Jsont codec for {!type:source}. *)
val source_jsont : source Jsont.t


type pull_view_detailed = {
  author : Actor.Defs.profile_view_basic;
  body : Markup.Markdown.main option;
  cid : string;
  created_at : string;
  repo : Repo.Defs.repo_view_basic;
  source : source option;
  state : string option;
  target_branch : string;
  title : string;
  uri : string;
  versions : patch_view list;  (** TODO: this should be paginated maybe even as a separate `listPatches` method *)
}

(** Jsont codec for {!type:pull_view_detailed}. *)
val pull_view_detailed_jsont : pull_view_detailed Jsont.t

      end
      module ListPullViews : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  state : string option;
  subject : string;  (** Repo DID to list pulls for *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : Defs.pull_view list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetPullView : sig

(** Query/procedure parameters. *)
type params = {
  uri : string;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = Defs.pull_view_detailed

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module Feed : sig
      module Star : sig

type repo = {
  did : string;
}

(** Jsont codec for {!type:repo}. *)
val repo_jsont : repo Jsont.t


type string_ = {
  uri : string;
}

(** Jsont codec for {!type:string_}. *)
val string__jsont : string_ Jsont.t


type main = {
  created_at : string;
  subject : Jsont.json;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module Reaction : sig

type main = {
  created_at : string;
  reaction : string;
  subject : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module Defs : sig

type comment_view = {
  body : Jsont.json;
  cid : string;
  created_at : string;
  editor : string option;  (** Who wrote this version (`x-tngl-editor`). A knot-hosted comment's uri names the repo, not the writer. *)
  embed : Embed.Commit.view option;
  reply_to : Jsont.json option;
  subject : Jsont.json;
  uri : string;
}

(** Jsont codec for {!type:comment_view}. *)
val comment_view_jsont : comment_view Jsont.t

      end
      module Comment : sig

type main = {
  body : Markup.Markdown.main;
  created_at : string;
  embed : Embed.Commit.main option;
  pull_round_idx : int option;  (** optional pull submission round index. required when subject is sh.tangled.repo.pull *)
  reply_to : Jsont.json option;
  subject : Jsont.json;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
    end
  end
end
