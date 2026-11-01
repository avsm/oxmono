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
    module Temp : sig
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
  usages : Defs.usage list;  (** non-zero usage rows, including active reservations and committed allocations *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

          end
          module List : sig
(** list all per-subject quota overrides *)


type output = {
  limits : Defs.limit list;  (** quota overrides ordered by DID and resource *)
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
  limit : Defs.limit;  (** quota override for the requested DID and resource *)
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
  bans : Defs.ban list;  (** active bans ordered by subject DID *)
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
  ban : Defs.ban;  (** active ban matching the requested DID *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

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
(** Get the active sites domain claim for the authenticated user, if any. *)


type output = {
  domain : string option;  (** The claimed domain (e.g. 'alice.sites.tangled.sh'). Absent if no active claim. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module ClaimDomain : sig
(** Claim a subdomain under the sites domain (e.g. 'alice' → alice.sites.tangled.sh) for the authenticated user. *)


type input = {
  subdomain : string;  (** Desired subdomain label (lowercase letters, digits, and hyphens; 4–63 characters). *)
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
  highlights : highlight list option;  (** Byte-offset ranges within content that match the query. *)
  line_start : int;  (** 1-based line number of the first line in content. *)
}

(** Jsont codec for {!type:chunk}. *)
val chunk_jsont : chunk Jsont.t


type file_result = {
  chunks : chunk list;
  language : string option;  (** Detected programming language. *)
  path : string;  (** File path relative to repository root. *)
  repo_did : string;  (** DID of the repository as minted by the knot. *)
}

(** Jsont codec for {!type:file_result}. *)
val file_result_jsont : file_result Jsont.t

(** Full-text code search across indexed repositories, backed by Zoekt. *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;
  lang : string option;  (** Restrict search to a specific programming language (e.g. 'go', 'rust'). *)
  limit : int option;
  q : string;  (** Search query string. *)
  repo_did : string option;  (** Restrict search to a specific repository, by its DID. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  results : file_result list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
      end
      module Repo : sig
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
        module UpdateSiteConfig : sig
(** Create or update the site configuration for a repository and trigger a deployment. *)


type input = {
  branch : string;  (** Branch to deploy from. *)
  dir : string;  (** Directory within the repository to deploy (e.g. '/' or '/docs'). *)
  is_index : bool option;  (** Whether this repo should serve as the index (root) for the claimed domain. *)
  repo_did : string;  (** DID of the repository as minted by the knot. *)
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
        module RetryWebhookDelivery : sig
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
  webhooks : webhook list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module ListWebhookDeliveries : sig

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
}

(** Jsont codec for {!type:delivery}. *)
val delivery_jsont : delivery Jsont.t

(** List recent delivery attempts for a webhook. *)

(** Query/procedure parameters. *)
type params = {
  id : int;  (** Webhook ID. *)
  limit : int option;
  repo_did : string;  (** DID of the repository as minted by the knot. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  deliveries : delivery list;
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
  config : site_config option;  (** Site config for the repository. Absent if no site is configured. *)
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
      module Notification : sig
        module UpdateSeen : sig
(** Mark a single notification as read or unread. *)


type input = {
  read : bool;
  uri : string;  (** at-uri of the notification to update. *)
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
  type_ : string;  (** Notification type: repo_starred, issue_created, issue_commented, issue_closed, issue_reopen, issue_assigned, issue_unassigned, pull_created, pull_commented, pull_merged, pull_closed, pull_reopen, pull_assigned, pull_unassigned, followed, user_mentioned. *)
  uri : string;  (** at-uri of this notification; the stable key for read/unread state. *)
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
  notifications : notification list;
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


type output = preferences

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

        end
        module DeleteNotification : sig
(** Delete a notification. *)


type input = {
  uri : string;  (** at-uri of the notification to delete. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

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
      module Account : sig
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
  emails : email list;
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
(** Complete signup: provision the PDS account for the chosen username using the verification code from the signup email. *)


type input = {
  code : string;  (** Verification code from the signup email. *)
  password : string;  (** Password for the new account. *)
  username : string;  (** Desired username; the assigned handle is <username>.<pds domain>. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
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
      end
    end
  end
end
module Com : sig
  module Atproto : sig
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
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Scope identifier whose string records to list. *)
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
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Owner DID whose spindle records to list. *)
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
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Spindle identifier whose member records to list. *)
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
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Actor DID whose spindle-member authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : ListMembers.list_item list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module Search : sig
      module Query : sig

type hit = {
  cid : string option;
  nsid : string;  (** Collection of the matched record. *)
  score : Jsont.json;  (** Relevance score of the hit, a floating-point number where higher ranks first. *)
  uri : string;
  value : Jsont.json;  (** Embedded matched record. *)
}

(** Jsont codec for {!type:hit}. *)
val hit_jsont : hit Jsont.t


(** Query/procedure parameters. *)
type params = {
  author : string option;  (** Restrict to records authored by this DID. *)
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  nsid : string option;  (** Restrict to records of this collection. *)
  q : string;  (** Full-text search query. *)
  repo : string option;  (** Restrict to records under this repo DID. *)
  since : string option;  (** Restrict to records created at or after this time. *)
  until : string option;  (** Restrict to records created at or before this time. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  hits : hit list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

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
      module Pull : sig
(** revisions of this pull request, newer rounds are appended to this array. appviews may reject records do not treat this field as append-only. the blob format is gzipped text-based git-format-patches. *)

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


type main = {
  blobs : Atp.Blob_ref.t list option;
  body : string option;
  created_at : string;
  dependent_on : string option;
  mentions : string list option;
  references : string list option;
  rounds : round list;
  source : source option;
  target : target;
  title : string;
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
(** Merge a patch into a repository branch *)


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
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Owner DID whose repo records to list. *)
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
      module ListPulls : sig

type pull_list_item = {
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
  order : string option;  (** Sort direction by createdAt. *)
  status : string option;  (** Restrict to pulls whose latest derived status matches. *)
  subject : string;  (** Repo DID to list pulls for *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : pull_list_item list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListIssues : sig

type issue_list_item = {
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
  order : string option;  (** Sort direction by createdAt. *)
  state : string option;  (** Restrict to issues whose latest derived state matches. *)
  subject : string;  (** Repo DID to list issues for *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : issue_list_item list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListCollaborators : sig

type list_item = {
  added_by : string;  (** DID that added this collaborator *)
  cid : string option;  (** Optional record CID for record-backed indexers *)
  created_at : string;  (** When the collaborator was added *)
  subject : string;  (** DID of the collaborator *)
  uri : string option;  (** Optional record AT-URI for record-backed indexers *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Repo DID whose collaborator records to list. *)
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
      module ListArtifacts : sig

type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.repo.artifact record *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Repo or release identifier whose artifacts to list. *)
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
  items : record_view list;
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
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.repo record. *)
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
  owner_did : string;  (** DID of the current owner according to the knot. *)
  repo_did : string;
  rkey : string;  (** Current rkey of the sh.tangled.repo record tracked by this knot *)
}

(** Jsont codec for {!type:output}. *)
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
      module Create : sig
(** Create a new repository *)


type input = {
  default_branch : string option;  (** Default branch to push to *)
  name : string;  (** Name of the repository *)
  repo_did : string option;  (** Optional user-provided did:web to use as the repo identity instead of minting a did:plc. *)
  rkey : string;  (** Rkey of the repository record *)
  source : string option;  (** A source URL to clone from, populate this when forking or importing a repository. *)
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
      module CountPulls : sig

(** Query/procedure parameters. *)
type params = {
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
      module CountIssues : sig

(** Query/procedure parameters. *)
type params = {
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
  repo : string;  (** DID of the repository *)
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
      module ListPullsBy : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  order : string option;  (** Sort direction by createdAt. *)
  status : string option;  (** Restrict to pulls whose latest derived status matches. *)
  subject : string;  (** Actor DID whose pull authorings to list *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : ListPulls.pull_list_item list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListIssuesBy : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  order : string option;  (** Sort direction by createdAt. *)
  state : string option;  (** Restrict to issues whose latest derived state matches. *)
  subject : string;  (** Actor DID whose issue authorings to list *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : ListIssues.issue_list_item list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListCollaboratorsBy : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Actor DID whose collaborator authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : ListCollaborators.list_item list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListArtifactsBy : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Actor DID whose artifact authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : ListArtifacts.list_item list;
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
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Owner DID whose public-key records to list. *)
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
  clone : clone_opts;
  engine : string;
  name : string;
  raw : string;
  runs_on : string list option;
}

(** Jsont codec for {!type:workflow}. *)
val workflow_jsont : workflow Jsont.t


type trigger_metadata = {
  kind : string;
  manual : manual_trigger_data option;
  pull_request : pull_request_trigger_data option;
  push : push_trigger_data option;
  repo : trigger_repo;
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
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Pipeline AT-URI whose status records to list. *)
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
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Repo or spindle identifier whose pipeline records to list. *)
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
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Actor DID whose pipeline-status authorings to list. *)
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
      module ListPipelinesBy : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Actor DID whose pipeline authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : ListPipelines.list_item list;
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
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Scope identifier whose label op records to list. *)
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
      module ListDefinitions : sig

type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.label.definition record *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Scope identifier whose label definitions to list. *)
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
      module CountDefinitions : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Scope identifier whose label definitions to list. *)
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
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Actor DID whose label-op authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : ListOps.list_item list;
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
      module SubscribeRepos : sig

type git_sync1 = {
  did : string;  (** Repository DID identifier *)
  seq : int;  (** The stream sequence number of this message. *)
}

(** Jsont codec for {!type:git_sync1}. *)
val git_sync1_jsont : git_sync1 Jsont.t


type git_sync2 = {
  repo : string;  (** Repository AT-URI identifier *)
  seq : int;  (** The stream sequence number of this message. *)
}

(** Jsont codec for {!type:git_sync2}. *)
val git_sync2_jsont : git_sync2 Jsont.t


type identity = {
  did : string;  (** Repository DID identifier *)
  seq : int;  (** The stream sequence number of this message. *)
  time : string;
}

(** Jsont codec for {!type:identity}. *)
val identity_jsont : identity Jsont.t

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
      module RemoveMember : sig
(** Remove a member from this knot *)


type input = {
  subject : string;  (** DID of the member to remove *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module MemberInvite : sig
(** Offer of membership on this knot, w/ invitee-DID as rkey *)

type main = {
  created_at : string;  (** Invite issue timestamp *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module MemberAcceptance : sig
(** Acceptance of a knot's membership offer, w/ knot-DID as rkey *)

type main = {
  created_at : string;  (** Invite acceptance timestamp *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module Member : sig

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
  created_at : string;  (** When the member was added *)
  subject : string;  (** DID of the member *)
  uri : string option;  (** Optional record AT-URI for record-backed indexers *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Knot identifier whose member records to list. *)
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
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Owner DID whose knot records to list. *)
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
      module AddMember : sig
(** Add a member to this knot *)


type input = {
  subject : string;  (** DID of the member to add *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module ListMembersBy : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Actor DID whose knot-member authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : ListMembers.list_item list;
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
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Vouchee DID whose inbound vouches to list. *)
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
      module ListFollows : sig

type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.graph.follow record *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Followee DID whose inbound follows to list. *)
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
      module ListVouchesBy : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Actor DID whose vouch authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : ListVouches.list_item list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListFollowsBy : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Actor DID whose follow authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : ListFollows.list_item list;
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
      module ListRefUpdates : sig

type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.git.refUpdate record *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Repo DID whose ref-update records to list. *)
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
      module CountRefUpdatesBy : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Actor DID whose ref-update authorings to list. *)
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
      module CountRefUpdates : sig

(** Query/procedure parameters. *)
type params = {
  subject : string;  (** Repo DID whose ref-update records to list. *)
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
      module ListRefUpdatesBy : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Actor DID whose ref-update authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : ListRefUpdates.list_item list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

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
  pipeline : string;  (** pipeline TID *)
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
  repo : string option;  (** Repository DID *)
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
  items : record_view list;
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
    end
    module Feed : sig
      module Subscription : sig

type repo = {
  did : string;
}

(** Jsont codec for {!type:repo}. *)
val repo_jsont : repo Jsont.t


type uri = {
  uri : string;
}

(** Jsont codec for {!type:uri}. *)
val uri_jsont : uri Jsont.t


type main = {
  collections : string list option;  (** Optional collection NSIDs to filter which notifications are sent. Empty or absent means all collections. *)
  created_at : string;
  subject : Jsont.json;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
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
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Repo DID to list star edges for. *)
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
      module ListReactions : sig

type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.feed.reaction record *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Record AT-URI the reactions target. *)
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
      module ListComments : sig

type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;  (** Embedded sh.tangled.feed.comment record *)
}

(** Jsont codec for {!type:list_item}. *)
val list_item_jsont : list_item Jsont.t


(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Record AT-URI the comments attach to. *)
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
  body : Markup.Markdown.main;
  created_at : string;
  pull_round_idx : int option;  (** optional pull submission round index. required when subject is sh.tangled.repo.pull *)
  reply_to : Com.Atproto.Repo.StrongRef.main option;
  subject : Com.Atproto.Repo.StrongRef.main;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module ListStarsBy : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Actor DID whose star authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : ListStars.list_item list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListReactionsBy : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Actor DID whose reaction authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : ListReactions.list_item list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListCommentsBy : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;
  order : string option;  (** Sort direction by createdAt. *)
  subject : string;  (** Actor DID whose comment authorings to list. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : ListComments.list_item list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
  end
end
