(* Atp_lexicon_standard_site - generated from atproto lexicons *)

(** AT Protocol lexicon types and Jsont codecs for Atp_lexicon_standard_site. *)

(** Utility functions for resilient parsing. *)
module Filter : sig
  val filter_list : 'a Jsont.t -> Jsont.json list -> 'a list
  (** [filter_list jsont json_list] parses each element with [jsont],
      returning only successfully parsed elements. Non-compliant records
      are silently skipped. *)
end

module Com : sig
  module Atproto : sig
    module Repo : sig
      module StrongRef : sig
(** A URI with a content-hash fingerprint. *)

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
module Site : sig
  module Standard : sig
    module Theme : sig
      module Color : sig

type rgb = {
  b : int;
  g : int;
  r : int;
}

(** Jsont codec for {!type:rgb}. *)
val rgb_jsont : rgb Jsont.t


type rgba = {
  a : int;
  b : int;
  g : int;
  r : int;
}

(** Jsont codec for {!type:rgba}. *)
val rgba_jsont : rgba Jsont.t

      end
      module Basic : sig
(** A simplified theme definition for publications, providing basic color customization for content display across different platforms and applications. *)

type main = {
  accent : Color.rgb;  (** Color used for links and button backgrounds. *)
  accent_foreground : Color.rgb;  (** Color used for button text. *)
  background : Color.rgb;  (** Color used for content background. *)
  foreground : Color.rgb;  (** Color used for content text. *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
    end
    module Graph : sig
      module Subscription : sig
(** Record declaring a subscription to a publication. *)

type main = {
  publication : string;  (** AT-URI reference to the publication record being subscribed to (ex: at://did:plc:abc123/site.standard.publication/xyz789). *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
    end
    module Document : sig
(** A document record representing a published article, blog post, or other content. Documents can belong to a publication or exist independently. *)

type main = {
  bsky_post_ref : Com.Atproto.Repo.StrongRef.main option;  (** Strong reference to a Bluesky post. Useful to keep track of comments off-platform. *)
  content : Jsont.json option;  (** Open union used to define the record's content. Each entry must specify a $type and may be extended with other lexicons to support additional content formats. *)
  cover_image : Atp.Blob_ref.t option;  (** Image to used for thumbnail or cover image. Less than 1MB is size. *)
  description : string option;  (** A brief description or excerpt from the document. *)
  path : string option;  (** Combine with site or publication url to construct a canonical URL to the document. Prepend with a leading slash. *)
  published_at : string;  (** Timestamp of the documents publish time. *)
  site : string;  (** Points to a publication record (at://) or a publication url (https://) for loose documents. Avoid trailing slashes. *)
  tags : string list option;  (** Array of strings used to tag or categorize the document. Avoid prepending tags with hashtags. *)
  text_content : string option;  (** Plaintext representation of the documents contents. Should not contain markdown or other formatting. *)
  title : string;  (** Title of the document. *)
  updated_at : string option;  (** Timestamp of the documents last edit. *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

    end
    module Publication : sig
(** Platform-specific preferences for the publication, including discovery and visibility settings. *)

type preferences = {
  show_in_discover : bool option;  (** Boolean which decides whether the publication should appear in discovery feeds. *)
}

(** Jsont codec for {!type:preferences}. *)
val preferences_jsont : preferences Jsont.t

(** A publication record representing a blog, website, or content platform. Publications serve as containers for documents and define the overall branding and settings. *)

type main = {
  basic_theme : Theme.Basic.main option;  (** Simplified publication theme for tools and apps to utilize when displaying content. *)
  description : string option;  (** Brief description of the publication. *)
  icon : Atp.Blob_ref.t option;  (** Square image to identify the publication. Should be at least 256x256. *)
  name : string;  (** Name of the publication. *)
  preferences : preferences option;  (** Object containing platform specific preferences (with a few shared properties). *)
  url : string;  (** Base publication url (ex: https://standard.site). The canonical document URL is formed by combining this value with the document path. *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

    end
  end
end
