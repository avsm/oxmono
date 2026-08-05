(* Atp_lexicon_atproto - generated from atproto lexicons *)

(** AT Protocol lexicon types and Jsont codecs for Atp_lexicon_atproto. *)

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

type main = {
  cid : string;
  uri : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module ListRecords : sig

type record = {
  cid : string;
  uri : string;
  value : Jsont.json;
}

(** Jsont codec for {!type:record}. *)
val record_jsont : record Jsont.t

(** List a range of records in a repository, matching a specific collection. Does not require auth. *)

(** Query/procedure parameters. *)
type params = {
  collection : string;  (** The NSID of the record type. *)
  cursor : string option;
  limit : int option;  (** The number of records to return. *)
  repo : string;  (** The handle or DID of the repo. *)
  reverse : bool option;  (** Flag to reverse the order of the returned records. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  records : record list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetRecord : sig
(** Get a single record from a repository. Does not require auth. *)

(** Query/procedure parameters. *)
type params = {
  cid : string option;  (** The CID of the version of the record. If not specified, then return the most recent version. *)
  collection : string;  (** The NSID of the record collection. *)
  repo : string;  (** The handle or DID of the repo. *)
  rkey : string;  (** The Record Key. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Defs : sig

type commit_meta = {
  cid : string;
  rev : string;
}

(** Jsont codec for {!type:commit_meta}. *)
val commit_meta_jsont : commit_meta Jsont.t

      end
      module PutRecord : sig
(** Write a repository record, creating or updating it as needed. Requires auth, implemented by PDS. *)


type input = {
  collection : string;  (** The NSID of the record collection. *)
  record : Jsont.json;  (** The record to write. *)
  repo : string;  (** The handle or DID of the repo (aka, current account). *)
  rkey : string;  (** The Record Key. *)
  swap_commit : string option;  (** Compare and swap with the previous commit by CID. *)
  swap_record : string option;  (** Compare and swap with the previous record by CID. WARNING: nullable and optional field; may cause problems with golang implementation *)
  validate : bool option;  (** Can be set to 'false' to skip Lexicon schema validation of record data, 'true' to require it, or leave unset to validate only for known Lexicons. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  cid : string;
  commit : Defs.commit_meta option;
  uri : string;
  validation_status : string option;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module DeleteRecord : sig
(** Delete a repository record, or ensure it doesn't exist. Requires auth, implemented by PDS. *)


type input = {
  collection : string;  (** The NSID of the record collection. *)
  repo : string;  (** The handle or DID of the repo (aka, current account). *)
  rkey : string;  (** The Record Key. *)
  swap_commit : string option;  (** Compare and swap with the previous commit by CID. *)
  swap_record : string option;  (** Compare and swap with the previous record by CID. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  commit : Defs.commit_meta option;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module CreateRecord : sig
(** Create a single new repository record. Requires auth, implemented by PDS. *)


type input = {
  collection : string;  (** The NSID of the record collection. *)
  record : Jsont.json;  (** The record itself. Must contain a $type field. *)
  repo : string;  (** The handle or DID of the repo (aka, current account). *)
  rkey : string option;  (** The Record Key. *)
  swap_commit : string option;  (** Compare and swap with the previous commit by CID. *)
  validate : bool option;  (** Can be set to 'false' to skip Lexicon schema validation of record data, 'true' to require it, or leave unset to validate only for known Lexicons. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  cid : string;
  commit : Defs.commit_meta option;
  uri : string;
  validation_status : string option;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
  end
end
