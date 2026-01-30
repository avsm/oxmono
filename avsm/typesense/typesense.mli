(** {1 Typesense}

    An open source search engine for building delightful search experiences.

    @version 30.0 *)

type t

val create :
  ?session:Requests.t ->
  sw:Eio.Switch.t ->
  < net : _ Eio.Net.t ; fs : Eio.Fs.dir_ty Eio.Path.t ; clock : _ Eio.Time.clock ; .. > ->
  base_url:string ->
  t

val base_url : t -> string
val session : t -> Requests.t

module VoiceQueryModelCollection : sig
  module Config : sig
    (** Configuration for the voice query model
     *)
    type t
    
    (** Construct a value *)
    val v : ?model_name:string -> unit -> t
    
    val model_name : t -> string option
    
    val jsont : t Jsont.t
  end
end

module SynonymSetDeleteSchema : sig
  module T : sig
    type t
    
    (** Construct a value
        @param name Name of the deleted synonym set
    *)
    val v : name:string -> unit -> t
    
    (** Name of the deleted synonym set *)
    val name : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** Delete a synonym set
  
      Delete a specific synonym set by its name 
      @param synonym_set_name The name of the synonym set to delete
  *)
  val delete_synonym_set : synonym_set_name:string -> t -> unit -> T.t
end

module SynonymItemUpsertSchema : sig
  module T : sig
    type t
    
    (** Construct a value
        @param synonyms Array of words that should be considered as synonyms
        @param locale Locale for the synonym, leave blank to use the standard tokenizer
        @param root For 1-way synonyms, indicates the root word that words in the synonyms parameter map to
        @param symbols_to_index By default, special characters are dropped from synonyms. Use this attribute to specify which special characters should be indexed as is
    *)
    val v : synonyms:string list -> ?locale:string -> ?root:string -> ?symbols_to_index:string list -> unit -> t
    
    (** Locale for the synonym, leave blank to use the standard tokenizer *)
    val locale : t -> string option
    
    (** For 1-way synonyms, indicates the root word that words in the synonyms parameter map to *)
    val root : t -> string option
    
    (** By default, special characters are dropped from synonyms. Use this attribute to specify which special characters should be indexed as is *)
    val symbols_to_index : t -> string list option
    
    (** Array of words that should be considered as synonyms *)
    val synonyms : t -> string list
    
    val jsont : t Jsont.t
  end
end

module SynonymItemSchema : sig
  module T : sig
    type t
    
    (** Construct a value
        @param id Unique identifier for the synonym item
        @param synonyms Array of words that should be considered as synonyms
        @param locale Locale for the synonym, leave blank to use the standard tokenizer
        @param root For 1-way synonyms, indicates the root word that words in the synonyms parameter map to
        @param symbols_to_index By default, special characters are dropped from synonyms. Use this attribute to specify which special characters should be indexed as is
    *)
    val v : id:string -> synonyms:string list -> ?locale:string -> ?root:string -> ?symbols_to_index:string list -> unit -> t
    
    (** Unique identifier for the synonym item *)
    val id : t -> string
    
    (** Locale for the synonym, leave blank to use the standard tokenizer *)
    val locale : t -> string option
    
    (** For 1-way synonyms, indicates the root word that words in the synonyms parameter map to *)
    val root : t -> string option
    
    (** By default, special characters are dropped from synonyms. Use this attribute to specify which special characters should be indexed as is *)
    val symbols_to_index : t -> string list option
    
    (** Array of words that should be considered as synonyms *)
    val synonyms : t -> string list
    
    val jsont : t Jsont.t
  end
  
  (** List items in a synonym set
  
      Retrieve all synonym items in a set 
      @param synonym_set_name The name of the synonym set to retrieve items for
  *)
  val retrieve_synonym_set_items : synonym_set_name:string -> t -> unit -> T.t
  
  (** Retrieve a synonym set item
  
      Retrieve a specific synonym item by its id 
      @param synonym_set_name The name of the synonym set
      @param item_id The id of the synonym item to retrieve
  *)
  val retrieve_synonym_set_item : synonym_set_name:string -> item_id:string -> t -> unit -> T.t
  
  (** Create or update a synonym set item
  
      Create or update a synonym set item with the given id 
      @param synonym_set_name The name of the synonym set
      @param item_id The id of the synonym item to upsert
  *)
  val upsert_synonym_set_item : synonym_set_name:string -> item_id:string -> body:SynonymItemUpsertSchema.T.t -> t -> unit -> T.t
end

module SynonymSetCreateSchema : sig
  module T : sig
    type t
    
    (** Construct a value
        @param items Array of synonym items
    *)
    val v : items:SynonymItemSchema.T.t list -> unit -> t
    
    (** Array of synonym items *)
    val items : t -> SynonymItemSchema.T.t list
    
    val jsont : t Jsont.t
  end
end

module SynonymSetSchema : sig
  module T : sig
    type t
    
    (** Construct a value
        @param items Array of synonym items
        @param name Name of the synonym set
    *)
    val v : items:SynonymItemSchema.T.t list -> name:string -> unit -> t
    
    (** Array of synonym items *)
    val items : t -> SynonymItemSchema.T.t list
    
    (** Name of the synonym set *)
    val name : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** List all synonym sets
  
      Retrieve all synonym sets *)
  val retrieve_synonym_sets : t -> unit -> T.t
  
  (** Retrieve a synonym set
  
      Retrieve a specific synonym set by its name 
      @param synonym_set_name The name of the synonym set to retrieve
  *)
  val retrieve_synonym_set : synonym_set_name:string -> t -> unit -> T.t
  
  (** Create or update a synonym set
  
      Create or update a synonym set with the given name 
      @param synonym_set_name The name of the synonym set to create/update
  *)
  val upsert_synonym_set : synonym_set_name:string -> body:SynonymSetCreateSchema.T.t -> t -> unit -> T.t
end

module SynonymSetsRetrieveSchema : sig
  module T : sig
    type t
    
    (** Construct a value
        @param synonym_sets Array of synonym sets
    *)
    val v : synonym_sets:SynonymSetSchema.T.t list -> unit -> t
    
    (** Array of synonym sets *)
    val synonym_sets : t -> SynonymSetSchema.T.t list
    
    val jsont : t Jsont.t
  end
end

module SynonymItemDeleteSchema : sig
  module T : sig
    type t
    
    (** Construct a value
        @param id ID of the deleted synonym item
    *)
    val v : id:string -> unit -> t
    
    (** ID of the deleted synonym item *)
    val id : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** Delete a synonym set item
  
      Delete a specific synonym item by its id 
      @param synonym_set_name The name of the synonym set
      @param item_id The id of the synonym item to delete
  *)
  val delete_synonym_set_item : synonym_set_name:string -> item_id:string -> t -> unit -> T.t
end

module Success : sig
  module Status : sig
    type t
    
    (** Construct a value *)
    val v : success:bool -> unit -> t
    
    val success : t -> bool
    
    val jsont : t Jsont.t
  end
  
  (** Toggle Slow Request Log
  
      Enable logging of requests that take over a defined threshold of time. Default is `-1` which disables slow request logging. Slow requests are logged to the primary log file, with the prefix SLOW REQUEST. *)
  val toggle_slow_request_log : t -> unit -> Status.t
  
  (** Clear the cached responses of search requests in the LRU cache.
  
      Clear the cached responses of search requests that are sent with `use_cache` parameter in the LRU cache. *)
  val clear_cache : t -> unit -> Status.t
  
  (** Compacting the on-disk database
  
      Typesense uses RocksDB to store your documents on the disk. If you do frequent writes or updates, you could benefit from running a compaction of the underlying RocksDB database. This could reduce the size of the database and decrease read latency. While the database will not block during this operation, we recommend running it during off-peak hours. *)
  val compact_db : t -> unit -> Status.t
  
  (** Creates a point-in-time snapshot of a Typesense node's state and data in the specified directory.
  
      Creates a point-in-time snapshot of a Typesense node's state and data in the specified directory. You can then backup the snapshot directory that gets created and later restore it as a data directory, as needed. 
      @param snapshot_path The directory on the server where the snapshot should be saved.
  *)
  val take_snapshot : snapshot_path:string -> t -> unit -> Status.t
  
  (** Triggers a follower node to initiate the raft voting process, which triggers leader re-election.
  
      Triggers a follower node to initiate the raft voting process, which triggers leader re-election. The follower node that you run this operation against will become the new leader, once this command succeeds. *)
  val vote : t -> unit -> Status.t
end

module StopwordsSetUpsertSchema : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : stopwords:string list -> ?locale:string -> unit -> t
    
    val locale : t -> string option
    
    val stopwords : t -> string list
    
    val jsont : t Jsont.t
  end
end

module StopwordsSetSchema : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : id:string -> stopwords:string list -> ?locale:string -> unit -> t
    
    val id : t -> string
    
    val locale : t -> string option
    
    val stopwords : t -> string list
    
    val jsont : t Jsont.t
  end
  
  (** Upserts a stopwords set.
  
      When an analytics rule is created, we give it a name and describe the type, the source collections and the destination collection. 
      @param set_id The ID of the stopwords set to upsert.
  *)
  val upsert_stopwords_set : set_id:string -> body:StopwordsSetUpsertSchema.T.t -> t -> unit -> T.t
end

module StopwordsSetsRetrieveAllSchema : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : stopwords:StopwordsSetSchema.T.t list -> unit -> t
    
    val stopwords : t -> StopwordsSetSchema.T.t list
    
    val jsont : t Jsont.t
  end
  
  (** Retrieves all stopwords sets.
  
      Retrieve the details of all stopwords sets *)
  val retrieve_stopwords_sets : t -> unit -> T.t
end

module StopwordsSetRetrieveSchema : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : stopwords:StopwordsSetSchema.T.t -> unit -> t
    
    val stopwords : t -> StopwordsSetSchema.T.t
    
    val jsont : t Jsont.t
  end
  
  (** Retrieves a stopwords set.
  
      Retrieve the details of a stopwords set, given it's name. 
      @param set_id The ID of the stopwords set to retrieve.
  *)
  val retrieve_stopwords_set : set_id:string -> t -> unit -> T.t
end

module StemmingDictionary : sig
  module T : sig
    type t
    
    (** Construct a value
        @param id Unique identifier for the dictionary
        @param words List of word mappings in the dictionary
    *)
    val v : id:string -> words:Jsont.json list -> unit -> t
    
    (** Unique identifier for the dictionary *)
    val id : t -> string
    
    (** List of word mappings in the dictionary *)
    val words : t -> Jsont.json list
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve a stemming dictionary
  
      Fetch details of a specific stemming dictionary. 
      @param dictionary_id The ID of the dictionary to retrieve
  *)
  val get_stemming_dictionary : dictionary_id:string -> t -> unit -> T.t
end

module SearchSynonymSchema : sig
  module T : sig
    type t
    
    (** Construct a value
        @param synonyms Array of words that should be considered as synonyms.
        @param locale Locale for the synonym, leave blank to use the standard tokenizer.
        @param root For 1-way synonyms, indicates the root word that words in the `synonyms` parameter map to.
        @param symbols_to_index By default, special characters are dropped from synonyms. Use this attribute to specify which special characters should be indexed as is.
    *)
    val v : synonyms:string list -> ?locale:string -> ?root:string -> ?symbols_to_index:string list -> unit -> t
    
    (** Locale for the synonym, leave blank to use the standard tokenizer. *)
    val locale : t -> string option
    
    (** For 1-way synonyms, indicates the root word that words in the `synonyms` parameter map to. *)
    val root : t -> string option
    
    (** By default, special characters are dropped from synonyms. Use this attribute to specify which special characters should be indexed as is. *)
    val symbols_to_index : t -> string list option
    
    (** Array of words that should be considered as synonyms. *)
    val synonyms : t -> string list
    
    val jsont : t Jsont.t
  end
end

module SearchSynonymDelete : sig
  module Response : sig
    type t
    
    (** Construct a value
        @param id The id of the synonym that was deleted
    *)
    val v : id:string -> unit -> t
    
    (** The id of the synonym that was deleted *)
    val id : t -> string
    
    val jsont : t Jsont.t
  end
end

module SearchSynonym : sig
  module T : sig
    type t
    
    (** Construct a value
        @param synonyms Array of words that should be considered as synonyms.
        @param locale Locale for the synonym, leave blank to use the standard tokenizer.
        @param root For 1-way synonyms, indicates the root word that words in the `synonyms` parameter map to.
        @param symbols_to_index By default, special characters are dropped from synonyms. Use this attribute to specify which special characters should be indexed as is.
    *)
    val v : synonyms:string list -> id:string -> ?locale:string -> ?root:string -> ?symbols_to_index:string list -> unit -> t
    
    (** Locale for the synonym, leave blank to use the standard tokenizer. *)
    val locale : t -> string option
    
    (** For 1-way synonyms, indicates the root word that words in the `synonyms` parameter map to. *)
    val root : t -> string option
    
    (** By default, special characters are dropped from synonyms. Use this attribute to specify which special characters should be indexed as is. *)
    val symbols_to_index : t -> string list option
    
    (** Array of words that should be considered as synonyms. *)
    val synonyms : t -> string list
    
    val id : t -> string
    
    val jsont : t Jsont.t
  end
end

module SearchSynonyms : sig
  module Response : sig
    type t
    
    (** Construct a value *)
    val v : synonyms:SearchSynonym.T.t list -> unit -> t
    
    val synonyms : t -> SearchSynonym.T.t list
    
    val jsont : t Jsont.t
  end
end

module SearchResultConversation : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : answer:string -> conversation_history:Jsont.json list -> conversation_id:string -> query:string -> unit -> t
    
    val answer : t -> string
    
    val conversation_history : t -> Jsont.json list
    
    val conversation_id : t -> string
    
    val query : t -> string
    
    val jsont : t Jsont.t
  end
end

module SearchRequestParams : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : collection_name:string -> per_page:int -> q:string -> ?voice_query:Jsont.json -> unit -> t
    
    val collection_name : t -> string
    
    val per_page : t -> int
    
    val q : t -> string
    
    val voice_query : t -> Jsont.json option
    
    val jsont : t Jsont.t
  end
end

module SearchHighlight : sig
  module T : sig
    type t
    
    (** Construct a value
        @param indices The indices property will be present only for string[] fields and will contain the corresponding indices of the snippets in the search field
        @param snippet Present only for (non-array) string fields
        @param snippets Present only for (array) string[] fields
        @param value Full field value with highlighting, present only for (non-array) string fields
        @param values Full field value with highlighting, present only for (array) string[] fields
    *)
    val v : ?field:string -> ?indices:int list -> ?matched_tokens:Jsont.json list -> ?snippet:string -> ?snippets:string list -> ?value:string -> ?values:string list -> unit -> t
    
    val field : t -> string option
    
    (** The indices property will be present only for string[] fields and will contain the corresponding indices of the snippets in the search field *)
    val indices : t -> int list option
    
    val matched_tokens : t -> Jsont.json list option
    
    (** Present only for (non-array) string fields *)
    val snippet : t -> string option
    
    (** Present only for (array) string[] fields *)
    val snippets : t -> string list option
    
    (** Full field value with highlighting, present only for (non-array) string fields *)
    val value : t -> string option
    
    (** Full field value with highlighting, present only for (array) string[] fields *)
    val values : t -> string list option
    
    val jsont : t Jsont.t
  end
end

module SearchResultHit : sig
  module T : sig
    type t
    
    (** Construct a value
        @param document Can be any key-value pair
        @param geo_distance_meters Can be any key-value pair
        @param highlight Highlighted version of the matching document
        @param highlights (Deprecated) Contains highlighted portions of the search fields
        @param hybrid_search_info Information about hybrid search scoring
        @param search_index Returned only for union query response. Indicates the index of the query which this document matched to.
        @param vector_distance Distance between the query vector and matching document's vector value
    *)
    val v : ?document:Jsont.json -> ?geo_distance_meters:Jsont.json -> ?highlight:Jsont.json -> ?highlights:SearchHighlight.T.t list -> ?hybrid_search_info:Jsont.json -> ?search_index:int -> ?text_match:int64 -> ?text_match_info:Jsont.json -> ?vector_distance:float -> unit -> t
    
    (** Can be any key-value pair *)
    val document : t -> Jsont.json option
    
    (** Can be any key-value pair *)
    val geo_distance_meters : t -> Jsont.json option
    
    (** Highlighted version of the matching document *)
    val highlight : t -> Jsont.json option
    
    (** (Deprecated) Contains highlighted portions of the search fields *)
    val highlights : t -> SearchHighlight.T.t list option
    
    (** Information about hybrid search scoring *)
    val hybrid_search_info : t -> Jsont.json option
    
    (** Returned only for union query response. Indicates the index of the query which this document matched to. *)
    val search_index : t -> int option
    
    val text_match : t -> int64 option
    
    val text_match_info : t -> Jsont.json option
    
    (** Distance between the query vector and matching document's vector value *)
    val vector_distance : t -> float option
    
    val jsont : t Jsont.t
  end
end

module SearchGroupedHit : sig
  module T : sig
    type t
    
    (** Construct a value
        @param hits The documents that matched the search query
    *)
    val v : group_key:Jsont.json list -> hits:SearchResultHit.T.t list -> ?found:int -> unit -> t
    
    val found : t -> int option
    
    val group_key : t -> Jsont.json list
    
    (** The documents that matched the search query *)
    val hits : t -> SearchResultHit.T.t list
    
    val jsont : t Jsont.t
  end
end

module SchemaChange : sig
  module Status : sig
    type t
    
    (** Construct a value
        @param altered_docs Number of documents that have been altered
        @param collection Name of the collection being modified
        @param validated_docs Number of documents that have been validated
    *)
    val v : ?altered_docs:int -> ?collection:string -> ?validated_docs:int -> unit -> t
    
    (** Number of documents that have been altered *)
    val altered_docs : t -> int option
    
    (** Name of the collection being modified *)
    val collection : t -> string option
    
    (** Number of documents that have been validated *)
    val validated_docs : t -> int option
    
    val jsont : t Jsont.t
  end
  
  (** Get the status of in-progress schema change operations
  
      Returns the status of any ongoing schema change operations. If no schema changes are in progress, returns an empty response. *)
  val get_schema_changes : t -> unit -> Status.t
end

module PresetUpsertSchema : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : value:Jsont.json -> unit -> t
    
    val value : t -> Jsont.json
    
    val jsont : t Jsont.t
  end
end

module PresetSchema : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : value:Jsont.json -> name:string -> unit -> t
    
    val value : t -> Jsont.json
    
    val name : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** Retrieves a preset.
  
      Retrieve the details of a preset, given it's name. 
      @param preset_id The ID of the preset to retrieve.
  *)
  val retrieve_preset : preset_id:string -> t -> unit -> T.t
  
  (** Upserts a preset.
  
      Create or update an existing preset. 
      @param preset_id The name of the preset set to upsert.
  *)
  val upsert_preset : preset_id:string -> body:PresetUpsertSchema.T.t -> t -> unit -> T.t
end

module PresetsRetrieveSchema : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : presets:PresetSchema.T.t list -> unit -> t
    
    val presets : t -> PresetSchema.T.t list
    
    val jsont : t Jsont.t
  end
  
  (** Retrieves all presets.
  
      Retrieve the details of all presets *)
  val retrieve_all_presets : t -> unit -> T.t
end

module PresetDeleteSchema : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : name:string -> unit -> t
    
    val name : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** Delete a preset.
  
      Permanently deletes a preset, given it's name. 
      @param preset_id The ID of the preset to delete.
  *)
  val delete_preset : preset_id:string -> t -> unit -> T.t
end

module NlsearchModelDeleteSchema : sig
  module T : sig
    type t
    
    (** Construct a value
        @param id ID of the deleted NL search model
    *)
    val v : id:string -> unit -> t
    
    (** ID of the deleted NL search model *)
    val id : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** Delete a NL search model
  
      Delete a specific NL search model by its ID. 
      @param model_id The ID of the NL search model to delete
  *)
  val delete_nlsearch_model : model_id:string -> t -> unit -> T.t
end

module NlsearchModelCreateSchema : sig
  module T : sig
    type t
    
    (** Construct a value
        @param access_token Access token for GCP Vertex AI
        @param account_id Account ID for Cloudflare-specific models
        @param api_key API key for the NL model service
        @param api_url Custom API URL for the NL model service
        @param api_version API version for the NL model service
        @param client_id Client ID for GCP Vertex AI
        @param client_secret Client secret for GCP Vertex AI
        @param max_bytes Maximum number of bytes to process
        @param max_output_tokens Maximum output tokens for GCP Vertex AI
        @param model_name Name of the NL model to use
        @param project_id Project ID for GCP Vertex AI
        @param refresh_token Refresh token for GCP Vertex AI
        @param region Region for GCP Vertex AI
        @param stop_sequences Stop sequences for the NL model (Google-specific)
        @param system_prompt System prompt for the NL model
        @param temperature Temperature parameter for the NL model
        @param top_k Top-k parameter for the NL model (Google-specific)
        @param top_p Top-p parameter for the NL model (Google-specific)
        @param id Optional ID for the NL search model
    *)
    val v : ?access_token:string -> ?account_id:string -> ?api_key:string -> ?api_url:string -> ?api_version:string -> ?client_id:string -> ?client_secret:string -> ?max_bytes:int -> ?max_output_tokens:int -> ?model_name:string -> ?project_id:string -> ?refresh_token:string -> ?region:string -> ?stop_sequences:string list -> ?system_prompt:string -> ?temperature:float -> ?top_k:int -> ?top_p:float -> ?id:string -> unit -> t
    
    (** Access token for GCP Vertex AI *)
    val access_token : t -> string option
    
    (** Account ID for Cloudflare-specific models *)
    val account_id : t -> string option
    
    (** API key for the NL model service *)
    val api_key : t -> string option
    
    (** Custom API URL for the NL model service *)
    val api_url : t -> string option
    
    (** API version for the NL model service *)
    val api_version : t -> string option
    
    (** Client ID for GCP Vertex AI *)
    val client_id : t -> string option
    
    (** Client secret for GCP Vertex AI *)
    val client_secret : t -> string option
    
    (** Maximum number of bytes to process *)
    val max_bytes : t -> int option
    
    (** Maximum output tokens for GCP Vertex AI *)
    val max_output_tokens : t -> int option
    
    (** Name of the NL model to use *)
    val model_name : t -> string option
    
    (** Project ID for GCP Vertex AI *)
    val project_id : t -> string option
    
    (** Refresh token for GCP Vertex AI *)
    val refresh_token : t -> string option
    
    (** Region for GCP Vertex AI *)
    val region : t -> string option
    
    (** Stop sequences for the NL model (Google-specific) *)
    val stop_sequences : t -> string list option
    
    (** System prompt for the NL model *)
    val system_prompt : t -> string option
    
    (** Temperature parameter for the NL model *)
    val temperature : t -> float option
    
    (** Top-k parameter for the NL model (Google-specific) *)
    val top_k : t -> int option
    
    (** Top-p parameter for the NL model (Google-specific) *)
    val top_p : t -> float option
    
    (** Optional ID for the NL search model *)
    val id : t -> string option
    
    val jsont : t Jsont.t
  end
end

module NlsearchModelSchema : sig
  module T : sig
    type t
    
    (** Construct a value
        @param id ID of the NL search model
        @param access_token Access token for GCP Vertex AI
        @param account_id Account ID for Cloudflare-specific models
        @param api_key API key for the NL model service
        @param api_url Custom API URL for the NL model service
        @param api_version API version for the NL model service
        @param client_id Client ID for GCP Vertex AI
        @param client_secret Client secret for GCP Vertex AI
        @param max_bytes Maximum number of bytes to process
        @param max_output_tokens Maximum output tokens for GCP Vertex AI
        @param model_name Name of the NL model to use
        @param project_id Project ID for GCP Vertex AI
        @param refresh_token Refresh token for GCP Vertex AI
        @param region Region for GCP Vertex AI
        @param stop_sequences Stop sequences for the NL model (Google-specific)
        @param system_prompt System prompt for the NL model
        @param temperature Temperature parameter for the NL model
        @param top_k Top-k parameter for the NL model (Google-specific)
        @param top_p Top-p parameter for the NL model (Google-specific)
    *)
    val v : id:string -> ?access_token:string -> ?account_id:string -> ?api_key:string -> ?api_url:string -> ?api_version:string -> ?client_id:string -> ?client_secret:string -> ?max_bytes:int -> ?max_output_tokens:int -> ?model_name:string -> ?project_id:string -> ?refresh_token:string -> ?region:string -> ?stop_sequences:string list -> ?system_prompt:string -> ?temperature:float -> ?top_k:int -> ?top_p:float -> unit -> t
    
    (** Access token for GCP Vertex AI *)
    val access_token : t -> string option
    
    (** Account ID for Cloudflare-specific models *)
    val account_id : t -> string option
    
    (** API key for the NL model service *)
    val api_key : t -> string option
    
    (** Custom API URL for the NL model service *)
    val api_url : t -> string option
    
    (** API version for the NL model service *)
    val api_version : t -> string option
    
    (** Client ID for GCP Vertex AI *)
    val client_id : t -> string option
    
    (** Client secret for GCP Vertex AI *)
    val client_secret : t -> string option
    
    (** Maximum number of bytes to process *)
    val max_bytes : t -> int option
    
    (** Maximum output tokens for GCP Vertex AI *)
    val max_output_tokens : t -> int option
    
    (** Name of the NL model to use *)
    val model_name : t -> string option
    
    (** Project ID for GCP Vertex AI *)
    val project_id : t -> string option
    
    (** Refresh token for GCP Vertex AI *)
    val refresh_token : t -> string option
    
    (** Region for GCP Vertex AI *)
    val region : t -> string option
    
    (** Stop sequences for the NL model (Google-specific) *)
    val stop_sequences : t -> string list option
    
    (** System prompt for the NL model *)
    val system_prompt : t -> string option
    
    (** Temperature parameter for the NL model *)
    val temperature : t -> float option
    
    (** Top-k parameter for the NL model (Google-specific) *)
    val top_k : t -> int option
    
    (** Top-p parameter for the NL model (Google-specific) *)
    val top_p : t -> float option
    
    (** ID of the NL search model *)
    val id : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** List all NL search models
  
      Retrieve all NL search models. *)
  val retrieve_all_nlsearch_models : t -> unit -> T.t
  
  (** Create a NL search model
  
      Create a new NL search model. *)
  val create_nlsearch_model : body:NlsearchModelCreateSchema.T.t -> t -> unit -> T.t
  
  (** Retrieve a NL search model
  
      Retrieve a specific NL search model by its ID. 
      @param model_id The ID of the NL search model to retrieve
  *)
  val retrieve_nlsearch_model : model_id:string -> t -> unit -> T.t
  
  (** Update a NL search model
  
      Update an existing NL search model. 
      @param model_id The ID of the NL search model to update
  *)
  val update_nlsearch_model : model_id:string -> t -> unit -> T.t
end

module NlsearchModelBase : sig
  module T : sig
    type t
    
    (** Construct a value
        @param access_token Access token for GCP Vertex AI
        @param account_id Account ID for Cloudflare-specific models
        @param api_key API key for the NL model service
        @param api_url Custom API URL for the NL model service
        @param api_version API version for the NL model service
        @param client_id Client ID for GCP Vertex AI
        @param client_secret Client secret for GCP Vertex AI
        @param max_bytes Maximum number of bytes to process
        @param max_output_tokens Maximum output tokens for GCP Vertex AI
        @param model_name Name of the NL model to use
        @param project_id Project ID for GCP Vertex AI
        @param refresh_token Refresh token for GCP Vertex AI
        @param region Region for GCP Vertex AI
        @param stop_sequences Stop sequences for the NL model (Google-specific)
        @param system_prompt System prompt for the NL model
        @param temperature Temperature parameter for the NL model
        @param top_k Top-k parameter for the NL model (Google-specific)
        @param top_p Top-p parameter for the NL model (Google-specific)
    *)
    val v : ?access_token:string -> ?account_id:string -> ?api_key:string -> ?api_url:string -> ?api_version:string -> ?client_id:string -> ?client_secret:string -> ?max_bytes:int -> ?max_output_tokens:int -> ?model_name:string -> ?project_id:string -> ?refresh_token:string -> ?region:string -> ?stop_sequences:string list -> ?system_prompt:string -> ?temperature:float -> ?top_k:int -> ?top_p:float -> unit -> t
    
    (** Access token for GCP Vertex AI *)
    val access_token : t -> string option
    
    (** Account ID for Cloudflare-specific models *)
    val account_id : t -> string option
    
    (** API key for the NL model service *)
    val api_key : t -> string option
    
    (** Custom API URL for the NL model service *)
    val api_url : t -> string option
    
    (** API version for the NL model service *)
    val api_version : t -> string option
    
    (** Client ID for GCP Vertex AI *)
    val client_id : t -> string option
    
    (** Client secret for GCP Vertex AI *)
    val client_secret : t -> string option
    
    (** Maximum number of bytes to process *)
    val max_bytes : t -> int option
    
    (** Maximum output tokens for GCP Vertex AI *)
    val max_output_tokens : t -> int option
    
    (** Name of the NL model to use *)
    val model_name : t -> string option
    
    (** Project ID for GCP Vertex AI *)
    val project_id : t -> string option
    
    (** Refresh token for GCP Vertex AI *)
    val refresh_token : t -> string option
    
    (** Region for GCP Vertex AI *)
    val region : t -> string option
    
    (** Stop sequences for the NL model (Google-specific) *)
    val stop_sequences : t -> string list option
    
    (** System prompt for the NL model *)
    val system_prompt : t -> string option
    
    (** Temperature parameter for the NL model *)
    val temperature : t -> float option
    
    (** Top-k parameter for the NL model (Google-specific) *)
    val top_k : t -> int option
    
    (** Top-p parameter for the NL model (Google-specific) *)
    val top_p : t -> float option
    
    val jsont : t Jsont.t
  end
end

module IndexAction : sig
  module T : sig
    type t = [
      | `Create
      | `Update
      | `Upsert
      | `Emplace
    ]
    
    val jsont : t Jsont.t
  end
end

module Health : sig
  module Status : sig
    type t
    
    (** Construct a value *)
    val v : ok:bool -> unit -> t
    
    val ok : t -> bool
    
    val jsont : t Jsont.t
  end
  
  (** Checks if Typesense server is ready to accept requests.
  
      Checks if Typesense server is ready to accept requests. *)
  val health : t -> unit -> Status.t
end

module Field : sig
  module T : sig
    type t
    
    (** Construct a value
        @param symbols_to_index List of symbols or special characters to be indexed.
    
        @param token_separators List of symbols or special characters to be used for splitting the text into individual words in addition to space and new-line characters.
    
        @param async_reference Allow documents to be indexed successfully even when the referenced document doesn't exist yet.
    
        @param range_index Enables an index optimized for range filtering on numerical fields (e.g. rating:>3.5). Default: false.
    
        @param reference Name of a field in another collection that should be linked to this collection so that it can be joined during query.
    
        @param stem Values are stemmed before indexing in-memory. Default: false.
    
        @param stem_dictionary Name of the stemming dictionary to use for this field
        @param store When set to false, the field value will not be stored on disk. Default: true.
    
        @param vec_dist The distance metric to be used for vector search. Default: `cosine`. You can also use `ip` for inner product.
    
    *)
    val v : name:string -> type_:string -> ?index:bool -> ?infix:bool -> ?symbols_to_index:string list -> ?token_separators:string list -> ?async_reference:bool -> ?drop:bool -> ?embed:Jsont.json -> ?facet:bool -> ?locale:string -> ?num_dim:int -> ?optional:bool -> ?range_index:bool -> ?reference:string -> ?sort:bool -> ?stem:bool -> ?stem_dictionary:string -> ?store:bool -> ?vec_dist:string -> unit -> t
    
    (** Allow documents to be indexed successfully even when the referenced document doesn't exist yet.
     *)
    val async_reference : t -> bool option
    
    val drop : t -> bool option
    
    val embed : t -> Jsont.json option
    
    val facet : t -> bool option
    
    val index : t -> bool
    
    val infix : t -> bool
    
    val locale : t -> string option
    
    val name : t -> string
    
    val num_dim : t -> int option
    
    val optional : t -> bool option
    
    (** Enables an index optimized for range filtering on numerical fields (e.g. rating:>3.5). Default: false.
     *)
    val range_index : t -> bool option
    
    (** Name of a field in another collection that should be linked to this collection so that it can be joined during query.
     *)
    val reference : t -> string option
    
    val sort : t -> bool option
    
    (** Values are stemmed before indexing in-memory. Default: false.
     *)
    val stem : t -> bool option
    
    (** Name of the stemming dictionary to use for this field *)
    val stem_dictionary : t -> string option
    
    (** When set to false, the field value will not be stored on disk. Default: true.
     *)
    val store : t -> bool option
    
    (** List of symbols or special characters to be indexed.
     *)
    val symbols_to_index : t -> string list
    
    (** List of symbols or special characters to be used for splitting the text into individual words in addition to space and new-line characters.
     *)
    val token_separators : t -> string list
    
    val type_ : t -> string
    
    (** The distance metric to be used for vector search. Default: `cosine`. You can also use `ip` for inner product.
     *)
    val vec_dist : t -> string option
    
    val jsont : t Jsont.t
  end
end

module CollectionUpdateSchema : sig
  module T : sig
    type t
    
    (** Construct a value
        @param fields A list of fields for querying, filtering and faceting
        @param metadata Optional details about the collection, e.g., when it was created, who created it etc.
    
        @param synonym_sets List of synonym set names to associate with this collection
    *)
    val v : fields:Field.T.t list -> ?metadata:Jsont.json -> ?synonym_sets:string list -> unit -> t
    
    (** A list of fields for querying, filtering and faceting *)
    val fields : t -> Field.T.t list
    
    (** Optional details about the collection, e.g., when it was created, who created it etc.
     *)
    val metadata : t -> Jsont.json option
    
    (** List of synonym set names to associate with this collection *)
    val synonym_sets : t -> string list option
    
    val jsont : t Jsont.t
  end
  
  (** Update a collection
  
      Update a collection's schema to modify the fields and their types. 
      @param collection_name The name of the collection to update
  *)
  val update_collection : collection_name:string -> body:T.t -> t -> unit -> T.t
end

module CollectionSchema : sig
  module T : sig
    type t
    
    (** Construct a value
        @param fields A list of fields for querying, filtering and faceting
        @param name Name of the collection
        @param default_sorting_field The name of an int32 / float field that determines the order in which the search results are ranked when a sort_by clause is not provided during searching. This field must indicate some kind of popularity.
        @param enable_nested_fields Enables experimental support at a collection level for nested object or object array fields. This field is only available if the Typesense server is version `0.24.0.rcn34` or later.
        @param symbols_to_index List of symbols or special characters to be indexed.
    
        @param token_separators List of symbols or special characters to be used for splitting the text into individual words in addition to space and new-line characters.
    
        @param metadata Optional details about the collection, e.g., when it was created, who created it etc.
    
        @param synonym_sets List of synonym set names to associate with this collection
    *)
    val v : fields:Field.T.t list -> name:string -> ?default_sorting_field:string -> ?enable_nested_fields:bool -> ?symbols_to_index:string list -> ?token_separators:string list -> ?metadata:Jsont.json -> ?synonym_sets:string list -> ?voice_query_model:VoiceQueryModelCollection.Config.t -> unit -> t
    
    (** The name of an int32 / float field that determines the order in which the search results are ranked when a sort_by clause is not provided during searching. This field must indicate some kind of popularity. *)
    val default_sorting_field : t -> string
    
    (** Enables experimental support at a collection level for nested object or object array fields. This field is only available if the Typesense server is version `0.24.0.rcn34` or later. *)
    val enable_nested_fields : t -> bool
    
    (** A list of fields for querying, filtering and faceting *)
    val fields : t -> Field.T.t list
    
    (** Optional details about the collection, e.g., when it was created, who created it etc.
     *)
    val metadata : t -> Jsont.json option
    
    (** Name of the collection *)
    val name : t -> string
    
    (** List of symbols or special characters to be indexed.
     *)
    val symbols_to_index : t -> string list
    
    (** List of synonym set names to associate with this collection *)
    val synonym_sets : t -> string list option
    
    (** List of symbols or special characters to be used for splitting the text into individual words in addition to space and new-line characters.
     *)
    val token_separators : t -> string list
    
    val voice_query_model : t -> VoiceQueryModelCollection.Config.t option
    
    val jsont : t Jsont.t
  end
end

module Collection : sig
  module Response : sig
    type t
    
    (** Construct a value
        @param fields A list of fields for querying, filtering and faceting
        @param name Name of the collection
        @param num_documents Number of documents in the collection
        @param created_at Timestamp of when the collection was created (Unix epoch in seconds)
        @param default_sorting_field The name of an int32 / float field that determines the order in which the search results are ranked when a sort_by clause is not provided during searching. This field must indicate some kind of popularity.
        @param enable_nested_fields Enables experimental support at a collection level for nested object or object array fields. This field is only available if the Typesense server is version `0.24.0.rcn34` or later.
        @param symbols_to_index List of symbols or special characters to be indexed.
    
        @param token_separators List of symbols or special characters to be used for splitting the text into individual words in addition to space and new-line characters.
    
        @param metadata Optional details about the collection, e.g., when it was created, who created it etc.
    
        @param synonym_sets List of synonym set names to associate with this collection
    *)
    val v : fields:Field.T.t list -> name:string -> num_documents:int64 -> created_at:int64 -> ?default_sorting_field:string -> ?enable_nested_fields:bool -> ?symbols_to_index:string list -> ?token_separators:string list -> ?metadata:Jsont.json -> ?synonym_sets:string list -> ?voice_query_model:VoiceQueryModelCollection.Config.t -> unit -> t
    
    (** The name of an int32 / float field that determines the order in which the search results are ranked when a sort_by clause is not provided during searching. This field must indicate some kind of popularity. *)
    val default_sorting_field : t -> string
    
    (** Enables experimental support at a collection level for nested object or object array fields. This field is only available if the Typesense server is version `0.24.0.rcn34` or later. *)
    val enable_nested_fields : t -> bool
    
    (** A list of fields for querying, filtering and faceting *)
    val fields : t -> Field.T.t list
    
    (** Optional details about the collection, e.g., when it was created, who created it etc.
     *)
    val metadata : t -> Jsont.json option
    
    (** Name of the collection *)
    val name : t -> string
    
    (** List of symbols or special characters to be indexed.
     *)
    val symbols_to_index : t -> string list
    
    (** List of synonym set names to associate with this collection *)
    val synonym_sets : t -> string list option
    
    (** List of symbols or special characters to be used for splitting the text into individual words in addition to space and new-line characters.
     *)
    val token_separators : t -> string list
    
    val voice_query_model : t -> VoiceQueryModelCollection.Config.t option
    
    (** Number of documents in the collection *)
    val num_documents : t -> int64
    
    (** Timestamp of when the collection was created (Unix epoch in seconds) *)
    val created_at : t -> int64
    
    val jsont : t Jsont.t
  end
  
  (** List all collections
  
      Returns a summary of all your collections. The collections are returned sorted by creation date, with the most recent collections appearing first. *)
  val get_collections : ?get_collections_parameters:string -> t -> unit -> Response.t
  
  (** Create a new collection
  
      When a collection is created, we give it a name and describe the fields that will be indexed from the documents added to the collection. *)
  val create_collection : body:CollectionSchema.T.t -> t -> unit -> Response.t
  
  (** Retrieve a single collection
  
      Retrieve the details of a collection, given its name. 
      @param collection_name The name of the collection to retrieve
  *)
  val get_collection : collection_name:string -> t -> unit -> Response.t
  
  (** Delete a collection
  
      Permanently drops a collection. This action cannot be undone. For large collections, this might have an impact on read latencies. 
      @param collection_name The name of the collection to delete
  *)
  val delete_collection : collection_name:string -> t -> unit -> Response.t
end

module FacetCounts : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?counts:Jsont.json list -> ?field_name:string -> ?stats:Jsont.json -> unit -> t
    
    val counts : t -> Jsont.json list option
    
    val field_name : t -> string option
    
    val stats : t -> Jsont.json option
    
    val jsont : t Jsont.t
  end
end

module Search : sig
  module Result : sig
    type t
    
    (** Construct a value
        @param found The number of documents found
        @param hits The documents that matched the search query
        @param metadata Custom JSON object that can be returned in the search response
        @param out_of The total number of documents in the collection
        @param page The search result page number
        @param search_cutoff Whether the search was cut off
        @param search_time_ms The number of milliseconds the search took
        @param union_request_params Returned only for union query response.
    *)
    val v : ?conversation:SearchResultConversation.T.t -> ?facet_counts:FacetCounts.T.t list -> ?found:int -> ?found_docs:int -> ?grouped_hits:SearchGroupedHit.T.t list -> ?hits:SearchResultHit.T.t list -> ?metadata:Jsont.json -> ?out_of:int -> ?page:int -> ?request_params:SearchRequestParams.T.t -> ?search_cutoff:bool -> ?search_time_ms:int -> ?union_request_params:SearchRequestParams.T.t list -> unit -> t
    
    val conversation : t -> SearchResultConversation.T.t option
    
    val facet_counts : t -> FacetCounts.T.t list option
    
    (** The number of documents found *)
    val found : t -> int option
    
    val found_docs : t -> int option
    
    val grouped_hits : t -> SearchGroupedHit.T.t list option
    
    (** The documents that matched the search query *)
    val hits : t -> SearchResultHit.T.t list option
    
    (** Custom JSON object that can be returned in the search response *)
    val metadata : t -> Jsont.json option
    
    (** The total number of documents in the collection *)
    val out_of : t -> int option
    
    (** The search result page number *)
    val page : t -> int option
    
    val request_params : t -> SearchRequestParams.T.t option
    
    (** Whether the search was cut off *)
    val search_cutoff : t -> bool option
    
    (** The number of milliseconds the search took *)
    val search_time_ms : t -> int option
    
    (** Returned only for union query response. *)
    val union_request_params : t -> SearchRequestParams.T.t list option
    
    val jsont : t Jsont.t
  end
  
  (** Search for documents in a collection
  
      Search for documents in a collection that match the search criteria. 
      @param collection_name The name of the collection to search for the document under
  *)
  val search_collection : collection_name:string -> search_parameters:string -> t -> unit -> Result.t
end

module MultiSearchResult : sig
  module Item : sig
    type t
    
    (** Construct a value
        @param found The number of documents found
        @param hits The documents that matched the search query
        @param metadata Custom JSON object that can be returned in the search response
        @param out_of The total number of documents in the collection
        @param page The search result page number
        @param search_cutoff Whether the search was cut off
        @param search_time_ms The number of milliseconds the search took
        @param union_request_params Returned only for union query response.
        @param code HTTP error code
        @param error Error description
    *)
    val v : ?conversation:SearchResultConversation.T.t -> ?facet_counts:FacetCounts.T.t list -> ?found:int -> ?found_docs:int -> ?grouped_hits:SearchGroupedHit.T.t list -> ?hits:SearchResultHit.T.t list -> ?metadata:Jsont.json -> ?out_of:int -> ?page:int -> ?request_params:SearchRequestParams.T.t -> ?search_cutoff:bool -> ?search_time_ms:int -> ?union_request_params:SearchRequestParams.T.t list -> ?code:int64 -> ?error:string -> unit -> t
    
    val conversation : t -> SearchResultConversation.T.t option
    
    val facet_counts : t -> FacetCounts.T.t list option
    
    (** The number of documents found *)
    val found : t -> int option
    
    val found_docs : t -> int option
    
    val grouped_hits : t -> SearchGroupedHit.T.t list option
    
    (** The documents that matched the search query *)
    val hits : t -> SearchResultHit.T.t list option
    
    (** Custom JSON object that can be returned in the search response *)
    val metadata : t -> Jsont.json option
    
    (** The total number of documents in the collection *)
    val out_of : t -> int option
    
    (** The search result page number *)
    val page : t -> int option
    
    val request_params : t -> SearchRequestParams.T.t option
    
    (** Whether the search was cut off *)
    val search_cutoff : t -> bool option
    
    (** The number of milliseconds the search took *)
    val search_time_ms : t -> int option
    
    (** Returned only for union query response. *)
    val union_request_params : t -> SearchRequestParams.T.t list option
    
    (** HTTP error code *)
    val code : t -> int64 option
    
    (** Error description *)
    val error : t -> string option
    
    val jsont : t Jsont.t
  end
end

module DropTokensMode : sig
  module T : sig
    (** Dictates the direction in which the words in the query must be dropped when the original words in the query do not appear in any document. Values: right_to_left (default), left_to_right, both_sides:3 A note on both_sides:3 - for queries up to 3 tokens (words) in length, this mode will drop tokens from both sides and exhaustively rank all matching results. If query length is greater than 3 words, Typesense will just fallback to default behavior of right_to_left
     *)
    type t = [
      | `Right_to_left
      | `Left_to_right
      | `Both_sides3
    ]
    
    val jsont : t Jsont.t
  end
end

module SearchParameters : sig
  module T : sig
    type t
    
    (** Construct a value
        @param enable_analytics Flag for enabling/disabling analytics aggregation for specific search queries (for e.g. those originating from a test script).
    
        @param enable_highlight_v1 Flag for enabling/disabling the deprecated, old highlight structure in the response. Default: true
    
        @param enable_overrides If you have some overrides defined but want to disable all of them during query time, you can do that by setting this parameter to false
    
        @param enable_typos_for_numerical_tokens Make Typesense disable typos for numerical tokens.
    
        @param prioritize_exact_match Set this parameter to true to ensure that an exact match is ranked above the others
    
        @param prioritize_num_matching_fields Make Typesense prioritize documents where the query words appear in more number of fields.
    
        @param prioritize_token_position Make Typesense prioritize documents where the query words appear earlier in the text.
    
        @param cache_ttl The duration (in seconds) that determines how long the search query is cached. This value can be set on a per-query basis. Default: 60.
    
        @param conversation Enable conversational search.
    
        @param conversation_id The Id of a previous conversation to continue, this tells Typesense to include prior context when communicating with the LLM.
    
        @param conversation_model_id The Id of Conversation Model to be used.
    
        @param drop_tokens_threshold If the number of results found for a specific query is less than this number, Typesense will attempt to drop the tokens in the query until enough results are found. Tokens that have the least individual hits are dropped first. Set to 0 to disable. Default: 10
    
        @param enable_synonyms If you have some synonyms defined but want to disable all of them for a particular search query, set enable_synonyms to false. Default: true
    
        @param enable_typos_for_alpha_numerical_tokens Set this parameter to false to disable typos on alphanumerical query tokens. Default: true.
    
        @param exclude_fields List of fields from the document to exclude in the search result
        @param exhaustive_search Setting this to true will make Typesense consider all prefixes and typo corrections of the words in the query without stopping early when enough results are found (drop_tokens_threshold and typo_tokens_threshold configurations are ignored).
    
        @param facet_by A list of fields that will be used for faceting your results on. Separate multiple fields with a comma.
        @param facet_query Facet values that are returned can now be filtered via this parameter. The matching facet text is also highlighted. For example, when faceting by `category`, you can set `facet_query=category:shoe` to return only facet values that contain the prefix "shoe".
        @param facet_return_parent Comma separated string of nested facet fields whose parent object should be returned in facet response.
    
        @param facet_strategy Choose the underlying faceting strategy used. Comma separated string of allows values: exhaustive, top_values or automatic (default).
    
        @param filter_by Filter conditions for refining your open api validator search results. Separate multiple conditions with &&.
        @param filter_curated_hits Whether the filter_by condition of the search query should be applicable to curated results (override definitions, pinned hits, hidden hits, etc.). Default: false
    
        @param group_by You can aggregate search results into groups or buckets by specify one or more `group_by` fields. Separate multiple fields with a comma. To group on a particular field, it must be a faceted field.
        @param group_limit Maximum number of hits to be returned for every group. If the `group_limit` is set as `K` then only the top K hits in each group are returned in the response. Default: 3
    
        @param group_missing_values Setting this parameter to true will place all documents that have a null value in the group_by field, into a single group. Setting this parameter to false, will cause each document with a null value in the group_by field to not be grouped with other documents. Default: true
    
        @param hidden_hits A list of records to unconditionally hide from search results. A list of `record_id`s to hide. Eg: to hide records with IDs 123 and 456, you'd specify `123,456`.
    You could also use the Overrides feature to override search results based on rules. Overrides are applied first, followed by `pinned_hits` and finally `hidden_hits`.
    
        @param highlight_affix_num_tokens The number of tokens that should surround the highlighted text on each side. Default: 4
    
        @param highlight_end_tag The end tag used for the highlighted snippets. Default: `</mark>`
    
        @param highlight_fields A list of custom fields that must be highlighted even if you don't query for them
    
        @param highlight_full_fields List of fields which should be highlighted fully without snippeting
        @param highlight_start_tag The start tag used for the highlighted snippets. Default: `<mark>`
    
        @param include_fields List of fields from the document to include in the search result
        @param infix If infix index is enabled for this field, infix searching can be done on a per-field basis by sending a comma separated string parameter called infix to the search query. This parameter can have 3 values; `off` infix search is disabled, which is default `always` infix search is performed along with regular search `fallback` infix search is performed if regular search does not produce results
        @param limit Number of hits to fetch. Can be used as an alternative to the per_page parameter. Default: 10.
    
        @param max_candidates Control the number of words that Typesense considers for typo and prefix searching.
    
        @param max_extra_prefix There are also 2 parameters that allow you to control the extent of infix searching max_extra_prefix and max_extra_suffix which specify the maximum number of symbols before or after the query that can be present in the token. For example query "K2100" has 2 extra symbols in "6PK2100". By default, any number of prefixes/suffixes can be present for a match.
        @param max_extra_suffix There are also 2 parameters that allow you to control the extent of infix searching max_extra_prefix and max_extra_suffix which specify the maximum number of symbols before or after the query that can be present in the token. For example query "K2100" has 2 extra symbols in "6PK2100". By default, any number of prefixes/suffixes can be present for a match.
        @param max_facet_values Maximum number of facet values to be returned.
        @param max_filter_by_candidates Controls the number of similar words that Typesense considers during fuzzy search on filter_by values. Useful for controlling prefix matches like company_name:Acm*.
        @param min_len_1typo Minimum word length for 1-typo correction to be applied. The value of num_typos is still treated as the maximum allowed typos.
    
        @param min_len_2typo Minimum word length for 2-typo correction to be applied. The value of num_typos is still treated as the maximum allowed typos.
    
        @param nl_model_id The ID of the natural language model to use.
        @param nl_query Whether to use natural language processing to parse the query.
        @param num_typos The number of typographical errors (1 or 2) that would be tolerated. Default: 2
    
        @param offset Identifies the starting point to return hits from a result set. Can be used as an alternative to the page parameter.
        @param override_tags Comma separated list of tags to trigger the curations rules that match the tags.
        @param page Results from this specific page number would be fetched.
        @param per_page Number of results to fetch per page. Default: 10
        @param pinned_hits A list of records to unconditionally include in the search results at specific positions. An example use case would be to feature or promote certain items on the top of search results. A list of `record_id:hit_position`. Eg: to include a record with ID 123 at Position 1 and another record with ID 456 at Position 5, you'd specify `123:1,456:5`.
    You could also use the Overrides feature to override search results based on rules. Overrides are applied first, followed by `pinned_hits` and finally `hidden_hits`.
    
        @param pre_segmented_query You can index content from any logographic language into Typesense if you are able to segment / split the text into space-separated words yourself before indexing and querying.
    Set this parameter to true to do the same
    
        @param prefix Boolean field to indicate that the last word in the query should be treated as a prefix, and not as a whole word. This is used for building autocomplete and instant search interfaces. Defaults to true.
        @param preset Search using a bunch of search parameters by setting this parameter to the name of the existing Preset.
    
        @param q The query text to search for in the collection. Use * as the search string to return all documents. This is typically useful when used in conjunction with filter_by.
        @param query_by A list of `string` fields that should be queried against. Multiple fields are separated with a comma.
        @param query_by_weights The relative weight to give each `query_by` field when ranking results. This can be used to boost fields in priority, when looking for matches. Multiple fields are separated with a comma.
        @param remote_embedding_num_tries Number of times to retry fetching remote embeddings.
    
        @param remote_embedding_timeout_ms Timeout (in milliseconds) for fetching remote embeddings.
    
        @param search_cutoff_ms Typesense will attempt to return results early if the cutoff time has elapsed. This is not a strict guarantee and facet computation is not bound by this parameter.
    
        @param snippet_threshold Field values under this length will be fully highlighted, instead of showing a snippet of relevant portion. Default: 30
    
        @param sort_by A list of numerical fields and their corresponding sort orders that will be used for ordering your results. Up to 3 sort fields can be specified. The text similarity score is exposed as a special `_text_match` field that you can use in the list of sorting fields. If no `sort_by` parameter is specified, results are sorted by `_text_match:desc,default_sorting_field:desc`
        @param split_join_tokens Treat space as typo: search for q=basket ball if q=basketball is not found or vice-versa. Splitting/joining of tokens will only be attempted if the original query produces no results. To always trigger this behavior, set value to `always``. To disable, set value to `off`. Default is `fallback`.
    
        @param stopwords Name of the stopwords set to apply for this search, the keywords present in the set will be removed from the search query.
    
        @param synonym_num_typos Allow synonym resolution on typo-corrected words in the query. Default: 0
    
        @param synonym_prefix Allow synonym resolution on word prefixes in the query. Default: false
    
        @param synonym_sets List of synonym set names to associate with this search query
        @param text_match_type In a multi-field matching context, this parameter determines how the representative text match score of a record is calculated. Possible values are max_score (default) or max_weight.
        @param typo_tokens_threshold If the number of results found for a specific query is less than this number, Typesense will attempt to look for tokens with more typos until enough results are found. Default: 100
    
        @param use_cache Enable server side caching of search query results. By default, caching is disabled.
    
        @param vector_query Vector query expression for fetching documents "closest" to a given query/document vector.
    
        @param voice_query The base64 encoded audio file in 16 khz 16-bit WAV format.
    
    *)
    val v : ?enable_analytics:bool -> ?enable_highlight_v1:bool -> ?enable_overrides:bool -> ?enable_typos_for_numerical_tokens:bool -> ?prioritize_exact_match:bool -> ?prioritize_num_matching_fields:bool -> ?prioritize_token_position:bool -> ?cache_ttl:int -> ?conversation:bool -> ?conversation_id:string -> ?conversation_model_id:string -> ?drop_tokens_mode:DropTokensMode.T.t -> ?drop_tokens_threshold:int -> ?enable_synonyms:bool -> ?enable_typos_for_alpha_numerical_tokens:bool -> ?exclude_fields:string -> ?exhaustive_search:bool -> ?facet_by:string -> ?facet_query:string -> ?facet_return_parent:string -> ?facet_strategy:string -> ?filter_by:string -> ?filter_curated_hits:bool -> ?group_by:string -> ?group_limit:int -> ?group_missing_values:bool -> ?hidden_hits:string -> ?highlight_affix_num_tokens:int -> ?highlight_end_tag:string -> ?highlight_fields:string -> ?highlight_full_fields:string -> ?highlight_start_tag:string -> ?include_fields:string -> ?infix:string -> ?limit:int -> ?max_candidates:int -> ?max_extra_prefix:int -> ?max_extra_suffix:int -> ?max_facet_values:int -> ?max_filter_by_candidates:int -> ?min_len_1typo:int -> ?min_len_2typo:int -> ?nl_model_id:string -> ?nl_query:bool -> ?num_typos:string -> ?offset:int -> ?override_tags:string -> ?page:int -> ?per_page:int -> ?pinned_hits:string -> ?pre_segmented_query:bool -> ?prefix:string -> ?preset:string -> ?q:string -> ?query_by:string -> ?query_by_weights:string -> ?remote_embedding_num_tries:int -> ?remote_embedding_timeout_ms:int -> ?search_cutoff_ms:int -> ?snippet_threshold:int -> ?sort_by:string -> ?split_join_tokens:string -> ?stopwords:string -> ?synonym_num_typos:int -> ?synonym_prefix:bool -> ?synonym_sets:string -> ?text_match_type:string -> ?typo_tokens_threshold:int -> ?use_cache:bool -> ?vector_query:string -> ?voice_query:string -> unit -> t
    
    (** The duration (in seconds) that determines how long the search query is cached. This value can be set on a per-query basis. Default: 60.
     *)
    val cache_ttl : t -> int option
    
    (** Enable conversational search.
     *)
    val conversation : t -> bool option
    
    (** The Id of a previous conversation to continue, this tells Typesense to include prior context when communicating with the LLM.
     *)
    val conversation_id : t -> string option
    
    (** The Id of Conversation Model to be used.
     *)
    val conversation_model_id : t -> string option
    
    val drop_tokens_mode : t -> DropTokensMode.T.t option
    
    (** If the number of results found for a specific query is less than this number, Typesense will attempt to drop the tokens in the query until enough results are found. Tokens that have the least individual hits are dropped first. Set to 0 to disable. Default: 10
     *)
    val drop_tokens_threshold : t -> int option
    
    (** Flag for enabling/disabling analytics aggregation for specific search queries (for e.g. those originating from a test script).
     *)
    val enable_analytics : t -> bool
    
    (** Flag for enabling/disabling the deprecated, old highlight structure in the response. Default: true
     *)
    val enable_highlight_v1 : t -> bool
    
    (** If you have some overrides defined but want to disable all of them during query time, you can do that by setting this parameter to false
     *)
    val enable_overrides : t -> bool
    
    (** If you have some synonyms defined but want to disable all of them for a particular search query, set enable_synonyms to false. Default: true
     *)
    val enable_synonyms : t -> bool option
    
    (** Set this parameter to false to disable typos on alphanumerical query tokens. Default: true.
     *)
    val enable_typos_for_alpha_numerical_tokens : t -> bool option
    
    (** Make Typesense disable typos for numerical tokens.
     *)
    val enable_typos_for_numerical_tokens : t -> bool
    
    (** List of fields from the document to exclude in the search result *)
    val exclude_fields : t -> string option
    
    (** Setting this to true will make Typesense consider all prefixes and typo corrections of the words in the query without stopping early when enough results are found (drop_tokens_threshold and typo_tokens_threshold configurations are ignored).
     *)
    val exhaustive_search : t -> bool option
    
    (** A list of fields that will be used for faceting your results on. Separate multiple fields with a comma. *)
    val facet_by : t -> string option
    
    (** Facet values that are returned can now be filtered via this parameter. The matching facet text is also highlighted. For example, when faceting by `category`, you can set `facet_query=category:shoe` to return only facet values that contain the prefix "shoe". *)
    val facet_query : t -> string option
    
    (** Comma separated string of nested facet fields whose parent object should be returned in facet response.
     *)
    val facet_return_parent : t -> string option
    
    (** Choose the underlying faceting strategy used. Comma separated string of allows values: exhaustive, top_values or automatic (default).
     *)
    val facet_strategy : t -> string option
    
    (** Filter conditions for refining your open api validator search results. Separate multiple conditions with &&. *)
    val filter_by : t -> string option
    
    (** Whether the filter_by condition of the search query should be applicable to curated results (override definitions, pinned hits, hidden hits, etc.). Default: false
     *)
    val filter_curated_hits : t -> bool option
    
    (** You can aggregate search results into groups or buckets by specify one or more `group_by` fields. Separate multiple fields with a comma. To group on a particular field, it must be a faceted field. *)
    val group_by : t -> string option
    
    (** Maximum number of hits to be returned for every group. If the `group_limit` is set as `K` then only the top K hits in each group are returned in the response. Default: 3
     *)
    val group_limit : t -> int option
    
    (** Setting this parameter to true will place all documents that have a null value in the group_by field, into a single group. Setting this parameter to false, will cause each document with a null value in the group_by field to not be grouped with other documents. Default: true
     *)
    val group_missing_values : t -> bool option
    
    (** A list of records to unconditionally hide from search results. A list of `record_id`s to hide. Eg: to hide records with IDs 123 and 456, you'd specify `123,456`.
    You could also use the Overrides feature to override search results based on rules. Overrides are applied first, followed by `pinned_hits` and finally `hidden_hits`.
     *)
    val hidden_hits : t -> string option
    
    (** The number of tokens that should surround the highlighted text on each side. Default: 4
     *)
    val highlight_affix_num_tokens : t -> int option
    
    (** The end tag used for the highlighted snippets. Default: `</mark>`
     *)
    val highlight_end_tag : t -> string option
    
    (** A list of custom fields that must be highlighted even if you don't query for them
     *)
    val highlight_fields : t -> string option
    
    (** List of fields which should be highlighted fully without snippeting *)
    val highlight_full_fields : t -> string option
    
    (** The start tag used for the highlighted snippets. Default: `<mark>`
     *)
    val highlight_start_tag : t -> string option
    
    (** List of fields from the document to include in the search result *)
    val include_fields : t -> string option
    
    (** If infix index is enabled for this field, infix searching can be done on a per-field basis by sending a comma separated string parameter called infix to the search query. This parameter can have 3 values; `off` infix search is disabled, which is default `always` infix search is performed along with regular search `fallback` infix search is performed if regular search does not produce results *)
    val infix : t -> string option
    
    (** Number of hits to fetch. Can be used as an alternative to the per_page parameter. Default: 10.
     *)
    val limit : t -> int option
    
    (** Control the number of words that Typesense considers for typo and prefix searching.
     *)
    val max_candidates : t -> int option
    
    (** There are also 2 parameters that allow you to control the extent of infix searching max_extra_prefix and max_extra_suffix which specify the maximum number of symbols before or after the query that can be present in the token. For example query "K2100" has 2 extra symbols in "6PK2100". By default, any number of prefixes/suffixes can be present for a match. *)
    val max_extra_prefix : t -> int option
    
    (** There are also 2 parameters that allow you to control the extent of infix searching max_extra_prefix and max_extra_suffix which specify the maximum number of symbols before or after the query that can be present in the token. For example query "K2100" has 2 extra symbols in "6PK2100". By default, any number of prefixes/suffixes can be present for a match. *)
    val max_extra_suffix : t -> int option
    
    (** Maximum number of facet values to be returned. *)
    val max_facet_values : t -> int option
    
    (** Controls the number of similar words that Typesense considers during fuzzy search on filter_by values. Useful for controlling prefix matches like company_name:Acm*. *)
    val max_filter_by_candidates : t -> int option
    
    (** Minimum word length for 1-typo correction to be applied. The value of num_typos is still treated as the maximum allowed typos.
     *)
    val min_len_1typo : t -> int option
    
    (** Minimum word length for 2-typo correction to be applied. The value of num_typos is still treated as the maximum allowed typos.
     *)
    val min_len_2typo : t -> int option
    
    (** The ID of the natural language model to use. *)
    val nl_model_id : t -> string option
    
    (** Whether to use natural language processing to parse the query. *)
    val nl_query : t -> bool option
    
    (** The number of typographical errors (1 or 2) that would be tolerated. Default: 2
     *)
    val num_typos : t -> string option
    
    (** Identifies the starting point to return hits from a result set. Can be used as an alternative to the page parameter. *)
    val offset : t -> int option
    
    (** Comma separated list of tags to trigger the curations rules that match the tags. *)
    val override_tags : t -> string option
    
    (** Results from this specific page number would be fetched. *)
    val page : t -> int option
    
    (** Number of results to fetch per page. Default: 10 *)
    val per_page : t -> int option
    
    (** A list of records to unconditionally include in the search results at specific positions. An example use case would be to feature or promote certain items on the top of search results. A list of `record_id:hit_position`. Eg: to include a record with ID 123 at Position 1 and another record with ID 456 at Position 5, you'd specify `123:1,456:5`.
    You could also use the Overrides feature to override search results based on rules. Overrides are applied first, followed by `pinned_hits` and finally `hidden_hits`.
     *)
    val pinned_hits : t -> string option
    
    (** You can index content from any logographic language into Typesense if you are able to segment / split the text into space-separated words yourself before indexing and querying.
    Set this parameter to true to do the same
     *)
    val pre_segmented_query : t -> bool option
    
    (** Boolean field to indicate that the last word in the query should be treated as a prefix, and not as a whole word. This is used for building autocomplete and instant search interfaces. Defaults to true. *)
    val prefix : t -> string option
    
    (** Search using a bunch of search parameters by setting this parameter to the name of the existing Preset.
     *)
    val preset : t -> string option
    
    (** Set this parameter to true to ensure that an exact match is ranked above the others
     *)
    val prioritize_exact_match : t -> bool
    
    (** Make Typesense prioritize documents where the query words appear in more number of fields.
     *)
    val prioritize_num_matching_fields : t -> bool
    
    (** Make Typesense prioritize documents where the query words appear earlier in the text.
     *)
    val prioritize_token_position : t -> bool
    
    (** The query text to search for in the collection. Use * as the search string to return all documents. This is typically useful when used in conjunction with filter_by. *)
    val q : t -> string option
    
    (** A list of `string` fields that should be queried against. Multiple fields are separated with a comma. *)
    val query_by : t -> string option
    
    (** The relative weight to give each `query_by` field when ranking results. This can be used to boost fields in priority, when looking for matches. Multiple fields are separated with a comma. *)
    val query_by_weights : t -> string option
    
    (** Number of times to retry fetching remote embeddings.
     *)
    val remote_embedding_num_tries : t -> int option
    
    (** Timeout (in milliseconds) for fetching remote embeddings.
     *)
    val remote_embedding_timeout_ms : t -> int option
    
    (** Typesense will attempt to return results early if the cutoff time has elapsed. This is not a strict guarantee and facet computation is not bound by this parameter.
     *)
    val search_cutoff_ms : t -> int option
    
    (** Field values under this length will be fully highlighted, instead of showing a snippet of relevant portion. Default: 30
     *)
    val snippet_threshold : t -> int option
    
    (** A list of numerical fields and their corresponding sort orders that will be used for ordering your results. Up to 3 sort fields can be specified. The text similarity score is exposed as a special `_text_match` field that you can use in the list of sorting fields. If no `sort_by` parameter is specified, results are sorted by `_text_match:desc,default_sorting_field:desc` *)
    val sort_by : t -> string option
    
    (** Treat space as typo: search for q=basket ball if q=basketball is not found or vice-versa. Splitting/joining of tokens will only be attempted if the original query produces no results. To always trigger this behavior, set value to `always``. To disable, set value to `off`. Default is `fallback`.
     *)
    val split_join_tokens : t -> string option
    
    (** Name of the stopwords set to apply for this search, the keywords present in the set will be removed from the search query.
     *)
    val stopwords : t -> string option
    
    (** Allow synonym resolution on typo-corrected words in the query. Default: 0
     *)
    val synonym_num_typos : t -> int option
    
    (** Allow synonym resolution on word prefixes in the query. Default: false
     *)
    val synonym_prefix : t -> bool option
    
    (** List of synonym set names to associate with this search query *)
    val synonym_sets : t -> string option
    
    (** In a multi-field matching context, this parameter determines how the representative text match score of a record is calculated. Possible values are max_score (default) or max_weight. *)
    val text_match_type : t -> string option
    
    (** If the number of results found for a specific query is less than this number, Typesense will attempt to look for tokens with more typos until enough results are found. Default: 100
     *)
    val typo_tokens_threshold : t -> int option
    
    (** Enable server side caching of search query results. By default, caching is disabled.
     *)
    val use_cache : t -> bool option
    
    (** Vector query expression for fetching documents "closest" to a given query/document vector.
     *)
    val vector_query : t -> string option
    
    (** The base64 encoded audio file in 16 khz 16-bit WAV format.
     *)
    val voice_query : t -> string option
    
    val jsont : t Jsont.t
  end
end

module MultiSearchParameters : sig
  module T : sig
    (** Parameters for the multi search API.
     *)
    type t
    
    (** Construct a value
        @param enable_analytics Flag for enabling/disabling analytics aggregation for specific search queries (for e.g. those originating from a test script).
    
        @param enable_overrides If you have some overrides defined but want to disable all of them during query time, you can do that by setting this parameter to false
    
        @param enable_typos_for_numerical_tokens Make Typesense disable typos for numerical tokens.
    
        @param pre_segmented_query You can index content from any logographic language into Typesense if you are able to segment / split the text into space-separated words yourself before indexing and querying.
    Set this parameter to true to do the same
    
        @param prioritize_exact_match Set this parameter to true to ensure that an exact match is ranked above the others
    
        @param prioritize_num_matching_fields Make Typesense prioritize documents where the query words appear in more number of fields.
    
        @param prioritize_token_position Make Typesense prioritize documents where the query words appear earlier in the text.
    
        @param cache_ttl The duration (in seconds) that determines how long the search query is cached. This value can be set on a per-query basis. Default: 60.
    
        @param conversation Enable conversational search.
    
        @param conversation_id The Id of a previous conversation to continue, this tells Typesense to include prior context when communicating with the LLM.
    
        @param conversation_model_id The Id of Conversation Model to be used.
    
        @param drop_tokens_threshold If the number of results found for a specific query is less than this number, Typesense will attempt to drop the tokens in the query until enough results are found. Tokens that have the least individual hits are dropped first. Set to 0 to disable. Default: 10
    
        @param enable_synonyms If you have some synonyms defined but want to disable all of them for a particular search query, set enable_synonyms to false. Default: true
    
        @param enable_typos_for_alpha_numerical_tokens Set this parameter to false to disable typos on alphanumerical query tokens. Default: true.
    
        @param exclude_fields List of fields from the document to exclude in the search result
        @param exhaustive_search Setting this to true will make Typesense consider all prefixes and typo corrections of the words in the query without stopping early when enough results are found (drop_tokens_threshold and typo_tokens_threshold configurations are ignored).
    
        @param facet_by A list of fields that will be used for faceting your results on. Separate multiple fields with a comma.
        @param facet_query Facet values that are returned can now be filtered via this parameter. The matching facet text is also highlighted. For example, when faceting by `category`, you can set `facet_query=category:shoe` to return only facet values that contain the prefix "shoe".
        @param facet_return_parent Comma separated string of nested facet fields whose parent object should be returned in facet response.
    
        @param facet_strategy Choose the underlying faceting strategy used. Comma separated string of allows values: exhaustive, top_values or automatic (default).
    
        @param filter_by Filter conditions for refining youropen api validator search results. Separate multiple conditions with &&.
        @param filter_curated_hits Whether the filter_by condition of the search query should be applicable to curated results (override definitions, pinned hits, hidden hits, etc.). Default: false
    
        @param group_by You can aggregate search results into groups or buckets by specify one or more `group_by` fields. Separate multiple fields with a comma. To group on a particular field, it must be a faceted field.
        @param group_limit Maximum number of hits to be returned for every group. If the `group_limit` is set as `K` then only the top K hits in each group are returned in the response. Default: 3
    
        @param group_missing_values Setting this parameter to true will place all documents that have a null value in the group_by field, into a single group. Setting this parameter to false, will cause each document with a null value in the group_by field to not be grouped with other documents. Default: true
    
        @param hidden_hits A list of records to unconditionally hide from search results. A list of `record_id`s to hide. Eg: to hide records with IDs 123 and 456, you'd specify `123,456`.
    You could also use the Overrides feature to override search results based on rules. Overrides are applied first, followed by `pinned_hits` and finally `hidden_hits`.
    
        @param highlight_affix_num_tokens The number of tokens that should surround the highlighted text on each side. Default: 4
    
        @param highlight_end_tag The end tag used for the highlighted snippets. Default: `</mark>`
    
        @param highlight_fields A list of custom fields that must be highlighted even if you don't query for them
    
        @param highlight_full_fields List of fields which should be highlighted fully without snippeting
        @param highlight_start_tag The start tag used for the highlighted snippets. Default: `<mark>`
    
        @param include_fields List of fields from the document to include in the search result
        @param infix If infix index is enabled for this field, infix searching can be done on a per-field basis by sending a comma separated string parameter called infix to the search query. This parameter can have 3 values; `off` infix search is disabled, which is default `always` infix search is performed along with regular search `fallback` infix search is performed if regular search does not produce results
        @param limit Number of hits to fetch. Can be used as an alternative to the per_page parameter. Default: 10.
    
        @param max_extra_prefix There are also 2 parameters that allow you to control the extent of infix searching max_extra_prefix and max_extra_suffix which specify the maximum number of symbols before or after the query that can be present in the token. For example query "K2100" has 2 extra symbols in "6PK2100". By default, any number of prefixes/suffixes can be present for a match.
        @param max_extra_suffix There are also 2 parameters that allow you to control the extent of infix searching max_extra_prefix and max_extra_suffix which specify the maximum number of symbols before or after the query that can be present in the token. For example query "K2100" has 2 extra symbols in "6PK2100". By default, any number of prefixes/suffixes can be present for a match.
        @param max_facet_values Maximum number of facet values to be returned.
        @param min_len_1typo Minimum word length for 1-typo correction to be applied. The value of num_typos is still treated as the maximum allowed typos.
    
        @param min_len_2typo Minimum word length for 2-typo correction to be applied. The value of num_typos is still treated as the maximum allowed typos.
    
        @param num_typos The number of typographical errors (1 or 2) that would be tolerated. Default: 2
    
        @param offset Identifies the starting point to return hits from a result set. Can be used as an alternative to the page parameter.
        @param override_tags Comma separated list of tags to trigger the curations rules that match the tags.
        @param page Results from this specific page number would be fetched.
        @param per_page Number of results to fetch per page. Default: 10
        @param pinned_hits A list of records to unconditionally include in the search results at specific positions. An example use case would be to feature or promote certain items on the top of search results. A list of `record_id:hit_position`. Eg: to include a record with ID 123 at Position 1 and another record with ID 456 at Position 5, you'd specify `123:1,456:5`.
    You could also use the Overrides feature to override search results based on rules. Overrides are applied first, followed by `pinned_hits` and finally `hidden_hits`.
    
        @param prefix Boolean field to indicate that the last word in the query should be treated as a prefix, and not as a whole word. This is used for building autocomplete and instant search interfaces. Defaults to true.
        @param preset Search using a bunch of search parameters by setting this parameter to the name of the existing Preset.
    
        @param q The query text to search for in the collection. Use * as the search string to return all documents. This is typically useful when used in conjunction with filter_by.
        @param query_by A list of `string` fields that should be queried against. Multiple fields are separated with a comma.
        @param query_by_weights The relative weight to give each `query_by` field when ranking results. This can be used to boost fields in priority, when looking for matches. Multiple fields are separated with a comma.
        @param remote_embedding_num_tries Number of times to retry fetching remote embeddings.
    
        @param remote_embedding_timeout_ms Timeout (in milliseconds) for fetching remote embeddings.
    
        @param search_cutoff_ms Typesense will attempt to return results early if the cutoff time has elapsed. This is not a strict guarantee and facet computation is not bound by this parameter.
    
        @param snippet_threshold Field values under this length will be fully highlighted, instead of showing a snippet of relevant portion. Default: 30
    
        @param sort_by A list of numerical fields and their corresponding sort orders that will be used for ordering your results. Up to 3 sort fields can be specified. The text similarity score is exposed as a special `_text_match` field that you can use in the list of sorting fields. If no `sort_by` parameter is specified, results are sorted by `_text_match:desc,default_sorting_field:desc`
        @param stopwords Name of the stopwords set to apply for this search, the keywords present in the set will be removed from the search query.
    
        @param synonym_num_typos Allow synonym resolution on typo-corrected words in the query. Default: 0
    
        @param synonym_prefix Allow synonym resolution on word prefixes in the query. Default: false
    
        @param text_match_type In a multi-field matching context, this parameter determines how the representative text match score of a record is calculated. Possible values are max_score (default) or max_weight.
        @param typo_tokens_threshold If the number of results found for a specific query is less than this number, Typesense will attempt to look for tokens with more typos until enough results are found. Default: 100
    
        @param use_cache Enable server side caching of search query results. By default, caching is disabled.
    
        @param vector_query Vector query expression for fetching documents "closest" to a given query/document vector.
    
        @param voice_query The base64 encoded audio file in 16 khz 16-bit WAV format.
    
    *)
    val v : ?enable_analytics:bool -> ?enable_overrides:bool -> ?enable_typos_for_numerical_tokens:bool -> ?pre_segmented_query:bool -> ?prioritize_exact_match:bool -> ?prioritize_num_matching_fields:bool -> ?prioritize_token_position:bool -> ?cache_ttl:int -> ?conversation:bool -> ?conversation_id:string -> ?conversation_model_id:string -> ?drop_tokens_mode:DropTokensMode.T.t -> ?drop_tokens_threshold:int -> ?enable_synonyms:bool -> ?enable_typos_for_alpha_numerical_tokens:bool -> ?exclude_fields:string -> ?exhaustive_search:bool -> ?facet_by:string -> ?facet_query:string -> ?facet_return_parent:string -> ?facet_strategy:string -> ?filter_by:string -> ?filter_curated_hits:bool -> ?group_by:string -> ?group_limit:int -> ?group_missing_values:bool -> ?hidden_hits:string -> ?highlight_affix_num_tokens:int -> ?highlight_end_tag:string -> ?highlight_fields:string -> ?highlight_full_fields:string -> ?highlight_start_tag:string -> ?include_fields:string -> ?infix:string -> ?limit:int -> ?max_extra_prefix:int -> ?max_extra_suffix:int -> ?max_facet_values:int -> ?min_len_1typo:int -> ?min_len_2typo:int -> ?num_typos:string -> ?offset:int -> ?override_tags:string -> ?page:int -> ?per_page:int -> ?pinned_hits:string -> ?prefix:string -> ?preset:string -> ?q:string -> ?query_by:string -> ?query_by_weights:string -> ?remote_embedding_num_tries:int -> ?remote_embedding_timeout_ms:int -> ?search_cutoff_ms:int -> ?snippet_threshold:int -> ?sort_by:string -> ?stopwords:string -> ?synonym_num_typos:int -> ?synonym_prefix:bool -> ?text_match_type:string -> ?typo_tokens_threshold:int -> ?use_cache:bool -> ?vector_query:string -> ?voice_query:string -> unit -> t
    
    (** The duration (in seconds) that determines how long the search query is cached. This value can be set on a per-query basis. Default: 60.
     *)
    val cache_ttl : t -> int option
    
    (** Enable conversational search.
     *)
    val conversation : t -> bool option
    
    (** The Id of a previous conversation to continue, this tells Typesense to include prior context when communicating with the LLM.
     *)
    val conversation_id : t -> string option
    
    (** The Id of Conversation Model to be used.
     *)
    val conversation_model_id : t -> string option
    
    val drop_tokens_mode : t -> DropTokensMode.T.t option
    
    (** If the number of results found for a specific query is less than this number, Typesense will attempt to drop the tokens in the query until enough results are found. Tokens that have the least individual hits are dropped first. Set to 0 to disable. Default: 10
     *)
    val drop_tokens_threshold : t -> int option
    
    (** Flag for enabling/disabling analytics aggregation for specific search queries (for e.g. those originating from a test script).
     *)
    val enable_analytics : t -> bool
    
    (** If you have some overrides defined but want to disable all of them during query time, you can do that by setting this parameter to false
     *)
    val enable_overrides : t -> bool
    
    (** If you have some synonyms defined but want to disable all of them for a particular search query, set enable_synonyms to false. Default: true
     *)
    val enable_synonyms : t -> bool option
    
    (** Set this parameter to false to disable typos on alphanumerical query tokens. Default: true.
     *)
    val enable_typos_for_alpha_numerical_tokens : t -> bool option
    
    (** Make Typesense disable typos for numerical tokens.
     *)
    val enable_typos_for_numerical_tokens : t -> bool
    
    (** List of fields from the document to exclude in the search result *)
    val exclude_fields : t -> string option
    
    (** Setting this to true will make Typesense consider all prefixes and typo corrections of the words in the query without stopping early when enough results are found (drop_tokens_threshold and typo_tokens_threshold configurations are ignored).
     *)
    val exhaustive_search : t -> bool option
    
    (** A list of fields that will be used for faceting your results on. Separate multiple fields with a comma. *)
    val facet_by : t -> string option
    
    (** Facet values that are returned can now be filtered via this parameter. The matching facet text is also highlighted. For example, when faceting by `category`, you can set `facet_query=category:shoe` to return only facet values that contain the prefix "shoe". *)
    val facet_query : t -> string option
    
    (** Comma separated string of nested facet fields whose parent object should be returned in facet response.
     *)
    val facet_return_parent : t -> string option
    
    (** Choose the underlying faceting strategy used. Comma separated string of allows values: exhaustive, top_values or automatic (default).
     *)
    val facet_strategy : t -> string option
    
    (** Filter conditions for refining youropen api validator search results. Separate multiple conditions with &&. *)
    val filter_by : t -> string option
    
    (** Whether the filter_by condition of the search query should be applicable to curated results (override definitions, pinned hits, hidden hits, etc.). Default: false
     *)
    val filter_curated_hits : t -> bool option
    
    (** You can aggregate search results into groups or buckets by specify one or more `group_by` fields. Separate multiple fields with a comma. To group on a particular field, it must be a faceted field. *)
    val group_by : t -> string option
    
    (** Maximum number of hits to be returned for every group. If the `group_limit` is set as `K` then only the top K hits in each group are returned in the response. Default: 3
     *)
    val group_limit : t -> int option
    
    (** Setting this parameter to true will place all documents that have a null value in the group_by field, into a single group. Setting this parameter to false, will cause each document with a null value in the group_by field to not be grouped with other documents. Default: true
     *)
    val group_missing_values : t -> bool option
    
    (** A list of records to unconditionally hide from search results. A list of `record_id`s to hide. Eg: to hide records with IDs 123 and 456, you'd specify `123,456`.
    You could also use the Overrides feature to override search results based on rules. Overrides are applied first, followed by `pinned_hits` and finally `hidden_hits`.
     *)
    val hidden_hits : t -> string option
    
    (** The number of tokens that should surround the highlighted text on each side. Default: 4
     *)
    val highlight_affix_num_tokens : t -> int option
    
    (** The end tag used for the highlighted snippets. Default: `</mark>`
     *)
    val highlight_end_tag : t -> string option
    
    (** A list of custom fields that must be highlighted even if you don't query for them
     *)
    val highlight_fields : t -> string option
    
    (** List of fields which should be highlighted fully without snippeting *)
    val highlight_full_fields : t -> string option
    
    (** The start tag used for the highlighted snippets. Default: `<mark>`
     *)
    val highlight_start_tag : t -> string option
    
    (** List of fields from the document to include in the search result *)
    val include_fields : t -> string option
    
    (** If infix index is enabled for this field, infix searching can be done on a per-field basis by sending a comma separated string parameter called infix to the search query. This parameter can have 3 values; `off` infix search is disabled, which is default `always` infix search is performed along with regular search `fallback` infix search is performed if regular search does not produce results *)
    val infix : t -> string option
    
    (** Number of hits to fetch. Can be used as an alternative to the per_page parameter. Default: 10.
     *)
    val limit : t -> int option
    
    (** There are also 2 parameters that allow you to control the extent of infix searching max_extra_prefix and max_extra_suffix which specify the maximum number of symbols before or after the query that can be present in the token. For example query "K2100" has 2 extra symbols in "6PK2100". By default, any number of prefixes/suffixes can be present for a match. *)
    val max_extra_prefix : t -> int option
    
    (** There are also 2 parameters that allow you to control the extent of infix searching max_extra_prefix and max_extra_suffix which specify the maximum number of symbols before or after the query that can be present in the token. For example query "K2100" has 2 extra symbols in "6PK2100". By default, any number of prefixes/suffixes can be present for a match. *)
    val max_extra_suffix : t -> int option
    
    (** Maximum number of facet values to be returned. *)
    val max_facet_values : t -> int option
    
    (** Minimum word length for 1-typo correction to be applied. The value of num_typos is still treated as the maximum allowed typos.
     *)
    val min_len_1typo : t -> int option
    
    (** Minimum word length for 2-typo correction to be applied. The value of num_typos is still treated as the maximum allowed typos.
     *)
    val min_len_2typo : t -> int option
    
    (** The number of typographical errors (1 or 2) that would be tolerated. Default: 2
     *)
    val num_typos : t -> string option
    
    (** Identifies the starting point to return hits from a result set. Can be used as an alternative to the page parameter. *)
    val offset : t -> int option
    
    (** Comma separated list of tags to trigger the curations rules that match the tags. *)
    val override_tags : t -> string option
    
    (** Results from this specific page number would be fetched. *)
    val page : t -> int option
    
    (** Number of results to fetch per page. Default: 10 *)
    val per_page : t -> int option
    
    (** A list of records to unconditionally include in the search results at specific positions. An example use case would be to feature or promote certain items on the top of search results. A list of `record_id:hit_position`. Eg: to include a record with ID 123 at Position 1 and another record with ID 456 at Position 5, you'd specify `123:1,456:5`.
    You could also use the Overrides feature to override search results based on rules. Overrides are applied first, followed by `pinned_hits` and finally `hidden_hits`.
     *)
    val pinned_hits : t -> string option
    
    (** You can index content from any logographic language into Typesense if you are able to segment / split the text into space-separated words yourself before indexing and querying.
    Set this parameter to true to do the same
     *)
    val pre_segmented_query : t -> bool
    
    (** Boolean field to indicate that the last word in the query should be treated as a prefix, and not as a whole word. This is used for building autocomplete and instant search interfaces. Defaults to true. *)
    val prefix : t -> string option
    
    (** Search using a bunch of search parameters by setting this parameter to the name of the existing Preset.
     *)
    val preset : t -> string option
    
    (** Set this parameter to true to ensure that an exact match is ranked above the others
     *)
    val prioritize_exact_match : t -> bool
    
    (** Make Typesense prioritize documents where the query words appear in more number of fields.
     *)
    val prioritize_num_matching_fields : t -> bool
    
    (** Make Typesense prioritize documents where the query words appear earlier in the text.
     *)
    val prioritize_token_position : t -> bool
    
    (** The query text to search for in the collection. Use * as the search string to return all documents. This is typically useful when used in conjunction with filter_by. *)
    val q : t -> string option
    
    (** A list of `string` fields that should be queried against. Multiple fields are separated with a comma. *)
    val query_by : t -> string option
    
    (** The relative weight to give each `query_by` field when ranking results. This can be used to boost fields in priority, when looking for matches. Multiple fields are separated with a comma. *)
    val query_by_weights : t -> string option
    
    (** Number of times to retry fetching remote embeddings.
     *)
    val remote_embedding_num_tries : t -> int option
    
    (** Timeout (in milliseconds) for fetching remote embeddings.
     *)
    val remote_embedding_timeout_ms : t -> int option
    
    (** Typesense will attempt to return results early if the cutoff time has elapsed. This is not a strict guarantee and facet computation is not bound by this parameter.
     *)
    val search_cutoff_ms : t -> int option
    
    (** Field values under this length will be fully highlighted, instead of showing a snippet of relevant portion. Default: 30
     *)
    val snippet_threshold : t -> int option
    
    (** A list of numerical fields and their corresponding sort orders that will be used for ordering your results. Up to 3 sort fields can be specified. The text similarity score is exposed as a special `_text_match` field that you can use in the list of sorting fields. If no `sort_by` parameter is specified, results are sorted by `_text_match:desc,default_sorting_field:desc` *)
    val sort_by : t -> string option
    
    (** Name of the stopwords set to apply for this search, the keywords present in the set will be removed from the search query.
     *)
    val stopwords : t -> string option
    
    (** Allow synonym resolution on typo-corrected words in the query. Default: 0
     *)
    val synonym_num_typos : t -> int option
    
    (** Allow synonym resolution on word prefixes in the query. Default: false
     *)
    val synonym_prefix : t -> bool option
    
    (** In a multi-field matching context, this parameter determines how the representative text match score of a record is calculated. Possible values are max_score (default) or max_weight. *)
    val text_match_type : t -> string option
    
    (** If the number of results found for a specific query is less than this number, Typesense will attempt to look for tokens with more typos until enough results are found. Default: 100
     *)
    val typo_tokens_threshold : t -> int option
    
    (** Enable server side caching of search query results. By default, caching is disabled.
     *)
    val use_cache : t -> bool option
    
    (** Vector query expression for fetching documents "closest" to a given query/document vector.
     *)
    val vector_query : t -> string option
    
    (** The base64 encoded audio file in 16 khz 16-bit WAV format.
     *)
    val voice_query : t -> string option
    
    val jsont : t Jsont.t
  end
end

module MultiSearchCollectionParameters : sig
  module T : sig
    type t
    
    (** Construct a value
        @param enable_analytics Flag for enabling/disabling analytics aggregation for specific search queries (for e.g. those originating from a test script).
    
        @param enable_overrides If you have some overrides defined but want to disable all of them during query time, you can do that by setting this parameter to false
    
        @param enable_typos_for_numerical_tokens Make Typesense disable typos for numerical tokens.
    
        @param pre_segmented_query You can index content from any logographic language into Typesense if you are able to segment / split the text into space-separated words yourself before indexing and querying.
    Set this parameter to true to do the same
    
        @param prioritize_exact_match Set this parameter to true to ensure that an exact match is ranked above the others
    
        @param prioritize_num_matching_fields Make Typesense prioritize documents where the query words appear in more number of fields.
    
        @param prioritize_token_position Make Typesense prioritize documents where the query words appear earlier in the text.
    
        @param rerank_hybrid_matches When true, computes both text match and vector distance scores for all matches in hybrid search. Documents found only through keyword search will get a vector distance score, and documents found only through vector search will get a text match score.
    
        @param cache_ttl The duration (in seconds) that determines how long the search query is cached. This value can be set on a per-query basis. Default: 60.
    
        @param conversation Enable conversational search.
    
        @param conversation_id The Id of a previous conversation to continue, this tells Typesense to include prior context when communicating with the LLM.
    
        @param conversation_model_id The Id of Conversation Model to be used.
    
        @param drop_tokens_threshold If the number of results found for a specific query is less than this number, Typesense will attempt to drop the tokens in the query until enough results are found. Tokens that have the least individual hits are dropped first. Set to 0 to disable. Default: 10
    
        @param enable_synonyms If you have some synonyms defined but want to disable all of them for a particular search query, set enable_synonyms to false. Default: true
    
        @param enable_typos_for_alpha_numerical_tokens Set this parameter to false to disable typos on alphanumerical query tokens. Default: true.
    
        @param exclude_fields List of fields from the document to exclude in the search result
        @param exhaustive_search Setting this to true will make Typesense consider all prefixes and typo corrections of the words in the query without stopping early when enough results are found (drop_tokens_threshold and typo_tokens_threshold configurations are ignored).
    
        @param facet_by A list of fields that will be used for faceting your results on. Separate multiple fields with a comma.
        @param facet_query Facet values that are returned can now be filtered via this parameter. The matching facet text is also highlighted. For example, when faceting by `category`, you can set `facet_query=category:shoe` to return only facet values that contain the prefix "shoe".
        @param facet_return_parent Comma separated string of nested facet fields whose parent object should be returned in facet response.
    
        @param facet_strategy Choose the underlying faceting strategy used. Comma separated string of allows values: exhaustive, top_values or automatic (default).
    
        @param filter_by Filter conditions for refining youropen api validator search results. Separate multiple conditions with &&.
        @param filter_curated_hits Whether the filter_by condition of the search query should be applicable to curated results (override definitions, pinned hits, hidden hits, etc.). Default: false
    
        @param group_by You can aggregate search results into groups or buckets by specify one or more `group_by` fields. Separate multiple fields with a comma. To group on a particular field, it must be a faceted field.
        @param group_limit Maximum number of hits to be returned for every group. If the `group_limit` is set as `K` then only the top K hits in each group are returned in the response. Default: 3
    
        @param group_missing_values Setting this parameter to true will place all documents that have a null value in the group_by field, into a single group. Setting this parameter to false, will cause each document with a null value in the group_by field to not be grouped with other documents. Default: true
    
        @param hidden_hits A list of records to unconditionally hide from search results. A list of `record_id`s to hide. Eg: to hide records with IDs 123 and 456, you'd specify `123,456`.
    You could also use the Overrides feature to override search results based on rules. Overrides are applied first, followed by `pinned_hits` and finally `hidden_hits`.
    
        @param highlight_affix_num_tokens The number of tokens that should surround the highlighted text on each side. Default: 4
    
        @param highlight_end_tag The end tag used for the highlighted snippets. Default: `</mark>`
    
        @param highlight_fields A list of custom fields that must be highlighted even if you don't query for them
    
        @param highlight_full_fields List of fields which should be highlighted fully without snippeting
        @param highlight_start_tag The start tag used for the highlighted snippets. Default: `<mark>`
    
        @param include_fields List of fields from the document to include in the search result
        @param infix If infix index is enabled for this field, infix searching can be done on a per-field basis by sending a comma separated string parameter called infix to the search query. This parameter can have 3 values; `off` infix search is disabled, which is default `always` infix search is performed along with regular search `fallback` infix search is performed if regular search does not produce results
        @param limit Number of hits to fetch. Can be used as an alternative to the per_page parameter. Default: 10.
    
        @param max_extra_prefix There are also 2 parameters that allow you to control the extent of infix searching max_extra_prefix and max_extra_suffix which specify the maximum number of symbols before or after the query that can be present in the token. For example query "K2100" has 2 extra symbols in "6PK2100". By default, any number of prefixes/suffixes can be present for a match.
        @param max_extra_suffix There are also 2 parameters that allow you to control the extent of infix searching max_extra_prefix and max_extra_suffix which specify the maximum number of symbols before or after the query that can be present in the token. For example query "K2100" has 2 extra symbols in "6PK2100". By default, any number of prefixes/suffixes can be present for a match.
        @param max_facet_values Maximum number of facet values to be returned.
        @param min_len_1typo Minimum word length for 1-typo correction to be applied. The value of num_typos is still treated as the maximum allowed typos.
    
        @param min_len_2typo Minimum word length for 2-typo correction to be applied. The value of num_typos is still treated as the maximum allowed typos.
    
        @param num_typos The number of typographical errors (1 or 2) that would be tolerated. Default: 2
    
        @param offset Identifies the starting point to return hits from a result set. Can be used as an alternative to the page parameter.
        @param override_tags Comma separated list of tags to trigger the curations rules that match the tags.
        @param page Results from this specific page number would be fetched.
        @param per_page Number of results to fetch per page. Default: 10
        @param pinned_hits A list of records to unconditionally include in the search results at specific positions. An example use case would be to feature or promote certain items on the top of search results. A list of `record_id:hit_position`. Eg: to include a record with ID 123 at Position 1 and another record with ID 456 at Position 5, you'd specify `123:1,456:5`.
    You could also use the Overrides feature to override search results based on rules. Overrides are applied first, followed by `pinned_hits` and finally `hidden_hits`.
    
        @param prefix Boolean field to indicate that the last word in the query should be treated as a prefix, and not as a whole word. This is used for building autocomplete and instant search interfaces. Defaults to true.
        @param preset Search using a bunch of search parameters by setting this parameter to the name of the existing Preset.
    
        @param q The query text to search for in the collection. Use * as the search string to return all documents. This is typically useful when used in conjunction with filter_by.
        @param query_by A list of `string` fields that should be queried against. Multiple fields are separated with a comma.
        @param query_by_weights The relative weight to give each `query_by` field when ranking results. This can be used to boost fields in priority, when looking for matches. Multiple fields are separated with a comma.
        @param remote_embedding_num_tries Number of times to retry fetching remote embeddings.
    
        @param remote_embedding_timeout_ms Timeout (in milliseconds) for fetching remote embeddings.
    
        @param search_cutoff_ms Typesense will attempt to return results early if the cutoff time has elapsed. This is not a strict guarantee and facet computation is not bound by this parameter.
    
        @param snippet_threshold Field values under this length will be fully highlighted, instead of showing a snippet of relevant portion. Default: 30
    
        @param sort_by A list of numerical fields and their corresponding sort orders that will be used for ordering your results. Up to 3 sort fields can be specified. The text similarity score is exposed as a special `_text_match` field that you can use in the list of sorting fields. If no `sort_by` parameter is specified, results are sorted by `_text_match:desc,default_sorting_field:desc`
        @param stopwords Name of the stopwords set to apply for this search, the keywords present in the set will be removed from the search query.
    
        @param synonym_num_typos Allow synonym resolution on typo-corrected words in the query. Default: 0
    
        @param synonym_prefix Allow synonym resolution on word prefixes in the query. Default: false
    
        @param text_match_type In a multi-field matching context, this parameter determines how the representative text match score of a record is calculated. Possible values are max_score (default) or max_weight.
        @param typo_tokens_threshold If the number of results found for a specific query is less than this number, Typesense will attempt to look for tokens with more typos until enough results are found. Default: 100
    
        @param use_cache Enable server side caching of search query results. By default, caching is disabled.
    
        @param vector_query Vector query expression for fetching documents "closest" to a given query/document vector.
    
        @param voice_query The base64 encoded audio file in 16 khz 16-bit WAV format.
    
        @param collection The collection to search in.
    
        @param x_typesense_api_key A separate search API key for each search within a multi_search request
    *)
    val v : ?enable_analytics:bool -> ?enable_overrides:bool -> ?enable_typos_for_numerical_tokens:bool -> ?pre_segmented_query:bool -> ?prioritize_exact_match:bool -> ?prioritize_num_matching_fields:bool -> ?prioritize_token_position:bool -> ?rerank_hybrid_matches:bool -> ?cache_ttl:int -> ?conversation:bool -> ?conversation_id:string -> ?conversation_model_id:string -> ?drop_tokens_mode:DropTokensMode.T.t -> ?drop_tokens_threshold:int -> ?enable_synonyms:bool -> ?enable_typos_for_alpha_numerical_tokens:bool -> ?exclude_fields:string -> ?exhaustive_search:bool -> ?facet_by:string -> ?facet_query:string -> ?facet_return_parent:string -> ?facet_strategy:string -> ?filter_by:string -> ?filter_curated_hits:bool -> ?group_by:string -> ?group_limit:int -> ?group_missing_values:bool -> ?hidden_hits:string -> ?highlight_affix_num_tokens:int -> ?highlight_end_tag:string -> ?highlight_fields:string -> ?highlight_full_fields:string -> ?highlight_start_tag:string -> ?include_fields:string -> ?infix:string -> ?limit:int -> ?max_extra_prefix:int -> ?max_extra_suffix:int -> ?max_facet_values:int -> ?min_len_1typo:int -> ?min_len_2typo:int -> ?num_typos:string -> ?offset:int -> ?override_tags:string -> ?page:int -> ?per_page:int -> ?pinned_hits:string -> ?prefix:string -> ?preset:string -> ?q:string -> ?query_by:string -> ?query_by_weights:string -> ?remote_embedding_num_tries:int -> ?remote_embedding_timeout_ms:int -> ?search_cutoff_ms:int -> ?snippet_threshold:int -> ?sort_by:string -> ?stopwords:string -> ?synonym_num_typos:int -> ?synonym_prefix:bool -> ?text_match_type:string -> ?typo_tokens_threshold:int -> ?use_cache:bool -> ?vector_query:string -> ?voice_query:string -> ?collection:string -> ?x_typesense_api_key:string -> unit -> t
    
    (** The duration (in seconds) that determines how long the search query is cached. This value can be set on a per-query basis. Default: 60.
     *)
    val cache_ttl : t -> int option
    
    (** Enable conversational search.
     *)
    val conversation : t -> bool option
    
    (** The Id of a previous conversation to continue, this tells Typesense to include prior context when communicating with the LLM.
     *)
    val conversation_id : t -> string option
    
    (** The Id of Conversation Model to be used.
     *)
    val conversation_model_id : t -> string option
    
    val drop_tokens_mode : t -> DropTokensMode.T.t option
    
    (** If the number of results found for a specific query is less than this number, Typesense will attempt to drop the tokens in the query until enough results are found. Tokens that have the least individual hits are dropped first. Set to 0 to disable. Default: 10
     *)
    val drop_tokens_threshold : t -> int option
    
    (** Flag for enabling/disabling analytics aggregation for specific search queries (for e.g. those originating from a test script).
     *)
    val enable_analytics : t -> bool
    
    (** If you have some overrides defined but want to disable all of them during query time, you can do that by setting this parameter to false
     *)
    val enable_overrides : t -> bool
    
    (** If you have some synonyms defined but want to disable all of them for a particular search query, set enable_synonyms to false. Default: true
     *)
    val enable_synonyms : t -> bool option
    
    (** Set this parameter to false to disable typos on alphanumerical query tokens. Default: true.
     *)
    val enable_typos_for_alpha_numerical_tokens : t -> bool option
    
    (** Make Typesense disable typos for numerical tokens.
     *)
    val enable_typos_for_numerical_tokens : t -> bool
    
    (** List of fields from the document to exclude in the search result *)
    val exclude_fields : t -> string option
    
    (** Setting this to true will make Typesense consider all prefixes and typo corrections of the words in the query without stopping early when enough results are found (drop_tokens_threshold and typo_tokens_threshold configurations are ignored).
     *)
    val exhaustive_search : t -> bool option
    
    (** A list of fields that will be used for faceting your results on. Separate multiple fields with a comma. *)
    val facet_by : t -> string option
    
    (** Facet values that are returned can now be filtered via this parameter. The matching facet text is also highlighted. For example, when faceting by `category`, you can set `facet_query=category:shoe` to return only facet values that contain the prefix "shoe". *)
    val facet_query : t -> string option
    
    (** Comma separated string of nested facet fields whose parent object should be returned in facet response.
     *)
    val facet_return_parent : t -> string option
    
    (** Choose the underlying faceting strategy used. Comma separated string of allows values: exhaustive, top_values or automatic (default).
     *)
    val facet_strategy : t -> string option
    
    (** Filter conditions for refining youropen api validator search results. Separate multiple conditions with &&. *)
    val filter_by : t -> string option
    
    (** Whether the filter_by condition of the search query should be applicable to curated results (override definitions, pinned hits, hidden hits, etc.). Default: false
     *)
    val filter_curated_hits : t -> bool option
    
    (** You can aggregate search results into groups or buckets by specify one or more `group_by` fields. Separate multiple fields with a comma. To group on a particular field, it must be a faceted field. *)
    val group_by : t -> string option
    
    (** Maximum number of hits to be returned for every group. If the `group_limit` is set as `K` then only the top K hits in each group are returned in the response. Default: 3
     *)
    val group_limit : t -> int option
    
    (** Setting this parameter to true will place all documents that have a null value in the group_by field, into a single group. Setting this parameter to false, will cause each document with a null value in the group_by field to not be grouped with other documents. Default: true
     *)
    val group_missing_values : t -> bool option
    
    (** A list of records to unconditionally hide from search results. A list of `record_id`s to hide. Eg: to hide records with IDs 123 and 456, you'd specify `123,456`.
    You could also use the Overrides feature to override search results based on rules. Overrides are applied first, followed by `pinned_hits` and finally `hidden_hits`.
     *)
    val hidden_hits : t -> string option
    
    (** The number of tokens that should surround the highlighted text on each side. Default: 4
     *)
    val highlight_affix_num_tokens : t -> int option
    
    (** The end tag used for the highlighted snippets. Default: `</mark>`
     *)
    val highlight_end_tag : t -> string option
    
    (** A list of custom fields that must be highlighted even if you don't query for them
     *)
    val highlight_fields : t -> string option
    
    (** List of fields which should be highlighted fully without snippeting *)
    val highlight_full_fields : t -> string option
    
    (** The start tag used for the highlighted snippets. Default: `<mark>`
     *)
    val highlight_start_tag : t -> string option
    
    (** List of fields from the document to include in the search result *)
    val include_fields : t -> string option
    
    (** If infix index is enabled for this field, infix searching can be done on a per-field basis by sending a comma separated string parameter called infix to the search query. This parameter can have 3 values; `off` infix search is disabled, which is default `always` infix search is performed along with regular search `fallback` infix search is performed if regular search does not produce results *)
    val infix : t -> string option
    
    (** Number of hits to fetch. Can be used as an alternative to the per_page parameter. Default: 10.
     *)
    val limit : t -> int option
    
    (** There are also 2 parameters that allow you to control the extent of infix searching max_extra_prefix and max_extra_suffix which specify the maximum number of symbols before or after the query that can be present in the token. For example query "K2100" has 2 extra symbols in "6PK2100". By default, any number of prefixes/suffixes can be present for a match. *)
    val max_extra_prefix : t -> int option
    
    (** There are also 2 parameters that allow you to control the extent of infix searching max_extra_prefix and max_extra_suffix which specify the maximum number of symbols before or after the query that can be present in the token. For example query "K2100" has 2 extra symbols in "6PK2100". By default, any number of prefixes/suffixes can be present for a match. *)
    val max_extra_suffix : t -> int option
    
    (** Maximum number of facet values to be returned. *)
    val max_facet_values : t -> int option
    
    (** Minimum word length for 1-typo correction to be applied. The value of num_typos is still treated as the maximum allowed typos.
     *)
    val min_len_1typo : t -> int option
    
    (** Minimum word length for 2-typo correction to be applied. The value of num_typos is still treated as the maximum allowed typos.
     *)
    val min_len_2typo : t -> int option
    
    (** The number of typographical errors (1 or 2) that would be tolerated. Default: 2
     *)
    val num_typos : t -> string option
    
    (** Identifies the starting point to return hits from a result set. Can be used as an alternative to the page parameter. *)
    val offset : t -> int option
    
    (** Comma separated list of tags to trigger the curations rules that match the tags. *)
    val override_tags : t -> string option
    
    (** Results from this specific page number would be fetched. *)
    val page : t -> int option
    
    (** Number of results to fetch per page. Default: 10 *)
    val per_page : t -> int option
    
    (** A list of records to unconditionally include in the search results at specific positions. An example use case would be to feature or promote certain items on the top of search results. A list of `record_id:hit_position`. Eg: to include a record with ID 123 at Position 1 and another record with ID 456 at Position 5, you'd specify `123:1,456:5`.
    You could also use the Overrides feature to override search results based on rules. Overrides are applied first, followed by `pinned_hits` and finally `hidden_hits`.
     *)
    val pinned_hits : t -> string option
    
    (** You can index content from any logographic language into Typesense if you are able to segment / split the text into space-separated words yourself before indexing and querying.
    Set this parameter to true to do the same
     *)
    val pre_segmented_query : t -> bool
    
    (** Boolean field to indicate that the last word in the query should be treated as a prefix, and not as a whole word. This is used for building autocomplete and instant search interfaces. Defaults to true. *)
    val prefix : t -> string option
    
    (** Search using a bunch of search parameters by setting this parameter to the name of the existing Preset.
     *)
    val preset : t -> string option
    
    (** Set this parameter to true to ensure that an exact match is ranked above the others
     *)
    val prioritize_exact_match : t -> bool
    
    (** Make Typesense prioritize documents where the query words appear in more number of fields.
     *)
    val prioritize_num_matching_fields : t -> bool
    
    (** Make Typesense prioritize documents where the query words appear earlier in the text.
     *)
    val prioritize_token_position : t -> bool
    
    (** The query text to search for in the collection. Use * as the search string to return all documents. This is typically useful when used in conjunction with filter_by. *)
    val q : t -> string option
    
    (** A list of `string` fields that should be queried against. Multiple fields are separated with a comma. *)
    val query_by : t -> string option
    
    (** The relative weight to give each `query_by` field when ranking results. This can be used to boost fields in priority, when looking for matches. Multiple fields are separated with a comma. *)
    val query_by_weights : t -> string option
    
    (** Number of times to retry fetching remote embeddings.
     *)
    val remote_embedding_num_tries : t -> int option
    
    (** Timeout (in milliseconds) for fetching remote embeddings.
     *)
    val remote_embedding_timeout_ms : t -> int option
    
    (** Typesense will attempt to return results early if the cutoff time has elapsed. This is not a strict guarantee and facet computation is not bound by this parameter.
     *)
    val search_cutoff_ms : t -> int option
    
    (** Field values under this length will be fully highlighted, instead of showing a snippet of relevant portion. Default: 30
     *)
    val snippet_threshold : t -> int option
    
    (** A list of numerical fields and their corresponding sort orders that will be used for ordering your results. Up to 3 sort fields can be specified. The text similarity score is exposed as a special `_text_match` field that you can use in the list of sorting fields. If no `sort_by` parameter is specified, results are sorted by `_text_match:desc,default_sorting_field:desc` *)
    val sort_by : t -> string option
    
    (** Name of the stopwords set to apply for this search, the keywords present in the set will be removed from the search query.
     *)
    val stopwords : t -> string option
    
    (** Allow synonym resolution on typo-corrected words in the query. Default: 0
     *)
    val synonym_num_typos : t -> int option
    
    (** Allow synonym resolution on word prefixes in the query. Default: false
     *)
    val synonym_prefix : t -> bool option
    
    (** In a multi-field matching context, this parameter determines how the representative text match score of a record is calculated. Possible values are max_score (default) or max_weight. *)
    val text_match_type : t -> string option
    
    (** If the number of results found for a specific query is less than this number, Typesense will attempt to look for tokens with more typos until enough results are found. Default: 100
     *)
    val typo_tokens_threshold : t -> int option
    
    (** Enable server side caching of search query results. By default, caching is disabled.
     *)
    val use_cache : t -> bool option
    
    (** Vector query expression for fetching documents "closest" to a given query/document vector.
     *)
    val vector_query : t -> string option
    
    (** The base64 encoded audio file in 16 khz 16-bit WAV format.
     *)
    val voice_query : t -> string option
    
    (** The collection to search in.
     *)
    val collection : t -> string option
    
    (** A separate search API key for each search within a multi_search request *)
    val x_typesense_api_key : t -> string option
    
    (** When true, computes both text match and vector distance scores for all matches in hybrid search. Documents found only through keyword search will get a vector distance score, and documents found only through vector search will get a text match score.
     *)
    val rerank_hybrid_matches : t -> bool
    
    val jsont : t Jsont.t
  end
end

module MultiSearchSearchesParameter : sig
  module T : sig
    type t
    
    (** Construct a value
        @param union When true, merges the search results from each search query into a single ordered set of hits.
    *)
    val v : searches:MultiSearchCollectionParameters.T.t list -> ?union:bool -> unit -> t
    
    val searches : t -> MultiSearchCollectionParameters.T.t list
    
    (** When true, merges the search results from each search query into a single ordered set of hits. *)
    val union : t -> bool
    
    val jsont : t Jsont.t
  end
end

module MultiSearch : sig
  module Result : sig
    type t
    
    (** Construct a value *)
    val v : results:MultiSearchResult.Item.t list -> ?conversation:SearchResultConversation.T.t -> unit -> t
    
    val conversation : t -> SearchResultConversation.T.t option
    
    val results : t -> MultiSearchResult.Item.t list
    
    val jsont : t Jsont.t
  end
  
  (** send multiple search requests in a single HTTP request
  
      This is especially useful to avoid round-trip network latencies incurred otherwise if each of these requests are sent in separate HTTP requests. You can also use this feature to do a federated search across multiple collections in a single HTTP request. *)
  val multi_search : multi_search_parameters:string -> body:MultiSearchSearchesParameter.T.t -> t -> unit -> Result.t
end

module DirtyValues : sig
  module T : sig
    type t = [
      | `Coerce_or_reject
      | `Coerce_or_drop
      | `Drop
      | `Reject
    ]
    
    val jsont : t Jsont.t
  end
end

module CurationSetDeleteSchema : sig
  module T : sig
    type t
    
    (** Construct a value
        @param name Name of the deleted curation set
    *)
    val v : name:string -> unit -> t
    
    (** Name of the deleted curation set *)
    val name : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** Delete a curation set
  
      Delete a specific curation set by its name 
      @param curation_set_name The name of the curation set to delete
  *)
  val delete_curation_set : curation_set_name:string -> t -> unit -> T.t
end

module CurationRule : sig
  module T : sig
    type t
    
    (** Construct a value
        @param filter_by Indicates that the curation should apply when the filter_by parameter in a search query exactly matches the string specified here (including backticks, spaces, brackets, etc).
    
        @param match_ Indicates whether the match on the query term should be `exact` or `contains`. If we want to match all queries that contained the word `apple`, we will use the `contains` match instead.
    
        @param query Indicates what search queries should be curated
        @param tags List of tag values to associate with this curation rule.
    *)
    val v : ?filter_by:string -> ?match_:string -> ?query:string -> ?tags:string list -> unit -> t
    
    (** Indicates that the curation should apply when the filter_by parameter in a search query exactly matches the string specified here (including backticks, spaces, brackets, etc).
     *)
    val filter_by : t -> string option
    
    (** Indicates whether the match on the query term should be `exact` or `contains`. If we want to match all queries that contained the word `apple`, we will use the `contains` match instead.
     *)
    val match_ : t -> string option
    
    (** Indicates what search queries should be curated *)
    val query : t -> string option
    
    (** List of tag values to associate with this curation rule. *)
    val tags : t -> string list option
    
    val jsont : t Jsont.t
  end
end

module CurationItemDeleteSchema : sig
  module T : sig
    type t
    
    (** Construct a value
        @param id ID of the deleted curation item
    *)
    val v : id:string -> unit -> t
    
    (** ID of the deleted curation item *)
    val id : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** Delete a curation set item
  
      Delete a specific curation item by its id 
      @param curation_set_name The name of the curation set
      @param item_id The id of the curation item to delete
  *)
  val delete_curation_set_item : curation_set_name:string -> item_id:string -> t -> unit -> T.t
end

module CurationInclude : sig
  module T : sig
    type t
    
    (** Construct a value
        @param id document id that should be included
        @param position position number where document should be included in the search results
    *)
    val v : id:string -> position:int -> unit -> t
    
    (** document id that should be included *)
    val id : t -> string
    
    (** position number where document should be included in the search results *)
    val position : t -> int
    
    val jsont : t Jsont.t
  end
end

module CurationExclude : sig
  module T : sig
    type t
    
    (** Construct a value
        @param id document id that should be excluded from the search results.
    *)
    val v : id:string -> unit -> t
    
    (** document id that should be excluded from the search results. *)
    val id : t -> string
    
    val jsont : t Jsont.t
  end
end

module CurationItemCreateSchema : sig
  module T : sig
    type t
    
    (** Construct a value
        @param effective_from_ts A Unix timestamp that indicates the date/time from which the curation will be active. You can use this to create rules that start applying from a future point in time.
    
        @param effective_to_ts A Unix timestamp that indicates the date/time until which the curation will be active. You can use this to create rules that stop applying after a period of time.
    
        @param excludes List of document `id`s that should be excluded from the search results.
        @param filter_by A filter by clause that is applied to any search query that matches the curation rule.
    
        @param filter_curated_hits When set to true, the filter conditions of the query is applied to the curated records as well. Default: false.
    
        @param id ID of the curation item
        @param includes List of document `id`s that should be included in the search results with their corresponding `position`s.
        @param metadata Return a custom JSON object in the Search API response, when this rule is triggered. This can can be used to display a pre-defined message (eg: a promotion banner) on the front-end when a particular rule is triggered.
    
        @param remove_matched_tokens Indicates whether search query tokens that exist in the curation's rule should be removed from the search query.
    
        @param replace_query Replaces the current search query with this value, when the search query matches the curation rule.
    
        @param sort_by A sort by clause that is applied to any search query that matches the curation rule.
    
        @param stop_processing When set to true, curation processing will stop at the first matching rule. When set to false curation processing will continue and multiple curation actions will be triggered in sequence. Curations are processed in the lexical sort order of their id field.
    
    *)
    val v : rule:CurationRule.T.t -> ?effective_from_ts:int -> ?effective_to_ts:int -> ?excludes:CurationExclude.T.t list -> ?filter_by:string -> ?filter_curated_hits:bool -> ?id:string -> ?includes:CurationInclude.T.t list -> ?metadata:Jsont.json -> ?remove_matched_tokens:bool -> ?replace_query:string -> ?sort_by:string -> ?stop_processing:bool -> unit -> t
    
    (** A Unix timestamp that indicates the date/time from which the curation will be active. You can use this to create rules that start applying from a future point in time.
     *)
    val effective_from_ts : t -> int option
    
    (** A Unix timestamp that indicates the date/time until which the curation will be active. You can use this to create rules that stop applying after a period of time.
     *)
    val effective_to_ts : t -> int option
    
    (** List of document `id`s that should be excluded from the search results. *)
    val excludes : t -> CurationExclude.T.t list option
    
    (** A filter by clause that is applied to any search query that matches the curation rule.
     *)
    val filter_by : t -> string option
    
    (** When set to true, the filter conditions of the query is applied to the curated records as well. Default: false.
     *)
    val filter_curated_hits : t -> bool option
    
    (** ID of the curation item *)
    val id : t -> string option
    
    (** List of document `id`s that should be included in the search results with their corresponding `position`s. *)
    val includes : t -> CurationInclude.T.t list option
    
    (** Return a custom JSON object in the Search API response, when this rule is triggered. This can can be used to display a pre-defined message (eg: a promotion banner) on the front-end when a particular rule is triggered.
     *)
    val metadata : t -> Jsont.json option
    
    (** Indicates whether search query tokens that exist in the curation's rule should be removed from the search query.
     *)
    val remove_matched_tokens : t -> bool option
    
    (** Replaces the current search query with this value, when the search query matches the curation rule.
     *)
    val replace_query : t -> string option
    
    val rule : t -> CurationRule.T.t
    
    (** A sort by clause that is applied to any search query that matches the curation rule.
     *)
    val sort_by : t -> string option
    
    (** When set to true, curation processing will stop at the first matching rule. When set to false curation processing will continue and multiple curation actions will be triggered in sequence. Curations are processed in the lexical sort order of their id field.
     *)
    val stop_processing : t -> bool option
    
    val jsont : t Jsont.t
  end
end

module CurationSetCreateSchema : sig
  module T : sig
    type t
    
    (** Construct a value
        @param items Array of curation items
        @param description Optional description for the curation set
    *)
    val v : items:CurationItemCreateSchema.T.t list -> ?description:string -> unit -> t
    
    (** Optional description for the curation set *)
    val description : t -> string option
    
    (** Array of curation items *)
    val items : t -> CurationItemCreateSchema.T.t list
    
    val jsont : t Jsont.t
  end
end

module CurationSetSchema : sig
  module T : sig
    type t
    
    (** Construct a value
        @param items Array of curation items
        @param description Optional description for the curation set
    *)
    val v : items:CurationItemCreateSchema.T.t list -> name:string -> ?description:string -> unit -> t
    
    (** Optional description for the curation set *)
    val description : t -> string option
    
    (** Array of curation items *)
    val items : t -> CurationItemCreateSchema.T.t list
    
    val name : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** List all curation sets
  
      Retrieve all curation sets *)
  val retrieve_curation_sets : t -> unit -> T.t
  
  (** Retrieve a curation set
  
      Retrieve a specific curation set by its name 
      @param curation_set_name The name of the curation set to retrieve
  *)
  val retrieve_curation_set : curation_set_name:string -> t -> unit -> T.t
  
  (** Create or update a curation set
  
      Create or update a curation set with the given name 
      @param curation_set_name The name of the curation set to create/update
  *)
  val upsert_curation_set : curation_set_name:string -> body:CurationSetCreateSchema.T.t -> t -> unit -> T.t
end

module CurationItemSchema : sig
  module T : sig
    type t
    
    (** Construct a value
        @param effective_from_ts A Unix timestamp that indicates the date/time from which the curation will be active. You can use this to create rules that start applying from a future point in time.
    
        @param effective_to_ts A Unix timestamp that indicates the date/time until which the curation will be active. You can use this to create rules that stop applying after a period of time.
    
        @param excludes List of document `id`s that should be excluded from the search results.
        @param filter_by A filter by clause that is applied to any search query that matches the curation rule.
    
        @param filter_curated_hits When set to true, the filter conditions of the query is applied to the curated records as well. Default: false.
    
        @param includes List of document `id`s that should be included in the search results with their corresponding `position`s.
        @param metadata Return a custom JSON object in the Search API response, when this rule is triggered. This can can be used to display a pre-defined message (eg: a promotion banner) on the front-end when a particular rule is triggered.
    
        @param remove_matched_tokens Indicates whether search query tokens that exist in the curation's rule should be removed from the search query.
    
        @param replace_query Replaces the current search query with this value, when the search query matches the curation rule.
    
        @param sort_by A sort by clause that is applied to any search query that matches the curation rule.
    
        @param stop_processing When set to true, curation processing will stop at the first matching rule. When set to false curation processing will continue and multiple curation actions will be triggered in sequence. Curations are processed in the lexical sort order of their id field.
    
    *)
    val v : rule:CurationRule.T.t -> id:string -> ?effective_from_ts:int -> ?effective_to_ts:int -> ?excludes:CurationExclude.T.t list -> ?filter_by:string -> ?filter_curated_hits:bool -> ?includes:CurationInclude.T.t list -> ?metadata:Jsont.json -> ?remove_matched_tokens:bool -> ?replace_query:string -> ?sort_by:string -> ?stop_processing:bool -> unit -> t
    
    (** A Unix timestamp that indicates the date/time from which the curation will be active. You can use this to create rules that start applying from a future point in time.
     *)
    val effective_from_ts : t -> int option
    
    (** A Unix timestamp that indicates the date/time until which the curation will be active. You can use this to create rules that stop applying after a period of time.
     *)
    val effective_to_ts : t -> int option
    
    (** List of document `id`s that should be excluded from the search results. *)
    val excludes : t -> CurationExclude.T.t list option
    
    (** A filter by clause that is applied to any search query that matches the curation rule.
     *)
    val filter_by : t -> string option
    
    (** When set to true, the filter conditions of the query is applied to the curated records as well. Default: false.
     *)
    val filter_curated_hits : t -> bool option
    
    (** List of document `id`s that should be included in the search results with their corresponding `position`s. *)
    val includes : t -> CurationInclude.T.t list option
    
    (** Return a custom JSON object in the Search API response, when this rule is triggered. This can can be used to display a pre-defined message (eg: a promotion banner) on the front-end when a particular rule is triggered.
     *)
    val metadata : t -> Jsont.json option
    
    (** Indicates whether search query tokens that exist in the curation's rule should be removed from the search query.
     *)
    val remove_matched_tokens : t -> bool option
    
    (** Replaces the current search query with this value, when the search query matches the curation rule.
     *)
    val replace_query : t -> string option
    
    val rule : t -> CurationRule.T.t
    
    (** A sort by clause that is applied to any search query that matches the curation rule.
     *)
    val sort_by : t -> string option
    
    (** When set to true, curation processing will stop at the first matching rule. When set to false curation processing will continue and multiple curation actions will be triggered in sequence. Curations are processed in the lexical sort order of their id field.
     *)
    val stop_processing : t -> bool option
    
    val id : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** List items in a curation set
  
      Retrieve all curation items in a set 
      @param curation_set_name The name of the curation set to retrieve items for
  *)
  val retrieve_curation_set_items : curation_set_name:string -> t -> unit -> T.t
  
  (** Retrieve a curation set item
  
      Retrieve a specific curation item by its id 
      @param curation_set_name The name of the curation set
      @param item_id The id of the curation item to retrieve
  *)
  val retrieve_curation_set_item : curation_set_name:string -> item_id:string -> t -> unit -> T.t
  
  (** Create or update a curation set item
  
      Create or update a curation set item with the given id 
      @param curation_set_name The name of the curation set
      @param item_id The id of the curation item to upsert
  *)
  val upsert_curation_set_item : curation_set_name:string -> item_id:string -> body:CurationItemCreateSchema.T.t -> t -> unit -> T.t
end

module ConversationModelUpdateSchema : sig
  module T : sig
    type t
    
    (** Construct a value
        @param account_id LLM service's account ID (only applicable for Cloudflare)
        @param api_key The LLM service's API Key
        @param history_collection Typesense collection that stores the historical conversations
        @param id An explicit id for the model, otherwise the API will return a response with an auto-generated conversation model id.
        @param max_bytes The maximum number of bytes to send to the LLM in every API call. Consult the LLM's documentation on the number of bytes supported in the context window.
    
        @param model_name Name of the LLM model offered by OpenAI, Cloudflare or vLLM
        @param system_prompt The system prompt that contains special instructions to the LLM
        @param ttl Time interval in seconds after which the messages would be deleted. Default: 86400 (24 hours)
    
        @param vllm_url URL of vLLM service
    *)
    val v : ?account_id:string -> ?api_key:string -> ?history_collection:string -> ?id:string -> ?max_bytes:int -> ?model_name:string -> ?system_prompt:string -> ?ttl:int -> ?vllm_url:string -> unit -> t
    
    (** LLM service's account ID (only applicable for Cloudflare) *)
    val account_id : t -> string option
    
    (** The LLM service's API Key *)
    val api_key : t -> string option
    
    (** Typesense collection that stores the historical conversations *)
    val history_collection : t -> string option
    
    (** An explicit id for the model, otherwise the API will return a response with an auto-generated conversation model id. *)
    val id : t -> string option
    
    (** The maximum number of bytes to send to the LLM in every API call. Consult the LLM's documentation on the number of bytes supported in the context window.
     *)
    val max_bytes : t -> int option
    
    (** Name of the LLM model offered by OpenAI, Cloudflare or vLLM *)
    val model_name : t -> string option
    
    (** The system prompt that contains special instructions to the LLM *)
    val system_prompt : t -> string option
    
    (** Time interval in seconds after which the messages would be deleted. Default: 86400 (24 hours)
     *)
    val ttl : t -> int option
    
    (** URL of vLLM service *)
    val vllm_url : t -> string option
    
    val jsont : t Jsont.t
  end
end

module ConversationModelCreateSchema : sig
  module T : sig
    type t
    
    (** Construct a value
        @param model_name Name of the LLM model offered by OpenAI, Cloudflare or vLLM
        @param max_bytes The maximum number of bytes to send to the LLM in every API call. Consult the LLM's documentation on the number of bytes supported in the context window.
    
        @param history_collection Typesense collection that stores the historical conversations
        @param account_id LLM service's account ID (only applicable for Cloudflare)
        @param api_key The LLM service's API Key
        @param id An explicit id for the model, otherwise the API will return a response with an auto-generated conversation model id.
        @param system_prompt The system prompt that contains special instructions to the LLM
        @param ttl Time interval in seconds after which the messages would be deleted. Default: 86400 (24 hours)
    
        @param vllm_url URL of vLLM service
    *)
    val v : model_name:string -> max_bytes:int -> history_collection:string -> ?account_id:string -> ?api_key:string -> ?id:string -> ?system_prompt:string -> ?ttl:int -> ?vllm_url:string -> unit -> t
    
    (** LLM service's account ID (only applicable for Cloudflare) *)
    val account_id : t -> string option
    
    (** The LLM service's API Key *)
    val api_key : t -> string option
    
    (** An explicit id for the model, otherwise the API will return a response with an auto-generated conversation model id. *)
    val id : t -> string option
    
    (** The system prompt that contains special instructions to the LLM *)
    val system_prompt : t -> string option
    
    (** Time interval in seconds after which the messages would be deleted. Default: 86400 (24 hours)
     *)
    val ttl : t -> int option
    
    (** URL of vLLM service *)
    val vllm_url : t -> string option
    
    (** Name of the LLM model offered by OpenAI, Cloudflare or vLLM *)
    val model_name : t -> string
    
    (** The maximum number of bytes to send to the LLM in every API call. Consult the LLM's documentation on the number of bytes supported in the context window.
     *)
    val max_bytes : t -> int
    
    (** Typesense collection that stores the historical conversations *)
    val history_collection : t -> string
    
    val jsont : t Jsont.t
  end
end

module ConversationModelSchema : sig
  module T : sig
    type t
    
    (** Construct a value
        @param model_name Name of the LLM model offered by OpenAI, Cloudflare or vLLM
        @param max_bytes The maximum number of bytes to send to the LLM in every API call. Consult the LLM's documentation on the number of bytes supported in the context window.
    
        @param history_collection Typesense collection that stores the historical conversations
        @param id An explicit id for the model, otherwise the API will return a response with an auto-generated conversation model id.
        @param account_id LLM service's account ID (only applicable for Cloudflare)
        @param api_key The LLM service's API Key
        @param system_prompt The system prompt that contains special instructions to the LLM
        @param ttl Time interval in seconds after which the messages would be deleted. Default: 86400 (24 hours)
    
        @param vllm_url URL of vLLM service
    *)
    val v : model_name:string -> max_bytes:int -> history_collection:string -> id:string -> ?account_id:string -> ?api_key:string -> ?system_prompt:string -> ?ttl:int -> ?vllm_url:string -> unit -> t
    
    (** LLM service's account ID (only applicable for Cloudflare) *)
    val account_id : t -> string option
    
    (** The LLM service's API Key *)
    val api_key : t -> string option
    
    (** The system prompt that contains special instructions to the LLM *)
    val system_prompt : t -> string option
    
    (** Time interval in seconds after which the messages would be deleted. Default: 86400 (24 hours)
     *)
    val ttl : t -> int option
    
    (** URL of vLLM service *)
    val vllm_url : t -> string option
    
    (** Name of the LLM model offered by OpenAI, Cloudflare or vLLM *)
    val model_name : t -> string
    
    (** The maximum number of bytes to send to the LLM in every API call. Consult the LLM's documentation on the number of bytes supported in the context window.
     *)
    val max_bytes : t -> int
    
    (** Typesense collection that stores the historical conversations *)
    val history_collection : t -> string
    
    (** An explicit id for the model, otherwise the API will return a response with an auto-generated conversation model id. *)
    val id : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** List all conversation models
  
      Retrieve all conversation models *)
  val retrieve_all_conversation_models : t -> unit -> T.t
  
  (** Create a conversation model
  
      Create a Conversation Model *)
  val create_conversation_model : body:ConversationModelCreateSchema.T.t -> t -> unit -> T.t
  
  (** Retrieve a conversation model
  
      Retrieve a conversation model 
      @param model_id The id of the conversation model to retrieve
  *)
  val retrieve_conversation_model : model_id:string -> t -> unit -> T.t
  
  (** Update a conversation model
  
      Update a conversation model 
      @param model_id The id of the conversation model to update
  *)
  val update_conversation_model : model_id:string -> body:ConversationModelUpdateSchema.T.t -> t -> unit -> T.t
  
  (** Delete a conversation model
  
      Delete a conversation model 
      @param model_id The id of the conversation model to delete
  *)
  val delete_conversation_model : model_id:string -> t -> unit -> T.t
end

module CollectionAliasSchema : sig
  module T : sig
    type t
    
    (** Construct a value
        @param collection_name Name of the collection you wish to map the alias to
    *)
    val v : collection_name:string -> unit -> t
    
    (** Name of the collection you wish to map the alias to *)
    val collection_name : t -> string
    
    val jsont : t Jsont.t
  end
end

module CollectionAlias : sig
  module T : sig
    type t
    
    (** Construct a value
        @param collection_name Name of the collection the alias mapped to
        @param name Name of the collection alias
    *)
    val v : collection_name:string -> name:string -> unit -> t
    
    (** Name of the collection the alias mapped to *)
    val collection_name : t -> string
    
    (** Name of the collection alias *)
    val name : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve an alias
  
      Find out which collection an alias points to by fetching it 
      @param alias_name The name of the alias to retrieve
  *)
  val get_alias : alias_name:string -> t -> unit -> T.t
  
  (** Create or update a collection alias
  
      Create or update a collection alias. An alias is a virtual collection name that points to a real collection. If you're familiar with symbolic links on Linux, it's very similar to that. Aliases are useful when you want to reindex your data in the background on a new collection and switch your application to it without any changes to your code. 
      @param alias_name The name of the alias to create/update
  *)
  val upsert_alias : alias_name:string -> body:CollectionAliasSchema.T.t -> t -> unit -> T.t
  
  (** Delete an alias 
      @param alias_name The name of the alias to delete
  *)
  val delete_alias : alias_name:string -> t -> unit -> T.t
end

module CollectionAliases : sig
  module Response : sig
    type t
    
    (** Construct a value *)
    val v : aliases:CollectionAlias.T.t list -> unit -> t
    
    val aliases : t -> CollectionAlias.T.t list
    
    val jsont : t Jsont.t
  end
  
  (** List all aliases
  
      List all aliases and the corresponding collections that they map to. *)
  val get_aliases : t -> unit -> Response.t
end

module Client : sig
  (** Create analytics rule(s)
  
      Create one or more analytics rules. You can send a single rule object or an array of rule objects. *)
  val create_analytics_rule : t -> unit -> Jsont.json
  
  (** Index a document
  
      A document to be indexed in a given collection must conform to the schema of the collection. 
      @param collection_name The name of the collection to add the document to
      @param action Additional action to perform
      @param dirty_values Dealing with Dirty Data
  *)
  val index_document : collection_name:string -> ?action:string -> ?dirty_values:string -> t -> unit -> Jsont.json
  
  (** Delete a bunch of documents
  
      Delete a bunch of documents that match a specific filter condition. Use the `batch_size` parameter to control the number of documents that should deleted at a time. A larger value will speed up deletions, but will impact performance of other operations running on the server. 
      @param collection_name The name of the collection to delete documents from
  *)
  val delete_documents : collection_name:string -> ?delete_documents_parameters:string -> t -> unit -> Jsont.json
  
  (** Update documents with conditional query
  
      The filter_by query parameter is used to filter to specify a condition against which the documents are matched. The request body contains the fields that should be updated for any documents that match the filter condition. This endpoint is only available if the Typesense server is version `0.25.0.rc12` or later. 
      @param collection_name The name of the collection to update documents in
  *)
  val update_documents : collection_name:string -> ?update_documents_parameters:string -> t -> unit -> Jsont.json
  
  (** Export all documents in a collection
  
      Export all documents in a collection in JSON lines format. 
      @param collection_name The name of the collection
  *)
  val export_documents : collection_name:string -> ?export_documents_parameters:string -> t -> unit -> Jsont.json
  
  (** Import documents into a collection
  
      The documents to be imported must be formatted in a newline delimited JSON structure. You can feed the output file from a Typesense export operation directly as import. 
      @param collection_name The name of the collection
  *)
  val import_documents : collection_name:string -> ?import_documents_parameters:string -> t -> unit -> Jsont.json
  
  (** Retrieve a document
  
      Fetch an individual document from a collection by using its ID. 
      @param collection_name The name of the collection to search for the document under
      @param document_id The Document ID
  *)
  val get_document : collection_name:string -> document_id:string -> t -> unit -> Jsont.json
  
  (** Delete a document
  
      Delete an individual document from a collection by using its ID. 
      @param collection_name The name of the collection to search for the document under
      @param document_id The Document ID
  *)
  val delete_document : collection_name:string -> document_id:string -> t -> unit -> Jsont.json
  
  (** Update a document
  
      Update an individual document from a collection by using its ID. The update can be partial. 
      @param collection_name The name of the collection to search for the document under
      @param document_id The Document ID
      @param dirty_values Dealing with Dirty Data
  *)
  val update_document : collection_name:string -> document_id:string -> ?dirty_values:string -> t -> unit -> Jsont.json
  
  (** Print debugging information
  
      Print debugging information *)
  val debug : t -> unit -> Jsont.json
  
  (** Get current RAM, CPU, Disk & Network usage metrics.
  
      Retrieve the metrics. *)
  val retrieve_metrics : t -> unit -> Jsont.json
  
  (** List all stemming dictionaries
  
      Retrieve a list of all available stemming dictionaries. *)
  val list_stemming_dictionaries : t -> unit -> Jsont.json
  
  (** Import a stemming dictionary
  
      Upload a JSONL file containing word mappings to create or update a stemming dictionary. 
      @param id The ID to assign to the dictionary
  *)
  val import_stemming_dictionary : id:string -> t -> unit -> Jsont.json
  
  (** Delete a stopwords set.
  
      Permanently deletes a stopwords set, given it's name. 
      @param set_id The ID of the stopwords set to delete.
  *)
  val delete_stopwords_set : set_id:string -> t -> unit -> Jsont.json
end

module Apistats : sig
  module Response : sig
    type t
    
    (** Construct a value *)
    val v : ?delete_latency_ms:float -> ?delete_requests_per_second:float -> ?import_latency_ms:float -> ?import_requests_per_second:float -> ?latency_ms:Jsont.json -> ?overloaded_requests_per_second:float -> ?pending_write_batches:float -> ?requests_per_second:Jsont.json -> ?search_latency_ms:float -> ?search_requests_per_second:float -> ?total_requests_per_second:float -> ?write_latency_ms:float -> ?write_requests_per_second:float -> unit -> t
    
    val delete_latency_ms : t -> float option
    
    val delete_requests_per_second : t -> float option
    
    val import_latency_ms : t -> float option
    
    val import_requests_per_second : t -> float option
    
    val latency_ms : t -> Jsont.json option
    
    val overloaded_requests_per_second : t -> float option
    
    val pending_write_batches : t -> float option
    
    val requests_per_second : t -> Jsont.json option
    
    val search_latency_ms : t -> float option
    
    val search_requests_per_second : t -> float option
    
    val total_requests_per_second : t -> float option
    
    val write_latency_ms : t -> float option
    
    val write_requests_per_second : t -> float option
    
    val jsont : t Jsont.t
  end
  
  (** Get stats about API endpoints.
  
      Retrieve the stats about API endpoints. *)
  val retrieve_apistats : t -> unit -> Response.t
end

module ApiKeySchema : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : actions:string list -> collections:string list -> description:string -> ?expires_at:int64 -> ?value:string -> unit -> t
    
    val actions : t -> string list
    
    val collections : t -> string list
    
    val description : t -> string
    
    val expires_at : t -> int64 option
    
    val value : t -> string option
    
    val jsont : t Jsont.t
  end
end

module ApiKey : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : actions:string list -> collections:string list -> description:string -> ?expires_at:int64 -> ?value:string -> ?id:int64 -> ?value_prefix:string -> unit -> t
    
    val actions : t -> string list
    
    val collections : t -> string list
    
    val description : t -> string
    
    val expires_at : t -> int64 option
    
    val value : t -> string option
    
    val id : t -> int64 option
    
    val value_prefix : t -> string option
    
    val jsont : t Jsont.t
  end
  
  (** Create an API Key
  
      Create an API Key with fine-grain access control. You can restrict access on both a per-collection and per-action level. The generated key is returned only during creation. You want to store this key carefully in a secure place. *)
  val create_key : body:ApiKeySchema.T.t -> t -> unit -> T.t
  
  (** Retrieve (metadata about) a key
  
      Retrieve (metadata about) a key. Only the key prefix is returned when you retrieve a key. Due to security reasons, only the create endpoint returns the full API key. 
      @param key_id The ID of the key to retrieve
  *)
  val get_key : key_id:string -> t -> unit -> T.t
end

module ApiKeys : sig
  module Response : sig
    type t
    
    (** Construct a value *)
    val v : keys:ApiKey.T.t list -> unit -> t
    
    val keys : t -> ApiKey.T.t list
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve (metadata about) all keys. *)
  val get_keys : t -> unit -> Response.t
end

module ApiKeyDelete : sig
  module Response : sig
    type t
    
    (** Construct a value
        @param id The id of the API key that was deleted
    *)
    val v : id:int64 -> unit -> t
    
    (** The id of the API key that was deleted *)
    val id : t -> int64
    
    val jsont : t Jsont.t
  end
  
  (** Delete an API key given its ID. 
      @param key_id The ID of the key to delete
  *)
  val delete_key : key_id:string -> t -> unit -> Response.t
end

module Api : sig
  module Response : sig
    type t
    
    (** Construct a value *)
    val v : message:string -> unit -> t
    
    val message : t -> string
    
    val jsont : t Jsont.t
  end
end

module AnalyticsRule : sig
  module Update : sig
    (** Fields allowed to update on an analytics rule *)
    type t
    
    (** Construct a value *)
    val v : ?name:string -> ?params:Jsont.json -> ?rule_tag:string -> unit -> t
    
    val name : t -> string option
    
    val params : t -> Jsont.json option
    
    val rule_tag : t -> string option
    
    val jsont : t Jsont.t
  end
  
  module Type : sig
    type t = [
      | `Popular_queries
      | `Nohits_queries
      | `Counter
      | `Log
    ]
    
    val jsont : t Jsont.t
  end
  
  module Create : sig
    type t
    
    (** Construct a value *)
    val v : collection:string -> event_type:string -> name:string -> type_:Type.t -> ?params:Jsont.json -> ?rule_tag:string -> unit -> t
    
    val collection : t -> string
    
    val event_type : t -> string
    
    val name : t -> string
    
    val params : t -> Jsont.json option
    
    val rule_tag : t -> string option
    
    val type_ : t -> Type.t
    
    val jsont : t Jsont.t
  end
  
  module T : sig
    type t
    
    (** Construct a value *)
    val v : collection:string -> event_type:string -> name:string -> type_:Type.t -> ?params:Jsont.json -> ?rule_tag:string -> unit -> t
    
    val collection : t -> string
    
    val event_type : t -> string
    
    val name : t -> string
    
    val params : t -> Jsont.json option
    
    val rule_tag : t -> string option
    
    val type_ : t -> Type.t
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve analytics rules
  
      Retrieve all analytics rules. Use the optional rule_tag filter to narrow down results. 
      @param rule_tag Filter rules by rule_tag
  *)
  val retrieve_analytics_rules : ?rule_tag:string -> t -> unit -> T.t
  
  (** Retrieves an analytics rule
  
      Retrieve the details of an analytics rule, given it's name 
      @param rule_name The name of the analytics rule to retrieve
  *)
  val retrieve_analytics_rule : rule_name:string -> t -> unit -> T.t
  
  (** Upserts an analytics rule
  
      Upserts an analytics rule with the given name. 
      @param rule_name The name of the analytics rule to upsert
  *)
  val upsert_analytics_rule : rule_name:string -> body:Update.t -> t -> unit -> T.t
  
  (** Delete an analytics rule
  
      Permanently deletes an analytics rule, given it's name 
      @param rule_name The name of the analytics rule to delete
  *)
  val delete_analytics_rule : rule_name:string -> t -> unit -> T.t
end

module AnalyticsEvents : sig
  module Response : sig
    type t
    
    (** Construct a value *)
    val v : events:Jsont.json list -> unit -> t
    
    val events : t -> Jsont.json list
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve analytics events
  
      Retrieve the most recent events for a user and rule. 
      @param name Analytics rule name
      @param n Number of events to return (max 1000)
  *)
  val get_analytics_events : user_id:string -> name:string -> n:string -> t -> unit -> Response.t
end

module AnalyticsEvent : sig
  module T : sig
    type t
    
    (** Construct a value
        @param data Event payload
        @param event_type Type of event (e.g., click, conversion, query, visit)
        @param name Name of the analytics rule this event corresponds to
    *)
    val v : data:Jsont.json -> event_type:string -> name:string -> unit -> t
    
    (** Event payload *)
    val data : t -> Jsont.json
    
    (** Type of event (e.g., click, conversion, query, visit) *)
    val event_type : t -> string
    
    (** Name of the analytics rule this event corresponds to *)
    val name : t -> string
    
    val jsont : t Jsont.t
  end
end

module AnalyticsEventCreate : sig
  module Response : sig
    type t
    
    (** Construct a value *)
    val v : ok:bool -> unit -> t
    
    val ok : t -> bool
    
    val jsont : t Jsont.t
  end
  
  (** Create an analytics event
  
      Submit a single analytics event. The event must correspond to an existing analytics rule by name. *)
  val create_analytics_event : body:AnalyticsEvent.T.t -> t -> unit -> Response.t
  
  (** Flush in-memory analytics to disk
  
      Triggers a flush of analytics data to persistent storage. *)
  val flush_analytics : t -> unit -> Response.t
end

module Analytics : sig
  module Status : sig
    type t
    
    (** Construct a value *)
    val v : ?doc_counter_events:int -> ?doc_log_events:int -> ?log_prefix_queries:int -> ?nohits_prefix_queries:int -> ?popular_prefix_queries:int -> ?query_counter_events:int -> ?query_log_events:int -> unit -> t
    
    val doc_counter_events : t -> int option
    
    val doc_log_events : t -> int option
    
    val log_prefix_queries : t -> int option
    
    val nohits_prefix_queries : t -> int option
    
    val popular_prefix_queries : t -> int option
    
    val query_counter_events : t -> int option
    
    val query_log_events : t -> int option
    
    val jsont : t Jsont.t
  end
  
  (** Get analytics subsystem status
  
      Returns sizes of internal analytics buffers and queues. *)
  val get_analytics_status : t -> unit -> Status.t
end
