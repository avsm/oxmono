(** {1 Typesense}

    An open source search engine for building delightful search experiences.

    @version 30.0 *)

type t = {
  session : Requests.t;
  base_url : string;
}

let create ?session ~sw env ~base_url =
  let session = match session with
    | Some s -> s
    | None -> Requests.create ~sw env
  in
  { session; base_url }

let base_url t = t.base_url
let session t = t.session

module VoiceQueryModelCollection = struct
  module Types = struct
    module Config = struct
      (** Configuration for the voice query model
       *)
      type t = {
        model_name : string option;
      }
    end
  end
  
  module Config = struct
    include Types.Config
    
    let v ?model_name () = { model_name }
    
    let model_name t = t.model_name
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VoiceQueryModelCollectionConfig"
        (fun model_name -> { model_name })
      |> Jsont.Object.opt_mem "model_name" Jsont.string ~enc:(fun r -> r.model_name)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SynonymSetDeleteSchema = struct
  module Types = struct
    module T = struct
      type t = {
        name : string;  (** Name of the deleted synonym set *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~name () = { name }
    
    let name t = t.name
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SynonymSetDeleteSchema"
        (fun name -> { name })
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Delete a synonym set
  
      Delete a specific synonym set by its name 
      @param synonym_set_name The name of the synonym set to delete
  *)
  let delete_synonym_set ~synonym_set_name client () =
    let op_name = "delete_synonym_set" in
    let url_path = Openapi.Runtime.Path.render ~params:[("synonymSetName", synonym_set_name)] "/synonym_sets/{synonymSetName}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status;
        body;
        parsed_body;
      })
end

module SynonymItemUpsertSchema = struct
  module Types = struct
    module T = struct
      type t = {
        locale : string option;  (** Locale for the synonym, leave blank to use the standard tokenizer *)
        root : string option;  (** For 1-way synonyms, indicates the root word that words in the synonyms parameter map to *)
        symbols_to_index : string list option;  (** By default, special characters are dropped from synonyms. Use this attribute to specify which special characters should be indexed as is *)
        synonyms : string list;  (** Array of words that should be considered as synonyms *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~synonyms ?locale ?root ?symbols_to_index () = { locale; root; symbols_to_index; synonyms }
    
    let locale t = t.locale
    let root t = t.root
    let symbols_to_index t = t.symbols_to_index
    let synonyms t = t.synonyms
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SynonymItemUpsertSchema"
        (fun locale root symbols_to_index synonyms -> { locale; root; symbols_to_index; synonyms })
      |> Jsont.Object.opt_mem "locale" Jsont.string ~enc:(fun r -> r.locale)
      |> Jsont.Object.opt_mem "root" Jsont.string ~enc:(fun r -> r.root)
      |> Jsont.Object.opt_mem "symbols_to_index" (Jsont.list Jsont.string) ~enc:(fun r -> r.symbols_to_index)
      |> Jsont.Object.mem "synonyms" (Jsont.list Jsont.string) ~enc:(fun r -> r.synonyms)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SynonymItemSchema = struct
  module Types = struct
    module T = struct
      type t = {
        id : string;  (** Unique identifier for the synonym item *)
        locale : string option;  (** Locale for the synonym, leave blank to use the standard tokenizer *)
        root : string option;  (** For 1-way synonyms, indicates the root word that words in the synonyms parameter map to *)
        symbols_to_index : string list option;  (** By default, special characters are dropped from synonyms. Use this attribute to specify which special characters should be indexed as is *)
        synonyms : string list;  (** Array of words that should be considered as synonyms *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~id ~synonyms ?locale ?root ?symbols_to_index () = { id; locale; root; symbols_to_index; synonyms }
    
    let id t = t.id
    let locale t = t.locale
    let root t = t.root
    let symbols_to_index t = t.symbols_to_index
    let synonyms t = t.synonyms
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SynonymItemSchema"
        (fun id locale root symbols_to_index synonyms -> { id; locale; root; symbols_to_index; synonyms })
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "locale" Jsont.string ~enc:(fun r -> r.locale)
      |> Jsont.Object.opt_mem "root" Jsont.string ~enc:(fun r -> r.root)
      |> Jsont.Object.opt_mem "symbols_to_index" (Jsont.list Jsont.string) ~enc:(fun r -> r.symbols_to_index)
      |> Jsont.Object.mem "synonyms" (Jsont.list Jsont.string) ~enc:(fun r -> r.synonyms)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** List items in a synonym set
  
      Retrieve all synonym items in a set 
      @param synonym_set_name The name of the synonym set to retrieve items for
  *)
  let retrieve_synonym_set_items ~synonym_set_name client () =
    let op_name = "retrieve_synonym_set_items" in
    let url_path = Openapi.Runtime.Path.render ~params:[("synonymSetName", synonym_set_name)] "/synonym_sets/{synonymSetName}/items" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status;
        body;
        parsed_body;
      })
  
  (** Retrieve a synonym set item
  
      Retrieve a specific synonym item by its id 
      @param synonym_set_name The name of the synonym set
      @param item_id The id of the synonym item to retrieve
  *)
  let retrieve_synonym_set_item ~synonym_set_name ~item_id client () =
    let op_name = "retrieve_synonym_set_item" in
    let url_path = Openapi.Runtime.Path.render ~params:[("synonymSetName", synonym_set_name); ("itemId", item_id)] "/synonym_sets/{synonymSetName}/items/{itemId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status;
        body;
        parsed_body;
      })
  
  (** Create or update a synonym set item
  
      Create or update a synonym set item with the given id 
      @param synonym_set_name The name of the synonym set
      @param item_id The id of the synonym item to upsert
  *)
  let upsert_synonym_set_item ~synonym_set_name ~item_id ~body client () =
    let op_name = "upsert_synonym_set_item" in
    let url_path = Openapi.Runtime.Path.render ~params:[("synonymSetName", synonym_set_name); ("itemId", item_id)] "/synonym_sets/{synonymSetName}/items/{itemId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json SynonymItemUpsertSchema.T.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 400 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status;
        body;
        parsed_body;
      })
end

module SynonymSetCreateSchema = struct
  module Types = struct
    module T = struct
      type t = {
        items : SynonymItemSchema.T.t list;  (** Array of synonym items *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~items () = { items }
    
    let items t = t.items
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SynonymSetCreateSchema"
        (fun items -> { items })
      |> Jsont.Object.mem "items" (Jsont.list SynonymItemSchema.T.jsont) ~enc:(fun r -> r.items)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SynonymSetSchema = struct
  module Types = struct
    module T = struct
      type t = {
        items : SynonymItemSchema.T.t list;  (** Array of synonym items *)
        name : string;  (** Name of the synonym set *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~items ~name () = { items; name }
    
    let items t = t.items
    let name t = t.name
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SynonymSetSchema"
        (fun items name -> { items; name })
      |> Jsont.Object.mem "items" (Jsont.list SynonymItemSchema.T.jsont) ~enc:(fun r -> r.items)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** List all synonym sets
  
      Retrieve all synonym sets *)
  let retrieve_synonym_sets client () =
    let op_name = "retrieve_synonym_sets" in
    let url_path = "/synonym_sets" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Retrieve a synonym set
  
      Retrieve a specific synonym set by its name 
      @param synonym_set_name The name of the synonym set to retrieve
  *)
  let retrieve_synonym_set ~synonym_set_name client () =
    let op_name = "retrieve_synonym_set" in
    let url_path = Openapi.Runtime.Path.render ~params:[("synonymSetName", synonym_set_name)] "/synonym_sets/{synonymSetName}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status;
        body;
        parsed_body;
      })
  
  (** Create or update a synonym set
  
      Create or update a synonym set with the given name 
      @param synonym_set_name The name of the synonym set to create/update
  *)
  let upsert_synonym_set ~synonym_set_name ~body client () =
    let op_name = "upsert_synonym_set" in
    let url_path = Openapi.Runtime.Path.render ~params:[("synonymSetName", synonym_set_name)] "/synonym_sets/{synonymSetName}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json SynonymSetCreateSchema.T.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 400 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status;
        body;
        parsed_body;
      })
end

module SynonymSetsRetrieveSchema = struct
  module Types = struct
    module T = struct
      type t = {
        synonym_sets : SynonymSetSchema.T.t list;  (** Array of synonym sets *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~synonym_sets () = { synonym_sets }
    
    let synonym_sets t = t.synonym_sets
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SynonymSetsRetrieveSchema"
        (fun synonym_sets -> { synonym_sets })
      |> Jsont.Object.mem "synonym_sets" (Jsont.list SynonymSetSchema.T.jsont) ~enc:(fun r -> r.synonym_sets)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SynonymItemDeleteSchema = struct
  module Types = struct
    module T = struct
      type t = {
        id : string;  (** ID of the deleted synonym item *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~id () = { id }
    
    let id t = t.id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SynonymItemDeleteSchema"
        (fun id -> { id })
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Delete a synonym set item
  
      Delete a specific synonym item by its id 
      @param synonym_set_name The name of the synonym set
      @param item_id The id of the synonym item to delete
  *)
  let delete_synonym_set_item ~synonym_set_name ~item_id client () =
    let op_name = "delete_synonym_set_item" in
    let url_path = Openapi.Runtime.Path.render ~params:[("synonymSetName", synonym_set_name); ("itemId", item_id)] "/synonym_sets/{synonymSetName}/items/{itemId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status;
        body;
        parsed_body;
      })
end

module Success = struct
  module Types = struct
    module Status = struct
      type t = {
        success : bool;
      }
    end
  end
  
  module Status = struct
    include Types.Status
    
    let v ~success () = { success }
    
    let success t = t.success
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SuccessStatus"
        (fun success -> { success })
      |> Jsont.Object.mem "success" Jsont.bool ~enc:(fun r -> r.success)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Toggle Slow Request Log
  
      Enable logging of requests that take over a defined threshold of time. Default is `-1` which disables slow request logging. Slow requests are logged to the primary log file, with the prefix SLOW REQUEST. *)
  let toggle_slow_request_log client () =
    let op_name = "toggle_slow_request_log" in
    let url_path = "/config" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Status.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Clear the cached responses of search requests in the LRU cache.
  
      Clear the cached responses of search requests that are sent with `use_cache` parameter in the LRU cache. *)
  let clear_cache client () =
    let op_name = "clear_cache" in
    let url_path = "/operations/cache/clear" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Status.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Compacting the on-disk database
  
      Typesense uses RocksDB to store your documents on the disk. If you do frequent writes or updates, you could benefit from running a compaction of the underlying RocksDB database. This could reduce the size of the database and decrease read latency. While the database will not block during this operation, we recommend running it during off-peak hours. *)
  let compact_db client () =
    let op_name = "compact_db" in
    let url_path = "/operations/db/compact" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Status.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Creates a point-in-time snapshot of a Typesense node's state and data in the specified directory.
  
      Creates a point-in-time snapshot of a Typesense node's state and data in the specified directory. You can then backup the snapshot directory that gets created and later restore it as a data directory, as needed. 
      @param snapshot_path The directory on the server where the snapshot should be saved.
  *)
  let take_snapshot ~snapshot_path client () =
    let op_name = "take_snapshot" in
    let url_path = "/operations/snapshot" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.singleton ~key:"snapshot_path" ~value:snapshot_path]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Status.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Triggers a follower node to initiate the raft voting process, which triggers leader re-election.
  
      Triggers a follower node to initiate the raft voting process, which triggers leader re-election. The follower node that you run this operation against will become the new leader, once this command succeeds. *)
  let vote client () =
    let op_name = "vote" in
    let url_path = "/operations/vote" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Status.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module StopwordsSetUpsertSchema = struct
  module Types = struct
    module T = struct
      type t = {
        locale : string option;
        stopwords : string list;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~stopwords ?locale () = { locale; stopwords }
    
    let locale t = t.locale
    let stopwords t = t.stopwords
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"StopwordsSetUpsertSchema"
        (fun locale stopwords -> { locale; stopwords })
      |> Jsont.Object.opt_mem "locale" Jsont.string ~enc:(fun r -> r.locale)
      |> Jsont.Object.mem "stopwords" (Jsont.list Jsont.string) ~enc:(fun r -> r.stopwords)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module StopwordsSetSchema = struct
  module Types = struct
    module T = struct
      type t = {
        id : string;
        locale : string option;
        stopwords : string list;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~id ~stopwords ?locale () = { id; locale; stopwords }
    
    let id t = t.id
    let locale t = t.locale
    let stopwords t = t.stopwords
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"StopwordsSetSchema"
        (fun id locale stopwords -> { id; locale; stopwords })
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "locale" Jsont.string ~enc:(fun r -> r.locale)
      |> Jsont.Object.mem "stopwords" (Jsont.list Jsont.string) ~enc:(fun r -> r.stopwords)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Upserts a stopwords set.
  
      When an analytics rule is created, we give it a name and describe the type, the source collections and the destination collection. 
      @param set_id The ID of the stopwords set to upsert.
  *)
  let upsert_stopwords_set ~set_id ~body client () =
    let op_name = "upsert_stopwords_set" in
    let url_path = Openapi.Runtime.Path.render ~params:[("setId", set_id)] "/stopwords/{setId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json StopwordsSetUpsertSchema.T.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 400 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status;
        body;
        parsed_body;
      })
end

module StopwordsSetsRetrieveAllSchema = struct
  module Types = struct
    module T = struct
      type t = {
        stopwords : StopwordsSetSchema.T.t list;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~stopwords () = { stopwords }
    
    let stopwords t = t.stopwords
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"StopwordsSetsRetrieveAllSchema"
        (fun stopwords -> { stopwords })
      |> Jsont.Object.mem "stopwords" (Jsont.list StopwordsSetSchema.T.jsont) ~enc:(fun r -> r.stopwords)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieves all stopwords sets.
  
      Retrieve the details of all stopwords sets *)
  let retrieve_stopwords_sets client () =
    let op_name = "retrieve_stopwords_sets" in
    let url_path = "/stopwords" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module StopwordsSetRetrieveSchema = struct
  module Types = struct
    module T = struct
      type t = {
        stopwords : StopwordsSetSchema.T.t;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~stopwords () = { stopwords }
    
    let stopwords t = t.stopwords
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"StopwordsSetRetrieveSchema"
        (fun stopwords -> { stopwords })
      |> Jsont.Object.mem "stopwords" StopwordsSetSchema.T.jsont ~enc:(fun r -> r.stopwords)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieves a stopwords set.
  
      Retrieve the details of a stopwords set, given it's name. 
      @param set_id The ID of the stopwords set to retrieve.
  *)
  let retrieve_stopwords_set ~set_id client () =
    let op_name = "retrieve_stopwords_set" in
    let url_path = Openapi.Runtime.Path.render ~params:[("setId", set_id)] "/stopwords/{setId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status;
        body;
        parsed_body;
      })
end

module StemmingDictionary = struct
  module Types = struct
    module T = struct
      type t = {
        id : string;  (** Unique identifier for the dictionary *)
        words : Jsont.json list;  (** List of word mappings in the dictionary *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~id ~words () = { id; words }
    
    let id t = t.id
    let words t = t.words
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"StemmingDictionary"
        (fun id words -> { id; words })
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "words" (Jsont.list Jsont.json) ~enc:(fun r -> r.words)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve a stemming dictionary
  
      Fetch details of a specific stemming dictionary. 
      @param dictionary_id The ID of the dictionary to retrieve
  *)
  let get_stemming_dictionary ~dictionary_id client () =
    let op_name = "get_stemming_dictionary" in
    let url_path = Openapi.Runtime.Path.render ~params:[("dictionaryId", dictionary_id)] "/stemming/dictionaries/{dictionaryId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status;
        body;
        parsed_body;
      })
end

module SearchSynonymSchema = struct
  module Types = struct
    module T = struct
      type t = {
        locale : string option;  (** Locale for the synonym, leave blank to use the standard tokenizer. *)
        root : string option;  (** For 1-way synonyms, indicates the root word that words in the `synonyms` parameter map to. *)
        symbols_to_index : string list option;  (** By default, special characters are dropped from synonyms. Use this attribute to specify which special characters should be indexed as is. *)
        synonyms : string list;  (** Array of words that should be considered as synonyms. *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~synonyms ?locale ?root ?symbols_to_index () = { locale; root; symbols_to_index; synonyms }
    
    let locale t = t.locale
    let root t = t.root
    let symbols_to_index t = t.symbols_to_index
    let synonyms t = t.synonyms
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SearchSynonymSchema"
        (fun locale root symbols_to_index synonyms -> { locale; root; symbols_to_index; synonyms })
      |> Jsont.Object.opt_mem "locale" Jsont.string ~enc:(fun r -> r.locale)
      |> Jsont.Object.opt_mem "root" Jsont.string ~enc:(fun r -> r.root)
      |> Jsont.Object.opt_mem "symbols_to_index" (Jsont.list Jsont.string) ~enc:(fun r -> r.symbols_to_index)
      |> Jsont.Object.mem "synonyms" (Jsont.list Jsont.string) ~enc:(fun r -> r.synonyms)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SearchSynonymDelete = struct
  module Types = struct
    module Response = struct
      type t = {
        id : string;  (** The id of the synonym that was deleted *)
      }
    end
  end
  
  module Response = struct
    include Types.Response
    
    let v ~id () = { id }
    
    let id t = t.id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SearchSynonymDeleteResponse"
        (fun id -> { id })
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SearchSynonym = struct
  module Types = struct
    module T = struct
      type t = {
        locale : string option;  (** Locale for the synonym, leave blank to use the standard tokenizer. *)
        root : string option;  (** For 1-way synonyms, indicates the root word that words in the `synonyms` parameter map to. *)
        symbols_to_index : string list option;  (** By default, special characters are dropped from synonyms. Use this attribute to specify which special characters should be indexed as is. *)
        synonyms : string list;  (** Array of words that should be considered as synonyms. *)
        id : string;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~synonyms ~id ?locale ?root ?symbols_to_index () = { locale; root; symbols_to_index; synonyms; id }
    
    let locale t = t.locale
    let root t = t.root
    let symbols_to_index t = t.symbols_to_index
    let synonyms t = t.synonyms
    let id t = t.id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SearchSynonym"
        (fun locale root symbols_to_index synonyms id -> { locale; root; symbols_to_index; synonyms; id })
      |> Jsont.Object.opt_mem "locale" Jsont.string ~enc:(fun r -> r.locale)
      |> Jsont.Object.opt_mem "root" Jsont.string ~enc:(fun r -> r.root)
      |> Jsont.Object.opt_mem "symbols_to_index" (Jsont.list Jsont.string) ~enc:(fun r -> r.symbols_to_index)
      |> Jsont.Object.mem "synonyms" (Jsont.list Jsont.string) ~enc:(fun r -> r.synonyms)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SearchSynonyms = struct
  module Types = struct
    module Response = struct
      type t = {
        synonyms : SearchSynonym.T.t list;
      }
    end
  end
  
  module Response = struct
    include Types.Response
    
    let v ~synonyms () = { synonyms }
    
    let synonyms t = t.synonyms
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SearchSynonymsResponse"
        (fun synonyms -> { synonyms })
      |> Jsont.Object.mem "synonyms" (Jsont.list SearchSynonym.T.jsont) ~enc:(fun r -> r.synonyms)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SearchResultConversation = struct
  module Types = struct
    module T = struct
      type t = {
        answer : string;
        conversation_history : Jsont.json list;
        conversation_id : string;
        query : string;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~answer ~conversation_history ~conversation_id ~query () = { answer; conversation_history; conversation_id; query }
    
    let answer t = t.answer
    let conversation_history t = t.conversation_history
    let conversation_id t = t.conversation_id
    let query t = t.query
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SearchResultConversation"
        (fun answer conversation_history conversation_id query -> { answer; conversation_history; conversation_id; query })
      |> Jsont.Object.mem "answer" Jsont.string ~enc:(fun r -> r.answer)
      |> Jsont.Object.mem "conversation_history" (Jsont.list Jsont.json) ~enc:(fun r -> r.conversation_history)
      |> Jsont.Object.mem "conversation_id" Jsont.string ~enc:(fun r -> r.conversation_id)
      |> Jsont.Object.mem "query" Jsont.string ~enc:(fun r -> r.query)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SearchRequestParams = struct
  module Types = struct
    module T = struct
      type t = {
        collection_name : string;
        per_page : int;
        q : string;
        voice_query : Jsont.json option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~collection_name ~per_page ~q ?voice_query () = { collection_name; per_page; q; voice_query }
    
    let collection_name t = t.collection_name
    let per_page t = t.per_page
    let q t = t.q
    let voice_query t = t.voice_query
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SearchRequestParams"
        (fun collection_name per_page q voice_query -> { collection_name; per_page; q; voice_query })
      |> Jsont.Object.mem "collection_name" Jsont.string ~enc:(fun r -> r.collection_name)
      |> Jsont.Object.mem "per_page" Jsont.int ~enc:(fun r -> r.per_page)
      |> Jsont.Object.mem "q" Jsont.string ~enc:(fun r -> r.q)
      |> Jsont.Object.opt_mem "voice_query" Jsont.json ~enc:(fun r -> r.voice_query)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SearchHighlight = struct
  module Types = struct
    module T = struct
      type t = {
        field : string option;
        indices : int list option;  (** The indices property will be present only for string[] fields and will contain the corresponding indices of the snippets in the search field *)
        matched_tokens : Jsont.json list option;
        snippet : string option;  (** Present only for (non-array) string fields *)
        snippets : string list option;  (** Present only for (array) string[] fields *)
        value : string option;  (** Full field value with highlighting, present only for (non-array) string fields *)
        values : string list option;  (** Full field value with highlighting, present only for (array) string[] fields *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?field ?indices ?matched_tokens ?snippet ?snippets ?value ?values () = { field; indices; matched_tokens; snippet; snippets; value; values }
    
    let field t = t.field
    let indices t = t.indices
    let matched_tokens t = t.matched_tokens
    let snippet t = t.snippet
    let snippets t = t.snippets
    let value t = t.value
    let values t = t.values
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SearchHighlight"
        (fun field indices matched_tokens snippet snippets value values -> { field; indices; matched_tokens; snippet; snippets; value; values })
      |> Jsont.Object.opt_mem "field" Jsont.string ~enc:(fun r -> r.field)
      |> Jsont.Object.opt_mem "indices" (Jsont.list Jsont.int) ~enc:(fun r -> r.indices)
      |> Jsont.Object.opt_mem "matched_tokens" (Jsont.list Jsont.json) ~enc:(fun r -> r.matched_tokens)
      |> Jsont.Object.opt_mem "snippet" Jsont.string ~enc:(fun r -> r.snippet)
      |> Jsont.Object.opt_mem "snippets" (Jsont.list Jsont.string) ~enc:(fun r -> r.snippets)
      |> Jsont.Object.opt_mem "value" Jsont.string ~enc:(fun r -> r.value)
      |> Jsont.Object.opt_mem "values" (Jsont.list Jsont.string) ~enc:(fun r -> r.values)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SearchResultHit = struct
  module Types = struct
    module T = struct
      type t = {
        document : Jsont.json option;  (** Can be any key-value pair *)
        geo_distance_meters : Jsont.json option;  (** Can be any key-value pair *)
        highlight : Jsont.json option;  (** Highlighted version of the matching document *)
        highlights : SearchHighlight.T.t list option;  (** (Deprecated) Contains highlighted portions of the search fields *)
        hybrid_search_info : Jsont.json option;  (** Information about hybrid search scoring *)
        search_index : int option;  (** Returned only for union query response. Indicates the index of the query which this document matched to. *)
        text_match : int64 option;
        text_match_info : Jsont.json option;
        vector_distance : float option;  (** Distance between the query vector and matching document's vector value *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?document ?geo_distance_meters ?highlight ?highlights ?hybrid_search_info ?search_index ?text_match ?text_match_info ?vector_distance () = { document; geo_distance_meters; highlight; highlights; hybrid_search_info; search_index; text_match; text_match_info; vector_distance }
    
    let document t = t.document
    let geo_distance_meters t = t.geo_distance_meters
    let highlight t = t.highlight
    let highlights t = t.highlights
    let hybrid_search_info t = t.hybrid_search_info
    let search_index t = t.search_index
    let text_match t = t.text_match
    let text_match_info t = t.text_match_info
    let vector_distance t = t.vector_distance
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SearchResultHit"
        (fun document geo_distance_meters highlight highlights hybrid_search_info search_index text_match text_match_info vector_distance -> { document; geo_distance_meters; highlight; highlights; hybrid_search_info; search_index; text_match; text_match_info; vector_distance })
      |> Jsont.Object.opt_mem "document" Jsont.json ~enc:(fun r -> r.document)
      |> Jsont.Object.opt_mem "geo_distance_meters" Jsont.json ~enc:(fun r -> r.geo_distance_meters)
      |> Jsont.Object.opt_mem "highlight" Jsont.json ~enc:(fun r -> r.highlight)
      |> Jsont.Object.opt_mem "highlights" (Jsont.list SearchHighlight.T.jsont) ~enc:(fun r -> r.highlights)
      |> Jsont.Object.opt_mem "hybrid_search_info" Jsont.json ~enc:(fun r -> r.hybrid_search_info)
      |> Jsont.Object.opt_mem "search_index" Jsont.int ~enc:(fun r -> r.search_index)
      |> Jsont.Object.opt_mem "text_match" Jsont.int64 ~enc:(fun r -> r.text_match)
      |> Jsont.Object.opt_mem "text_match_info" Jsont.json ~enc:(fun r -> r.text_match_info)
      |> Jsont.Object.opt_mem "vector_distance" Jsont.number ~enc:(fun r -> r.vector_distance)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SearchGroupedHit = struct
  module Types = struct
    module T = struct
      type t = {
        found : int option;
        group_key : Jsont.json list;
        hits : SearchResultHit.T.t list;  (** The documents that matched the search query *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~group_key ~hits ?found () = { found; group_key; hits }
    
    let found t = t.found
    let group_key t = t.group_key
    let hits t = t.hits
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SearchGroupedHit"
        (fun found group_key hits -> { found; group_key; hits })
      |> Jsont.Object.opt_mem "found" Jsont.int ~enc:(fun r -> r.found)
      |> Jsont.Object.mem "group_key" (Jsont.list Jsont.json) ~enc:(fun r -> r.group_key)
      |> Jsont.Object.mem "hits" (Jsont.list SearchResultHit.T.jsont) ~enc:(fun r -> r.hits)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module SchemaChange = struct
  module Types = struct
    module Status = struct
      type t = {
        altered_docs : int option;  (** Number of documents that have been altered *)
        collection : string option;  (** Name of the collection being modified *)
        validated_docs : int option;  (** Number of documents that have been validated *)
      }
    end
  end
  
  module Status = struct
    include Types.Status
    
    let v ?altered_docs ?collection ?validated_docs () = { altered_docs; collection; validated_docs }
    
    let altered_docs t = t.altered_docs
    let collection t = t.collection
    let validated_docs t = t.validated_docs
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SchemaChangeStatus"
        (fun altered_docs collection validated_docs -> { altered_docs; collection; validated_docs })
      |> Jsont.Object.opt_mem "altered_docs" Jsont.int ~enc:(fun r -> r.altered_docs)
      |> Jsont.Object.opt_mem "collection" Jsont.string ~enc:(fun r -> r.collection)
      |> Jsont.Object.opt_mem "validated_docs" Jsont.int ~enc:(fun r -> r.validated_docs)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get the status of in-progress schema change operations
  
      Returns the status of any ongoing schema change operations. If no schema changes are in progress, returns an empty response. *)
  let get_schema_changes client () =
    let op_name = "get_schema_changes" in
    let url_path = "/operations/schema_changes" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Status.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module PresetUpsertSchema = struct
  module Types = struct
    module T = struct
      type t = {
        value : Jsont.json;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~value () = { value }
    
    let value t = t.value
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PresetUpsertSchema"
        (fun value -> { value })
      |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module PresetSchema = struct
  module Types = struct
    module T = struct
      type t = {
        value : Jsont.json;
        name : string;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~value ~name () = { value; name }
    
    let value t = t.value
    let name t = t.name
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PresetSchema"
        (fun value name -> { value; name })
      |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieves a preset.
  
      Retrieve the details of a preset, given it's name. 
      @param preset_id The ID of the preset to retrieve.
  *)
  let retrieve_preset ~preset_id client () =
    let op_name = "retrieve_preset" in
    let url_path = Openapi.Runtime.Path.render ~params:[("presetId", preset_id)] "/presets/{presetId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status;
        body;
        parsed_body;
      })
  
  (** Upserts a preset.
  
      Create or update an existing preset. 
      @param preset_id The name of the preset set to upsert.
  *)
  let upsert_preset ~preset_id ~body client () =
    let op_name = "upsert_preset" in
    let url_path = Openapi.Runtime.Path.render ~params:[("presetId", preset_id)] "/presets/{presetId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json PresetUpsertSchema.T.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 400 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status;
        body;
        parsed_body;
      })
end

module PresetsRetrieveSchema = struct
  module Types = struct
    module T = struct
      type t = {
        presets : PresetSchema.T.t list;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~presets () = { presets }
    
    let presets t = t.presets
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PresetsRetrieveSchema"
        (fun presets -> { presets })
      |> Jsont.Object.mem "presets" (Jsont.list PresetSchema.T.jsont) ~enc:(fun r -> r.presets)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieves all presets.
  
      Retrieve the details of all presets *)
  let retrieve_all_presets client () =
    let op_name = "retrieve_all_presets" in
    let url_path = "/presets" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module PresetDeleteSchema = struct
  module Types = struct
    module T = struct
      type t = {
        name : string;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~name () = { name }
    
    let name t = t.name
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PresetDeleteSchema"
        (fun name -> { name })
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Delete a preset.
  
      Permanently deletes a preset, given it's name. 
      @param preset_id The ID of the preset to delete.
  *)
  let delete_preset ~preset_id client () =
    let op_name = "delete_preset" in
    let url_path = Openapi.Runtime.Path.render ~params:[("presetId", preset_id)] "/presets/{presetId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status;
        body;
        parsed_body;
      })
end

module NlsearchModelDeleteSchema = struct
  module Types = struct
    module T = struct
      type t = {
        id : string;  (** ID of the deleted NL search model *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~id () = { id }
    
    let id t = t.id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"NLSearchModelDeleteSchema"
        (fun id -> { id })
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Delete a NL search model
  
      Delete a specific NL search model by its ID. 
      @param model_id The ID of the NL search model to delete
  *)
  let delete_nlsearch_model ~model_id client () =
    let op_name = "delete_nlsearch_model" in
    let url_path = Openapi.Runtime.Path.render ~params:[("modelId", model_id)] "/nl_search_models/{modelId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status;
        body;
        parsed_body;
      })
end

module NlsearchModelCreateSchema = struct
  module Types = struct
    module T = struct
      type t = {
        access_token : string option;  (** Access token for GCP Vertex AI *)
        account_id : string option;  (** Account ID for Cloudflare-specific models *)
        api_key : string option;  (** API key for the NL model service *)
        api_url : string option;  (** Custom API URL for the NL model service *)
        api_version : string option;  (** API version for the NL model service *)
        client_id : string option;  (** Client ID for GCP Vertex AI *)
        client_secret : string option;  (** Client secret for GCP Vertex AI *)
        max_bytes : int option;  (** Maximum number of bytes to process *)
        max_output_tokens : int option;  (** Maximum output tokens for GCP Vertex AI *)
        model_name : string option;  (** Name of the NL model to use *)
        project_id : string option;  (** Project ID for GCP Vertex AI *)
        refresh_token : string option;  (** Refresh token for GCP Vertex AI *)
        region : string option;  (** Region for GCP Vertex AI *)
        stop_sequences : string list option;  (** Stop sequences for the NL model (Google-specific) *)
        system_prompt : string option;  (** System prompt for the NL model *)
        temperature : float option;  (** Temperature parameter for the NL model *)
        top_k : int option;  (** Top-k parameter for the NL model (Google-specific) *)
        top_p : float option;  (** Top-p parameter for the NL model (Google-specific) *)
        id : string option;  (** Optional ID for the NL search model *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?access_token ?account_id ?api_key ?api_url ?api_version ?client_id ?client_secret ?max_bytes ?max_output_tokens ?model_name ?project_id ?refresh_token ?region ?stop_sequences ?system_prompt ?temperature ?top_k ?top_p ?id () = { access_token; account_id; api_key; api_url; api_version; client_id; client_secret; max_bytes; max_output_tokens; model_name; project_id; refresh_token; region; stop_sequences; system_prompt; temperature; top_k; top_p; id }
    
    let access_token t = t.access_token
    let account_id t = t.account_id
    let api_key t = t.api_key
    let api_url t = t.api_url
    let api_version t = t.api_version
    let client_id t = t.client_id
    let client_secret t = t.client_secret
    let max_bytes t = t.max_bytes
    let max_output_tokens t = t.max_output_tokens
    let model_name t = t.model_name
    let project_id t = t.project_id
    let refresh_token t = t.refresh_token
    let region t = t.region
    let stop_sequences t = t.stop_sequences
    let system_prompt t = t.system_prompt
    let temperature t = t.temperature
    let top_k t = t.top_k
    let top_p t = t.top_p
    let id t = t.id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"NLSearchModelCreateSchema"
        (fun access_token account_id api_key api_url api_version client_id client_secret max_bytes max_output_tokens model_name project_id refresh_token region stop_sequences system_prompt temperature top_k top_p id -> { access_token; account_id; api_key; api_url; api_version; client_id; client_secret; max_bytes; max_output_tokens; model_name; project_id; refresh_token; region; stop_sequences; system_prompt; temperature; top_k; top_p; id })
      |> Jsont.Object.opt_mem "access_token" Jsont.string ~enc:(fun r -> r.access_token)
      |> Jsont.Object.opt_mem "account_id" Jsont.string ~enc:(fun r -> r.account_id)
      |> Jsont.Object.opt_mem "api_key" Jsont.string ~enc:(fun r -> r.api_key)
      |> Jsont.Object.opt_mem "api_url" Jsont.string ~enc:(fun r -> r.api_url)
      |> Jsont.Object.opt_mem "api_version" Jsont.string ~enc:(fun r -> r.api_version)
      |> Jsont.Object.opt_mem "client_id" Jsont.string ~enc:(fun r -> r.client_id)
      |> Jsont.Object.opt_mem "client_secret" Jsont.string ~enc:(fun r -> r.client_secret)
      |> Jsont.Object.opt_mem "max_bytes" Jsont.int ~enc:(fun r -> r.max_bytes)
      |> Jsont.Object.opt_mem "max_output_tokens" Jsont.int ~enc:(fun r -> r.max_output_tokens)
      |> Jsont.Object.opt_mem "model_name" Jsont.string ~enc:(fun r -> r.model_name)
      |> Jsont.Object.opt_mem "project_id" Jsont.string ~enc:(fun r -> r.project_id)
      |> Jsont.Object.opt_mem "refresh_token" Jsont.string ~enc:(fun r -> r.refresh_token)
      |> Jsont.Object.opt_mem "region" Jsont.string ~enc:(fun r -> r.region)
      |> Jsont.Object.opt_mem "stop_sequences" (Jsont.list Jsont.string) ~enc:(fun r -> r.stop_sequences)
      |> Jsont.Object.opt_mem "system_prompt" Jsont.string ~enc:(fun r -> r.system_prompt)
      |> Jsont.Object.opt_mem "temperature" Jsont.number ~enc:(fun r -> r.temperature)
      |> Jsont.Object.opt_mem "top_k" Jsont.int ~enc:(fun r -> r.top_k)
      |> Jsont.Object.opt_mem "top_p" Jsont.number ~enc:(fun r -> r.top_p)
      |> Jsont.Object.opt_mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module NlsearchModelSchema = struct
  module Types = struct
    module T = struct
      type t = {
        access_token : string option;  (** Access token for GCP Vertex AI *)
        account_id : string option;  (** Account ID for Cloudflare-specific models *)
        api_key : string option;  (** API key for the NL model service *)
        api_url : string option;  (** Custom API URL for the NL model service *)
        api_version : string option;  (** API version for the NL model service *)
        client_id : string option;  (** Client ID for GCP Vertex AI *)
        client_secret : string option;  (** Client secret for GCP Vertex AI *)
        max_bytes : int option;  (** Maximum number of bytes to process *)
        max_output_tokens : int option;  (** Maximum output tokens for GCP Vertex AI *)
        model_name : string option;  (** Name of the NL model to use *)
        project_id : string option;  (** Project ID for GCP Vertex AI *)
        refresh_token : string option;  (** Refresh token for GCP Vertex AI *)
        region : string option;  (** Region for GCP Vertex AI *)
        stop_sequences : string list option;  (** Stop sequences for the NL model (Google-specific) *)
        system_prompt : string option;  (** System prompt for the NL model *)
        temperature : float option;  (** Temperature parameter for the NL model *)
        top_k : int option;  (** Top-k parameter for the NL model (Google-specific) *)
        top_p : float option;  (** Top-p parameter for the NL model (Google-specific) *)
        id : string;  (** ID of the NL search model *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~id ?access_token ?account_id ?api_key ?api_url ?api_version ?client_id ?client_secret ?max_bytes ?max_output_tokens ?model_name ?project_id ?refresh_token ?region ?stop_sequences ?system_prompt ?temperature ?top_k ?top_p () = { access_token; account_id; api_key; api_url; api_version; client_id; client_secret; max_bytes; max_output_tokens; model_name; project_id; refresh_token; region; stop_sequences; system_prompt; temperature; top_k; top_p; id }
    
    let access_token t = t.access_token
    let account_id t = t.account_id
    let api_key t = t.api_key
    let api_url t = t.api_url
    let api_version t = t.api_version
    let client_id t = t.client_id
    let client_secret t = t.client_secret
    let max_bytes t = t.max_bytes
    let max_output_tokens t = t.max_output_tokens
    let model_name t = t.model_name
    let project_id t = t.project_id
    let refresh_token t = t.refresh_token
    let region t = t.region
    let stop_sequences t = t.stop_sequences
    let system_prompt t = t.system_prompt
    let temperature t = t.temperature
    let top_k t = t.top_k
    let top_p t = t.top_p
    let id t = t.id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"NLSearchModelSchema"
        (fun access_token account_id api_key api_url api_version client_id client_secret max_bytes max_output_tokens model_name project_id refresh_token region stop_sequences system_prompt temperature top_k top_p id -> { access_token; account_id; api_key; api_url; api_version; client_id; client_secret; max_bytes; max_output_tokens; model_name; project_id; refresh_token; region; stop_sequences; system_prompt; temperature; top_k; top_p; id })
      |> Jsont.Object.opt_mem "access_token" Jsont.string ~enc:(fun r -> r.access_token)
      |> Jsont.Object.opt_mem "account_id" Jsont.string ~enc:(fun r -> r.account_id)
      |> Jsont.Object.opt_mem "api_key" Jsont.string ~enc:(fun r -> r.api_key)
      |> Jsont.Object.opt_mem "api_url" Jsont.string ~enc:(fun r -> r.api_url)
      |> Jsont.Object.opt_mem "api_version" Jsont.string ~enc:(fun r -> r.api_version)
      |> Jsont.Object.opt_mem "client_id" Jsont.string ~enc:(fun r -> r.client_id)
      |> Jsont.Object.opt_mem "client_secret" Jsont.string ~enc:(fun r -> r.client_secret)
      |> Jsont.Object.opt_mem "max_bytes" Jsont.int ~enc:(fun r -> r.max_bytes)
      |> Jsont.Object.opt_mem "max_output_tokens" Jsont.int ~enc:(fun r -> r.max_output_tokens)
      |> Jsont.Object.opt_mem "model_name" Jsont.string ~enc:(fun r -> r.model_name)
      |> Jsont.Object.opt_mem "project_id" Jsont.string ~enc:(fun r -> r.project_id)
      |> Jsont.Object.opt_mem "refresh_token" Jsont.string ~enc:(fun r -> r.refresh_token)
      |> Jsont.Object.opt_mem "region" Jsont.string ~enc:(fun r -> r.region)
      |> Jsont.Object.opt_mem "stop_sequences" (Jsont.list Jsont.string) ~enc:(fun r -> r.stop_sequences)
      |> Jsont.Object.opt_mem "system_prompt" Jsont.string ~enc:(fun r -> r.system_prompt)
      |> Jsont.Object.opt_mem "temperature" Jsont.number ~enc:(fun r -> r.temperature)
      |> Jsont.Object.opt_mem "top_k" Jsont.int ~enc:(fun r -> r.top_k)
      |> Jsont.Object.opt_mem "top_p" Jsont.number ~enc:(fun r -> r.top_p)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** List all NL search models
  
      Retrieve all NL search models. *)
  let retrieve_all_nlsearch_models client () =
    let op_name = "retrieve_all_nlsearch_models" in
    let url_path = "/nl_search_models" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Create a NL search model
  
      Create a new NL search model. *)
  let create_nlsearch_model ~body client () =
    let op_name = "create_nlsearch_model" in
    let url_path = "/nl_search_models" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json NlsearchModelCreateSchema.T.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 400 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status;
        body;
        parsed_body;
      })
  
  (** Retrieve a NL search model
  
      Retrieve a specific NL search model by its ID. 
      @param model_id The ID of the NL search model to retrieve
  *)
  let retrieve_nlsearch_model ~model_id client () =
    let op_name = "retrieve_nlsearch_model" in
    let url_path = Openapi.Runtime.Path.render ~params:[("modelId", model_id)] "/nl_search_models/{modelId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status;
        body;
        parsed_body;
      })
  
  (** Update a NL search model
  
      Update an existing NL search model. 
      @param model_id The ID of the NL search model to update
  *)
  let update_nlsearch_model ~model_id client () =
    let op_name = "update_nlsearch_model" in
    let url_path = Openapi.Runtime.Path.render ~params:[("modelId", model_id)] "/nl_search_models/{modelId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 400 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status;
        body;
        parsed_body;
      })
end

module NlsearchModelBase = struct
  module Types = struct
    module T = struct
      type t = {
        access_token : string option;  (** Access token for GCP Vertex AI *)
        account_id : string option;  (** Account ID for Cloudflare-specific models *)
        api_key : string option;  (** API key for the NL model service *)
        api_url : string option;  (** Custom API URL for the NL model service *)
        api_version : string option;  (** API version for the NL model service *)
        client_id : string option;  (** Client ID for GCP Vertex AI *)
        client_secret : string option;  (** Client secret for GCP Vertex AI *)
        max_bytes : int option;  (** Maximum number of bytes to process *)
        max_output_tokens : int option;  (** Maximum output tokens for GCP Vertex AI *)
        model_name : string option;  (** Name of the NL model to use *)
        project_id : string option;  (** Project ID for GCP Vertex AI *)
        refresh_token : string option;  (** Refresh token for GCP Vertex AI *)
        region : string option;  (** Region for GCP Vertex AI *)
        stop_sequences : string list option;  (** Stop sequences for the NL model (Google-specific) *)
        system_prompt : string option;  (** System prompt for the NL model *)
        temperature : float option;  (** Temperature parameter for the NL model *)
        top_k : int option;  (** Top-k parameter for the NL model (Google-specific) *)
        top_p : float option;  (** Top-p parameter for the NL model (Google-specific) *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?access_token ?account_id ?api_key ?api_url ?api_version ?client_id ?client_secret ?max_bytes ?max_output_tokens ?model_name ?project_id ?refresh_token ?region ?stop_sequences ?system_prompt ?temperature ?top_k ?top_p () = { access_token; account_id; api_key; api_url; api_version; client_id; client_secret; max_bytes; max_output_tokens; model_name; project_id; refresh_token; region; stop_sequences; system_prompt; temperature; top_k; top_p }
    
    let access_token t = t.access_token
    let account_id t = t.account_id
    let api_key t = t.api_key
    let api_url t = t.api_url
    let api_version t = t.api_version
    let client_id t = t.client_id
    let client_secret t = t.client_secret
    let max_bytes t = t.max_bytes
    let max_output_tokens t = t.max_output_tokens
    let model_name t = t.model_name
    let project_id t = t.project_id
    let refresh_token t = t.refresh_token
    let region t = t.region
    let stop_sequences t = t.stop_sequences
    let system_prompt t = t.system_prompt
    let temperature t = t.temperature
    let top_k t = t.top_k
    let top_p t = t.top_p
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"NLSearchModelBase"
        (fun access_token account_id api_key api_url api_version client_id client_secret max_bytes max_output_tokens model_name project_id refresh_token region stop_sequences system_prompt temperature top_k top_p -> { access_token; account_id; api_key; api_url; api_version; client_id; client_secret; max_bytes; max_output_tokens; model_name; project_id; refresh_token; region; stop_sequences; system_prompt; temperature; top_k; top_p })
      |> Jsont.Object.opt_mem "access_token" Jsont.string ~enc:(fun r -> r.access_token)
      |> Jsont.Object.opt_mem "account_id" Jsont.string ~enc:(fun r -> r.account_id)
      |> Jsont.Object.opt_mem "api_key" Jsont.string ~enc:(fun r -> r.api_key)
      |> Jsont.Object.opt_mem "api_url" Jsont.string ~enc:(fun r -> r.api_url)
      |> Jsont.Object.opt_mem "api_version" Jsont.string ~enc:(fun r -> r.api_version)
      |> Jsont.Object.opt_mem "client_id" Jsont.string ~enc:(fun r -> r.client_id)
      |> Jsont.Object.opt_mem "client_secret" Jsont.string ~enc:(fun r -> r.client_secret)
      |> Jsont.Object.opt_mem "max_bytes" Jsont.int ~enc:(fun r -> r.max_bytes)
      |> Jsont.Object.opt_mem "max_output_tokens" Jsont.int ~enc:(fun r -> r.max_output_tokens)
      |> Jsont.Object.opt_mem "model_name" Jsont.string ~enc:(fun r -> r.model_name)
      |> Jsont.Object.opt_mem "project_id" Jsont.string ~enc:(fun r -> r.project_id)
      |> Jsont.Object.opt_mem "refresh_token" Jsont.string ~enc:(fun r -> r.refresh_token)
      |> Jsont.Object.opt_mem "region" Jsont.string ~enc:(fun r -> r.region)
      |> Jsont.Object.opt_mem "stop_sequences" (Jsont.list Jsont.string) ~enc:(fun r -> r.stop_sequences)
      |> Jsont.Object.opt_mem "system_prompt" Jsont.string ~enc:(fun r -> r.system_prompt)
      |> Jsont.Object.opt_mem "temperature" Jsont.number ~enc:(fun r -> r.temperature)
      |> Jsont.Object.opt_mem "top_k" Jsont.int ~enc:(fun r -> r.top_k)
      |> Jsont.Object.opt_mem "top_p" Jsont.number ~enc:(fun r -> r.top_p)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module IndexAction = struct
  module Types = struct
    module T = struct
      type t = [
        | `Create
        | `Update
        | `Upsert
        | `Emplace
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"IndexAction"
        ~dec:(function
          | "create" -> `Create
          | "update" -> `Update
          | "upsert" -> `Upsert
          | "emplace" -> `Emplace
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Create -> "create"
          | `Update -> "update"
          | `Upsert -> "upsert"
          | `Emplace -> "emplace")
  end
end

module Health = struct
  module Types = struct
    module Status = struct
      type t = {
        ok : bool;
      }
    end
  end
  
  module Status = struct
    include Types.Status
    
    let v ~ok () = { ok }
    
    let ok t = t.ok
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"HealthStatus"
        (fun ok -> { ok })
      |> Jsont.Object.mem "ok" Jsont.bool ~enc:(fun r -> r.ok)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Checks if Typesense server is ready to accept requests.
  
      Checks if Typesense server is ready to accept requests. *)
  let health client () =
    let op_name = "health" in
    let url_path = "/health" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Status.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module Field = struct
  module Types = struct
    module T = struct
      type t = {
        async_reference : bool option;  (** Allow documents to be indexed successfully even when the referenced document doesn't exist yet.
       *)
        drop : bool option;
        embed : Jsont.json option;
        facet : bool option;
        index : bool;
        infix : bool;
        locale : string option;
        name : string;
        num_dim : int option;
        optional : bool option;
        range_index : bool option;  (** Enables an index optimized for range filtering on numerical fields (e.g. rating:>3.5). Default: false.
       *)
        reference : string option;  (** Name of a field in another collection that should be linked to this collection so that it can be joined during query.
       *)
        sort : bool option;
        stem : bool option;  (** Values are stemmed before indexing in-memory. Default: false.
       *)
        stem_dictionary : string option;  (** Name of the stemming dictionary to use for this field *)
        store : bool option;  (** When set to false, the field value will not be stored on disk. Default: true.
       *)
        symbols_to_index : string list;  (** List of symbols or special characters to be indexed.
       *)
        token_separators : string list;  (** List of symbols or special characters to be used for splitting the text into individual words in addition to space and new-line characters.
       *)
        type_ : string;
        vec_dist : string option;  (** The distance metric to be used for vector search. Default: `cosine`. You can also use `ip` for inner product.
       *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~name ~type_ ?(index=true) ?(infix=false) ?(symbols_to_index=[]) ?(token_separators=[]) ?async_reference ?drop ?embed ?facet ?locale ?num_dim ?optional ?range_index ?reference ?sort ?stem ?stem_dictionary ?store ?vec_dist () = { async_reference; drop; embed; facet; index; infix; locale; name; num_dim; optional; range_index; reference; sort; stem; stem_dictionary; store; symbols_to_index; token_separators; type_; vec_dist }
    
    let async_reference t = t.async_reference
    let drop t = t.drop
    let embed t = t.embed
    let facet t = t.facet
    let index t = t.index
    let infix t = t.infix
    let locale t = t.locale
    let name t = t.name
    let num_dim t = t.num_dim
    let optional t = t.optional
    let range_index t = t.range_index
    let reference t = t.reference
    let sort t = t.sort
    let stem t = t.stem
    let stem_dictionary t = t.stem_dictionary
    let store t = t.store
    let symbols_to_index t = t.symbols_to_index
    let token_separators t = t.token_separators
    let type_ t = t.type_
    let vec_dist t = t.vec_dist
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"Field"
        (fun async_reference drop embed facet index infix locale name num_dim optional range_index reference sort stem stem_dictionary store symbols_to_index token_separators type_ vec_dist -> { async_reference; drop; embed; facet; index; infix; locale; name; num_dim; optional; range_index; reference; sort; stem; stem_dictionary; store; symbols_to_index; token_separators; type_; vec_dist })
      |> Jsont.Object.opt_mem "async_reference" Jsont.bool ~enc:(fun r -> r.async_reference)
      |> Jsont.Object.opt_mem "drop" Jsont.bool ~enc:(fun r -> r.drop)
      |> Jsont.Object.opt_mem "embed" Jsont.json ~enc:(fun r -> r.embed)
      |> Jsont.Object.opt_mem "facet" Jsont.bool ~enc:(fun r -> r.facet)
      |> Jsont.Object.mem "index" Jsont.bool ~dec_absent:true ~enc:(fun r -> r.index)
      |> Jsont.Object.mem "infix" Jsont.bool ~dec_absent:false ~enc:(fun r -> r.infix)
      |> Jsont.Object.opt_mem "locale" Jsont.string ~enc:(fun r -> r.locale)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "num_dim" Jsont.int ~enc:(fun r -> r.num_dim)
      |> Jsont.Object.opt_mem "optional" Jsont.bool ~enc:(fun r -> r.optional)
      |> Jsont.Object.opt_mem "range_index" Jsont.bool ~enc:(fun r -> r.range_index)
      |> Jsont.Object.opt_mem "reference" Jsont.string ~enc:(fun r -> r.reference)
      |> Jsont.Object.opt_mem "sort" Jsont.bool ~enc:(fun r -> r.sort)
      |> Jsont.Object.opt_mem "stem" Jsont.bool ~enc:(fun r -> r.stem)
      |> Jsont.Object.opt_mem "stem_dictionary" Jsont.string ~enc:(fun r -> r.stem_dictionary)
      |> Jsont.Object.opt_mem "store" Jsont.bool ~enc:(fun r -> r.store)
      |> Jsont.Object.mem "symbols_to_index" (Jsont.list Jsont.string) ~dec_absent:[] ~enc:(fun r -> r.symbols_to_index)
      |> Jsont.Object.mem "token_separators" (Jsont.list Jsont.string) ~dec_absent:[] ~enc:(fun r -> r.token_separators)
      |> Jsont.Object.mem "type" Jsont.string ~enc:(fun r -> r.type_)
      |> Jsont.Object.opt_mem "vec_dist" Jsont.string ~enc:(fun r -> r.vec_dist)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module CollectionUpdateSchema = struct
  module Types = struct
    module T = struct
      type t = {
        fields : Field.T.t list;  (** A list of fields for querying, filtering and faceting *)
        metadata : Jsont.json option;  (** Optional details about the collection, e.g., when it was created, who created it etc.
       *)
        synonym_sets : string list option;  (** List of synonym set names to associate with this collection *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~fields ?metadata ?synonym_sets () = { fields; metadata; synonym_sets }
    
    let fields t = t.fields
    let metadata t = t.metadata
    let synonym_sets t = t.synonym_sets
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"CollectionUpdateSchema"
        (fun fields metadata synonym_sets -> { fields; metadata; synonym_sets })
      |> Jsont.Object.mem "fields" (Jsont.list Field.T.jsont) ~enc:(fun r -> r.fields)
      |> Jsont.Object.opt_mem "metadata" Jsont.json ~enc:(fun r -> r.metadata)
      |> Jsont.Object.opt_mem "synonym_sets" (Jsont.list Jsont.string) ~enc:(fun r -> r.synonym_sets)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Update a collection
  
      Update a collection's schema to modify the fields and their types. 
      @param collection_name The name of the collection to update
  *)
  let update_collection ~collection_name ~body client () =
    let op_name = "update_collection" in
    let url_path = Openapi.Runtime.Path.render ~params:[("collectionName", collection_name)] "/collections/{collectionName}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.patch client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json T.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PATCH" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 400 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PATCH";
        url;
        status;
        body;
        parsed_body;
      })
end

module CollectionSchema = struct
  module Types = struct
    module T = struct
      type t = {
        default_sorting_field : string;  (** The name of an int32 / float field that determines the order in which the search results are ranked when a sort_by clause is not provided during searching. This field must indicate some kind of popularity. *)
        enable_nested_fields : bool;  (** Enables experimental support at a collection level for nested object or object array fields. This field is only available if the Typesense server is version `0.24.0.rcn34` or later. *)
        fields : Field.T.t list;  (** A list of fields for querying, filtering and faceting *)
        metadata : Jsont.json option;  (** Optional details about the collection, e.g., when it was created, who created it etc.
       *)
        name : string;  (** Name of the collection *)
        symbols_to_index : string list;  (** List of symbols or special characters to be indexed.
       *)
        synonym_sets : string list option;  (** List of synonym set names to associate with this collection *)
        token_separators : string list;  (** List of symbols or special characters to be used for splitting the text into individual words in addition to space and new-line characters.
       *)
        voice_query_model : VoiceQueryModelCollection.Config.t option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~fields ~name ?(default_sorting_field="") ?(enable_nested_fields=false) ?(symbols_to_index=[]) ?(token_separators=[]) ?metadata ?synonym_sets ?voice_query_model () = { default_sorting_field; enable_nested_fields; fields; metadata; name; symbols_to_index; synonym_sets; token_separators; voice_query_model }
    
    let default_sorting_field t = t.default_sorting_field
    let enable_nested_fields t = t.enable_nested_fields
    let fields t = t.fields
    let metadata t = t.metadata
    let name t = t.name
    let symbols_to_index t = t.symbols_to_index
    let synonym_sets t = t.synonym_sets
    let token_separators t = t.token_separators
    let voice_query_model t = t.voice_query_model
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"CollectionSchema"
        (fun default_sorting_field enable_nested_fields fields metadata name symbols_to_index synonym_sets token_separators voice_query_model -> { default_sorting_field; enable_nested_fields; fields; metadata; name; symbols_to_index; synonym_sets; token_separators; voice_query_model })
      |> Jsont.Object.mem "default_sorting_field" Jsont.string ~dec_absent:"" ~enc:(fun r -> r.default_sorting_field)
      |> Jsont.Object.mem "enable_nested_fields" Jsont.bool ~dec_absent:false ~enc:(fun r -> r.enable_nested_fields)
      |> Jsont.Object.mem "fields" (Jsont.list Field.T.jsont) ~enc:(fun r -> r.fields)
      |> Jsont.Object.opt_mem "metadata" Jsont.json ~enc:(fun r -> r.metadata)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "symbols_to_index" (Jsont.list Jsont.string) ~dec_absent:[] ~enc:(fun r -> r.symbols_to_index)
      |> Jsont.Object.opt_mem "synonym_sets" (Jsont.list Jsont.string) ~enc:(fun r -> r.synonym_sets)
      |> Jsont.Object.mem "token_separators" (Jsont.list Jsont.string) ~dec_absent:[] ~enc:(fun r -> r.token_separators)
      |> Jsont.Object.opt_mem "voice_query_model" VoiceQueryModelCollection.Config.jsont ~enc:(fun r -> r.voice_query_model)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Collection = struct
  module Types = struct
    module Response = struct
      type t = {
        default_sorting_field : string;  (** The name of an int32 / float field that determines the order in which the search results are ranked when a sort_by clause is not provided during searching. This field must indicate some kind of popularity. *)
        enable_nested_fields : bool;  (** Enables experimental support at a collection level for nested object or object array fields. This field is only available if the Typesense server is version `0.24.0.rcn34` or later. *)
        fields : Field.T.t list;  (** A list of fields for querying, filtering and faceting *)
        metadata : Jsont.json option;  (** Optional details about the collection, e.g., when it was created, who created it etc.
       *)
        name : string;  (** Name of the collection *)
        symbols_to_index : string list;  (** List of symbols or special characters to be indexed.
       *)
        synonym_sets : string list option;  (** List of synonym set names to associate with this collection *)
        token_separators : string list;  (** List of symbols or special characters to be used for splitting the text into individual words in addition to space and new-line characters.
       *)
        voice_query_model : VoiceQueryModelCollection.Config.t option;
        num_documents : int64;  (** Number of documents in the collection *)
        created_at : int64;  (** Timestamp of when the collection was created (Unix epoch in seconds) *)
      }
    end
  end
  
  module Response = struct
    include Types.Response
    
    let v ~fields ~name ~num_documents ~created_at ?(default_sorting_field="") ?(enable_nested_fields=false) ?(symbols_to_index=[]) ?(token_separators=[]) ?metadata ?synonym_sets ?voice_query_model () = { default_sorting_field; enable_nested_fields; fields; metadata; name; symbols_to_index; synonym_sets; token_separators; voice_query_model; num_documents; created_at }
    
    let default_sorting_field t = t.default_sorting_field
    let enable_nested_fields t = t.enable_nested_fields
    let fields t = t.fields
    let metadata t = t.metadata
    let name t = t.name
    let symbols_to_index t = t.symbols_to_index
    let synonym_sets t = t.synonym_sets
    let token_separators t = t.token_separators
    let voice_query_model t = t.voice_query_model
    let num_documents t = t.num_documents
    let created_at t = t.created_at
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"CollectionResponse"
        (fun default_sorting_field enable_nested_fields fields metadata name symbols_to_index synonym_sets token_separators voice_query_model num_documents created_at -> { default_sorting_field; enable_nested_fields; fields; metadata; name; symbols_to_index; synonym_sets; token_separators; voice_query_model; num_documents; created_at })
      |> Jsont.Object.mem "default_sorting_field" Jsont.string ~dec_absent:"" ~enc:(fun r -> r.default_sorting_field)
      |> Jsont.Object.mem "enable_nested_fields" Jsont.bool ~dec_absent:false ~enc:(fun r -> r.enable_nested_fields)
      |> Jsont.Object.mem "fields" (Jsont.list Field.T.jsont) ~enc:(fun r -> r.fields)
      |> Jsont.Object.opt_mem "metadata" Jsont.json ~enc:(fun r -> r.metadata)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "symbols_to_index" (Jsont.list Jsont.string) ~dec_absent:[] ~enc:(fun r -> r.symbols_to_index)
      |> Jsont.Object.opt_mem "synonym_sets" (Jsont.list Jsont.string) ~enc:(fun r -> r.synonym_sets)
      |> Jsont.Object.mem "token_separators" (Jsont.list Jsont.string) ~dec_absent:[] ~enc:(fun r -> r.token_separators)
      |> Jsont.Object.opt_mem "voice_query_model" VoiceQueryModelCollection.Config.jsont ~enc:(fun r -> r.voice_query_model)
      |> Jsont.Object.mem "num_documents" Jsont.int64 ~enc:(fun r -> r.num_documents)
      |> Jsont.Object.mem "created_at" Jsont.int64 ~enc:(fun r -> r.created_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** List all collections
  
      Returns a summary of all your collections. The collections are returned sorted by creation date, with the most recent collections appearing first. *)
  let get_collections ?get_collections_parameters client () =
    let op_name = "get_collections" in
    let url_path = "/collections" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"getCollectionsParameters" ~value:get_collections_parameters]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Create a new collection
  
      When a collection is created, we give it a name and describe the fields that will be indexed from the documents added to the collection. *)
  let create_collection ~body client () =
    let op_name = "create_collection" in
    let url_path = "/collections" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json CollectionSchema.T.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 400 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | 409 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status;
        body;
        parsed_body;
      })
  
  (** Retrieve a single collection
  
      Retrieve the details of a collection, given its name. 
      @param collection_name The name of the collection to retrieve
  *)
  let get_collection ~collection_name client () =
    let op_name = "get_collection" in
    let url_path = Openapi.Runtime.Path.render ~params:[("collectionName", collection_name)] "/collections/{collectionName}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status;
        body;
        parsed_body;
      })
  
  (** Delete a collection
  
      Permanently drops a collection. This action cannot be undone. For large collections, this might have an impact on read latencies. 
      @param collection_name The name of the collection to delete
  *)
  let delete_collection ~collection_name client () =
    let op_name = "delete_collection" in
    let url_path = Openapi.Runtime.Path.render ~params:[("collectionName", collection_name)] "/collections/{collectionName}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status;
        body;
        parsed_body;
      })
end

module FacetCounts = struct
  module Types = struct
    module T = struct
      type t = {
        counts : Jsont.json list option;
        field_name : string option;
        stats : Jsont.json option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?counts ?field_name ?stats () = { counts; field_name; stats }
    
    let counts t = t.counts
    let field_name t = t.field_name
    let stats t = t.stats
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"FacetCounts"
        (fun counts field_name stats -> { counts; field_name; stats })
      |> Jsont.Object.opt_mem "counts" (Jsont.list Jsont.json) ~enc:(fun r -> r.counts)
      |> Jsont.Object.opt_mem "field_name" Jsont.string ~enc:(fun r -> r.field_name)
      |> Jsont.Object.opt_mem "stats" Jsont.json ~enc:(fun r -> r.stats)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Search = struct
  module Types = struct
    module Result = struct
      type t = {
        conversation : SearchResultConversation.T.t option;
        facet_counts : FacetCounts.T.t list option;
        found : int option;  (** The number of documents found *)
        found_docs : int option;
        grouped_hits : SearchGroupedHit.T.t list option;
        hits : SearchResultHit.T.t list option;  (** The documents that matched the search query *)
        metadata : Jsont.json option;  (** Custom JSON object that can be returned in the search response *)
        out_of : int option;  (** The total number of documents in the collection *)
        page : int option;  (** The search result page number *)
        request_params : SearchRequestParams.T.t option;
        search_cutoff : bool option;  (** Whether the search was cut off *)
        search_time_ms : int option;  (** The number of milliseconds the search took *)
        union_request_params : SearchRequestParams.T.t list option;  (** Returned only for union query response. *)
      }
    end
  end
  
  module Result = struct
    include Types.Result
    
    let v ?conversation ?facet_counts ?found ?found_docs ?grouped_hits ?hits ?metadata ?out_of ?page ?request_params ?search_cutoff ?search_time_ms ?union_request_params () = { conversation; facet_counts; found; found_docs; grouped_hits; hits; metadata; out_of; page; request_params; search_cutoff; search_time_ms; union_request_params }
    
    let conversation t = t.conversation
    let facet_counts t = t.facet_counts
    let found t = t.found
    let found_docs t = t.found_docs
    let grouped_hits t = t.grouped_hits
    let hits t = t.hits
    let metadata t = t.metadata
    let out_of t = t.out_of
    let page t = t.page
    let request_params t = t.request_params
    let search_cutoff t = t.search_cutoff
    let search_time_ms t = t.search_time_ms
    let union_request_params t = t.union_request_params
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SearchResult"
        (fun conversation facet_counts found found_docs grouped_hits hits metadata out_of page request_params search_cutoff search_time_ms union_request_params -> { conversation; facet_counts; found; found_docs; grouped_hits; hits; metadata; out_of; page; request_params; search_cutoff; search_time_ms; union_request_params })
      |> Jsont.Object.opt_mem "conversation" SearchResultConversation.T.jsont ~enc:(fun r -> r.conversation)
      |> Jsont.Object.opt_mem "facet_counts" (Jsont.list FacetCounts.T.jsont) ~enc:(fun r -> r.facet_counts)
      |> Jsont.Object.opt_mem "found" Jsont.int ~enc:(fun r -> r.found)
      |> Jsont.Object.opt_mem "found_docs" Jsont.int ~enc:(fun r -> r.found_docs)
      |> Jsont.Object.opt_mem "grouped_hits" (Jsont.list SearchGroupedHit.T.jsont) ~enc:(fun r -> r.grouped_hits)
      |> Jsont.Object.opt_mem "hits" (Jsont.list SearchResultHit.T.jsont) ~enc:(fun r -> r.hits)
      |> Jsont.Object.opt_mem "metadata" Jsont.json ~enc:(fun r -> r.metadata)
      |> Jsont.Object.opt_mem "out_of" Jsont.int ~enc:(fun r -> r.out_of)
      |> Jsont.Object.opt_mem "page" Jsont.int ~enc:(fun r -> r.page)
      |> Jsont.Object.opt_mem "request_params" SearchRequestParams.T.jsont ~enc:(fun r -> r.request_params)
      |> Jsont.Object.opt_mem "search_cutoff" Jsont.bool ~enc:(fun r -> r.search_cutoff)
      |> Jsont.Object.opt_mem "search_time_ms" Jsont.int ~enc:(fun r -> r.search_time_ms)
      |> Jsont.Object.opt_mem "union_request_params" (Jsont.list SearchRequestParams.T.jsont) ~enc:(fun r -> r.union_request_params)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Search for documents in a collection
  
      Search for documents in a collection that match the search criteria. 
      @param collection_name The name of the collection to search for the document under
  *)
  let search_collection ~collection_name ~search_parameters client () =
    let op_name = "search_collection" in
    let url_path = Openapi.Runtime.Path.render ~params:[("collectionName", collection_name)] "/collections/{collectionName}/documents/search" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.singleton ~key:"searchParameters" ~value:search_parameters]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Result.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 400 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status;
        body;
        parsed_body;
      })
end

module MultiSearchResult = struct
  module Types = struct
    module Item = struct
      type t = {
        conversation : SearchResultConversation.T.t option;
        facet_counts : FacetCounts.T.t list option;
        found : int option;  (** The number of documents found *)
        found_docs : int option;
        grouped_hits : SearchGroupedHit.T.t list option;
        hits : SearchResultHit.T.t list option;  (** The documents that matched the search query *)
        metadata : Jsont.json option;  (** Custom JSON object that can be returned in the search response *)
        out_of : int option;  (** The total number of documents in the collection *)
        page : int option;  (** The search result page number *)
        request_params : SearchRequestParams.T.t option;
        search_cutoff : bool option;  (** Whether the search was cut off *)
        search_time_ms : int option;  (** The number of milliseconds the search took *)
        union_request_params : SearchRequestParams.T.t list option;  (** Returned only for union query response. *)
        code : int64 option;  (** HTTP error code *)
        error : string option;  (** Error description *)
      }
    end
  end
  
  module Item = struct
    include Types.Item
    
    let v ?conversation ?facet_counts ?found ?found_docs ?grouped_hits ?hits ?metadata ?out_of ?page ?request_params ?search_cutoff ?search_time_ms ?union_request_params ?code ?error () = { conversation; facet_counts; found; found_docs; grouped_hits; hits; metadata; out_of; page; request_params; search_cutoff; search_time_ms; union_request_params; code; error }
    
    let conversation t = t.conversation
    let facet_counts t = t.facet_counts
    let found t = t.found
    let found_docs t = t.found_docs
    let grouped_hits t = t.grouped_hits
    let hits t = t.hits
    let metadata t = t.metadata
    let out_of t = t.out_of
    let page t = t.page
    let request_params t = t.request_params
    let search_cutoff t = t.search_cutoff
    let search_time_ms t = t.search_time_ms
    let union_request_params t = t.union_request_params
    let code t = t.code
    let error t = t.error
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"MultiSearchResultItem"
        (fun conversation facet_counts found found_docs grouped_hits hits metadata out_of page request_params search_cutoff search_time_ms union_request_params code error -> { conversation; facet_counts; found; found_docs; grouped_hits; hits; metadata; out_of; page; request_params; search_cutoff; search_time_ms; union_request_params; code; error })
      |> Jsont.Object.opt_mem "conversation" SearchResultConversation.T.jsont ~enc:(fun r -> r.conversation)
      |> Jsont.Object.opt_mem "facet_counts" (Jsont.list FacetCounts.T.jsont) ~enc:(fun r -> r.facet_counts)
      |> Jsont.Object.opt_mem "found" Jsont.int ~enc:(fun r -> r.found)
      |> Jsont.Object.opt_mem "found_docs" Jsont.int ~enc:(fun r -> r.found_docs)
      |> Jsont.Object.opt_mem "grouped_hits" (Jsont.list SearchGroupedHit.T.jsont) ~enc:(fun r -> r.grouped_hits)
      |> Jsont.Object.opt_mem "hits" (Jsont.list SearchResultHit.T.jsont) ~enc:(fun r -> r.hits)
      |> Jsont.Object.opt_mem "metadata" Jsont.json ~enc:(fun r -> r.metadata)
      |> Jsont.Object.opt_mem "out_of" Jsont.int ~enc:(fun r -> r.out_of)
      |> Jsont.Object.opt_mem "page" Jsont.int ~enc:(fun r -> r.page)
      |> Jsont.Object.opt_mem "request_params" SearchRequestParams.T.jsont ~enc:(fun r -> r.request_params)
      |> Jsont.Object.opt_mem "search_cutoff" Jsont.bool ~enc:(fun r -> r.search_cutoff)
      |> Jsont.Object.opt_mem "search_time_ms" Jsont.int ~enc:(fun r -> r.search_time_ms)
      |> Jsont.Object.opt_mem "union_request_params" (Jsont.list SearchRequestParams.T.jsont) ~enc:(fun r -> r.union_request_params)
      |> Jsont.Object.opt_mem "code" Jsont.int64 ~enc:(fun r -> r.code)
      |> Jsont.Object.opt_mem "error" Jsont.string ~enc:(fun r -> r.error)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module DropTokensMode = struct
  module Types = struct
    module T = struct
      (** Dictates the direction in which the words in the query must be dropped when the original words in the query do not appear in any document. Values: right_to_left (default), left_to_right, both_sides:3 A note on both_sides:3 - for queries up to 3 tokens (words) in length, this mode will drop tokens from both sides and exhaustively rank all matching results. If query length is greater than 3 words, Typesense will just fallback to default behavior of right_to_left
       *)
      type t = [
        | `Right_to_left
        | `Left_to_right
        | `Both_sides3
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"DropTokensMode"
        ~dec:(function
          | "right_to_left" -> `Right_to_left
          | "left_to_right" -> `Left_to_right
          | "both_sides:3" -> `Both_sides3
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Right_to_left -> "right_to_left"
          | `Left_to_right -> "left_to_right"
          | `Both_sides3 -> "both_sides:3")
  end
end

module SearchParameters = struct
  module Types = struct
    module T = struct
      type t = {
        cache_ttl : int option;  (** The duration (in seconds) that determines how long the search query is cached. This value can be set on a per-query basis. Default: 60.
       *)
        conversation : bool option;  (** Enable conversational search.
       *)
        conversation_id : string option;  (** The Id of a previous conversation to continue, this tells Typesense to include prior context when communicating with the LLM.
       *)
        conversation_model_id : string option;  (** The Id of Conversation Model to be used.
       *)
        drop_tokens_mode : DropTokensMode.T.t option;
        drop_tokens_threshold : int option;  (** If the number of results found for a specific query is less than this number, Typesense will attempt to drop the tokens in the query until enough results are found. Tokens that have the least individual hits are dropped first. Set to 0 to disable. Default: 10
       *)
        enable_analytics : bool;  (** Flag for enabling/disabling analytics aggregation for specific search queries (for e.g. those originating from a test script).
       *)
        enable_highlight_v1 : bool;  (** Flag for enabling/disabling the deprecated, old highlight structure in the response. Default: true
       *)
        enable_overrides : bool;  (** If you have some overrides defined but want to disable all of them during query time, you can do that by setting this parameter to false
       *)
        enable_synonyms : bool option;  (** If you have some synonyms defined but want to disable all of them for a particular search query, set enable_synonyms to false. Default: true
       *)
        enable_typos_for_alpha_numerical_tokens : bool option;  (** Set this parameter to false to disable typos on alphanumerical query tokens. Default: true.
       *)
        enable_typos_for_numerical_tokens : bool;  (** Make Typesense disable typos for numerical tokens.
       *)
        exclude_fields : string option;  (** List of fields from the document to exclude in the search result *)
        exhaustive_search : bool option;  (** Setting this to true will make Typesense consider all prefixes and typo corrections of the words in the query without stopping early when enough results are found (drop_tokens_threshold and typo_tokens_threshold configurations are ignored).
       *)
        facet_by : string option;  (** A list of fields that will be used for faceting your results on. Separate multiple fields with a comma. *)
        facet_query : string option;  (** Facet values that are returned can now be filtered via this parameter. The matching facet text is also highlighted. For example, when faceting by `category`, you can set `facet_query=category:shoe` to return only facet values that contain the prefix "shoe". *)
        facet_return_parent : string option;  (** Comma separated string of nested facet fields whose parent object should be returned in facet response.
       *)
        facet_strategy : string option;  (** Choose the underlying faceting strategy used. Comma separated string of allows values: exhaustive, top_values or automatic (default).
       *)
        filter_by : string option;  (** Filter conditions for refining your open api validator search results. Separate multiple conditions with &&. *)
        filter_curated_hits : bool option;  (** Whether the filter_by condition of the search query should be applicable to curated results (override definitions, pinned hits, hidden hits, etc.). Default: false
       *)
        group_by : string option;  (** You can aggregate search results into groups or buckets by specify one or more `group_by` fields. Separate multiple fields with a comma. To group on a particular field, it must be a faceted field. *)
        group_limit : int option;  (** Maximum number of hits to be returned for every group. If the `group_limit` is set as `K` then only the top K hits in each group are returned in the response. Default: 3
       *)
        group_missing_values : bool option;  (** Setting this parameter to true will place all documents that have a null value in the group_by field, into a single group. Setting this parameter to false, will cause each document with a null value in the group_by field to not be grouped with other documents. Default: true
       *)
        hidden_hits : string option;  (** A list of records to unconditionally hide from search results. A list of `record_id`s to hide. Eg: to hide records with IDs 123 and 456, you'd specify `123,456`.
      You could also use the Overrides feature to override search results based on rules. Overrides are applied first, followed by `pinned_hits` and finally `hidden_hits`.
       *)
        highlight_affix_num_tokens : int option;  (** The number of tokens that should surround the highlighted text on each side. Default: 4
       *)
        highlight_end_tag : string option;  (** The end tag used for the highlighted snippets. Default: `</mark>`
       *)
        highlight_fields : string option;  (** A list of custom fields that must be highlighted even if you don't query for them
       *)
        highlight_full_fields : string option;  (** List of fields which should be highlighted fully without snippeting *)
        highlight_start_tag : string option;  (** The start tag used for the highlighted snippets. Default: `<mark>`
       *)
        include_fields : string option;  (** List of fields from the document to include in the search result *)
        infix : string option;  (** If infix index is enabled for this field, infix searching can be done on a per-field basis by sending a comma separated string parameter called infix to the search query. This parameter can have 3 values; `off` infix search is disabled, which is default `always` infix search is performed along with regular search `fallback` infix search is performed if regular search does not produce results *)
        limit : int option;  (** Number of hits to fetch. Can be used as an alternative to the per_page parameter. Default: 10.
       *)
        max_candidates : int option;  (** Control the number of words that Typesense considers for typo and prefix searching.
       *)
        max_extra_prefix : int option;  (** There are also 2 parameters that allow you to control the extent of infix searching max_extra_prefix and max_extra_suffix which specify the maximum number of symbols before or after the query that can be present in the token. For example query "K2100" has 2 extra symbols in "6PK2100". By default, any number of prefixes/suffixes can be present for a match. *)
        max_extra_suffix : int option;  (** There are also 2 parameters that allow you to control the extent of infix searching max_extra_prefix and max_extra_suffix which specify the maximum number of symbols before or after the query that can be present in the token. For example query "K2100" has 2 extra symbols in "6PK2100". By default, any number of prefixes/suffixes can be present for a match. *)
        max_facet_values : int option;  (** Maximum number of facet values to be returned. *)
        max_filter_by_candidates : int option;  (** Controls the number of similar words that Typesense considers during fuzzy search on filter_by values. Useful for controlling prefix matches like company_name:Acm*. *)
        min_len_1typo : int option;  (** Minimum word length for 1-typo correction to be applied. The value of num_typos is still treated as the maximum allowed typos.
       *)
        min_len_2typo : int option;  (** Minimum word length for 2-typo correction to be applied. The value of num_typos is still treated as the maximum allowed typos.
       *)
        nl_model_id : string option;  (** The ID of the natural language model to use. *)
        nl_query : bool option;  (** Whether to use natural language processing to parse the query. *)
        num_typos : string option;  (** The number of typographical errors (1 or 2) that would be tolerated. Default: 2
       *)
        offset : int option;  (** Identifies the starting point to return hits from a result set. Can be used as an alternative to the page parameter. *)
        override_tags : string option;  (** Comma separated list of tags to trigger the curations rules that match the tags. *)
        page : int option;  (** Results from this specific page number would be fetched. *)
        per_page : int option;  (** Number of results to fetch per page. Default: 10 *)
        pinned_hits : string option;  (** A list of records to unconditionally include in the search results at specific positions. An example use case would be to feature or promote certain items on the top of search results. A list of `record_id:hit_position`. Eg: to include a record with ID 123 at Position 1 and another record with ID 456 at Position 5, you'd specify `123:1,456:5`.
      You could also use the Overrides feature to override search results based on rules. Overrides are applied first, followed by `pinned_hits` and finally `hidden_hits`.
       *)
        pre_segmented_query : bool option;  (** You can index content from any logographic language into Typesense if you are able to segment / split the text into space-separated words yourself before indexing and querying.
      Set this parameter to true to do the same
       *)
        prefix : string option;  (** Boolean field to indicate that the last word in the query should be treated as a prefix, and not as a whole word. This is used for building autocomplete and instant search interfaces. Defaults to true. *)
        preset : string option;  (** Search using a bunch of search parameters by setting this parameter to the name of the existing Preset.
       *)
        prioritize_exact_match : bool;  (** Set this parameter to true to ensure that an exact match is ranked above the others
       *)
        prioritize_num_matching_fields : bool;  (** Make Typesense prioritize documents where the query words appear in more number of fields.
       *)
        prioritize_token_position : bool;  (** Make Typesense prioritize documents where the query words appear earlier in the text.
       *)
        q : string option;  (** The query text to search for in the collection. Use * as the search string to return all documents. This is typically useful when used in conjunction with filter_by. *)
        query_by : string option;  (** A list of `string` fields that should be queried against. Multiple fields are separated with a comma. *)
        query_by_weights : string option;  (** The relative weight to give each `query_by` field when ranking results. This can be used to boost fields in priority, when looking for matches. Multiple fields are separated with a comma. *)
        remote_embedding_num_tries : int option;  (** Number of times to retry fetching remote embeddings.
       *)
        remote_embedding_timeout_ms : int option;  (** Timeout (in milliseconds) for fetching remote embeddings.
       *)
        search_cutoff_ms : int option;  (** Typesense will attempt to return results early if the cutoff time has elapsed. This is not a strict guarantee and facet computation is not bound by this parameter.
       *)
        snippet_threshold : int option;  (** Field values under this length will be fully highlighted, instead of showing a snippet of relevant portion. Default: 30
       *)
        sort_by : string option;  (** A list of numerical fields and their corresponding sort orders that will be used for ordering your results. Up to 3 sort fields can be specified. The text similarity score is exposed as a special `_text_match` field that you can use in the list of sorting fields. If no `sort_by` parameter is specified, results are sorted by `_text_match:desc,default_sorting_field:desc` *)
        split_join_tokens : string option;  (** Treat space as typo: search for q=basket ball if q=basketball is not found or vice-versa. Splitting/joining of tokens will only be attempted if the original query produces no results. To always trigger this behavior, set value to `always``. To disable, set value to `off`. Default is `fallback`.
       *)
        stopwords : string option;  (** Name of the stopwords set to apply for this search, the keywords present in the set will be removed from the search query.
       *)
        synonym_num_typos : int option;  (** Allow synonym resolution on typo-corrected words in the query. Default: 0
       *)
        synonym_prefix : bool option;  (** Allow synonym resolution on word prefixes in the query. Default: false
       *)
        synonym_sets : string option;  (** List of synonym set names to associate with this search query *)
        text_match_type : string option;  (** In a multi-field matching context, this parameter determines how the representative text match score of a record is calculated. Possible values are max_score (default) or max_weight. *)
        typo_tokens_threshold : int option;  (** If the number of results found for a specific query is less than this number, Typesense will attempt to look for tokens with more typos until enough results are found. Default: 100
       *)
        use_cache : bool option;  (** Enable server side caching of search query results. By default, caching is disabled.
       *)
        vector_query : string option;  (** Vector query expression for fetching documents "closest" to a given query/document vector.
       *)
        voice_query : string option;  (** The base64 encoded audio file in 16 khz 16-bit WAV format.
       *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?(enable_analytics=true) ?(enable_highlight_v1=true) ?(enable_overrides=false) ?(enable_typos_for_numerical_tokens=true) ?(prioritize_exact_match=true) ?(prioritize_num_matching_fields=true) ?(prioritize_token_position=false) ?cache_ttl ?conversation ?conversation_id ?conversation_model_id ?drop_tokens_mode ?drop_tokens_threshold ?enable_synonyms ?enable_typos_for_alpha_numerical_tokens ?exclude_fields ?exhaustive_search ?facet_by ?facet_query ?facet_return_parent ?facet_strategy ?filter_by ?filter_curated_hits ?group_by ?group_limit ?group_missing_values ?hidden_hits ?highlight_affix_num_tokens ?highlight_end_tag ?highlight_fields ?highlight_full_fields ?highlight_start_tag ?include_fields ?infix ?limit ?max_candidates ?max_extra_prefix ?max_extra_suffix ?max_facet_values ?max_filter_by_candidates ?min_len_1typo ?min_len_2typo ?nl_model_id ?nl_query ?num_typos ?offset ?override_tags ?page ?per_page ?pinned_hits ?pre_segmented_query ?prefix ?preset ?q ?query_by ?query_by_weights ?remote_embedding_num_tries ?remote_embedding_timeout_ms ?search_cutoff_ms ?snippet_threshold ?sort_by ?split_join_tokens ?stopwords ?synonym_num_typos ?synonym_prefix ?synonym_sets ?text_match_type ?typo_tokens_threshold ?use_cache ?vector_query ?voice_query () = { cache_ttl; conversation; conversation_id; conversation_model_id; drop_tokens_mode; drop_tokens_threshold; enable_analytics; enable_highlight_v1; enable_overrides; enable_synonyms; enable_typos_for_alpha_numerical_tokens; enable_typos_for_numerical_tokens; exclude_fields; exhaustive_search; facet_by; facet_query; facet_return_parent; facet_strategy; filter_by; filter_curated_hits; group_by; group_limit; group_missing_values; hidden_hits; highlight_affix_num_tokens; highlight_end_tag; highlight_fields; highlight_full_fields; highlight_start_tag; include_fields; infix; limit; max_candidates; max_extra_prefix; max_extra_suffix; max_facet_values; max_filter_by_candidates; min_len_1typo; min_len_2typo; nl_model_id; nl_query; num_typos; offset; override_tags; page; per_page; pinned_hits; pre_segmented_query; prefix; preset; prioritize_exact_match; prioritize_num_matching_fields; prioritize_token_position; q; query_by; query_by_weights; remote_embedding_num_tries; remote_embedding_timeout_ms; search_cutoff_ms; snippet_threshold; sort_by; split_join_tokens; stopwords; synonym_num_typos; synonym_prefix; synonym_sets; text_match_type; typo_tokens_threshold; use_cache; vector_query; voice_query }
    
    let cache_ttl t = t.cache_ttl
    let conversation t = t.conversation
    let conversation_id t = t.conversation_id
    let conversation_model_id t = t.conversation_model_id
    let drop_tokens_mode t = t.drop_tokens_mode
    let drop_tokens_threshold t = t.drop_tokens_threshold
    let enable_analytics t = t.enable_analytics
    let enable_highlight_v1 t = t.enable_highlight_v1
    let enable_overrides t = t.enable_overrides
    let enable_synonyms t = t.enable_synonyms
    let enable_typos_for_alpha_numerical_tokens t = t.enable_typos_for_alpha_numerical_tokens
    let enable_typos_for_numerical_tokens t = t.enable_typos_for_numerical_tokens
    let exclude_fields t = t.exclude_fields
    let exhaustive_search t = t.exhaustive_search
    let facet_by t = t.facet_by
    let facet_query t = t.facet_query
    let facet_return_parent t = t.facet_return_parent
    let facet_strategy t = t.facet_strategy
    let filter_by t = t.filter_by
    let filter_curated_hits t = t.filter_curated_hits
    let group_by t = t.group_by
    let group_limit t = t.group_limit
    let group_missing_values t = t.group_missing_values
    let hidden_hits t = t.hidden_hits
    let highlight_affix_num_tokens t = t.highlight_affix_num_tokens
    let highlight_end_tag t = t.highlight_end_tag
    let highlight_fields t = t.highlight_fields
    let highlight_full_fields t = t.highlight_full_fields
    let highlight_start_tag t = t.highlight_start_tag
    let include_fields t = t.include_fields
    let infix t = t.infix
    let limit t = t.limit
    let max_candidates t = t.max_candidates
    let max_extra_prefix t = t.max_extra_prefix
    let max_extra_suffix t = t.max_extra_suffix
    let max_facet_values t = t.max_facet_values
    let max_filter_by_candidates t = t.max_filter_by_candidates
    let min_len_1typo t = t.min_len_1typo
    let min_len_2typo t = t.min_len_2typo
    let nl_model_id t = t.nl_model_id
    let nl_query t = t.nl_query
    let num_typos t = t.num_typos
    let offset t = t.offset
    let override_tags t = t.override_tags
    let page t = t.page
    let per_page t = t.per_page
    let pinned_hits t = t.pinned_hits
    let pre_segmented_query t = t.pre_segmented_query
    let prefix t = t.prefix
    let preset t = t.preset
    let prioritize_exact_match t = t.prioritize_exact_match
    let prioritize_num_matching_fields t = t.prioritize_num_matching_fields
    let prioritize_token_position t = t.prioritize_token_position
    let q t = t.q
    let query_by t = t.query_by
    let query_by_weights t = t.query_by_weights
    let remote_embedding_num_tries t = t.remote_embedding_num_tries
    let remote_embedding_timeout_ms t = t.remote_embedding_timeout_ms
    let search_cutoff_ms t = t.search_cutoff_ms
    let snippet_threshold t = t.snippet_threshold
    let sort_by t = t.sort_by
    let split_join_tokens t = t.split_join_tokens
    let stopwords t = t.stopwords
    let synonym_num_typos t = t.synonym_num_typos
    let synonym_prefix t = t.synonym_prefix
    let synonym_sets t = t.synonym_sets
    let text_match_type t = t.text_match_type
    let typo_tokens_threshold t = t.typo_tokens_threshold
    let use_cache t = t.use_cache
    let vector_query t = t.vector_query
    let voice_query t = t.voice_query
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SearchParameters"
        (fun cache_ttl conversation conversation_id conversation_model_id drop_tokens_mode drop_tokens_threshold enable_analytics enable_highlight_v1 enable_overrides enable_synonyms enable_typos_for_alpha_numerical_tokens enable_typos_for_numerical_tokens exclude_fields exhaustive_search facet_by facet_query facet_return_parent facet_strategy filter_by filter_curated_hits group_by group_limit group_missing_values hidden_hits highlight_affix_num_tokens highlight_end_tag highlight_fields highlight_full_fields highlight_start_tag include_fields infix limit max_candidates max_extra_prefix max_extra_suffix max_facet_values max_filter_by_candidates min_len_1typo min_len_2typo nl_model_id nl_query num_typos offset override_tags page per_page pinned_hits pre_segmented_query prefix preset prioritize_exact_match prioritize_num_matching_fields prioritize_token_position q query_by query_by_weights remote_embedding_num_tries remote_embedding_timeout_ms search_cutoff_ms snippet_threshold sort_by split_join_tokens stopwords synonym_num_typos synonym_prefix synonym_sets text_match_type typo_tokens_threshold use_cache vector_query voice_query -> { cache_ttl; conversation; conversation_id; conversation_model_id; drop_tokens_mode; drop_tokens_threshold; enable_analytics; enable_highlight_v1; enable_overrides; enable_synonyms; enable_typos_for_alpha_numerical_tokens; enable_typos_for_numerical_tokens; exclude_fields; exhaustive_search; facet_by; facet_query; facet_return_parent; facet_strategy; filter_by; filter_curated_hits; group_by; group_limit; group_missing_values; hidden_hits; highlight_affix_num_tokens; highlight_end_tag; highlight_fields; highlight_full_fields; highlight_start_tag; include_fields; infix; limit; max_candidates; max_extra_prefix; max_extra_suffix; max_facet_values; max_filter_by_candidates; min_len_1typo; min_len_2typo; nl_model_id; nl_query; num_typos; offset; override_tags; page; per_page; pinned_hits; pre_segmented_query; prefix; preset; prioritize_exact_match; prioritize_num_matching_fields; prioritize_token_position; q; query_by; query_by_weights; remote_embedding_num_tries; remote_embedding_timeout_ms; search_cutoff_ms; snippet_threshold; sort_by; split_join_tokens; stopwords; synonym_num_typos; synonym_prefix; synonym_sets; text_match_type; typo_tokens_threshold; use_cache; vector_query; voice_query })
      |> Jsont.Object.opt_mem "cache_ttl" Jsont.int ~enc:(fun r -> r.cache_ttl)
      |> Jsont.Object.opt_mem "conversation" Jsont.bool ~enc:(fun r -> r.conversation)
      |> Jsont.Object.opt_mem "conversation_id" Jsont.string ~enc:(fun r -> r.conversation_id)
      |> Jsont.Object.opt_mem "conversation_model_id" Jsont.string ~enc:(fun r -> r.conversation_model_id)
      |> Jsont.Object.opt_mem "drop_tokens_mode" DropTokensMode.T.jsont ~enc:(fun r -> r.drop_tokens_mode)
      |> Jsont.Object.opt_mem "drop_tokens_threshold" Jsont.int ~enc:(fun r -> r.drop_tokens_threshold)
      |> Jsont.Object.mem "enable_analytics" Jsont.bool ~dec_absent:true ~enc:(fun r -> r.enable_analytics)
      |> Jsont.Object.mem "enable_highlight_v1" Jsont.bool ~dec_absent:true ~enc:(fun r -> r.enable_highlight_v1)
      |> Jsont.Object.mem "enable_overrides" Jsont.bool ~dec_absent:false ~enc:(fun r -> r.enable_overrides)
      |> Jsont.Object.opt_mem "enable_synonyms" Jsont.bool ~enc:(fun r -> r.enable_synonyms)
      |> Jsont.Object.opt_mem "enable_typos_for_alpha_numerical_tokens" Jsont.bool ~enc:(fun r -> r.enable_typos_for_alpha_numerical_tokens)
      |> Jsont.Object.mem "enable_typos_for_numerical_tokens" Jsont.bool ~dec_absent:true ~enc:(fun r -> r.enable_typos_for_numerical_tokens)
      |> Jsont.Object.opt_mem "exclude_fields" Jsont.string ~enc:(fun r -> r.exclude_fields)
      |> Jsont.Object.opt_mem "exhaustive_search" Jsont.bool ~enc:(fun r -> r.exhaustive_search)
      |> Jsont.Object.opt_mem "facet_by" Jsont.string ~enc:(fun r -> r.facet_by)
      |> Jsont.Object.opt_mem "facet_query" Jsont.string ~enc:(fun r -> r.facet_query)
      |> Jsont.Object.opt_mem "facet_return_parent" Jsont.string ~enc:(fun r -> r.facet_return_parent)
      |> Jsont.Object.opt_mem "facet_strategy" Jsont.string ~enc:(fun r -> r.facet_strategy)
      |> Jsont.Object.opt_mem "filter_by" Jsont.string ~enc:(fun r -> r.filter_by)
      |> Jsont.Object.opt_mem "filter_curated_hits" Jsont.bool ~enc:(fun r -> r.filter_curated_hits)
      |> Jsont.Object.opt_mem "group_by" Jsont.string ~enc:(fun r -> r.group_by)
      |> Jsont.Object.opt_mem "group_limit" Jsont.int ~enc:(fun r -> r.group_limit)
      |> Jsont.Object.opt_mem "group_missing_values" Jsont.bool ~enc:(fun r -> r.group_missing_values)
      |> Jsont.Object.opt_mem "hidden_hits" Jsont.string ~enc:(fun r -> r.hidden_hits)
      |> Jsont.Object.opt_mem "highlight_affix_num_tokens" Jsont.int ~enc:(fun r -> r.highlight_affix_num_tokens)
      |> Jsont.Object.opt_mem "highlight_end_tag" Jsont.string ~enc:(fun r -> r.highlight_end_tag)
      |> Jsont.Object.opt_mem "highlight_fields" Jsont.string ~enc:(fun r -> r.highlight_fields)
      |> Jsont.Object.opt_mem "highlight_full_fields" Jsont.string ~enc:(fun r -> r.highlight_full_fields)
      |> Jsont.Object.opt_mem "highlight_start_tag" Jsont.string ~enc:(fun r -> r.highlight_start_tag)
      |> Jsont.Object.opt_mem "include_fields" Jsont.string ~enc:(fun r -> r.include_fields)
      |> Jsont.Object.opt_mem "infix" Jsont.string ~enc:(fun r -> r.infix)
      |> Jsont.Object.opt_mem "limit" Jsont.int ~enc:(fun r -> r.limit)
      |> Jsont.Object.opt_mem "max_candidates" Jsont.int ~enc:(fun r -> r.max_candidates)
      |> Jsont.Object.opt_mem "max_extra_prefix" Jsont.int ~enc:(fun r -> r.max_extra_prefix)
      |> Jsont.Object.opt_mem "max_extra_suffix" Jsont.int ~enc:(fun r -> r.max_extra_suffix)
      |> Jsont.Object.opt_mem "max_facet_values" Jsont.int ~enc:(fun r -> r.max_facet_values)
      |> Jsont.Object.opt_mem "max_filter_by_candidates" Jsont.int ~enc:(fun r -> r.max_filter_by_candidates)
      |> Jsont.Object.opt_mem "min_len_1typo" Jsont.int ~enc:(fun r -> r.min_len_1typo)
      |> Jsont.Object.opt_mem "min_len_2typo" Jsont.int ~enc:(fun r -> r.min_len_2typo)
      |> Jsont.Object.opt_mem "nl_model_id" Jsont.string ~enc:(fun r -> r.nl_model_id)
      |> Jsont.Object.opt_mem "nl_query" Jsont.bool ~enc:(fun r -> r.nl_query)
      |> Jsont.Object.opt_mem "num_typos" Jsont.string ~enc:(fun r -> r.num_typos)
      |> Jsont.Object.opt_mem "offset" Jsont.int ~enc:(fun r -> r.offset)
      |> Jsont.Object.opt_mem "override_tags" Jsont.string ~enc:(fun r -> r.override_tags)
      |> Jsont.Object.opt_mem "page" Jsont.int ~enc:(fun r -> r.page)
      |> Jsont.Object.opt_mem "per_page" Jsont.int ~enc:(fun r -> r.per_page)
      |> Jsont.Object.opt_mem "pinned_hits" Jsont.string ~enc:(fun r -> r.pinned_hits)
      |> Jsont.Object.opt_mem "pre_segmented_query" Jsont.bool ~enc:(fun r -> r.pre_segmented_query)
      |> Jsont.Object.opt_mem "prefix" Jsont.string ~enc:(fun r -> r.prefix)
      |> Jsont.Object.opt_mem "preset" Jsont.string ~enc:(fun r -> r.preset)
      |> Jsont.Object.mem "prioritize_exact_match" Jsont.bool ~dec_absent:true ~enc:(fun r -> r.prioritize_exact_match)
      |> Jsont.Object.mem "prioritize_num_matching_fields" Jsont.bool ~dec_absent:true ~enc:(fun r -> r.prioritize_num_matching_fields)
      |> Jsont.Object.mem "prioritize_token_position" Jsont.bool ~dec_absent:false ~enc:(fun r -> r.prioritize_token_position)
      |> Jsont.Object.opt_mem "q" Jsont.string ~enc:(fun r -> r.q)
      |> Jsont.Object.opt_mem "query_by" Jsont.string ~enc:(fun r -> r.query_by)
      |> Jsont.Object.opt_mem "query_by_weights" Jsont.string ~enc:(fun r -> r.query_by_weights)
      |> Jsont.Object.opt_mem "remote_embedding_num_tries" Jsont.int ~enc:(fun r -> r.remote_embedding_num_tries)
      |> Jsont.Object.opt_mem "remote_embedding_timeout_ms" Jsont.int ~enc:(fun r -> r.remote_embedding_timeout_ms)
      |> Jsont.Object.opt_mem "search_cutoff_ms" Jsont.int ~enc:(fun r -> r.search_cutoff_ms)
      |> Jsont.Object.opt_mem "snippet_threshold" Jsont.int ~enc:(fun r -> r.snippet_threshold)
      |> Jsont.Object.opt_mem "sort_by" Jsont.string ~enc:(fun r -> r.sort_by)
      |> Jsont.Object.opt_mem "split_join_tokens" Jsont.string ~enc:(fun r -> r.split_join_tokens)
      |> Jsont.Object.opt_mem "stopwords" Jsont.string ~enc:(fun r -> r.stopwords)
      |> Jsont.Object.opt_mem "synonym_num_typos" Jsont.int ~enc:(fun r -> r.synonym_num_typos)
      |> Jsont.Object.opt_mem "synonym_prefix" Jsont.bool ~enc:(fun r -> r.synonym_prefix)
      |> Jsont.Object.opt_mem "synonym_sets" Jsont.string ~enc:(fun r -> r.synonym_sets)
      |> Jsont.Object.opt_mem "text_match_type" Jsont.string ~enc:(fun r -> r.text_match_type)
      |> Jsont.Object.opt_mem "typo_tokens_threshold" Jsont.int ~enc:(fun r -> r.typo_tokens_threshold)
      |> Jsont.Object.opt_mem "use_cache" Jsont.bool ~enc:(fun r -> r.use_cache)
      |> Jsont.Object.opt_mem "vector_query" Jsont.string ~enc:(fun r -> r.vector_query)
      |> Jsont.Object.opt_mem "voice_query" Jsont.string ~enc:(fun r -> r.voice_query)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module MultiSearchParameters = struct
  module Types = struct
    module T = struct
      (** Parameters for the multi search API.
       *)
      type t = {
        cache_ttl : int option;  (** The duration (in seconds) that determines how long the search query is cached. This value can be set on a per-query basis. Default: 60.
       *)
        conversation : bool option;  (** Enable conversational search.
       *)
        conversation_id : string option;  (** The Id of a previous conversation to continue, this tells Typesense to include prior context when communicating with the LLM.
       *)
        conversation_model_id : string option;  (** The Id of Conversation Model to be used.
       *)
        drop_tokens_mode : DropTokensMode.T.t option;
        drop_tokens_threshold : int option;  (** If the number of results found for a specific query is less than this number, Typesense will attempt to drop the tokens in the query until enough results are found. Tokens that have the least individual hits are dropped first. Set to 0 to disable. Default: 10
       *)
        enable_analytics : bool;  (** Flag for enabling/disabling analytics aggregation for specific search queries (for e.g. those originating from a test script).
       *)
        enable_overrides : bool;  (** If you have some overrides defined but want to disable all of them during query time, you can do that by setting this parameter to false
       *)
        enable_synonyms : bool option;  (** If you have some synonyms defined but want to disable all of them for a particular search query, set enable_synonyms to false. Default: true
       *)
        enable_typos_for_alpha_numerical_tokens : bool option;  (** Set this parameter to false to disable typos on alphanumerical query tokens. Default: true.
       *)
        enable_typos_for_numerical_tokens : bool;  (** Make Typesense disable typos for numerical tokens.
       *)
        exclude_fields : string option;  (** List of fields from the document to exclude in the search result *)
        exhaustive_search : bool option;  (** Setting this to true will make Typesense consider all prefixes and typo corrections of the words in the query without stopping early when enough results are found (drop_tokens_threshold and typo_tokens_threshold configurations are ignored).
       *)
        facet_by : string option;  (** A list of fields that will be used for faceting your results on. Separate multiple fields with a comma. *)
        facet_query : string option;  (** Facet values that are returned can now be filtered via this parameter. The matching facet text is also highlighted. For example, when faceting by `category`, you can set `facet_query=category:shoe` to return only facet values that contain the prefix "shoe". *)
        facet_return_parent : string option;  (** Comma separated string of nested facet fields whose parent object should be returned in facet response.
       *)
        facet_strategy : string option;  (** Choose the underlying faceting strategy used. Comma separated string of allows values: exhaustive, top_values or automatic (default).
       *)
        filter_by : string option;  (** Filter conditions for refining youropen api validator search results. Separate multiple conditions with &&. *)
        filter_curated_hits : bool option;  (** Whether the filter_by condition of the search query should be applicable to curated results (override definitions, pinned hits, hidden hits, etc.). Default: false
       *)
        group_by : string option;  (** You can aggregate search results into groups or buckets by specify one or more `group_by` fields. Separate multiple fields with a comma. To group on a particular field, it must be a faceted field. *)
        group_limit : int option;  (** Maximum number of hits to be returned for every group. If the `group_limit` is set as `K` then only the top K hits in each group are returned in the response. Default: 3
       *)
        group_missing_values : bool option;  (** Setting this parameter to true will place all documents that have a null value in the group_by field, into a single group. Setting this parameter to false, will cause each document with a null value in the group_by field to not be grouped with other documents. Default: true
       *)
        hidden_hits : string option;  (** A list of records to unconditionally hide from search results. A list of `record_id`s to hide. Eg: to hide records with IDs 123 and 456, you'd specify `123,456`.
      You could also use the Overrides feature to override search results based on rules. Overrides are applied first, followed by `pinned_hits` and finally `hidden_hits`.
       *)
        highlight_affix_num_tokens : int option;  (** The number of tokens that should surround the highlighted text on each side. Default: 4
       *)
        highlight_end_tag : string option;  (** The end tag used for the highlighted snippets. Default: `</mark>`
       *)
        highlight_fields : string option;  (** A list of custom fields that must be highlighted even if you don't query for them
       *)
        highlight_full_fields : string option;  (** List of fields which should be highlighted fully without snippeting *)
        highlight_start_tag : string option;  (** The start tag used for the highlighted snippets. Default: `<mark>`
       *)
        include_fields : string option;  (** List of fields from the document to include in the search result *)
        infix : string option;  (** If infix index is enabled for this field, infix searching can be done on a per-field basis by sending a comma separated string parameter called infix to the search query. This parameter can have 3 values; `off` infix search is disabled, which is default `always` infix search is performed along with regular search `fallback` infix search is performed if regular search does not produce results *)
        limit : int option;  (** Number of hits to fetch. Can be used as an alternative to the per_page parameter. Default: 10.
       *)
        max_extra_prefix : int option;  (** There are also 2 parameters that allow you to control the extent of infix searching max_extra_prefix and max_extra_suffix which specify the maximum number of symbols before or after the query that can be present in the token. For example query "K2100" has 2 extra symbols in "6PK2100". By default, any number of prefixes/suffixes can be present for a match. *)
        max_extra_suffix : int option;  (** There are also 2 parameters that allow you to control the extent of infix searching max_extra_prefix and max_extra_suffix which specify the maximum number of symbols before or after the query that can be present in the token. For example query "K2100" has 2 extra symbols in "6PK2100". By default, any number of prefixes/suffixes can be present for a match. *)
        max_facet_values : int option;  (** Maximum number of facet values to be returned. *)
        min_len_1typo : int option;  (** Minimum word length for 1-typo correction to be applied. The value of num_typos is still treated as the maximum allowed typos.
       *)
        min_len_2typo : int option;  (** Minimum word length for 2-typo correction to be applied. The value of num_typos is still treated as the maximum allowed typos.
       *)
        num_typos : string option;  (** The number of typographical errors (1 or 2) that would be tolerated. Default: 2
       *)
        offset : int option;  (** Identifies the starting point to return hits from a result set. Can be used as an alternative to the page parameter. *)
        override_tags : string option;  (** Comma separated list of tags to trigger the curations rules that match the tags. *)
        page : int option;  (** Results from this specific page number would be fetched. *)
        per_page : int option;  (** Number of results to fetch per page. Default: 10 *)
        pinned_hits : string option;  (** A list of records to unconditionally include in the search results at specific positions. An example use case would be to feature or promote certain items on the top of search results. A list of `record_id:hit_position`. Eg: to include a record with ID 123 at Position 1 and another record with ID 456 at Position 5, you'd specify `123:1,456:5`.
      You could also use the Overrides feature to override search results based on rules. Overrides are applied first, followed by `pinned_hits` and finally `hidden_hits`.
       *)
        pre_segmented_query : bool;  (** You can index content from any logographic language into Typesense if you are able to segment / split the text into space-separated words yourself before indexing and querying.
      Set this parameter to true to do the same
       *)
        prefix : string option;  (** Boolean field to indicate that the last word in the query should be treated as a prefix, and not as a whole word. This is used for building autocomplete and instant search interfaces. Defaults to true. *)
        preset : string option;  (** Search using a bunch of search parameters by setting this parameter to the name of the existing Preset.
       *)
        prioritize_exact_match : bool;  (** Set this parameter to true to ensure that an exact match is ranked above the others
       *)
        prioritize_num_matching_fields : bool;  (** Make Typesense prioritize documents where the query words appear in more number of fields.
       *)
        prioritize_token_position : bool;  (** Make Typesense prioritize documents where the query words appear earlier in the text.
       *)
        q : string option;  (** The query text to search for in the collection. Use * as the search string to return all documents. This is typically useful when used in conjunction with filter_by. *)
        query_by : string option;  (** A list of `string` fields that should be queried against. Multiple fields are separated with a comma. *)
        query_by_weights : string option;  (** The relative weight to give each `query_by` field when ranking results. This can be used to boost fields in priority, when looking for matches. Multiple fields are separated with a comma. *)
        remote_embedding_num_tries : int option;  (** Number of times to retry fetching remote embeddings.
       *)
        remote_embedding_timeout_ms : int option;  (** Timeout (in milliseconds) for fetching remote embeddings.
       *)
        search_cutoff_ms : int option;  (** Typesense will attempt to return results early if the cutoff time has elapsed. This is not a strict guarantee and facet computation is not bound by this parameter.
       *)
        snippet_threshold : int option;  (** Field values under this length will be fully highlighted, instead of showing a snippet of relevant portion. Default: 30
       *)
        sort_by : string option;  (** A list of numerical fields and their corresponding sort orders that will be used for ordering your results. Up to 3 sort fields can be specified. The text similarity score is exposed as a special `_text_match` field that you can use in the list of sorting fields. If no `sort_by` parameter is specified, results are sorted by `_text_match:desc,default_sorting_field:desc` *)
        stopwords : string option;  (** Name of the stopwords set to apply for this search, the keywords present in the set will be removed from the search query.
       *)
        synonym_num_typos : int option;  (** Allow synonym resolution on typo-corrected words in the query. Default: 0
       *)
        synonym_prefix : bool option;  (** Allow synonym resolution on word prefixes in the query. Default: false
       *)
        text_match_type : string option;  (** In a multi-field matching context, this parameter determines how the representative text match score of a record is calculated. Possible values are max_score (default) or max_weight. *)
        typo_tokens_threshold : int option;  (** If the number of results found for a specific query is less than this number, Typesense will attempt to look for tokens with more typos until enough results are found. Default: 100
       *)
        use_cache : bool option;  (** Enable server side caching of search query results. By default, caching is disabled.
       *)
        vector_query : string option;  (** Vector query expression for fetching documents "closest" to a given query/document vector.
       *)
        voice_query : string option;  (** The base64 encoded audio file in 16 khz 16-bit WAV format.
       *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?(enable_analytics=true) ?(enable_overrides=false) ?(enable_typos_for_numerical_tokens=true) ?(pre_segmented_query=false) ?(prioritize_exact_match=true) ?(prioritize_num_matching_fields=true) ?(prioritize_token_position=false) ?cache_ttl ?conversation ?conversation_id ?conversation_model_id ?drop_tokens_mode ?drop_tokens_threshold ?enable_synonyms ?enable_typos_for_alpha_numerical_tokens ?exclude_fields ?exhaustive_search ?facet_by ?facet_query ?facet_return_parent ?facet_strategy ?filter_by ?filter_curated_hits ?group_by ?group_limit ?group_missing_values ?hidden_hits ?highlight_affix_num_tokens ?highlight_end_tag ?highlight_fields ?highlight_full_fields ?highlight_start_tag ?include_fields ?infix ?limit ?max_extra_prefix ?max_extra_suffix ?max_facet_values ?min_len_1typo ?min_len_2typo ?num_typos ?offset ?override_tags ?page ?per_page ?pinned_hits ?prefix ?preset ?q ?query_by ?query_by_weights ?remote_embedding_num_tries ?remote_embedding_timeout_ms ?search_cutoff_ms ?snippet_threshold ?sort_by ?stopwords ?synonym_num_typos ?synonym_prefix ?text_match_type ?typo_tokens_threshold ?use_cache ?vector_query ?voice_query () = { cache_ttl; conversation; conversation_id; conversation_model_id; drop_tokens_mode; drop_tokens_threshold; enable_analytics; enable_overrides; enable_synonyms; enable_typos_for_alpha_numerical_tokens; enable_typos_for_numerical_tokens; exclude_fields; exhaustive_search; facet_by; facet_query; facet_return_parent; facet_strategy; filter_by; filter_curated_hits; group_by; group_limit; group_missing_values; hidden_hits; highlight_affix_num_tokens; highlight_end_tag; highlight_fields; highlight_full_fields; highlight_start_tag; include_fields; infix; limit; max_extra_prefix; max_extra_suffix; max_facet_values; min_len_1typo; min_len_2typo; num_typos; offset; override_tags; page; per_page; pinned_hits; pre_segmented_query; prefix; preset; prioritize_exact_match; prioritize_num_matching_fields; prioritize_token_position; q; query_by; query_by_weights; remote_embedding_num_tries; remote_embedding_timeout_ms; search_cutoff_ms; snippet_threshold; sort_by; stopwords; synonym_num_typos; synonym_prefix; text_match_type; typo_tokens_threshold; use_cache; vector_query; voice_query }
    
    let cache_ttl t = t.cache_ttl
    let conversation t = t.conversation
    let conversation_id t = t.conversation_id
    let conversation_model_id t = t.conversation_model_id
    let drop_tokens_mode t = t.drop_tokens_mode
    let drop_tokens_threshold t = t.drop_tokens_threshold
    let enable_analytics t = t.enable_analytics
    let enable_overrides t = t.enable_overrides
    let enable_synonyms t = t.enable_synonyms
    let enable_typos_for_alpha_numerical_tokens t = t.enable_typos_for_alpha_numerical_tokens
    let enable_typos_for_numerical_tokens t = t.enable_typos_for_numerical_tokens
    let exclude_fields t = t.exclude_fields
    let exhaustive_search t = t.exhaustive_search
    let facet_by t = t.facet_by
    let facet_query t = t.facet_query
    let facet_return_parent t = t.facet_return_parent
    let facet_strategy t = t.facet_strategy
    let filter_by t = t.filter_by
    let filter_curated_hits t = t.filter_curated_hits
    let group_by t = t.group_by
    let group_limit t = t.group_limit
    let group_missing_values t = t.group_missing_values
    let hidden_hits t = t.hidden_hits
    let highlight_affix_num_tokens t = t.highlight_affix_num_tokens
    let highlight_end_tag t = t.highlight_end_tag
    let highlight_fields t = t.highlight_fields
    let highlight_full_fields t = t.highlight_full_fields
    let highlight_start_tag t = t.highlight_start_tag
    let include_fields t = t.include_fields
    let infix t = t.infix
    let limit t = t.limit
    let max_extra_prefix t = t.max_extra_prefix
    let max_extra_suffix t = t.max_extra_suffix
    let max_facet_values t = t.max_facet_values
    let min_len_1typo t = t.min_len_1typo
    let min_len_2typo t = t.min_len_2typo
    let num_typos t = t.num_typos
    let offset t = t.offset
    let override_tags t = t.override_tags
    let page t = t.page
    let per_page t = t.per_page
    let pinned_hits t = t.pinned_hits
    let pre_segmented_query t = t.pre_segmented_query
    let prefix t = t.prefix
    let preset t = t.preset
    let prioritize_exact_match t = t.prioritize_exact_match
    let prioritize_num_matching_fields t = t.prioritize_num_matching_fields
    let prioritize_token_position t = t.prioritize_token_position
    let q t = t.q
    let query_by t = t.query_by
    let query_by_weights t = t.query_by_weights
    let remote_embedding_num_tries t = t.remote_embedding_num_tries
    let remote_embedding_timeout_ms t = t.remote_embedding_timeout_ms
    let search_cutoff_ms t = t.search_cutoff_ms
    let snippet_threshold t = t.snippet_threshold
    let sort_by t = t.sort_by
    let stopwords t = t.stopwords
    let synonym_num_typos t = t.synonym_num_typos
    let synonym_prefix t = t.synonym_prefix
    let text_match_type t = t.text_match_type
    let typo_tokens_threshold t = t.typo_tokens_threshold
    let use_cache t = t.use_cache
    let vector_query t = t.vector_query
    let voice_query t = t.voice_query
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"MultiSearchParameters"
        (fun cache_ttl conversation conversation_id conversation_model_id drop_tokens_mode drop_tokens_threshold enable_analytics enable_overrides enable_synonyms enable_typos_for_alpha_numerical_tokens enable_typos_for_numerical_tokens exclude_fields exhaustive_search facet_by facet_query facet_return_parent facet_strategy filter_by filter_curated_hits group_by group_limit group_missing_values hidden_hits highlight_affix_num_tokens highlight_end_tag highlight_fields highlight_full_fields highlight_start_tag include_fields infix limit max_extra_prefix max_extra_suffix max_facet_values min_len_1typo min_len_2typo num_typos offset override_tags page per_page pinned_hits pre_segmented_query prefix preset prioritize_exact_match prioritize_num_matching_fields prioritize_token_position q query_by query_by_weights remote_embedding_num_tries remote_embedding_timeout_ms search_cutoff_ms snippet_threshold sort_by stopwords synonym_num_typos synonym_prefix text_match_type typo_tokens_threshold use_cache vector_query voice_query -> { cache_ttl; conversation; conversation_id; conversation_model_id; drop_tokens_mode; drop_tokens_threshold; enable_analytics; enable_overrides; enable_synonyms; enable_typos_for_alpha_numerical_tokens; enable_typos_for_numerical_tokens; exclude_fields; exhaustive_search; facet_by; facet_query; facet_return_parent; facet_strategy; filter_by; filter_curated_hits; group_by; group_limit; group_missing_values; hidden_hits; highlight_affix_num_tokens; highlight_end_tag; highlight_fields; highlight_full_fields; highlight_start_tag; include_fields; infix; limit; max_extra_prefix; max_extra_suffix; max_facet_values; min_len_1typo; min_len_2typo; num_typos; offset; override_tags; page; per_page; pinned_hits; pre_segmented_query; prefix; preset; prioritize_exact_match; prioritize_num_matching_fields; prioritize_token_position; q; query_by; query_by_weights; remote_embedding_num_tries; remote_embedding_timeout_ms; search_cutoff_ms; snippet_threshold; sort_by; stopwords; synonym_num_typos; synonym_prefix; text_match_type; typo_tokens_threshold; use_cache; vector_query; voice_query })
      |> Jsont.Object.opt_mem "cache_ttl" Jsont.int ~enc:(fun r -> r.cache_ttl)
      |> Jsont.Object.opt_mem "conversation" Jsont.bool ~enc:(fun r -> r.conversation)
      |> Jsont.Object.opt_mem "conversation_id" Jsont.string ~enc:(fun r -> r.conversation_id)
      |> Jsont.Object.opt_mem "conversation_model_id" Jsont.string ~enc:(fun r -> r.conversation_model_id)
      |> Jsont.Object.opt_mem "drop_tokens_mode" DropTokensMode.T.jsont ~enc:(fun r -> r.drop_tokens_mode)
      |> Jsont.Object.opt_mem "drop_tokens_threshold" Jsont.int ~enc:(fun r -> r.drop_tokens_threshold)
      |> Jsont.Object.mem "enable_analytics" Jsont.bool ~dec_absent:true ~enc:(fun r -> r.enable_analytics)
      |> Jsont.Object.mem "enable_overrides" Jsont.bool ~dec_absent:false ~enc:(fun r -> r.enable_overrides)
      |> Jsont.Object.opt_mem "enable_synonyms" Jsont.bool ~enc:(fun r -> r.enable_synonyms)
      |> Jsont.Object.opt_mem "enable_typos_for_alpha_numerical_tokens" Jsont.bool ~enc:(fun r -> r.enable_typos_for_alpha_numerical_tokens)
      |> Jsont.Object.mem "enable_typos_for_numerical_tokens" Jsont.bool ~dec_absent:true ~enc:(fun r -> r.enable_typos_for_numerical_tokens)
      |> Jsont.Object.opt_mem "exclude_fields" Jsont.string ~enc:(fun r -> r.exclude_fields)
      |> Jsont.Object.opt_mem "exhaustive_search" Jsont.bool ~enc:(fun r -> r.exhaustive_search)
      |> Jsont.Object.opt_mem "facet_by" Jsont.string ~enc:(fun r -> r.facet_by)
      |> Jsont.Object.opt_mem "facet_query" Jsont.string ~enc:(fun r -> r.facet_query)
      |> Jsont.Object.opt_mem "facet_return_parent" Jsont.string ~enc:(fun r -> r.facet_return_parent)
      |> Jsont.Object.opt_mem "facet_strategy" Jsont.string ~enc:(fun r -> r.facet_strategy)
      |> Jsont.Object.opt_mem "filter_by" Jsont.string ~enc:(fun r -> r.filter_by)
      |> Jsont.Object.opt_mem "filter_curated_hits" Jsont.bool ~enc:(fun r -> r.filter_curated_hits)
      |> Jsont.Object.opt_mem "group_by" Jsont.string ~enc:(fun r -> r.group_by)
      |> Jsont.Object.opt_mem "group_limit" Jsont.int ~enc:(fun r -> r.group_limit)
      |> Jsont.Object.opt_mem "group_missing_values" Jsont.bool ~enc:(fun r -> r.group_missing_values)
      |> Jsont.Object.opt_mem "hidden_hits" Jsont.string ~enc:(fun r -> r.hidden_hits)
      |> Jsont.Object.opt_mem "highlight_affix_num_tokens" Jsont.int ~enc:(fun r -> r.highlight_affix_num_tokens)
      |> Jsont.Object.opt_mem "highlight_end_tag" Jsont.string ~enc:(fun r -> r.highlight_end_tag)
      |> Jsont.Object.opt_mem "highlight_fields" Jsont.string ~enc:(fun r -> r.highlight_fields)
      |> Jsont.Object.opt_mem "highlight_full_fields" Jsont.string ~enc:(fun r -> r.highlight_full_fields)
      |> Jsont.Object.opt_mem "highlight_start_tag" Jsont.string ~enc:(fun r -> r.highlight_start_tag)
      |> Jsont.Object.opt_mem "include_fields" Jsont.string ~enc:(fun r -> r.include_fields)
      |> Jsont.Object.opt_mem "infix" Jsont.string ~enc:(fun r -> r.infix)
      |> Jsont.Object.opt_mem "limit" Jsont.int ~enc:(fun r -> r.limit)
      |> Jsont.Object.opt_mem "max_extra_prefix" Jsont.int ~enc:(fun r -> r.max_extra_prefix)
      |> Jsont.Object.opt_mem "max_extra_suffix" Jsont.int ~enc:(fun r -> r.max_extra_suffix)
      |> Jsont.Object.opt_mem "max_facet_values" Jsont.int ~enc:(fun r -> r.max_facet_values)
      |> Jsont.Object.opt_mem "min_len_1typo" Jsont.int ~enc:(fun r -> r.min_len_1typo)
      |> Jsont.Object.opt_mem "min_len_2typo" Jsont.int ~enc:(fun r -> r.min_len_2typo)
      |> Jsont.Object.opt_mem "num_typos" Jsont.string ~enc:(fun r -> r.num_typos)
      |> Jsont.Object.opt_mem "offset" Jsont.int ~enc:(fun r -> r.offset)
      |> Jsont.Object.opt_mem "override_tags" Jsont.string ~enc:(fun r -> r.override_tags)
      |> Jsont.Object.opt_mem "page" Jsont.int ~enc:(fun r -> r.page)
      |> Jsont.Object.opt_mem "per_page" Jsont.int ~enc:(fun r -> r.per_page)
      |> Jsont.Object.opt_mem "pinned_hits" Jsont.string ~enc:(fun r -> r.pinned_hits)
      |> Jsont.Object.mem "pre_segmented_query" Jsont.bool ~dec_absent:false ~enc:(fun r -> r.pre_segmented_query)
      |> Jsont.Object.opt_mem "prefix" Jsont.string ~enc:(fun r -> r.prefix)
      |> Jsont.Object.opt_mem "preset" Jsont.string ~enc:(fun r -> r.preset)
      |> Jsont.Object.mem "prioritize_exact_match" Jsont.bool ~dec_absent:true ~enc:(fun r -> r.prioritize_exact_match)
      |> Jsont.Object.mem "prioritize_num_matching_fields" Jsont.bool ~dec_absent:true ~enc:(fun r -> r.prioritize_num_matching_fields)
      |> Jsont.Object.mem "prioritize_token_position" Jsont.bool ~dec_absent:false ~enc:(fun r -> r.prioritize_token_position)
      |> Jsont.Object.opt_mem "q" Jsont.string ~enc:(fun r -> r.q)
      |> Jsont.Object.opt_mem "query_by" Jsont.string ~enc:(fun r -> r.query_by)
      |> Jsont.Object.opt_mem "query_by_weights" Jsont.string ~enc:(fun r -> r.query_by_weights)
      |> Jsont.Object.opt_mem "remote_embedding_num_tries" Jsont.int ~enc:(fun r -> r.remote_embedding_num_tries)
      |> Jsont.Object.opt_mem "remote_embedding_timeout_ms" Jsont.int ~enc:(fun r -> r.remote_embedding_timeout_ms)
      |> Jsont.Object.opt_mem "search_cutoff_ms" Jsont.int ~enc:(fun r -> r.search_cutoff_ms)
      |> Jsont.Object.opt_mem "snippet_threshold" Jsont.int ~enc:(fun r -> r.snippet_threshold)
      |> Jsont.Object.opt_mem "sort_by" Jsont.string ~enc:(fun r -> r.sort_by)
      |> Jsont.Object.opt_mem "stopwords" Jsont.string ~enc:(fun r -> r.stopwords)
      |> Jsont.Object.opt_mem "synonym_num_typos" Jsont.int ~enc:(fun r -> r.synonym_num_typos)
      |> Jsont.Object.opt_mem "synonym_prefix" Jsont.bool ~enc:(fun r -> r.synonym_prefix)
      |> Jsont.Object.opt_mem "text_match_type" Jsont.string ~enc:(fun r -> r.text_match_type)
      |> Jsont.Object.opt_mem "typo_tokens_threshold" Jsont.int ~enc:(fun r -> r.typo_tokens_threshold)
      |> Jsont.Object.opt_mem "use_cache" Jsont.bool ~enc:(fun r -> r.use_cache)
      |> Jsont.Object.opt_mem "vector_query" Jsont.string ~enc:(fun r -> r.vector_query)
      |> Jsont.Object.opt_mem "voice_query" Jsont.string ~enc:(fun r -> r.voice_query)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module MultiSearchCollectionParameters = struct
  module Types = struct
    module T = struct
      type t = {
        cache_ttl : int option;  (** The duration (in seconds) that determines how long the search query is cached. This value can be set on a per-query basis. Default: 60.
       *)
        conversation : bool option;  (** Enable conversational search.
       *)
        conversation_id : string option;  (** The Id of a previous conversation to continue, this tells Typesense to include prior context when communicating with the LLM.
       *)
        conversation_model_id : string option;  (** The Id of Conversation Model to be used.
       *)
        drop_tokens_mode : DropTokensMode.T.t option;
        drop_tokens_threshold : int option;  (** If the number of results found for a specific query is less than this number, Typesense will attempt to drop the tokens in the query until enough results are found. Tokens that have the least individual hits are dropped first. Set to 0 to disable. Default: 10
       *)
        enable_analytics : bool;  (** Flag for enabling/disabling analytics aggregation for specific search queries (for e.g. those originating from a test script).
       *)
        enable_overrides : bool;  (** If you have some overrides defined but want to disable all of them during query time, you can do that by setting this parameter to false
       *)
        enable_synonyms : bool option;  (** If you have some synonyms defined but want to disable all of them for a particular search query, set enable_synonyms to false. Default: true
       *)
        enable_typos_for_alpha_numerical_tokens : bool option;  (** Set this parameter to false to disable typos on alphanumerical query tokens. Default: true.
       *)
        enable_typos_for_numerical_tokens : bool;  (** Make Typesense disable typos for numerical tokens.
       *)
        exclude_fields : string option;  (** List of fields from the document to exclude in the search result *)
        exhaustive_search : bool option;  (** Setting this to true will make Typesense consider all prefixes and typo corrections of the words in the query without stopping early when enough results are found (drop_tokens_threshold and typo_tokens_threshold configurations are ignored).
       *)
        facet_by : string option;  (** A list of fields that will be used for faceting your results on. Separate multiple fields with a comma. *)
        facet_query : string option;  (** Facet values that are returned can now be filtered via this parameter. The matching facet text is also highlighted. For example, when faceting by `category`, you can set `facet_query=category:shoe` to return only facet values that contain the prefix "shoe". *)
        facet_return_parent : string option;  (** Comma separated string of nested facet fields whose parent object should be returned in facet response.
       *)
        facet_strategy : string option;  (** Choose the underlying faceting strategy used. Comma separated string of allows values: exhaustive, top_values or automatic (default).
       *)
        filter_by : string option;  (** Filter conditions for refining youropen api validator search results. Separate multiple conditions with &&. *)
        filter_curated_hits : bool option;  (** Whether the filter_by condition of the search query should be applicable to curated results (override definitions, pinned hits, hidden hits, etc.). Default: false
       *)
        group_by : string option;  (** You can aggregate search results into groups or buckets by specify one or more `group_by` fields. Separate multiple fields with a comma. To group on a particular field, it must be a faceted field. *)
        group_limit : int option;  (** Maximum number of hits to be returned for every group. If the `group_limit` is set as `K` then only the top K hits in each group are returned in the response. Default: 3
       *)
        group_missing_values : bool option;  (** Setting this parameter to true will place all documents that have a null value in the group_by field, into a single group. Setting this parameter to false, will cause each document with a null value in the group_by field to not be grouped with other documents. Default: true
       *)
        hidden_hits : string option;  (** A list of records to unconditionally hide from search results. A list of `record_id`s to hide. Eg: to hide records with IDs 123 and 456, you'd specify `123,456`.
      You could also use the Overrides feature to override search results based on rules. Overrides are applied first, followed by `pinned_hits` and finally `hidden_hits`.
       *)
        highlight_affix_num_tokens : int option;  (** The number of tokens that should surround the highlighted text on each side. Default: 4
       *)
        highlight_end_tag : string option;  (** The end tag used for the highlighted snippets. Default: `</mark>`
       *)
        highlight_fields : string option;  (** A list of custom fields that must be highlighted even if you don't query for them
       *)
        highlight_full_fields : string option;  (** List of fields which should be highlighted fully without snippeting *)
        highlight_start_tag : string option;  (** The start tag used for the highlighted snippets. Default: `<mark>`
       *)
        include_fields : string option;  (** List of fields from the document to include in the search result *)
        infix : string option;  (** If infix index is enabled for this field, infix searching can be done on a per-field basis by sending a comma separated string parameter called infix to the search query. This parameter can have 3 values; `off` infix search is disabled, which is default `always` infix search is performed along with regular search `fallback` infix search is performed if regular search does not produce results *)
        limit : int option;  (** Number of hits to fetch. Can be used as an alternative to the per_page parameter. Default: 10.
       *)
        max_extra_prefix : int option;  (** There are also 2 parameters that allow you to control the extent of infix searching max_extra_prefix and max_extra_suffix which specify the maximum number of symbols before or after the query that can be present in the token. For example query "K2100" has 2 extra symbols in "6PK2100". By default, any number of prefixes/suffixes can be present for a match. *)
        max_extra_suffix : int option;  (** There are also 2 parameters that allow you to control the extent of infix searching max_extra_prefix and max_extra_suffix which specify the maximum number of symbols before or after the query that can be present in the token. For example query "K2100" has 2 extra symbols in "6PK2100". By default, any number of prefixes/suffixes can be present for a match. *)
        max_facet_values : int option;  (** Maximum number of facet values to be returned. *)
        min_len_1typo : int option;  (** Minimum word length for 1-typo correction to be applied. The value of num_typos is still treated as the maximum allowed typos.
       *)
        min_len_2typo : int option;  (** Minimum word length for 2-typo correction to be applied. The value of num_typos is still treated as the maximum allowed typos.
       *)
        num_typos : string option;  (** The number of typographical errors (1 or 2) that would be tolerated. Default: 2
       *)
        offset : int option;  (** Identifies the starting point to return hits from a result set. Can be used as an alternative to the page parameter. *)
        override_tags : string option;  (** Comma separated list of tags to trigger the curations rules that match the tags. *)
        page : int option;  (** Results from this specific page number would be fetched. *)
        per_page : int option;  (** Number of results to fetch per page. Default: 10 *)
        pinned_hits : string option;  (** A list of records to unconditionally include in the search results at specific positions. An example use case would be to feature or promote certain items on the top of search results. A list of `record_id:hit_position`. Eg: to include a record with ID 123 at Position 1 and another record with ID 456 at Position 5, you'd specify `123:1,456:5`.
      You could also use the Overrides feature to override search results based on rules. Overrides are applied first, followed by `pinned_hits` and finally `hidden_hits`.
       *)
        pre_segmented_query : bool;  (** You can index content from any logographic language into Typesense if you are able to segment / split the text into space-separated words yourself before indexing and querying.
      Set this parameter to true to do the same
       *)
        prefix : string option;  (** Boolean field to indicate that the last word in the query should be treated as a prefix, and not as a whole word. This is used for building autocomplete and instant search interfaces. Defaults to true. *)
        preset : string option;  (** Search using a bunch of search parameters by setting this parameter to the name of the existing Preset.
       *)
        prioritize_exact_match : bool;  (** Set this parameter to true to ensure that an exact match is ranked above the others
       *)
        prioritize_num_matching_fields : bool;  (** Make Typesense prioritize documents where the query words appear in more number of fields.
       *)
        prioritize_token_position : bool;  (** Make Typesense prioritize documents where the query words appear earlier in the text.
       *)
        q : string option;  (** The query text to search for in the collection. Use * as the search string to return all documents. This is typically useful when used in conjunction with filter_by. *)
        query_by : string option;  (** A list of `string` fields that should be queried against. Multiple fields are separated with a comma. *)
        query_by_weights : string option;  (** The relative weight to give each `query_by` field when ranking results. This can be used to boost fields in priority, when looking for matches. Multiple fields are separated with a comma. *)
        remote_embedding_num_tries : int option;  (** Number of times to retry fetching remote embeddings.
       *)
        remote_embedding_timeout_ms : int option;  (** Timeout (in milliseconds) for fetching remote embeddings.
       *)
        search_cutoff_ms : int option;  (** Typesense will attempt to return results early if the cutoff time has elapsed. This is not a strict guarantee and facet computation is not bound by this parameter.
       *)
        snippet_threshold : int option;  (** Field values under this length will be fully highlighted, instead of showing a snippet of relevant portion. Default: 30
       *)
        sort_by : string option;  (** A list of numerical fields and their corresponding sort orders that will be used for ordering your results. Up to 3 sort fields can be specified. The text similarity score is exposed as a special `_text_match` field that you can use in the list of sorting fields. If no `sort_by` parameter is specified, results are sorted by `_text_match:desc,default_sorting_field:desc` *)
        stopwords : string option;  (** Name of the stopwords set to apply for this search, the keywords present in the set will be removed from the search query.
       *)
        synonym_num_typos : int option;  (** Allow synonym resolution on typo-corrected words in the query. Default: 0
       *)
        synonym_prefix : bool option;  (** Allow synonym resolution on word prefixes in the query. Default: false
       *)
        text_match_type : string option;  (** In a multi-field matching context, this parameter determines how the representative text match score of a record is calculated. Possible values are max_score (default) or max_weight. *)
        typo_tokens_threshold : int option;  (** If the number of results found for a specific query is less than this number, Typesense will attempt to look for tokens with more typos until enough results are found. Default: 100
       *)
        use_cache : bool option;  (** Enable server side caching of search query results. By default, caching is disabled.
       *)
        vector_query : string option;  (** Vector query expression for fetching documents "closest" to a given query/document vector.
       *)
        voice_query : string option;  (** The base64 encoded audio file in 16 khz 16-bit WAV format.
       *)
        collection : string option;  (** The collection to search in.
       *)
        x_typesense_api_key : string option;  (** A separate search API key for each search within a multi_search request *)
        rerank_hybrid_matches : bool;  (** When true, computes both text match and vector distance scores for all matches in hybrid search. Documents found only through keyword search will get a vector distance score, and documents found only through vector search will get a text match score.
       *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?(enable_analytics=true) ?(enable_overrides=false) ?(enable_typos_for_numerical_tokens=true) ?(pre_segmented_query=false) ?(prioritize_exact_match=true) ?(prioritize_num_matching_fields=true) ?(prioritize_token_position=false) ?(rerank_hybrid_matches=false) ?cache_ttl ?conversation ?conversation_id ?conversation_model_id ?drop_tokens_mode ?drop_tokens_threshold ?enable_synonyms ?enable_typos_for_alpha_numerical_tokens ?exclude_fields ?exhaustive_search ?facet_by ?facet_query ?facet_return_parent ?facet_strategy ?filter_by ?filter_curated_hits ?group_by ?group_limit ?group_missing_values ?hidden_hits ?highlight_affix_num_tokens ?highlight_end_tag ?highlight_fields ?highlight_full_fields ?highlight_start_tag ?include_fields ?infix ?limit ?max_extra_prefix ?max_extra_suffix ?max_facet_values ?min_len_1typo ?min_len_2typo ?num_typos ?offset ?override_tags ?page ?per_page ?pinned_hits ?prefix ?preset ?q ?query_by ?query_by_weights ?remote_embedding_num_tries ?remote_embedding_timeout_ms ?search_cutoff_ms ?snippet_threshold ?sort_by ?stopwords ?synonym_num_typos ?synonym_prefix ?text_match_type ?typo_tokens_threshold ?use_cache ?vector_query ?voice_query ?collection ?x_typesense_api_key () = { cache_ttl; conversation; conversation_id; conversation_model_id; drop_tokens_mode; drop_tokens_threshold; enable_analytics; enable_overrides; enable_synonyms; enable_typos_for_alpha_numerical_tokens; enable_typos_for_numerical_tokens; exclude_fields; exhaustive_search; facet_by; facet_query; facet_return_parent; facet_strategy; filter_by; filter_curated_hits; group_by; group_limit; group_missing_values; hidden_hits; highlight_affix_num_tokens; highlight_end_tag; highlight_fields; highlight_full_fields; highlight_start_tag; include_fields; infix; limit; max_extra_prefix; max_extra_suffix; max_facet_values; min_len_1typo; min_len_2typo; num_typos; offset; override_tags; page; per_page; pinned_hits; pre_segmented_query; prefix; preset; prioritize_exact_match; prioritize_num_matching_fields; prioritize_token_position; q; query_by; query_by_weights; remote_embedding_num_tries; remote_embedding_timeout_ms; search_cutoff_ms; snippet_threshold; sort_by; stopwords; synonym_num_typos; synonym_prefix; text_match_type; typo_tokens_threshold; use_cache; vector_query; voice_query; collection; x_typesense_api_key; rerank_hybrid_matches }
    
    let cache_ttl t = t.cache_ttl
    let conversation t = t.conversation
    let conversation_id t = t.conversation_id
    let conversation_model_id t = t.conversation_model_id
    let drop_tokens_mode t = t.drop_tokens_mode
    let drop_tokens_threshold t = t.drop_tokens_threshold
    let enable_analytics t = t.enable_analytics
    let enable_overrides t = t.enable_overrides
    let enable_synonyms t = t.enable_synonyms
    let enable_typos_for_alpha_numerical_tokens t = t.enable_typos_for_alpha_numerical_tokens
    let enable_typos_for_numerical_tokens t = t.enable_typos_for_numerical_tokens
    let exclude_fields t = t.exclude_fields
    let exhaustive_search t = t.exhaustive_search
    let facet_by t = t.facet_by
    let facet_query t = t.facet_query
    let facet_return_parent t = t.facet_return_parent
    let facet_strategy t = t.facet_strategy
    let filter_by t = t.filter_by
    let filter_curated_hits t = t.filter_curated_hits
    let group_by t = t.group_by
    let group_limit t = t.group_limit
    let group_missing_values t = t.group_missing_values
    let hidden_hits t = t.hidden_hits
    let highlight_affix_num_tokens t = t.highlight_affix_num_tokens
    let highlight_end_tag t = t.highlight_end_tag
    let highlight_fields t = t.highlight_fields
    let highlight_full_fields t = t.highlight_full_fields
    let highlight_start_tag t = t.highlight_start_tag
    let include_fields t = t.include_fields
    let infix t = t.infix
    let limit t = t.limit
    let max_extra_prefix t = t.max_extra_prefix
    let max_extra_suffix t = t.max_extra_suffix
    let max_facet_values t = t.max_facet_values
    let min_len_1typo t = t.min_len_1typo
    let min_len_2typo t = t.min_len_2typo
    let num_typos t = t.num_typos
    let offset t = t.offset
    let override_tags t = t.override_tags
    let page t = t.page
    let per_page t = t.per_page
    let pinned_hits t = t.pinned_hits
    let pre_segmented_query t = t.pre_segmented_query
    let prefix t = t.prefix
    let preset t = t.preset
    let prioritize_exact_match t = t.prioritize_exact_match
    let prioritize_num_matching_fields t = t.prioritize_num_matching_fields
    let prioritize_token_position t = t.prioritize_token_position
    let q t = t.q
    let query_by t = t.query_by
    let query_by_weights t = t.query_by_weights
    let remote_embedding_num_tries t = t.remote_embedding_num_tries
    let remote_embedding_timeout_ms t = t.remote_embedding_timeout_ms
    let search_cutoff_ms t = t.search_cutoff_ms
    let snippet_threshold t = t.snippet_threshold
    let sort_by t = t.sort_by
    let stopwords t = t.stopwords
    let synonym_num_typos t = t.synonym_num_typos
    let synonym_prefix t = t.synonym_prefix
    let text_match_type t = t.text_match_type
    let typo_tokens_threshold t = t.typo_tokens_threshold
    let use_cache t = t.use_cache
    let vector_query t = t.vector_query
    let voice_query t = t.voice_query
    let collection t = t.collection
    let x_typesense_api_key t = t.x_typesense_api_key
    let rerank_hybrid_matches t = t.rerank_hybrid_matches
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"MultiSearchCollectionParameters"
        (fun cache_ttl conversation conversation_id conversation_model_id drop_tokens_mode drop_tokens_threshold enable_analytics enable_overrides enable_synonyms enable_typos_for_alpha_numerical_tokens enable_typos_for_numerical_tokens exclude_fields exhaustive_search facet_by facet_query facet_return_parent facet_strategy filter_by filter_curated_hits group_by group_limit group_missing_values hidden_hits highlight_affix_num_tokens highlight_end_tag highlight_fields highlight_full_fields highlight_start_tag include_fields infix limit max_extra_prefix max_extra_suffix max_facet_values min_len_1typo min_len_2typo num_typos offset override_tags page per_page pinned_hits pre_segmented_query prefix preset prioritize_exact_match prioritize_num_matching_fields prioritize_token_position q query_by query_by_weights remote_embedding_num_tries remote_embedding_timeout_ms search_cutoff_ms snippet_threshold sort_by stopwords synonym_num_typos synonym_prefix text_match_type typo_tokens_threshold use_cache vector_query voice_query collection x_typesense_api_key rerank_hybrid_matches -> { cache_ttl; conversation; conversation_id; conversation_model_id; drop_tokens_mode; drop_tokens_threshold; enable_analytics; enable_overrides; enable_synonyms; enable_typos_for_alpha_numerical_tokens; enable_typos_for_numerical_tokens; exclude_fields; exhaustive_search; facet_by; facet_query; facet_return_parent; facet_strategy; filter_by; filter_curated_hits; group_by; group_limit; group_missing_values; hidden_hits; highlight_affix_num_tokens; highlight_end_tag; highlight_fields; highlight_full_fields; highlight_start_tag; include_fields; infix; limit; max_extra_prefix; max_extra_suffix; max_facet_values; min_len_1typo; min_len_2typo; num_typos; offset; override_tags; page; per_page; pinned_hits; pre_segmented_query; prefix; preset; prioritize_exact_match; prioritize_num_matching_fields; prioritize_token_position; q; query_by; query_by_weights; remote_embedding_num_tries; remote_embedding_timeout_ms; search_cutoff_ms; snippet_threshold; sort_by; stopwords; synonym_num_typos; synonym_prefix; text_match_type; typo_tokens_threshold; use_cache; vector_query; voice_query; collection; x_typesense_api_key; rerank_hybrid_matches })
      |> Jsont.Object.opt_mem "cache_ttl" Jsont.int ~enc:(fun r -> r.cache_ttl)
      |> Jsont.Object.opt_mem "conversation" Jsont.bool ~enc:(fun r -> r.conversation)
      |> Jsont.Object.opt_mem "conversation_id" Jsont.string ~enc:(fun r -> r.conversation_id)
      |> Jsont.Object.opt_mem "conversation_model_id" Jsont.string ~enc:(fun r -> r.conversation_model_id)
      |> Jsont.Object.opt_mem "drop_tokens_mode" DropTokensMode.T.jsont ~enc:(fun r -> r.drop_tokens_mode)
      |> Jsont.Object.opt_mem "drop_tokens_threshold" Jsont.int ~enc:(fun r -> r.drop_tokens_threshold)
      |> Jsont.Object.mem "enable_analytics" Jsont.bool ~dec_absent:true ~enc:(fun r -> r.enable_analytics)
      |> Jsont.Object.mem "enable_overrides" Jsont.bool ~dec_absent:false ~enc:(fun r -> r.enable_overrides)
      |> Jsont.Object.opt_mem "enable_synonyms" Jsont.bool ~enc:(fun r -> r.enable_synonyms)
      |> Jsont.Object.opt_mem "enable_typos_for_alpha_numerical_tokens" Jsont.bool ~enc:(fun r -> r.enable_typos_for_alpha_numerical_tokens)
      |> Jsont.Object.mem "enable_typos_for_numerical_tokens" Jsont.bool ~dec_absent:true ~enc:(fun r -> r.enable_typos_for_numerical_tokens)
      |> Jsont.Object.opt_mem "exclude_fields" Jsont.string ~enc:(fun r -> r.exclude_fields)
      |> Jsont.Object.opt_mem "exhaustive_search" Jsont.bool ~enc:(fun r -> r.exhaustive_search)
      |> Jsont.Object.opt_mem "facet_by" Jsont.string ~enc:(fun r -> r.facet_by)
      |> Jsont.Object.opt_mem "facet_query" Jsont.string ~enc:(fun r -> r.facet_query)
      |> Jsont.Object.opt_mem "facet_return_parent" Jsont.string ~enc:(fun r -> r.facet_return_parent)
      |> Jsont.Object.opt_mem "facet_strategy" Jsont.string ~enc:(fun r -> r.facet_strategy)
      |> Jsont.Object.opt_mem "filter_by" Jsont.string ~enc:(fun r -> r.filter_by)
      |> Jsont.Object.opt_mem "filter_curated_hits" Jsont.bool ~enc:(fun r -> r.filter_curated_hits)
      |> Jsont.Object.opt_mem "group_by" Jsont.string ~enc:(fun r -> r.group_by)
      |> Jsont.Object.opt_mem "group_limit" Jsont.int ~enc:(fun r -> r.group_limit)
      |> Jsont.Object.opt_mem "group_missing_values" Jsont.bool ~enc:(fun r -> r.group_missing_values)
      |> Jsont.Object.opt_mem "hidden_hits" Jsont.string ~enc:(fun r -> r.hidden_hits)
      |> Jsont.Object.opt_mem "highlight_affix_num_tokens" Jsont.int ~enc:(fun r -> r.highlight_affix_num_tokens)
      |> Jsont.Object.opt_mem "highlight_end_tag" Jsont.string ~enc:(fun r -> r.highlight_end_tag)
      |> Jsont.Object.opt_mem "highlight_fields" Jsont.string ~enc:(fun r -> r.highlight_fields)
      |> Jsont.Object.opt_mem "highlight_full_fields" Jsont.string ~enc:(fun r -> r.highlight_full_fields)
      |> Jsont.Object.opt_mem "highlight_start_tag" Jsont.string ~enc:(fun r -> r.highlight_start_tag)
      |> Jsont.Object.opt_mem "include_fields" Jsont.string ~enc:(fun r -> r.include_fields)
      |> Jsont.Object.opt_mem "infix" Jsont.string ~enc:(fun r -> r.infix)
      |> Jsont.Object.opt_mem "limit" Jsont.int ~enc:(fun r -> r.limit)
      |> Jsont.Object.opt_mem "max_extra_prefix" Jsont.int ~enc:(fun r -> r.max_extra_prefix)
      |> Jsont.Object.opt_mem "max_extra_suffix" Jsont.int ~enc:(fun r -> r.max_extra_suffix)
      |> Jsont.Object.opt_mem "max_facet_values" Jsont.int ~enc:(fun r -> r.max_facet_values)
      |> Jsont.Object.opt_mem "min_len_1typo" Jsont.int ~enc:(fun r -> r.min_len_1typo)
      |> Jsont.Object.opt_mem "min_len_2typo" Jsont.int ~enc:(fun r -> r.min_len_2typo)
      |> Jsont.Object.opt_mem "num_typos" Jsont.string ~enc:(fun r -> r.num_typos)
      |> Jsont.Object.opt_mem "offset" Jsont.int ~enc:(fun r -> r.offset)
      |> Jsont.Object.opt_mem "override_tags" Jsont.string ~enc:(fun r -> r.override_tags)
      |> Jsont.Object.opt_mem "page" Jsont.int ~enc:(fun r -> r.page)
      |> Jsont.Object.opt_mem "per_page" Jsont.int ~enc:(fun r -> r.per_page)
      |> Jsont.Object.opt_mem "pinned_hits" Jsont.string ~enc:(fun r -> r.pinned_hits)
      |> Jsont.Object.mem "pre_segmented_query" Jsont.bool ~dec_absent:false ~enc:(fun r -> r.pre_segmented_query)
      |> Jsont.Object.opt_mem "prefix" Jsont.string ~enc:(fun r -> r.prefix)
      |> Jsont.Object.opt_mem "preset" Jsont.string ~enc:(fun r -> r.preset)
      |> Jsont.Object.mem "prioritize_exact_match" Jsont.bool ~dec_absent:true ~enc:(fun r -> r.prioritize_exact_match)
      |> Jsont.Object.mem "prioritize_num_matching_fields" Jsont.bool ~dec_absent:true ~enc:(fun r -> r.prioritize_num_matching_fields)
      |> Jsont.Object.mem "prioritize_token_position" Jsont.bool ~dec_absent:false ~enc:(fun r -> r.prioritize_token_position)
      |> Jsont.Object.opt_mem "q" Jsont.string ~enc:(fun r -> r.q)
      |> Jsont.Object.opt_mem "query_by" Jsont.string ~enc:(fun r -> r.query_by)
      |> Jsont.Object.opt_mem "query_by_weights" Jsont.string ~enc:(fun r -> r.query_by_weights)
      |> Jsont.Object.opt_mem "remote_embedding_num_tries" Jsont.int ~enc:(fun r -> r.remote_embedding_num_tries)
      |> Jsont.Object.opt_mem "remote_embedding_timeout_ms" Jsont.int ~enc:(fun r -> r.remote_embedding_timeout_ms)
      |> Jsont.Object.opt_mem "search_cutoff_ms" Jsont.int ~enc:(fun r -> r.search_cutoff_ms)
      |> Jsont.Object.opt_mem "snippet_threshold" Jsont.int ~enc:(fun r -> r.snippet_threshold)
      |> Jsont.Object.opt_mem "sort_by" Jsont.string ~enc:(fun r -> r.sort_by)
      |> Jsont.Object.opt_mem "stopwords" Jsont.string ~enc:(fun r -> r.stopwords)
      |> Jsont.Object.opt_mem "synonym_num_typos" Jsont.int ~enc:(fun r -> r.synonym_num_typos)
      |> Jsont.Object.opt_mem "synonym_prefix" Jsont.bool ~enc:(fun r -> r.synonym_prefix)
      |> Jsont.Object.opt_mem "text_match_type" Jsont.string ~enc:(fun r -> r.text_match_type)
      |> Jsont.Object.opt_mem "typo_tokens_threshold" Jsont.int ~enc:(fun r -> r.typo_tokens_threshold)
      |> Jsont.Object.opt_mem "use_cache" Jsont.bool ~enc:(fun r -> r.use_cache)
      |> Jsont.Object.opt_mem "vector_query" Jsont.string ~enc:(fun r -> r.vector_query)
      |> Jsont.Object.opt_mem "voice_query" Jsont.string ~enc:(fun r -> r.voice_query)
      |> Jsont.Object.opt_mem "collection" Jsont.string ~enc:(fun r -> r.collection)
      |> Jsont.Object.opt_mem "x-typesense-api-key" Jsont.string ~enc:(fun r -> r.x_typesense_api_key)
      |> Jsont.Object.mem "rerank_hybrid_matches" Jsont.bool ~dec_absent:false ~enc:(fun r -> r.rerank_hybrid_matches)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module MultiSearchSearchesParameter = struct
  module Types = struct
    module T = struct
      type t = {
        searches : MultiSearchCollectionParameters.T.t list;
        union : bool;  (** When true, merges the search results from each search query into a single ordered set of hits. *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~searches ?(union=false) () = { searches; union }
    
    let searches t = t.searches
    let union t = t.union
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"MultiSearchSearchesParameter"
        (fun searches union -> { searches; union })
      |> Jsont.Object.mem "searches" (Jsont.list MultiSearchCollectionParameters.T.jsont) ~enc:(fun r -> r.searches)
      |> Jsont.Object.mem "union" Jsont.bool ~dec_absent:false ~enc:(fun r -> r.union)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module MultiSearch = struct
  module Types = struct
    module Result = struct
      type t = {
        conversation : SearchResultConversation.T.t option;
        results : MultiSearchResult.Item.t list;
      }
    end
  end
  
  module Result = struct
    include Types.Result
    
    let v ~results ?conversation () = { conversation; results }
    
    let conversation t = t.conversation
    let results t = t.results
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"MultiSearchResult"
        (fun conversation results -> { conversation; results })
      |> Jsont.Object.opt_mem "conversation" SearchResultConversation.T.jsont ~enc:(fun r -> r.conversation)
      |> Jsont.Object.mem "results" (Jsont.list MultiSearchResult.Item.jsont) ~enc:(fun r -> r.results)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** send multiple search requests in a single HTTP request
  
      This is especially useful to avoid round-trip network latencies incurred otherwise if each of these requests are sent in separate HTTP requests. You can also use this feature to do a federated search across multiple collections in a single HTTP request. *)
  let multi_search ~multi_search_parameters ~body client () =
    let op_name = "multi_search" in
    let url_path = "/multi_search" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.singleton ~key:"multiSearchParameters" ~value:multi_search_parameters]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json MultiSearchSearchesParameter.T.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Result.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 400 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status;
        body;
        parsed_body;
      })
end

module DirtyValues = struct
  module Types = struct
    module T = struct
      type t = [
        | `Coerce_or_reject
        | `Coerce_or_drop
        | `Drop
        | `Reject
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"DirtyValues"
        ~dec:(function
          | "coerce_or_reject" -> `Coerce_or_reject
          | "coerce_or_drop" -> `Coerce_or_drop
          | "drop" -> `Drop
          | "reject" -> `Reject
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Coerce_or_reject -> "coerce_or_reject"
          | `Coerce_or_drop -> "coerce_or_drop"
          | `Drop -> "drop"
          | `Reject -> "reject")
  end
end

module CurationSetDeleteSchema = struct
  module Types = struct
    module T = struct
      type t = {
        name : string;  (** Name of the deleted curation set *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~name () = { name }
    
    let name t = t.name
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"CurationSetDeleteSchema"
        (fun name -> { name })
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Delete a curation set
  
      Delete a specific curation set by its name 
      @param curation_set_name The name of the curation set to delete
  *)
  let delete_curation_set ~curation_set_name client () =
    let op_name = "delete_curation_set" in
    let url_path = Openapi.Runtime.Path.render ~params:[("curationSetName", curation_set_name)] "/curation_sets/{curationSetName}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status;
        body;
        parsed_body;
      })
end

module CurationRule = struct
  module Types = struct
    module T = struct
      type t = {
        filter_by : string option;  (** Indicates that the curation should apply when the filter_by parameter in a search query exactly matches the string specified here (including backticks, spaces, brackets, etc).
       *)
        match_ : string option;  (** Indicates whether the match on the query term should be `exact` or `contains`. If we want to match all queries that contained the word `apple`, we will use the `contains` match instead.
       *)
        query : string option;  (** Indicates what search queries should be curated *)
        tags : string list option;  (** List of tag values to associate with this curation rule. *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?filter_by ?match_ ?query ?tags () = { filter_by; match_; query; tags }
    
    let filter_by t = t.filter_by
    let match_ t = t.match_
    let query t = t.query
    let tags t = t.tags
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"CurationRule"
        (fun filter_by match_ query tags -> { filter_by; match_; query; tags })
      |> Jsont.Object.opt_mem "filter_by" Jsont.string ~enc:(fun r -> r.filter_by)
      |> Jsont.Object.opt_mem "match" Jsont.string ~enc:(fun r -> r.match_)
      |> Jsont.Object.opt_mem "query" Jsont.string ~enc:(fun r -> r.query)
      |> Jsont.Object.opt_mem "tags" (Jsont.list Jsont.string) ~enc:(fun r -> r.tags)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module CurationItemDeleteSchema = struct
  module Types = struct
    module T = struct
      type t = {
        id : string;  (** ID of the deleted curation item *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~id () = { id }
    
    let id t = t.id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"CurationItemDeleteSchema"
        (fun id -> { id })
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Delete a curation set item
  
      Delete a specific curation item by its id 
      @param curation_set_name The name of the curation set
      @param item_id The id of the curation item to delete
  *)
  let delete_curation_set_item ~curation_set_name ~item_id client () =
    let op_name = "delete_curation_set_item" in
    let url_path = Openapi.Runtime.Path.render ~params:[("curationSetName", curation_set_name); ("itemId", item_id)] "/curation_sets/{curationSetName}/items/{itemId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status;
        body;
        parsed_body;
      })
end

module CurationInclude = struct
  module Types = struct
    module T = struct
      type t = {
        id : string;  (** document id that should be included *)
        position : int;  (** position number where document should be included in the search results *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~id ~position () = { id; position }
    
    let id t = t.id
    let position t = t.position
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"CurationInclude"
        (fun id position -> { id; position })
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "position" Jsont.int ~enc:(fun r -> r.position)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module CurationExclude = struct
  module Types = struct
    module T = struct
      type t = {
        id : string;  (** document id that should be excluded from the search results. *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~id () = { id }
    
    let id t = t.id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"CurationExclude"
        (fun id -> { id })
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module CurationItemCreateSchema = struct
  module Types = struct
    module T = struct
      type t = {
        effective_from_ts : int option;  (** A Unix timestamp that indicates the date/time from which the curation will be active. You can use this to create rules that start applying from a future point in time.
       *)
        effective_to_ts : int option;  (** A Unix timestamp that indicates the date/time until which the curation will be active. You can use this to create rules that stop applying after a period of time.
       *)
        excludes : CurationExclude.T.t list option;  (** List of document `id`s that should be excluded from the search results. *)
        filter_by : string option;  (** A filter by clause that is applied to any search query that matches the curation rule.
       *)
        filter_curated_hits : bool option;  (** When set to true, the filter conditions of the query is applied to the curated records as well. Default: false.
       *)
        id : string option;  (** ID of the curation item *)
        includes : CurationInclude.T.t list option;  (** List of document `id`s that should be included in the search results with their corresponding `position`s. *)
        metadata : Jsont.json option;  (** Return a custom JSON object in the Search API response, when this rule is triggered. This can can be used to display a pre-defined message (eg: a promotion banner) on the front-end when a particular rule is triggered.
       *)
        remove_matched_tokens : bool option;  (** Indicates whether search query tokens that exist in the curation's rule should be removed from the search query.
       *)
        replace_query : string option;  (** Replaces the current search query with this value, when the search query matches the curation rule.
       *)
        rule : CurationRule.T.t;
        sort_by : string option;  (** A sort by clause that is applied to any search query that matches the curation rule.
       *)
        stop_processing : bool option;  (** When set to true, curation processing will stop at the first matching rule. When set to false curation processing will continue and multiple curation actions will be triggered in sequence. Curations are processed in the lexical sort order of their id field.
       *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~rule ?effective_from_ts ?effective_to_ts ?excludes ?filter_by ?filter_curated_hits ?id ?includes ?metadata ?remove_matched_tokens ?replace_query ?sort_by ?stop_processing () = { effective_from_ts; effective_to_ts; excludes; filter_by; filter_curated_hits; id; includes; metadata; remove_matched_tokens; replace_query; rule; sort_by; stop_processing }
    
    let effective_from_ts t = t.effective_from_ts
    let effective_to_ts t = t.effective_to_ts
    let excludes t = t.excludes
    let filter_by t = t.filter_by
    let filter_curated_hits t = t.filter_curated_hits
    let id t = t.id
    let includes t = t.includes
    let metadata t = t.metadata
    let remove_matched_tokens t = t.remove_matched_tokens
    let replace_query t = t.replace_query
    let rule t = t.rule
    let sort_by t = t.sort_by
    let stop_processing t = t.stop_processing
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"CurationItemCreateSchema"
        (fun effective_from_ts effective_to_ts excludes filter_by filter_curated_hits id includes metadata remove_matched_tokens replace_query rule sort_by stop_processing -> { effective_from_ts; effective_to_ts; excludes; filter_by; filter_curated_hits; id; includes; metadata; remove_matched_tokens; replace_query; rule; sort_by; stop_processing })
      |> Jsont.Object.opt_mem "effective_from_ts" Jsont.int ~enc:(fun r -> r.effective_from_ts)
      |> Jsont.Object.opt_mem "effective_to_ts" Jsont.int ~enc:(fun r -> r.effective_to_ts)
      |> Jsont.Object.opt_mem "excludes" (Jsont.list CurationExclude.T.jsont) ~enc:(fun r -> r.excludes)
      |> Jsont.Object.opt_mem "filter_by" Jsont.string ~enc:(fun r -> r.filter_by)
      |> Jsont.Object.opt_mem "filter_curated_hits" Jsont.bool ~enc:(fun r -> r.filter_curated_hits)
      |> Jsont.Object.opt_mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "includes" (Jsont.list CurationInclude.T.jsont) ~enc:(fun r -> r.includes)
      |> Jsont.Object.opt_mem "metadata" Jsont.json ~enc:(fun r -> r.metadata)
      |> Jsont.Object.opt_mem "remove_matched_tokens" Jsont.bool ~enc:(fun r -> r.remove_matched_tokens)
      |> Jsont.Object.opt_mem "replace_query" Jsont.string ~enc:(fun r -> r.replace_query)
      |> Jsont.Object.mem "rule" CurationRule.T.jsont ~enc:(fun r -> r.rule)
      |> Jsont.Object.opt_mem "sort_by" Jsont.string ~enc:(fun r -> r.sort_by)
      |> Jsont.Object.opt_mem "stop_processing" Jsont.bool ~enc:(fun r -> r.stop_processing)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module CurationSetCreateSchema = struct
  module Types = struct
    module T = struct
      type t = {
        description : string option;  (** Optional description for the curation set *)
        items : CurationItemCreateSchema.T.t list;  (** Array of curation items *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~items ?description () = { description; items }
    
    let description t = t.description
    let items t = t.items
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"CurationSetCreateSchema"
        (fun description items -> { description; items })
      |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
      |> Jsont.Object.mem "items" (Jsont.list CurationItemCreateSchema.T.jsont) ~enc:(fun r -> r.items)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module CurationSetSchema = struct
  module Types = struct
    module T = struct
      type t = {
        description : string option;  (** Optional description for the curation set *)
        items : CurationItemCreateSchema.T.t list;  (** Array of curation items *)
        name : string;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~items ~name ?description () = { description; items; name }
    
    let description t = t.description
    let items t = t.items
    let name t = t.name
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"CurationSetSchema"
        (fun description items name -> { description; items; name })
      |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
      |> Jsont.Object.mem "items" (Jsont.list CurationItemCreateSchema.T.jsont) ~enc:(fun r -> r.items)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** List all curation sets
  
      Retrieve all curation sets *)
  let retrieve_curation_sets client () =
    let op_name = "retrieve_curation_sets" in
    let url_path = "/curation_sets" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Retrieve a curation set
  
      Retrieve a specific curation set by its name 
      @param curation_set_name The name of the curation set to retrieve
  *)
  let retrieve_curation_set ~curation_set_name client () =
    let op_name = "retrieve_curation_set" in
    let url_path = Openapi.Runtime.Path.render ~params:[("curationSetName", curation_set_name)] "/curation_sets/{curationSetName}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status;
        body;
        parsed_body;
      })
  
  (** Create or update a curation set
  
      Create or update a curation set with the given name 
      @param curation_set_name The name of the curation set to create/update
  *)
  let upsert_curation_set ~curation_set_name ~body client () =
    let op_name = "upsert_curation_set" in
    let url_path = Openapi.Runtime.Path.render ~params:[("curationSetName", curation_set_name)] "/curation_sets/{curationSetName}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json CurationSetCreateSchema.T.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 400 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status;
        body;
        parsed_body;
      })
end

module CurationItemSchema = struct
  module Types = struct
    module T = struct
      type t = {
        effective_from_ts : int option;  (** A Unix timestamp that indicates the date/time from which the curation will be active. You can use this to create rules that start applying from a future point in time.
       *)
        effective_to_ts : int option;  (** A Unix timestamp that indicates the date/time until which the curation will be active. You can use this to create rules that stop applying after a period of time.
       *)
        excludes : CurationExclude.T.t list option;  (** List of document `id`s that should be excluded from the search results. *)
        filter_by : string option;  (** A filter by clause that is applied to any search query that matches the curation rule.
       *)
        filter_curated_hits : bool option;  (** When set to true, the filter conditions of the query is applied to the curated records as well. Default: false.
       *)
        includes : CurationInclude.T.t list option;  (** List of document `id`s that should be included in the search results with their corresponding `position`s. *)
        metadata : Jsont.json option;  (** Return a custom JSON object in the Search API response, when this rule is triggered. This can can be used to display a pre-defined message (eg: a promotion banner) on the front-end when a particular rule is triggered.
       *)
        remove_matched_tokens : bool option;  (** Indicates whether search query tokens that exist in the curation's rule should be removed from the search query.
       *)
        replace_query : string option;  (** Replaces the current search query with this value, when the search query matches the curation rule.
       *)
        rule : CurationRule.T.t;
        sort_by : string option;  (** A sort by clause that is applied to any search query that matches the curation rule.
       *)
        stop_processing : bool option;  (** When set to true, curation processing will stop at the first matching rule. When set to false curation processing will continue and multiple curation actions will be triggered in sequence. Curations are processed in the lexical sort order of their id field.
       *)
        id : string;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~rule ~id ?effective_from_ts ?effective_to_ts ?excludes ?filter_by ?filter_curated_hits ?includes ?metadata ?remove_matched_tokens ?replace_query ?sort_by ?stop_processing () = { effective_from_ts; effective_to_ts; excludes; filter_by; filter_curated_hits; includes; metadata; remove_matched_tokens; replace_query; rule; sort_by; stop_processing; id }
    
    let effective_from_ts t = t.effective_from_ts
    let effective_to_ts t = t.effective_to_ts
    let excludes t = t.excludes
    let filter_by t = t.filter_by
    let filter_curated_hits t = t.filter_curated_hits
    let includes t = t.includes
    let metadata t = t.metadata
    let remove_matched_tokens t = t.remove_matched_tokens
    let replace_query t = t.replace_query
    let rule t = t.rule
    let sort_by t = t.sort_by
    let stop_processing t = t.stop_processing
    let id t = t.id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"CurationItemSchema"
        (fun effective_from_ts effective_to_ts excludes filter_by filter_curated_hits includes metadata remove_matched_tokens replace_query rule sort_by stop_processing id -> { effective_from_ts; effective_to_ts; excludes; filter_by; filter_curated_hits; includes; metadata; remove_matched_tokens; replace_query; rule; sort_by; stop_processing; id })
      |> Jsont.Object.opt_mem "effective_from_ts" Jsont.int ~enc:(fun r -> r.effective_from_ts)
      |> Jsont.Object.opt_mem "effective_to_ts" Jsont.int ~enc:(fun r -> r.effective_to_ts)
      |> Jsont.Object.opt_mem "excludes" (Jsont.list CurationExclude.T.jsont) ~enc:(fun r -> r.excludes)
      |> Jsont.Object.opt_mem "filter_by" Jsont.string ~enc:(fun r -> r.filter_by)
      |> Jsont.Object.opt_mem "filter_curated_hits" Jsont.bool ~enc:(fun r -> r.filter_curated_hits)
      |> Jsont.Object.opt_mem "includes" (Jsont.list CurationInclude.T.jsont) ~enc:(fun r -> r.includes)
      |> Jsont.Object.opt_mem "metadata" Jsont.json ~enc:(fun r -> r.metadata)
      |> Jsont.Object.opt_mem "remove_matched_tokens" Jsont.bool ~enc:(fun r -> r.remove_matched_tokens)
      |> Jsont.Object.opt_mem "replace_query" Jsont.string ~enc:(fun r -> r.replace_query)
      |> Jsont.Object.mem "rule" CurationRule.T.jsont ~enc:(fun r -> r.rule)
      |> Jsont.Object.opt_mem "sort_by" Jsont.string ~enc:(fun r -> r.sort_by)
      |> Jsont.Object.opt_mem "stop_processing" Jsont.bool ~enc:(fun r -> r.stop_processing)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** List items in a curation set
  
      Retrieve all curation items in a set 
      @param curation_set_name The name of the curation set to retrieve items for
  *)
  let retrieve_curation_set_items ~curation_set_name client () =
    let op_name = "retrieve_curation_set_items" in
    let url_path = Openapi.Runtime.Path.render ~params:[("curationSetName", curation_set_name)] "/curation_sets/{curationSetName}/items" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status;
        body;
        parsed_body;
      })
  
  (** Retrieve a curation set item
  
      Retrieve a specific curation item by its id 
      @param curation_set_name The name of the curation set
      @param item_id The id of the curation item to retrieve
  *)
  let retrieve_curation_set_item ~curation_set_name ~item_id client () =
    let op_name = "retrieve_curation_set_item" in
    let url_path = Openapi.Runtime.Path.render ~params:[("curationSetName", curation_set_name); ("itemId", item_id)] "/curation_sets/{curationSetName}/items/{itemId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status;
        body;
        parsed_body;
      })
  
  (** Create or update a curation set item
  
      Create or update a curation set item with the given id 
      @param curation_set_name The name of the curation set
      @param item_id The id of the curation item to upsert
  *)
  let upsert_curation_set_item ~curation_set_name ~item_id ~body client () =
    let op_name = "upsert_curation_set_item" in
    let url_path = Openapi.Runtime.Path.render ~params:[("curationSetName", curation_set_name); ("itemId", item_id)] "/curation_sets/{curationSetName}/items/{itemId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json CurationItemCreateSchema.T.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 400 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status;
        body;
        parsed_body;
      })
end

module ConversationModelUpdateSchema = struct
  module Types = struct
    module T = struct
      type t = {
        account_id : string option;  (** LLM service's account ID (only applicable for Cloudflare) *)
        api_key : string option;  (** The LLM service's API Key *)
        history_collection : string option;  (** Typesense collection that stores the historical conversations *)
        id : string option;  (** An explicit id for the model, otherwise the API will return a response with an auto-generated conversation model id. *)
        max_bytes : int option;  (** The maximum number of bytes to send to the LLM in every API call. Consult the LLM's documentation on the number of bytes supported in the context window.
       *)
        model_name : string option;  (** Name of the LLM model offered by OpenAI, Cloudflare or vLLM *)
        system_prompt : string option;  (** The system prompt that contains special instructions to the LLM *)
        ttl : int option;  (** Time interval in seconds after which the messages would be deleted. Default: 86400 (24 hours)
       *)
        vllm_url : string option;  (** URL of vLLM service *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?account_id ?api_key ?history_collection ?id ?max_bytes ?model_name ?system_prompt ?ttl ?vllm_url () = { account_id; api_key; history_collection; id; max_bytes; model_name; system_prompt; ttl; vllm_url }
    
    let account_id t = t.account_id
    let api_key t = t.api_key
    let history_collection t = t.history_collection
    let id t = t.id
    let max_bytes t = t.max_bytes
    let model_name t = t.model_name
    let system_prompt t = t.system_prompt
    let ttl t = t.ttl
    let vllm_url t = t.vllm_url
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ConversationModelUpdateSchema"
        (fun account_id api_key history_collection id max_bytes model_name system_prompt ttl vllm_url -> { account_id; api_key; history_collection; id; max_bytes; model_name; system_prompt; ttl; vllm_url })
      |> Jsont.Object.opt_mem "account_id" Jsont.string ~enc:(fun r -> r.account_id)
      |> Jsont.Object.opt_mem "api_key" Jsont.string ~enc:(fun r -> r.api_key)
      |> Jsont.Object.opt_mem "history_collection" Jsont.string ~enc:(fun r -> r.history_collection)
      |> Jsont.Object.opt_mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "max_bytes" Jsont.int ~enc:(fun r -> r.max_bytes)
      |> Jsont.Object.opt_mem "model_name" Jsont.string ~enc:(fun r -> r.model_name)
      |> Jsont.Object.opt_mem "system_prompt" Jsont.string ~enc:(fun r -> r.system_prompt)
      |> Jsont.Object.opt_mem "ttl" Jsont.int ~enc:(fun r -> r.ttl)
      |> Jsont.Object.opt_mem "vllm_url" Jsont.string ~enc:(fun r -> r.vllm_url)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module ConversationModelCreateSchema = struct
  module Types = struct
    module T = struct
      type t = {
        account_id : string option;  (** LLM service's account ID (only applicable for Cloudflare) *)
        api_key : string option;  (** The LLM service's API Key *)
        id : string option;  (** An explicit id for the model, otherwise the API will return a response with an auto-generated conversation model id. *)
        system_prompt : string option;  (** The system prompt that contains special instructions to the LLM *)
        ttl : int option;  (** Time interval in seconds after which the messages would be deleted. Default: 86400 (24 hours)
       *)
        vllm_url : string option;  (** URL of vLLM service *)
        model_name : string;  (** Name of the LLM model offered by OpenAI, Cloudflare or vLLM *)
        max_bytes : int;  (** The maximum number of bytes to send to the LLM in every API call. Consult the LLM's documentation on the number of bytes supported in the context window.
       *)
        history_collection : string;  (** Typesense collection that stores the historical conversations *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~model_name ~max_bytes ~history_collection ?account_id ?api_key ?id ?system_prompt ?ttl ?vllm_url () = { account_id; api_key; id; system_prompt; ttl; vllm_url; model_name; max_bytes; history_collection }
    
    let account_id t = t.account_id
    let api_key t = t.api_key
    let id t = t.id
    let system_prompt t = t.system_prompt
    let ttl t = t.ttl
    let vllm_url t = t.vllm_url
    let model_name t = t.model_name
    let max_bytes t = t.max_bytes
    let history_collection t = t.history_collection
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ConversationModelCreateSchema"
        (fun account_id api_key id system_prompt ttl vllm_url model_name max_bytes history_collection -> { account_id; api_key; id; system_prompt; ttl; vllm_url; model_name; max_bytes; history_collection })
      |> Jsont.Object.opt_mem "account_id" Jsont.string ~enc:(fun r -> r.account_id)
      |> Jsont.Object.opt_mem "api_key" Jsont.string ~enc:(fun r -> r.api_key)
      |> Jsont.Object.opt_mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "system_prompt" Jsont.string ~enc:(fun r -> r.system_prompt)
      |> Jsont.Object.opt_mem "ttl" Jsont.int ~enc:(fun r -> r.ttl)
      |> Jsont.Object.opt_mem "vllm_url" Jsont.string ~enc:(fun r -> r.vllm_url)
      |> Jsont.Object.mem "model_name" Jsont.string ~enc:(fun r -> r.model_name)
      |> Jsont.Object.mem "max_bytes" Jsont.int ~enc:(fun r -> r.max_bytes)
      |> Jsont.Object.mem "history_collection" Jsont.string ~enc:(fun r -> r.history_collection)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module ConversationModelSchema = struct
  module Types = struct
    module T = struct
      type t = {
        account_id : string option;  (** LLM service's account ID (only applicable for Cloudflare) *)
        api_key : string option;  (** The LLM service's API Key *)
        system_prompt : string option;  (** The system prompt that contains special instructions to the LLM *)
        ttl : int option;  (** Time interval in seconds after which the messages would be deleted. Default: 86400 (24 hours)
       *)
        vllm_url : string option;  (** URL of vLLM service *)
        model_name : string;  (** Name of the LLM model offered by OpenAI, Cloudflare or vLLM *)
        max_bytes : int;  (** The maximum number of bytes to send to the LLM in every API call. Consult the LLM's documentation on the number of bytes supported in the context window.
       *)
        history_collection : string;  (** Typesense collection that stores the historical conversations *)
        id : string;  (** An explicit id for the model, otherwise the API will return a response with an auto-generated conversation model id. *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~model_name ~max_bytes ~history_collection ~id ?account_id ?api_key ?system_prompt ?ttl ?vllm_url () = { account_id; api_key; system_prompt; ttl; vllm_url; model_name; max_bytes; history_collection; id }
    
    let account_id t = t.account_id
    let api_key t = t.api_key
    let system_prompt t = t.system_prompt
    let ttl t = t.ttl
    let vllm_url t = t.vllm_url
    let model_name t = t.model_name
    let max_bytes t = t.max_bytes
    let history_collection t = t.history_collection
    let id t = t.id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ConversationModelSchema"
        (fun account_id api_key system_prompt ttl vllm_url model_name max_bytes history_collection id -> { account_id; api_key; system_prompt; ttl; vllm_url; model_name; max_bytes; history_collection; id })
      |> Jsont.Object.opt_mem "account_id" Jsont.string ~enc:(fun r -> r.account_id)
      |> Jsont.Object.opt_mem "api_key" Jsont.string ~enc:(fun r -> r.api_key)
      |> Jsont.Object.opt_mem "system_prompt" Jsont.string ~enc:(fun r -> r.system_prompt)
      |> Jsont.Object.opt_mem "ttl" Jsont.int ~enc:(fun r -> r.ttl)
      |> Jsont.Object.opt_mem "vllm_url" Jsont.string ~enc:(fun r -> r.vllm_url)
      |> Jsont.Object.mem "model_name" Jsont.string ~enc:(fun r -> r.model_name)
      |> Jsont.Object.mem "max_bytes" Jsont.int ~enc:(fun r -> r.max_bytes)
      |> Jsont.Object.mem "history_collection" Jsont.string ~enc:(fun r -> r.history_collection)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** List all conversation models
  
      Retrieve all conversation models *)
  let retrieve_all_conversation_models client () =
    let op_name = "retrieve_all_conversation_models" in
    let url_path = "/conversations/models" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Create a conversation model
  
      Create a Conversation Model *)
  let create_conversation_model ~body client () =
    let op_name = "create_conversation_model" in
    let url_path = "/conversations/models" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json ConversationModelCreateSchema.T.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 400 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status;
        body;
        parsed_body;
      })
  
  (** Retrieve a conversation model
  
      Retrieve a conversation model 
      @param model_id The id of the conversation model to retrieve
  *)
  let retrieve_conversation_model ~model_id client () =
    let op_name = "retrieve_conversation_model" in
    let url_path = Openapi.Runtime.Path.render ~params:[("modelId", model_id)] "/conversations/models/{modelId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update a conversation model
  
      Update a conversation model 
      @param model_id The id of the conversation model to update
  *)
  let update_conversation_model ~model_id ~body client () =
    let op_name = "update_conversation_model" in
    let url_path = Openapi.Runtime.Path.render ~params:[("modelId", model_id)] "/conversations/models/{modelId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json ConversationModelUpdateSchema.T.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete a conversation model
  
      Delete a conversation model 
      @param model_id The id of the conversation model to delete
  *)
  let delete_conversation_model ~model_id client () =
    let op_name = "delete_conversation_model" in
    let url_path = Openapi.Runtime.Path.render ~params:[("modelId", model_id)] "/conversations/models/{modelId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module CollectionAliasSchema = struct
  module Types = struct
    module T = struct
      type t = {
        collection_name : string;  (** Name of the collection you wish to map the alias to *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~collection_name () = { collection_name }
    
    let collection_name t = t.collection_name
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"CollectionAliasSchema"
        (fun collection_name -> { collection_name })
      |> Jsont.Object.mem "collection_name" Jsont.string ~enc:(fun r -> r.collection_name)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module CollectionAlias = struct
  module Types = struct
    module T = struct
      type t = {
        collection_name : string;  (** Name of the collection the alias mapped to *)
        name : string;  (** Name of the collection alias *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~collection_name ~name () = { collection_name; name }
    
    let collection_name t = t.collection_name
    let name t = t.name
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"CollectionAlias"
        (fun collection_name name -> { collection_name; name })
      |> Jsont.Object.mem "collection_name" Jsont.string ~enc:(fun r -> r.collection_name)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve an alias
  
      Find out which collection an alias points to by fetching it 
      @param alias_name The name of the alias to retrieve
  *)
  let get_alias ~alias_name client () =
    let op_name = "get_alias" in
    let url_path = Openapi.Runtime.Path.render ~params:[("aliasName", alias_name)] "/aliases/{aliasName}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status;
        body;
        parsed_body;
      })
  
  (** Create or update a collection alias
  
      Create or update a collection alias. An alias is a virtual collection name that points to a real collection. If you're familiar with symbolic links on Linux, it's very similar to that. Aliases are useful when you want to reindex your data in the background on a new collection and switch your application to it without any changes to your code. 
      @param alias_name The name of the alias to create/update
  *)
  let upsert_alias ~alias_name ~body client () =
    let op_name = "upsert_alias" in
    let url_path = Openapi.Runtime.Path.render ~params:[("aliasName", alias_name)] "/aliases/{aliasName}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json CollectionAliasSchema.T.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 400 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status;
        body;
        parsed_body;
      })
  
  (** Delete an alias 
      @param alias_name The name of the alias to delete
  *)
  let delete_alias ~alias_name client () =
    let op_name = "delete_alias" in
    let url_path = Openapi.Runtime.Path.render ~params:[("aliasName", alias_name)] "/aliases/{aliasName}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status;
        body;
        parsed_body;
      })
end

module CollectionAliases = struct
  module Types = struct
    module Response = struct
      type t = {
        aliases : CollectionAlias.T.t list;
      }
    end
  end
  
  module Response = struct
    include Types.Response
    
    let v ~aliases () = { aliases }
    
    let aliases t = t.aliases
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"CollectionAliasesResponse"
        (fun aliases -> { aliases })
      |> Jsont.Object.mem "aliases" (Jsont.list CollectionAlias.T.jsont) ~enc:(fun r -> r.aliases)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** List all aliases
  
      List all aliases and the corresponding collections that they map to. *)
  let get_aliases client () =
    let op_name = "get_aliases" in
    let url_path = "/aliases" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module Client = struct
  (** Create analytics rule(s)
  
      Create one or more analytics rules. You can send a single rule object or an array of rule objects. *)
  let create_analytics_rule client () =
    let op_name = "create_analytics_rule" in
    let url_path = "/analytics/rules" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 400 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status;
        body;
        parsed_body;
      })
  
  (** Index a document
  
      A document to be indexed in a given collection must conform to the schema of the collection. 
      @param collection_name The name of the collection to add the document to
      @param action Additional action to perform
      @param dirty_values Dealing with Dirty Data
  *)
  let index_document ~collection_name ?action ?dirty_values client () =
    let op_name = "index_document" in
    let url_path = Openapi.Runtime.Path.render ~params:[("collectionName", collection_name)] "/collections/{collectionName}/documents" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"action" ~value:action; Openapi.Runtime.Query.optional ~key:"dirty_values" ~value:dirty_values]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status;
        body;
        parsed_body;
      })
  
  (** Delete a bunch of documents
  
      Delete a bunch of documents that match a specific filter condition. Use the `batch_size` parameter to control the number of documents that should deleted at a time. A larger value will speed up deletions, but will impact performance of other operations running on the server. 
      @param collection_name The name of the collection to delete documents from
  *)
  let delete_documents ~collection_name ?delete_documents_parameters client () =
    let op_name = "delete_documents" in
    let url_path = Openapi.Runtime.Path.render ~params:[("collectionName", collection_name)] "/collections/{collectionName}/documents" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"deleteDocumentsParameters" ~value:delete_documents_parameters]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status;
        body;
        parsed_body;
      })
  
  (** Update documents with conditional query
  
      The filter_by query parameter is used to filter to specify a condition against which the documents are matched. The request body contains the fields that should be updated for any documents that match the filter condition. This endpoint is only available if the Typesense server is version `0.25.0.rc12` or later. 
      @param collection_name The name of the collection to update documents in
  *)
  let update_documents ~collection_name ?update_documents_parameters client () =
    let op_name = "update_documents" in
    let url_path = Openapi.Runtime.Path.render ~params:[("collectionName", collection_name)] "/collections/{collectionName}/documents" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"updateDocumentsParameters" ~value:update_documents_parameters]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.patch client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PATCH" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 400 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PATCH";
        url;
        status;
        body;
        parsed_body;
      })
  
  (** Export all documents in a collection
  
      Export all documents in a collection in JSON lines format. 
      @param collection_name The name of the collection
  *)
  let export_documents ~collection_name ?export_documents_parameters client () =
    let op_name = "export_documents" in
    let url_path = Openapi.Runtime.Path.render ~params:[("collectionName", collection_name)] "/collections/{collectionName}/documents/export" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"exportDocumentsParameters" ~value:export_documents_parameters]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status;
        body;
        parsed_body;
      })
  
  (** Import documents into a collection
  
      The documents to be imported must be formatted in a newline delimited JSON structure. You can feed the output file from a Typesense export operation directly as import. 
      @param collection_name The name of the collection
  *)
  let import_documents ~collection_name ?import_documents_parameters client () =
    let op_name = "import_documents" in
    let url_path = Openapi.Runtime.Path.render ~params:[("collectionName", collection_name)] "/collections/{collectionName}/documents/import" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"importDocumentsParameters" ~value:import_documents_parameters]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 400 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status;
        body;
        parsed_body;
      })
  
  (** Retrieve a document
  
      Fetch an individual document from a collection by using its ID. 
      @param collection_name The name of the collection to search for the document under
      @param document_id The Document ID
  *)
  let get_document ~collection_name ~document_id client () =
    let op_name = "get_document" in
    let url_path = Openapi.Runtime.Path.render ~params:[("collectionName", collection_name); ("documentId", document_id)] "/collections/{collectionName}/documents/{documentId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status;
        body;
        parsed_body;
      })
  
  (** Delete a document
  
      Delete an individual document from a collection by using its ID. 
      @param collection_name The name of the collection to search for the document under
      @param document_id The Document ID
  *)
  let delete_document ~collection_name ~document_id client () =
    let op_name = "delete_document" in
    let url_path = Openapi.Runtime.Path.render ~params:[("collectionName", collection_name); ("documentId", document_id)] "/collections/{collectionName}/documents/{documentId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status;
        body;
        parsed_body;
      })
  
  (** Update a document
  
      Update an individual document from a collection by using its ID. The update can be partial. 
      @param collection_name The name of the collection to search for the document under
      @param document_id The Document ID
      @param dirty_values Dealing with Dirty Data
  *)
  let update_document ~collection_name ~document_id ?dirty_values client () =
    let op_name = "update_document" in
    let url_path = Openapi.Runtime.Path.render ~params:[("collectionName", collection_name); ("documentId", document_id)] "/collections/{collectionName}/documents/{documentId}" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"dirty_values" ~value:dirty_values]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.patch client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PATCH" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PATCH";
        url;
        status;
        body;
        parsed_body;
      })
  
  (** Print debugging information
  
      Print debugging information *)
  let debug client () =
    let op_name = "debug" in
    let url_path = "/debug" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get current RAM, CPU, Disk & Network usage metrics.
  
      Retrieve the metrics. *)
  let retrieve_metrics client () =
    let op_name = "retrieve_metrics" in
    let url_path = "/metrics.json" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List all stemming dictionaries
  
      Retrieve a list of all available stemming dictionaries. *)
  let list_stemming_dictionaries client () =
    let op_name = "list_stemming_dictionaries" in
    let url_path = "/stemming/dictionaries" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Import a stemming dictionary
  
      Upload a JSONL file containing word mappings to create or update a stemming dictionary. 
      @param id The ID to assign to the dictionary
  *)
  let import_stemming_dictionary ~id client () =
    let op_name = "import_stemming_dictionary" in
    let url_path = "/stemming/dictionaries/import" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.singleton ~key:"id" ~value:id]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 400 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status;
        body;
        parsed_body;
      })
  
  (** Delete a stopwords set.
  
      Permanently deletes a stopwords set, given it's name. 
      @param set_id The ID of the stopwords set to delete.
  *)
  let delete_stopwords_set ~set_id client () =
    let op_name = "delete_stopwords_set" in
    let url_path = Openapi.Runtime.Path.render ~params:[("setId", set_id)] "/stopwords/{setId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status;
        body;
        parsed_body;
      })
end

module Apistats = struct
  module Types = struct
    module Response = struct
      type t = {
        delete_latency_ms : float option;
        delete_requests_per_second : float option;
        import_latency_ms : float option;
        import_requests_per_second : float option;
        latency_ms : Jsont.json option;
        overloaded_requests_per_second : float option;
        pending_write_batches : float option;
        requests_per_second : Jsont.json option;
        search_latency_ms : float option;
        search_requests_per_second : float option;
        total_requests_per_second : float option;
        write_latency_ms : float option;
        write_requests_per_second : float option;
      }
    end
  end
  
  module Response = struct
    include Types.Response
    
    let v ?delete_latency_ms ?delete_requests_per_second ?import_latency_ms ?import_requests_per_second ?latency_ms ?overloaded_requests_per_second ?pending_write_batches ?requests_per_second ?search_latency_ms ?search_requests_per_second ?total_requests_per_second ?write_latency_ms ?write_requests_per_second () = { delete_latency_ms; delete_requests_per_second; import_latency_ms; import_requests_per_second; latency_ms; overloaded_requests_per_second; pending_write_batches; requests_per_second; search_latency_ms; search_requests_per_second; total_requests_per_second; write_latency_ms; write_requests_per_second }
    
    let delete_latency_ms t = t.delete_latency_ms
    let delete_requests_per_second t = t.delete_requests_per_second
    let import_latency_ms t = t.import_latency_ms
    let import_requests_per_second t = t.import_requests_per_second
    let latency_ms t = t.latency_ms
    let overloaded_requests_per_second t = t.overloaded_requests_per_second
    let pending_write_batches t = t.pending_write_batches
    let requests_per_second t = t.requests_per_second
    let search_latency_ms t = t.search_latency_ms
    let search_requests_per_second t = t.search_requests_per_second
    let total_requests_per_second t = t.total_requests_per_second
    let write_latency_ms t = t.write_latency_ms
    let write_requests_per_second t = t.write_requests_per_second
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"APIStatsResponse"
        (fun delete_latency_ms delete_requests_per_second import_latency_ms import_requests_per_second latency_ms overloaded_requests_per_second pending_write_batches requests_per_second search_latency_ms search_requests_per_second total_requests_per_second write_latency_ms write_requests_per_second -> { delete_latency_ms; delete_requests_per_second; import_latency_ms; import_requests_per_second; latency_ms; overloaded_requests_per_second; pending_write_batches; requests_per_second; search_latency_ms; search_requests_per_second; total_requests_per_second; write_latency_ms; write_requests_per_second })
      |> Jsont.Object.opt_mem "delete_latency_ms" Jsont.number ~enc:(fun r -> r.delete_latency_ms)
      |> Jsont.Object.opt_mem "delete_requests_per_second" Jsont.number ~enc:(fun r -> r.delete_requests_per_second)
      |> Jsont.Object.opt_mem "import_latency_ms" Jsont.number ~enc:(fun r -> r.import_latency_ms)
      |> Jsont.Object.opt_mem "import_requests_per_second" Jsont.number ~enc:(fun r -> r.import_requests_per_second)
      |> Jsont.Object.opt_mem "latency_ms" Jsont.json ~enc:(fun r -> r.latency_ms)
      |> Jsont.Object.opt_mem "overloaded_requests_per_second" Jsont.number ~enc:(fun r -> r.overloaded_requests_per_second)
      |> Jsont.Object.opt_mem "pending_write_batches" Jsont.number ~enc:(fun r -> r.pending_write_batches)
      |> Jsont.Object.opt_mem "requests_per_second" Jsont.json ~enc:(fun r -> r.requests_per_second)
      |> Jsont.Object.opt_mem "search_latency_ms" Jsont.number ~enc:(fun r -> r.search_latency_ms)
      |> Jsont.Object.opt_mem "search_requests_per_second" Jsont.number ~enc:(fun r -> r.search_requests_per_second)
      |> Jsont.Object.opt_mem "total_requests_per_second" Jsont.number ~enc:(fun r -> r.total_requests_per_second)
      |> Jsont.Object.opt_mem "write_latency_ms" Jsont.number ~enc:(fun r -> r.write_latency_ms)
      |> Jsont.Object.opt_mem "write_requests_per_second" Jsont.number ~enc:(fun r -> r.write_requests_per_second)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get stats about API endpoints.
  
      Retrieve the stats about API endpoints. *)
  let retrieve_apistats client () =
    let op_name = "retrieve_apistats" in
    let url_path = "/stats.json" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module ApiKeySchema = struct
  module Types = struct
    module T = struct
      type t = {
        actions : string list;
        collections : string list;
        description : string;
        expires_at : int64 option;
        value : string option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~actions ~collections ~description ?expires_at ?value () = { actions; collections; description; expires_at; value }
    
    let actions t = t.actions
    let collections t = t.collections
    let description t = t.description
    let expires_at t = t.expires_at
    let value t = t.value
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ApiKeySchema"
        (fun actions collections description expires_at value -> { actions; collections; description; expires_at; value })
      |> Jsont.Object.mem "actions" (Jsont.list Jsont.string) ~enc:(fun r -> r.actions)
      |> Jsont.Object.mem "collections" (Jsont.list Jsont.string) ~enc:(fun r -> r.collections)
      |> Jsont.Object.mem "description" Jsont.string ~enc:(fun r -> r.description)
      |> Jsont.Object.opt_mem "expires_at" Jsont.int64 ~enc:(fun r -> r.expires_at)
      |> Jsont.Object.opt_mem "value" Jsont.string ~enc:(fun r -> r.value)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module ApiKey = struct
  module Types = struct
    module T = struct
      type t = {
        actions : string list;
        collections : string list;
        description : string;
        expires_at : int64 option;
        value : string option;
        id : int64 option;
        value_prefix : string option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~actions ~collections ~description ?expires_at ?value ?id ?value_prefix () = { actions; collections; description; expires_at; value; id; value_prefix }
    
    let actions t = t.actions
    let collections t = t.collections
    let description t = t.description
    let expires_at t = t.expires_at
    let value t = t.value
    let id t = t.id
    let value_prefix t = t.value_prefix
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ApiKey"
        (fun actions collections description expires_at value id value_prefix -> { actions; collections; description; expires_at; value; id; value_prefix })
      |> Jsont.Object.mem "actions" (Jsont.list Jsont.string) ~enc:(fun r -> r.actions)
      |> Jsont.Object.mem "collections" (Jsont.list Jsont.string) ~enc:(fun r -> r.collections)
      |> Jsont.Object.mem "description" Jsont.string ~enc:(fun r -> r.description)
      |> Jsont.Object.opt_mem "expires_at" Jsont.int64 ~enc:(fun r -> r.expires_at)
      |> Jsont.Object.opt_mem "value" Jsont.string ~enc:(fun r -> r.value)
      |> Jsont.Object.opt_mem "id" Jsont.int64 ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "value_prefix" Jsont.string ~enc:(fun r -> r.value_prefix)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Create an API Key
  
      Create an API Key with fine-grain access control. You can restrict access on both a per-collection and per-action level. The generated key is returned only during creation. You want to store this key carefully in a secure place. *)
  let create_key ~body client () =
    let op_name = "create_key" in
    let url_path = "/keys" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json ApiKeySchema.T.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 400 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | 409 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status;
        body;
        parsed_body;
      })
  
  (** Retrieve (metadata about) a key
  
      Retrieve (metadata about) a key. Only the key prefix is returned when you retrieve a key. Due to security reasons, only the create endpoint returns the full API key. 
      @param key_id The ID of the key to retrieve
  *)
  let get_key ~key_id client () =
    let op_name = "get_key" in
    let url_path = Openapi.Runtime.Path.render ~params:[("keyId", key_id)] "/keys/{keyId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status;
        body;
        parsed_body;
      })
end

module ApiKeys = struct
  module Types = struct
    module Response = struct
      type t = {
        keys : ApiKey.T.t list;
      }
    end
  end
  
  module Response = struct
    include Types.Response
    
    let v ~keys () = { keys }
    
    let keys t = t.keys
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ApiKeysResponse"
        (fun keys -> { keys })
      |> Jsont.Object.mem "keys" (Jsont.list ApiKey.T.jsont) ~enc:(fun r -> r.keys)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve (metadata about) all keys. *)
  let get_keys client () =
    let op_name = "get_keys" in
    let url_path = "/keys" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module ApiKeyDelete = struct
  module Types = struct
    module Response = struct
      type t = {
        id : int64;  (** The id of the API key that was deleted *)
      }
    end
  end
  
  module Response = struct
    include Types.Response
    
    let v ~id () = { id }
    
    let id t = t.id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ApiKeyDeleteResponse"
        (fun id -> { id })
      |> Jsont.Object.mem "id" Jsont.int64 ~enc:(fun r -> r.id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Delete an API key given its ID. 
      @param key_id The ID of the key to delete
  *)
  let delete_key ~key_id client () =
    let op_name = "delete_key" in
    let url_path = Openapi.Runtime.Path.render ~params:[("keyId", key_id)] "/keys/{keyId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 400 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Jsont.json (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Jsont.json v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status;
        body;
        parsed_body;
      })
end

module Api = struct
  module Types = struct
    module Response = struct
      type t = {
        message : string;
      }
    end
  end
  
  module Response = struct
    include Types.Response
    
    let v ~message () = { message }
    
    let message t = t.message
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ApiResponse"
        (fun message -> { message })
      |> Jsont.Object.mem "message" Jsont.string ~enc:(fun r -> r.message)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module AnalyticsRule = struct
  module Types = struct
    module Update = struct
      (** Fields allowed to update on an analytics rule *)
      type t = {
        name : string option;
        params : Jsont.json option;
        rule_tag : string option;
      }
    end
  
    module Type = struct
      type t = [
        | `Popular_queries
        | `Nohits_queries
        | `Counter
        | `Log
      ]
    end
  
    module Create = struct
      type t = {
        collection : string;
        event_type : string;
        name : string;
        params : Jsont.json option;
        rule_tag : string option;
        type_ : Type.t;
      }
    end
  
    module T = struct
      type t = {
        collection : string;
        event_type : string;
        name : string;
        params : Jsont.json option;
        rule_tag : string option;
        type_ : Type.t;
      }
    end
  end
  
  module Update = struct
    include Types.Update
    
    let v ?name ?params ?rule_tag () = { name; params; rule_tag }
    
    let name t = t.name
    let params t = t.params
    let rule_tag t = t.rule_tag
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AnalyticsRuleUpdate"
        (fun name params rule_tag -> { name; params; rule_tag })
      |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "params" Jsont.json ~enc:(fun r -> r.params)
      |> Jsont.Object.opt_mem "rule_tag" Jsont.string ~enc:(fun r -> r.rule_tag)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module Type = struct
    include Types.Type
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"AnalyticsRuleType"
        ~dec:(function
          | "popular_queries" -> `Popular_queries
          | "nohits_queries" -> `Nohits_queries
          | "counter" -> `Counter
          | "log" -> `Log
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Popular_queries -> "popular_queries"
          | `Nohits_queries -> "nohits_queries"
          | `Counter -> "counter"
          | `Log -> "log")
  end
  
  module Create = struct
    include Types.Create
    
    let v ~collection ~event_type ~name ~type_ ?params ?rule_tag () = { collection; event_type; name; params; rule_tag; type_ }
    
    let collection t = t.collection
    let event_type t = t.event_type
    let name t = t.name
    let params t = t.params
    let rule_tag t = t.rule_tag
    let type_ t = t.type_
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AnalyticsRuleCreate"
        (fun collection event_type name params rule_tag type_ -> { collection; event_type; name; params; rule_tag; type_ })
      |> Jsont.Object.mem "collection" Jsont.string ~enc:(fun r -> r.collection)
      |> Jsont.Object.mem "event_type" Jsont.string ~enc:(fun r -> r.event_type)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "params" Jsont.json ~enc:(fun r -> r.params)
      |> Jsont.Object.opt_mem "rule_tag" Jsont.string ~enc:(fun r -> r.rule_tag)
      |> Jsont.Object.mem "type" Type.jsont ~enc:(fun r -> r.type_)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module T = struct
    include Types.T
    
    let v ~collection ~event_type ~name ~type_ ?params ?rule_tag () = { collection; event_type; name; params; rule_tag; type_ }
    
    let collection t = t.collection
    let event_type t = t.event_type
    let name t = t.name
    let params t = t.params
    let rule_tag t = t.rule_tag
    let type_ t = t.type_
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AnalyticsRule"
        (fun collection event_type name params rule_tag type_ -> { collection; event_type; name; params; rule_tag; type_ })
      |> Jsont.Object.mem "collection" Jsont.string ~enc:(fun r -> r.collection)
      |> Jsont.Object.mem "event_type" Jsont.string ~enc:(fun r -> r.event_type)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "params" Jsont.json ~enc:(fun r -> r.params)
      |> Jsont.Object.opt_mem "rule_tag" Jsont.string ~enc:(fun r -> r.rule_tag)
      |> Jsont.Object.mem "type" Type.jsont ~enc:(fun r -> r.type_)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve analytics rules
  
      Retrieve all analytics rules. Use the optional rule_tag filter to narrow down results. 
      @param rule_tag Filter rules by rule_tag
  *)
  let retrieve_analytics_rules ?rule_tag client () =
    let op_name = "retrieve_analytics_rules" in
    let url_path = "/analytics/rules" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"rule_tag" ~value:rule_tag]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Retrieves an analytics rule
  
      Retrieve the details of an analytics rule, given it's name 
      @param rule_name The name of the analytics rule to retrieve
  *)
  let retrieve_analytics_rule ~rule_name client () =
    let op_name = "retrieve_analytics_rule" in
    let url_path = Openapi.Runtime.Path.render ~params:[("ruleName", rule_name)] "/analytics/rules/{ruleName}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Api.Response.jsont (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Api.Response.jsont v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status;
        body;
        parsed_body;
      })
  
  (** Upserts an analytics rule
  
      Upserts an analytics rule with the given name. 
      @param rule_name The name of the analytics rule to upsert
  *)
  let upsert_analytics_rule ~rule_name ~body client () =
    let op_name = "upsert_analytics_rule" in
    let url_path = Openapi.Runtime.Path.render ~params:[("ruleName", rule_name)] "/analytics/rules/{ruleName}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json Update.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 400 ->
            (match Openapi.Runtime.Json.decode_json Api.Response.jsont (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Api.Response.jsont v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status;
        body;
        parsed_body;
      })
  
  (** Delete an analytics rule
  
      Permanently deletes an analytics rule, given it's name 
      @param rule_name The name of the analytics rule to delete
  *)
  let delete_analytics_rule ~rule_name client () =
    let op_name = "delete_analytics_rule" in
    let url_path = Openapi.Runtime.Path.render ~params:[("ruleName", rule_name)] "/analytics/rules/{ruleName}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 404 ->
            (match Openapi.Runtime.Json.decode_json Api.Response.jsont (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Api.Response.jsont v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status;
        body;
        parsed_body;
      })
end

module AnalyticsEvents = struct
  module Types = struct
    module Response = struct
      type t = {
        events : Jsont.json list;
      }
    end
  end
  
  module Response = struct
    include Types.Response
    
    let v ~events () = { events }
    
    let events t = t.events
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AnalyticsEventsResponse"
        (fun events -> { events })
      |> Jsont.Object.mem "events" (Jsont.list Jsont.json) ~enc:(fun r -> r.events)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Retrieve analytics events
  
      Retrieve the most recent events for a user and rule. 
      @param name Analytics rule name
      @param n Number of events to return (max 1000)
  *)
  let get_analytics_events ~user_id ~name ~n client () =
    let op_name = "get_analytics_events" in
    let url_path = "/analytics/events" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.singleton ~key:"user_id" ~value:user_id; Openapi.Runtime.Query.singleton ~key:"name" ~value:name; Openapi.Runtime.Query.singleton ~key:"n" ~value:n]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 400 ->
            (match Openapi.Runtime.Json.decode_json Api.Response.jsont (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Api.Response.jsont v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status;
        body;
        parsed_body;
      })
end

module AnalyticsEvent = struct
  module Types = struct
    module T = struct
      type t = {
        data : Jsont.json;  (** Event payload *)
        event_type : string;  (** Type of event (e.g., click, conversion, query, visit) *)
        name : string;  (** Name of the analytics rule this event corresponds to *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~data ~event_type ~name () = { data; event_type; name }
    
    let data t = t.data
    let event_type t = t.event_type
    let name t = t.name
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AnalyticsEvent"
        (fun data event_type name -> { data; event_type; name })
      |> Jsont.Object.mem "data" Jsont.json ~enc:(fun r -> r.data)
      |> Jsont.Object.mem "event_type" Jsont.string ~enc:(fun r -> r.event_type)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module AnalyticsEventCreate = struct
  module Types = struct
    module Response = struct
      type t = {
        ok : bool;
      }
    end
  end
  
  module Response = struct
    include Types.Response
    
    let v ~ok () = { ok }
    
    let ok t = t.ok
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AnalyticsEventCreateResponse"
        (fun ok -> { ok })
      |> Jsont.Object.mem "ok" Jsont.bool ~enc:(fun r -> r.ok)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Create an analytics event
  
      Submit a single analytics event. The event must correspond to an existing analytics rule by name. *)
  let create_analytics_event ~body client () =
    let op_name = "create_analytics_event" in
    let url_path = "/analytics/events" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json AnalyticsEvent.T.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 400 ->
            (match Openapi.Runtime.Json.decode_json Api.Response.jsont (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ApiResponse", Openapi.Runtime.Json.encode_json Api.Response.jsont v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status;
        body;
        parsed_body;
      })
  
  (** Flush in-memory analytics to disk
  
      Triggers a flush of analytics data to persistent storage. *)
  let flush_analytics client () =
    let op_name = "flush_analytics" in
    let url_path = "/analytics/flush" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module Analytics = struct
  module Types = struct
    module Status = struct
      type t = {
        doc_counter_events : int option;
        doc_log_events : int option;
        log_prefix_queries : int option;
        nohits_prefix_queries : int option;
        popular_prefix_queries : int option;
        query_counter_events : int option;
        query_log_events : int option;
      }
    end
  end
  
  module Status = struct
    include Types.Status
    
    let v ?doc_counter_events ?doc_log_events ?log_prefix_queries ?nohits_prefix_queries ?popular_prefix_queries ?query_counter_events ?query_log_events () = { doc_counter_events; doc_log_events; log_prefix_queries; nohits_prefix_queries; popular_prefix_queries; query_counter_events; query_log_events }
    
    let doc_counter_events t = t.doc_counter_events
    let doc_log_events t = t.doc_log_events
    let log_prefix_queries t = t.log_prefix_queries
    let nohits_prefix_queries t = t.nohits_prefix_queries
    let popular_prefix_queries t = t.popular_prefix_queries
    let query_counter_events t = t.query_counter_events
    let query_log_events t = t.query_log_events
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AnalyticsStatus"
        (fun doc_counter_events doc_log_events log_prefix_queries nohits_prefix_queries popular_prefix_queries query_counter_events query_log_events -> { doc_counter_events; doc_log_events; log_prefix_queries; nohits_prefix_queries; popular_prefix_queries; query_counter_events; query_log_events })
      |> Jsont.Object.opt_mem "doc_counter_events" Jsont.int ~enc:(fun r -> r.doc_counter_events)
      |> Jsont.Object.opt_mem "doc_log_events" Jsont.int ~enc:(fun r -> r.doc_log_events)
      |> Jsont.Object.opt_mem "log_prefix_queries" Jsont.int ~enc:(fun r -> r.log_prefix_queries)
      |> Jsont.Object.opt_mem "nohits_prefix_queries" Jsont.int ~enc:(fun r -> r.nohits_prefix_queries)
      |> Jsont.Object.opt_mem "popular_prefix_queries" Jsont.int ~enc:(fun r -> r.popular_prefix_queries)
      |> Jsont.Object.opt_mem "query_counter_events" Jsont.int ~enc:(fun r -> r.query_counter_events)
      |> Jsont.Object.opt_mem "query_log_events" Jsont.int ~enc:(fun r -> r.query_log_events)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get analytics subsystem status
  
      Returns sizes of internal analytics buffers and queues. *)
  let get_analytics_status client () =
    let op_name = "get_analytics_status" in
    let url_path = "/analytics/status" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Status.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end
