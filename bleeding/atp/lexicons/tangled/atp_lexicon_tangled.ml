(* Atp_lexicon_tangled - generated from atproto lexicons *)

(** Utility functions for resilient parsing. *)
module Filter = struct
  (** [filter_list jsont json_list] parses each element with [jsont],
      returning only successfully parsed elements. Non-compliant records
      are silently skipped. *)
  let filter_list (type a) (jsont : a Jsont.t) (json_list : Jsont.json list) : a list =
    List.filter_map (fun json ->
      match Jsont.Json.decode jsont json with
      | Ok v -> Some v
      | Error _ -> None
    ) json_list
end

module Org = struct
  module Tangled = struct
    module Webhook = struct
      module RetryWebhookDelivery = struct
type input = {
  delivery : string;
  repo : string;
  webhook : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ delivery repo webhook -> { delivery; repo; webhook })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.webhook.retryWebhookDelivery#input") ~enc:(fun _ -> "org.tangled.webhook.retryWebhookDelivery#input")
  |> Jsont.Object.mem "delivery" Jsont.string ~enc:(fun r -> r.delivery)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "webhook" Jsont.string ~enc:(fun r -> r.webhook)
  |> Jsont.Object.finish

      end
      module DeleteWebhook = struct
type input = {
  repo : string;
  webhook : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ repo webhook -> { repo; webhook })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.webhook.deleteWebhook#input") ~enc:(fun _ -> "org.tangled.webhook.deleteWebhook#input")
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "webhook" Jsont.string ~enc:(fun r -> r.webhook)
  |> Jsont.Object.finish

      end
      module Defs = struct
type event_type = string
let event_type_jsont = Jsont.string

type delivery = {
  created_at : string;
  event : Jsont.json;
  id : string;
  request_body : string option;
  response_body : string option;
  response_code : int option;
  success : bool;
  url : string;
  webhook : string;
}

let delivery_jsont =
  Jsont.Object.map ~kind:"Delivery"
    (fun _typ created_at event id request_body response_body response_code success url webhook -> { created_at; event; id; request_body; response_body; response_code; success; url; webhook })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.webhook.defs#delivery") ~enc:(fun _ -> "org.tangled.webhook.defs#delivery")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "event" Jsont.json ~enc:(fun r -> r.event)
  |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
  |> Jsont.Object.opt_mem "requestBody" Atp.Lex.bytes_jsont ~enc:(fun r -> r.request_body)
  |> Jsont.Object.opt_mem "responseBody" Atp.Lex.bytes_jsont ~enc:(fun r -> r.response_body)
  |> Jsont.Object.opt_mem "responseCode" Jsont.int ~enc:(fun r -> r.response_code)
  |> Jsont.Object.mem "success" Jsont.bool ~enc:(fun r -> r.success)
  |> Jsont.Object.mem "url" Jsont.string ~enc:(fun r -> r.url)
  |> Jsont.Object.mem "webhook" Jsont.string ~enc:(fun r -> r.webhook)
  |> Jsont.Object.finish

type webhook = {
  active : bool;
  created_at : string;
  events : Jsont.json list;
  id : string;
  updated_at : string option;
  url : string;
}

let webhook_jsont =
  Jsont.Object.map ~kind:"Webhook"
    (fun _typ active created_at events id updated_at url -> { active; created_at; events; id; updated_at; url })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.webhook.defs#webhook") ~enc:(fun _ -> "org.tangled.webhook.defs#webhook")
  |> Jsont.Object.mem "active" Jsont.bool ~enc:(fun r -> r.active)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "events" (Jsont.list Jsont.json) ~enc:(fun r -> r.events)
  |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
  |> Jsont.Object.opt_mem "updatedAt" Jsont.string ~enc:(fun r -> r.updated_at)
  |> Jsont.Object.mem "url" Jsont.string ~enc:(fun r -> r.url)
  |> Jsont.Object.finish

      end
      module UpdateWebhook = struct
type input = {
  active : bool option;
  events : Jsont.json list option;
  repo : string;
  secret : string option;
  url : string option;
  webhook : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ active events repo secret url webhook -> { active; events; repo; secret; url; webhook })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.webhook.updateWebhook#input") ~enc:(fun _ -> "org.tangled.webhook.updateWebhook#input")
  |> Jsont.Object.opt_mem "active" Jsont.bool ~enc:(fun r -> r.active)
  |> Jsont.Object.opt_mem "events" (Jsont.list Jsont.json) ~enc:(fun r -> r.events)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.opt_mem "secret" Jsont.string ~enc:(fun r -> r.secret)
  |> Jsont.Object.opt_mem "url" Jsont.string ~enc:(fun r -> r.url)
  |> Jsont.Object.mem "webhook" Jsont.string ~enc:(fun r -> r.webhook)
  |> Jsont.Object.finish

      end
      module ListWebhooks = struct
type params = {
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun repo -> {
      repo;
    })
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  webhooks : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ webhooks -> { webhooks })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.webhook.listWebhooks#output") ~enc:(fun _ -> "org.tangled.webhook.listWebhooks#output")
  |> Jsont.Object.mem "webhooks" (Jsont.list Jsont.json) ~enc:(fun r -> r.webhooks)
  |> Jsont.Object.finish

      end
      module ListWebhookDeliveries = struct
type params = {
  limit : int option;
  repo : string;
  webhooks : string list;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun limit repo webhooks -> {
      limit;
      repo;
      webhooks;
    })
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "webhooks" (Jsont.list Jsont.string)
       ~enc:(fun r -> r.webhooks)
  |> Jsont.Object.finish

type output = {
  deliveries : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ deliveries -> { deliveries })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.webhook.listWebhookDeliveries#output") ~enc:(fun _ -> "org.tangled.webhook.listWebhookDeliveries#output")
  |> Jsont.Object.mem "deliveries" (Jsont.list Jsont.json) ~enc:(fun r -> r.deliveries)
  |> Jsont.Object.finish

      end
      module CreateWebhook = struct
type input = {
  active : bool option;
  events : Jsont.json list;
  repo : string;
  secret : string option;
  url : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ active events repo secret url -> { active; events; repo; secret; url })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.webhook.createWebhook#input") ~enc:(fun _ -> "org.tangled.webhook.createWebhook#input")
  |> Jsont.Object.opt_mem "active" Jsont.bool ~enc:(fun r -> r.active)
  |> Jsont.Object.mem "events" (Jsont.list Jsont.json) ~enc:(fun r -> r.events)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.opt_mem "secret" Jsont.string ~enc:(fun r -> r.secret)
  |> Jsont.Object.mem "url" Jsont.string ~enc:(fun r -> r.url)
  |> Jsont.Object.finish

type output = {
  id : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ id -> { id })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.webhook.createWebhook#output") ~enc:(fun _ -> "org.tangled.webhook.createWebhook#output")
  |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
  |> Jsont.Object.finish

      end
    end
    module String = struct
      module ListStrings = struct
type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.string.listStrings#listItem") ~enc:(fun _ -> "org.tangled.string.listStrings#listItem")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order subject -> {
      cursor;
      limit;
      offset;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.string.listStrings#output") ~enc:(fun _ -> "org.tangled.string.listStrings#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module GetString = struct
type params = {
  uri : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun uri -> {
      uri;
    })
  |> Jsont.Object.mem "uri" Jsont.string
       ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type output = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.string.getString#output") ~enc:(fun _ -> "org.tangled.string.getString#output")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

      end
      module CountStrings = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.string.countStrings#output") ~enc:(fun _ -> "org.tangled.string.countStrings#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
    end
    module Spindle = struct
      module DescribeSpindle = struct
type contact = {
  email : string option;
}

let contact_jsont =
  Jsont.Object.map ~kind:"Contact"
    (fun _typ email -> { email })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.spindle.describeSpindle#contact") ~enc:(fun _ -> "org.tangled.spindle.describeSpindle#contact")
  |> Jsont.Object.opt_mem "email" Jsont.string ~enc:(fun r -> r.email)
  |> Jsont.Object.finish

type links = {
  privacy_policy : string option;
  terms_of_service : string option;
}

let links_jsont =
  Jsont.Object.map ~kind:"Links"
    (fun _typ privacy_policy terms_of_service -> { privacy_policy; terms_of_service })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.spindle.describeSpindle#links") ~enc:(fun _ -> "org.tangled.spindle.describeSpindle#links")
  |> Jsont.Object.opt_mem "privacyPolicy" Jsont.string ~enc:(fun r -> r.privacy_policy)
  |> Jsont.Object.opt_mem "termsOfService" Jsont.string ~enc:(fun r -> r.terms_of_service)
  |> Jsont.Object.finish

type output = {
  capabilities : string list option;
  contact : Jsont.json option;
  did : string;
  links : Jsont.json option;
  version : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ capabilities contact did links version -> { capabilities; contact; did; links; version })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.spindle.describeSpindle#output") ~enc:(fun _ -> "org.tangled.spindle.describeSpindle#output")
  |> Jsont.Object.opt_mem "capabilities" (Jsont.list Jsont.string) ~enc:(fun r -> r.capabilities)
  |> Jsont.Object.opt_mem "contact" Jsont.json ~enc:(fun r -> r.contact)
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.opt_mem "links" Jsont.json ~enc:(fun r -> r.links)
  |> Jsont.Object.mem "version" Jsont.string ~enc:(fun r -> r.version)
  |> Jsont.Object.finish

      end
    end
    module Snippet = struct
      module String = struct
type text_file = {
  content : Atp.Blob_ref.t;
  name : string;
}

let text_file_jsont =
  Jsont.Object.map ~kind:"Text_file"
    (fun _typ content name -> { content; name })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.snippet.string#textFile") ~enc:(fun _ -> "org.tangled.snippet.string#textFile")
  |> Jsont.Object.mem "content" Atp.Blob_ref.jsont ~enc:(fun r -> r.content)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.finish

type main = {
  created_at : string;
  description : string option;
  snippet : Jsont.json;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at description snippet -> { created_at; description; snippet })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.snippet.string") ~enc:(fun _ -> "org.tangled.snippet.string")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
  |> Jsont.Object.mem "snippet" Jsont.json ~enc:(fun r -> r.snippet)
  |> Jsont.Object.finish

      end
    end
    module Secret = struct
      module RemoveSecret = struct
type input = {
  key : string;
  repo : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ key repo -> { key; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.secret.removeSecret#input") ~enc:(fun _ -> "org.tangled.secret.removeSecret#input")
  |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

      end
      module ListSecrets = struct
type secret = {
  created_at : string;
  created_by : string;
  key : string;
}

let secret_jsont =
  Jsont.Object.map ~kind:"Secret"
    (fun _typ created_at created_by key -> { created_at; created_by; key })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.secret.listSecrets#secret") ~enc:(fun _ -> "org.tangled.secret.listSecrets#secret")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "createdBy" Jsont.string ~enc:(fun r -> r.created_by)
  |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
  |> Jsont.Object.finish

type params = {
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun repo -> {
      repo;
    })
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  secrets : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ secrets -> { secrets })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.secret.listSecrets#output") ~enc:(fun _ -> "org.tangled.secret.listSecrets#output")
  |> Jsont.Object.mem "secrets" (Jsont.list Jsont.json) ~enc:(fun r -> r.secrets)
  |> Jsont.Object.finish

      end
      module AddSecret = struct
type input = {
  key : string;
  repo : string;
  value : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ key repo value -> { key; repo; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.secret.addSecret#input") ~enc:(fun _ -> "org.tangled.secret.addSecret#input")
  |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "value" Jsont.string ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

      end
    end
    module Query = struct
      module EnrichResponse = struct
type labels = {
  labels : Jsont.json list;
}

let labels_jsont =
  Jsont.Object.map ~kind:"Labels"
    (fun _typ labels -> { labels })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.query.enrichResponse#labels") ~enc:(fun _ -> "org.tangled.query.enrichResponse#labels")
  |> Jsont.Object.mem "labels" (Jsont.list Jsont.json) ~enc:(fun r -> r.labels)
  |> Jsont.Object.finish

type link_descriptor = {
  source : string;
  targets : string list option;
  type_ : string;
}

let link_descriptor_jsont =
  Jsont.Object.map ~kind:"Link_descriptor"
    (fun _typ source targets type_ -> { source; targets; type_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.query.enrichResponse#linkDescriptor") ~enc:(fun _ -> "org.tangled.query.enrichResponse#linkDescriptor")
  |> Jsont.Object.mem "source" Jsont.string ~enc:(fun r -> r.source)
  |> Jsont.Object.opt_mem "targets" (Jsont.list Jsont.string) ~enc:(fun r -> r.targets)
  |> Jsont.Object.mem "type" Jsont.string ~enc:(fun r -> r.type_)
  |> Jsont.Object.finish

type input = {
  enrich : Jsont.json list;
  params : Jsont.json option;
  viewer : string option;
  xrpc : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ enrich params viewer xrpc -> { enrich; params; viewer; xrpc })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.query.enrichResponse#input") ~enc:(fun _ -> "org.tangled.query.enrichResponse#input")
  |> Jsont.Object.mem "enrich" (Jsont.list Jsont.json) ~enc:(fun r -> r.enrich)
  |> Jsont.Object.opt_mem "params" Jsont.json ~enc:(fun r -> r.params)
  |> Jsont.Object.opt_mem "viewer" Jsont.string ~enc:(fun r -> r.viewer)
  |> Jsont.Object.mem "xrpc" Jsont.string ~enc:(fun r -> r.xrpc)
  |> Jsont.Object.finish

type output = {
  data : Jsont.json;
  output : Jsont.json;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ data output -> { data; output })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.query.enrichResponse#output") ~enc:(fun _ -> "org.tangled.query.enrichResponse#output")
  |> Jsont.Object.mem "data" Jsont.json ~enc:(fun r -> r.data)
  |> Jsont.Object.mem "output" Jsont.json ~enc:(fun r -> r.output)
  |> Jsont.Object.finish

      end
    end
    module PublicKey = struct
      module ListKeys = struct
type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.publicKey.listKeys#listItem") ~enc:(fun _ -> "org.tangled.publicKey.listKeys#listItem")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order subject -> {
      cursor;
      limit;
      offset;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.publicKey.listKeys#output") ~enc:(fun _ -> "org.tangled.publicKey.listKeys#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module GetPublicKey = struct
type params = {
  public_key : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun public_key -> {
      public_key;
    })
  |> Jsont.Object.mem "publicKey" Jsont.string
       ~enc:(fun r -> r.public_key)
  |> Jsont.Object.finish

type output = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.publicKey.getPublicKey#output") ~enc:(fun _ -> "org.tangled.publicKey.getPublicKey#output")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

      end
      module CountKeys = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.publicKey.countKeys#output") ~enc:(fun _ -> "org.tangled.publicKey.countKeys#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
    end
    module Markup = struct
      module Plaintext = struct
type main = {
  text : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ text -> { text })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.markup.plaintext") ~enc:(fun _ -> "org.tangled.markup.plaintext")
  |> Jsont.Object.mem "text" Jsont.string ~enc:(fun r -> r.text)
  |> Jsont.Object.finish

      end
      module Embed = struct
        module Video = struct
type main = {
  video : Atp.Blob_ref.t;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ video -> { video })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.markup.embed.video") ~enc:(fun _ -> "org.tangled.markup.embed.video")
  |> Jsont.Object.mem "video" Atp.Blob_ref.jsont ~enc:(fun r -> r.video)
  |> Jsont.Object.finish

        end
        module Image = struct
type main = {
  image : Atp.Blob_ref.t;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ image -> { image })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.markup.embed.image") ~enc:(fun _ -> "org.tangled.markup.embed.image")
  |> Jsont.Object.mem "image" Atp.Blob_ref.jsont ~enc:(fun r -> r.image)
  |> Jsont.Object.finish

        end
      end
      module Markdown = struct
type inline = {
  text : string;
}

let inline_jsont =
  Jsont.Object.map ~kind:"Inline"
    (fun _typ text -> { text })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.markup.markdown#inline") ~enc:(fun _ -> "org.tangled.markup.markdown#inline")
  |> Jsont.Object.mem "text" Jsont.string ~enc:(fun r -> r.text)
  |> Jsont.Object.finish

type main = {
  blobs : Jsont.json list option;
  text : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ blobs text -> { blobs; text })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.markup.markdown") ~enc:(fun _ -> "org.tangled.markup.markdown")
  |> Jsont.Object.opt_mem "blobs" (Jsont.list Jsont.json) ~enc:(fun r -> r.blobs)
  |> Jsont.Object.mem "text" Jsont.string ~enc:(fun r -> r.text)
  |> Jsont.Object.finish

      end
    end
    module Label = struct
      module ListDefinitions = struct
type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.label.listDefinitions#listItem") ~enc:(fun _ -> "org.tangled.label.listDefinitions#listItem")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order subject -> {
      cursor;
      limit;
      offset;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.label.listDefinitions#output") ~enc:(fun _ -> "org.tangled.label.listDefinitions#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module Defs = struct
type label_token = {
  key : string;
}

let label_token_jsont =
  Jsont.Object.map ~kind:"Label_token"
    (fun _typ key -> { key })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.label.defs#labelToken") ~enc:(fun _ -> "org.tangled.label.defs#labelToken")
  |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
  |> Jsont.Object.finish

      end
      module Definition = struct
type main = {
  color : string option;
  created_at : string;
  name : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ color created_at name -> { color; created_at; name })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.label.definition") ~enc:(fun _ -> "org.tangled.label.definition")
  |> Jsont.Object.opt_mem "color" Jsont.string ~enc:(fun r -> r.color)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.finish

      end
      module CountDefinitions = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.label.countDefinitions#output") ~enc:(fun _ -> "org.tangled.label.countDefinitions#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
    end
    module Knot = struct
      module Member = struct
type main = {
  created_at : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at -> { created_at })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.knot.member") ~enc:(fun _ -> "org.tangled.knot.member")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.finish

      end
      module DescribeKnot = struct
type contact = {
  email : string option;
}

let contact_jsont =
  Jsont.Object.map ~kind:"Contact"
    (fun _typ email -> { email })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.knot.describeKnot#contact") ~enc:(fun _ -> "org.tangled.knot.describeKnot#contact")
  |> Jsont.Object.opt_mem "email" Jsont.string ~enc:(fun r -> r.email)
  |> Jsont.Object.finish

type links = {
  privacy_policy : string option;
  terms_of_service : string option;
}

let links_jsont =
  Jsont.Object.map ~kind:"Links"
    (fun _typ privacy_policy terms_of_service -> { privacy_policy; terms_of_service })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.knot.describeKnot#links") ~enc:(fun _ -> "org.tangled.knot.describeKnot#links")
  |> Jsont.Object.opt_mem "privacyPolicy" Jsont.string ~enc:(fun r -> r.privacy_policy)
  |> Jsont.Object.opt_mem "termsOfService" Jsont.string ~enc:(fun r -> r.terms_of_service)
  |> Jsont.Object.finish

type output = {
  capabilities : string list option;
  contact : Jsont.json option;
  did : string;
  links : Jsont.json option;
  version : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ capabilities contact did links version -> { capabilities; contact; did; links; version })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.knot.describeKnot#output") ~enc:(fun _ -> "org.tangled.knot.describeKnot#output")
  |> Jsont.Object.opt_mem "capabilities" (Jsont.list Jsont.string) ~enc:(fun r -> r.capabilities)
  |> Jsont.Object.opt_mem "contact" Jsont.json ~enc:(fun r -> r.contact)
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.opt_mem "links" Jsont.json ~enc:(fun r -> r.links)
  |> Jsont.Object.mem "version" Jsont.string ~enc:(fun r -> r.version)
  |> Jsont.Object.finish

      end
      module Block = struct
type main = {
  created_at : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at -> { created_at })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.knot.block") ~enc:(fun _ -> "org.tangled.knot.block")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.finish

      end
    end
    module Key = struct
      module Unspecced = struct
        module GpgKey = struct
type main = {
  created_at : string;
  name : string;
  public_key : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at name public_key -> { created_at; name; public_key })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.key.unspecced.gpgKey") ~enc:(fun _ -> "org.tangled.key.unspecced.gpgKey")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "publicKey" Jsont.string ~enc:(fun r -> r.public_key)
  |> Jsont.Object.finish

        end
      end
      module SshKey = struct
type main = {
  created_at : string;
  name : string;
  public_key : string;
  usages : string list option;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at name public_key usages -> { created_at; name; public_key; usages })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.key.sshKey") ~enc:(fun _ -> "org.tangled.key.sshKey")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "publicKey" Jsont.string ~enc:(fun r -> r.public_key)
  |> Jsont.Object.opt_mem "usages" (Jsont.list Jsont.string) ~enc:(fun r -> r.usages)
  |> Jsont.Object.finish

      end
    end
    module Graph = struct
      module Vouch = struct
type main = {
  created_at : string;
  evidences : Jsont.json list option;
  kind : string;
  reason : string option;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at evidences kind reason -> { created_at; evidences; kind; reason })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.graph.vouch") ~enc:(fun _ -> "org.tangled.graph.vouch")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "evidences" (Jsont.list Jsont.json) ~enc:(fun r -> r.evidences)
  |> Jsont.Object.mem "kind" Jsont.string ~enc:(fun r -> r.kind)
  |> Jsont.Object.opt_mem "reason" Jsont.string ~enc:(fun r -> r.reason)
  |> Jsont.Object.finish

      end
      module ListNetworkVouches = struct
type list_item = {
  actor : string;
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ actor cid uri value -> { actor; cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.graph.listNetworkVouches#listItem") ~enc:(fun _ -> "org.tangled.graph.listNetworkVouches#listItem")
  |> Jsont.Object.mem "actor" Jsont.string ~enc:(fun r -> r.actor)
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  actors : string list;
  cursor : string option;
  limit : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun actors cursor limit -> {
      actors;
      cursor;
      limit;
    })
  |> Jsont.Object.mem "actors" (Jsont.list Jsont.string)
       ~enc:(fun r -> r.actors)
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items -> { cursor; items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.graph.listNetworkVouches#output") ~enc:(fun _ -> "org.tangled.graph.listNetworkVouches#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

      end
      module ListFollows = struct
type list_item = {
  cid : string option;
  subject : string;
  uri : string;
  value : Jsont.json;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ cid subject uri value -> { cid; subject; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.graph.listFollows#listItem") ~enc:(fun _ -> "org.tangled.graph.listFollows#listItem")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order subject -> {
      cursor;
      limit;
      offset;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.graph.listFollows#output") ~enc:(fun _ -> "org.tangled.graph.listFollows#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module GetFollow = struct
type params = {
  actor : string;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun actor subject -> {
      actor;
      subject;
    })
  |> Jsont.Object.mem "actor" Jsont.string
       ~enc:(fun r -> r.actor)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  uri : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ uri -> { uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.graph.getFollow#output") ~enc:(fun _ -> "org.tangled.graph.getFollow#output")
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

      end
      module Follow = struct
type main = {
  created_at : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at -> { created_at })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.graph.follow") ~enc:(fun _ -> "org.tangled.graph.follow")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.finish

      end
      module CountFollowsBy = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.graph.countFollowsBy#output") ~enc:(fun _ -> "org.tangled.graph.countFollowsBy#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module CountFollows = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.graph.countFollows#output") ~enc:(fun _ -> "org.tangled.graph.countFollows#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module Collaborator = struct
type main = {
  created_at : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at -> { created_at })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.graph.collaborator") ~enc:(fun _ -> "org.tangled.graph.collaborator")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.finish

      end
      module Attestation = struct
type main = {
  change : string option;
  created_at : string;
  subject : Jsont.json;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ change created_at subject -> { change; created_at; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.graph.attestation") ~enc:(fun _ -> "org.tangled.graph.attestation")
  |> Jsont.Object.opt_mem "change" Jsont.string ~enc:(fun r -> r.change)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "subject" Jsont.json ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module ListFollowsBy = struct
type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order subject -> {
      cursor;
      limit;
      offset;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.graph.listFollowsBy#output") ~enc:(fun _ -> "org.tangled.graph.listFollowsBy#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
    end
    module Git = struct
      module SetDefaultBranch = struct
type input = {
  branch : string;
  repo : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ branch repo -> { branch; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.setDefaultBranch#input") ~enc:(fun _ -> "org.tangled.git.setDefaultBranch#input")
  |> Jsont.Object.mem "branch" Jsont.string ~enc:(fun r -> r.branch)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  cid : string;
  uri : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cid uri -> { cid; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.setDefaultBranch#output") ~enc:(fun _ -> "org.tangled.git.setDefaultBranch#output")
  |> Jsont.Object.mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

      end
      module Refs = struct
type main = {
  default_branch : string;
  refs : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ default_branch refs -> { default_branch; refs })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.refs") ~enc:(fun _ -> "org.tangled.git.refs")
  |> Jsont.Object.mem "defaultBranch" Jsont.string ~enc:(fun r -> r.default_branch)
  |> Jsont.Object.mem "refs" Atp.Lex.bytes_jsont ~enc:(fun r -> r.refs)
  |> Jsont.Object.finish

      end
      module ReceivePack = struct
      end
      module PushEvent = struct
type ref_edit = {
  name : string;
  new_ : string option;
  old : string option;
}

let ref_edit_jsont =
  Jsont.Object.map ~kind:"Ref_edit"
    (fun _typ name new_ old -> { name; new_; old })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.pushEvent#refEdit") ~enc:(fun _ -> "org.tangled.git.pushEvent#refEdit")
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "new" (Jsont.option Jsont.string) ~enc:(fun r -> r.new_)
  |> Jsont.Object.mem "old" (Jsont.option Jsont.string) ~enc:(fun r -> r.old)
  |> Jsont.Object.finish

type main = {
  edits : Jsont.json list;
  options : string list option;
  pusher : string;
  repo : string;
  seq : int;
  sig_ : string;
  time : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ edits options pusher repo seq sig_ time -> { edits; options; pusher; repo; seq; sig_; time })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.pushEvent") ~enc:(fun _ -> "org.tangled.git.pushEvent")
  |> Jsont.Object.mem "edits" (Jsont.list Jsont.json) ~enc:(fun r -> r.edits)
  |> Jsont.Object.opt_mem "options" (Jsont.list Jsont.string) ~enc:(fun r -> r.options)
  |> Jsont.Object.mem "pusher" Jsont.string ~enc:(fun r -> r.pusher)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "seq" Jsont.int ~enc:(fun r -> r.seq)
  |> Jsont.Object.mem "sig" Atp.Lex.bytes_jsont ~enc:(fun r -> r.sig_)
  |> Jsont.Object.mem "time" Jsont.string ~enc:(fun r -> r.time)
  |> Jsont.Object.finish

      end
      module Oid = struct
type main = {
  oid : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ oid -> { oid })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.oid") ~enc:(fun _ -> "org.tangled.git.oid")
  |> Jsont.Object.mem "oid" Jsont.string ~enc:(fun r -> r.oid)
  |> Jsont.Object.finish

      end
      module ListRefs = struct
type default_branch = {
  head : string option;
  ref_ : string;
}

let default_branch_jsont =
  Jsont.Object.map ~kind:"Default_branch"
    (fun _typ head ref_ -> { head; ref_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.listRefs#defaultBranch") ~enc:(fun _ -> "org.tangled.git.listRefs#defaultBranch")
  |> Jsont.Object.opt_mem "head" Jsont.string ~enc:(fun r -> r.head)
  |> Jsont.Object.mem "ref" Jsont.string ~enc:(fun r -> r.ref_)
  |> Jsont.Object.finish

type ref_ = {
  ref_ : string;
  sha : string;
}

let ref__jsont =
  Jsont.Object.map ~kind:"Ref_"
    (fun _typ ref_ sha -> { ref_; sha })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.listRefs#ref") ~enc:(fun _ -> "org.tangled.git.listRefs#ref")
  |> Jsont.Object.mem "ref" Jsont.string ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "sha" Jsont.string ~enc:(fun r -> r.sha)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit repo -> {
      cursor;
      limit;
      repo;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  default_branch : Jsont.json option;
  refs : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor default_branch refs -> { cursor; default_branch; refs })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.listRefs#output") ~enc:(fun _ -> "org.tangled.git.listRefs#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "defaultBranch" Jsont.json ~enc:(fun r -> r.default_branch)
  |> Jsont.Object.mem "refs" (Jsont.list Jsont.json) ~enc:(fun r -> r.refs)
  |> Jsont.Object.finish

      end
      module ListLanguages = struct
type language = {
  color : string option;
  extensions : string list option;
  file_count : int option;
  name : string;
  percentage : int;
  size : int;
}

let language_jsont =
  Jsont.Object.map ~kind:"Language"
    (fun _typ color extensions file_count name percentage size -> { color; extensions; file_count; name; percentage; size })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.listLanguages#language") ~enc:(fun _ -> "org.tangled.git.listLanguages#language")
  |> Jsont.Object.opt_mem "color" Jsont.string ~enc:(fun r -> r.color)
  |> Jsont.Object.opt_mem "extensions" (Jsont.list Jsont.string) ~enc:(fun r -> r.extensions)
  |> Jsont.Object.opt_mem "fileCount" Jsont.int ~enc:(fun r -> r.file_count)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "percentage" Jsont.int ~enc:(fun r -> r.percentage)
  |> Jsont.Object.mem "size" Jsont.int ~enc:(fun r -> r.size)
  |> Jsont.Object.finish

type params = {
  ref_ : string option;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun ref_ repo -> {
      ref_;
      repo;
    })
  |> Jsont.Object.opt_mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  languages : Jsont.json list;
  ref_ : string;
  total_files : int option;
  total_size : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ languages ref_ total_files total_size -> { languages; ref_; total_files; total_size })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.listLanguages#output") ~enc:(fun _ -> "org.tangled.git.listLanguages#output")
  |> Jsont.Object.mem "languages" (Jsont.list Jsont.json) ~enc:(fun r -> r.languages)
  |> Jsont.Object.mem "ref" Jsont.string ~enc:(fun r -> r.ref_)
  |> Jsont.Object.opt_mem "totalFiles" Jsont.int ~enc:(fun r -> r.total_files)
  |> Jsont.Object.opt_mem "totalSize" Jsont.int ~enc:(fun r -> r.total_size)
  |> Jsont.Object.finish

      end
      module GetBlob = struct
type params = {
  path : string;
  ref_ : string option;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun path ref_ repo -> {
      path;
      ref_;
      repo;
    })
  |> Jsont.Object.mem "path" Jsont.string
       ~enc:(fun r -> r.path)
  |> Jsont.Object.opt_mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = unit
let output_jsont = Jsont.ignore

      end
      module GetBlame = struct
type author = {
  email : string;
  name : string;
}

let author_jsont =
  Jsont.Object.map ~kind:"Author"
    (fun _typ email name -> { email; name })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.getBlame#author") ~enc:(fun _ -> "org.tangled.git.getBlame#author")
  |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.finish

type run = {
  commit : string;
  count : int;
  start : int;
}

let run_jsont =
  Jsont.Object.map ~kind:"Run"
    (fun _typ commit count start -> { commit; count; start })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.getBlame#run") ~enc:(fun _ -> "org.tangled.git.getBlame#run")
  |> Jsont.Object.mem "commit" Jsont.string ~enc:(fun r -> r.commit)
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "start" Jsont.int ~enc:(fun r -> r.start)
  |> Jsont.Object.finish

type commit = {
  author : Jsont.json;
  authored_at : string;
  sha : string;
  subject : string;
}

let commit_jsont =
  Jsont.Object.map ~kind:"Commit"
    (fun _typ author authored_at sha subject -> { author; authored_at; sha; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.getBlame#commit") ~enc:(fun _ -> "org.tangled.git.getBlame#commit")
  |> Jsont.Object.mem "author" Jsont.json ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "authoredAt" Jsont.string ~enc:(fun r -> r.authored_at)
  |> Jsont.Object.mem "sha" Jsont.string ~enc:(fun r -> r.sha)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type params = {
  path : string;
  ref_ : string;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun path ref_ repo -> {
      path;
      ref_;
      repo;
    })
  |> Jsont.Object.mem "path" Jsont.string
       ~enc:(fun r -> r.path)
  |> Jsont.Object.mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  commit : string;
  commits : Jsont.json list;
  runs : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ commit commits runs -> { commit; commits; runs })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.getBlame#output") ~enc:(fun _ -> "org.tangled.git.getBlame#output")
  |> Jsont.Object.mem "commit" Jsont.string ~enc:(fun r -> r.commit)
  |> Jsont.Object.mem "commits" (Jsont.list Jsont.json) ~enc:(fun r -> r.commits)
  |> Jsont.Object.mem "runs" (Jsont.list Jsont.json) ~enc:(fun r -> r.runs)
  |> Jsont.Object.finish

      end
      module GetArchive = struct
type params = {
  format : string option;
  prefix : string;
  ref_ : string;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun format prefix ref_ repo -> {
      format;
      prefix;
      ref_;
      repo;
    })
  |> Jsont.Object.opt_mem "format" Jsont.string
       ~enc:(fun r -> r.format)
  |> Jsont.Object.mem "prefix" Jsont.string
       ~enc:(fun r -> r.prefix)
  |> Jsont.Object.mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = unit
let output_jsont = Jsont.ignore

      end
      module DeleteBranch = struct
type input = {
  branch : string;
  repo : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ branch repo -> { branch; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.deleteBranch#input") ~enc:(fun _ -> "org.tangled.git.deleteBranch#input")
  |> Jsont.Object.mem "branch" Jsont.string ~enc:(fun r -> r.branch)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

      end
      module Defs = struct
type byte_changes = {
  lhs : int;
  rhs : int;
}

let byte_changes_jsont =
  Jsont.Object.map ~kind:"Byte_changes"
    (fun _typ lhs rhs -> { lhs; rhs })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.defs#byteChanges") ~enc:(fun _ -> "org.tangled.git.defs#byteChanges")
  |> Jsont.Object.mem "lhs" Jsont.int ~enc:(fun r -> r.lhs)
  |> Jsont.Object.mem "rhs" Jsont.int ~enc:(fun r -> r.rhs)
  |> Jsont.Object.finish

type diff_src = {
  content : string option;
  is_binary : bool;
  is_submodule : bool;
  oid : string;
  path : string;
  size : int;
}

let diff_src_jsont =
  Jsont.Object.map ~kind:"Diff_src"
    (fun _typ content is_binary is_submodule oid path size -> { content; is_binary; is_submodule; oid; path; size })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.defs#diffSrc") ~enc:(fun _ -> "org.tangled.git.defs#diffSrc")
  |> Jsont.Object.opt_mem "content" Jsont.string ~enc:(fun r -> r.content)
  |> Jsont.Object.mem "isBinary" Jsont.bool ~enc:(fun r -> r.is_binary)
  |> Jsont.Object.mem "isSubmodule" Jsont.bool ~enc:(fun r -> r.is_submodule)
  |> Jsont.Object.mem "oid" Jsont.string ~enc:(fun r -> r.oid)
  |> Jsont.Object.mem "path" Jsont.string ~enc:(fun r -> r.path)
  |> Jsont.Object.mem "size" Jsont.int ~enc:(fun r -> r.size)
  |> Jsont.Object.finish

type diff_stat = {
  deletions : int;
  files_changed : int;
  insertions : int;
}

let diff_stat_jsont =
  Jsont.Object.map ~kind:"Diff_stat"
    (fun _typ deletions files_changed insertions -> { deletions; files_changed; insertions })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.defs#diffStat") ~enc:(fun _ -> "org.tangled.git.defs#diffStat")
  |> Jsont.Object.mem "deletions" Jsont.int ~enc:(fun r -> r.deletions)
  |> Jsont.Object.mem "filesChanged" Jsont.int ~enc:(fun r -> r.files_changed)
  |> Jsont.Object.mem "insertions" Jsont.int ~enc:(fun r -> r.insertions)
  |> Jsont.Object.finish

type line_pair = {
  lhs : int option;
  rhs : int option;
}

let line_pair_jsont =
  Jsont.Object.map ~kind:"Line_pair"
    (fun _typ lhs rhs -> { lhs; rhs })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.defs#linePair") ~enc:(fun _ -> "org.tangled.git.defs#linePair")
  |> Jsont.Object.opt_mem "lhs" Jsont.int ~enc:(fun r -> r.lhs)
  |> Jsont.Object.opt_mem "rhs" Jsont.int ~enc:(fun r -> r.rhs)
  |> Jsont.Object.finish

type signature = {
  email : string;
  name : string;
  when_ : string;
}

let signature_jsont =
  Jsont.Object.map ~kind:"Signature"
    (fun _typ email name when_ -> { email; name; when_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.defs#signature") ~enc:(fun _ -> "org.tangled.git.defs#signature")
  |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "when" Jsont.string ~enc:(fun r -> r.when_)
  |> Jsont.Object.finish

type submodule = {
  branch : string option;
  name : string;
  url : string;
}

let submodule_jsont =
  Jsont.Object.map ~kind:"Submodule"
    (fun _typ branch name url -> { branch; name; url })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.defs#submodule") ~enc:(fun _ -> "org.tangled.git.defs#submodule")
  |> Jsont.Object.opt_mem "branch" Jsont.string ~enc:(fun r -> r.branch)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "url" Jsont.string ~enc:(fun r -> r.url)
  |> Jsont.Object.finish

type commit = {
  author : Jsont.json;
  committer : Jsont.json;
  hash : string;
  message : string;
  tree : string;
}

let commit_jsont =
  Jsont.Object.map ~kind:"Commit"
    (fun _typ author committer hash message tree -> { author; committer; hash; message; tree })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.defs#commit") ~enc:(fun _ -> "org.tangled.git.defs#commit")
  |> Jsont.Object.mem "author" Jsont.json ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "committer" Jsont.json ~enc:(fun r -> r.committer)
  |> Jsont.Object.mem "hash" Jsont.string ~enc:(fun r -> r.hash)
  |> Jsont.Object.mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.mem "tree" Jsont.string ~enc:(fun r -> r.tree)
  |> Jsont.Object.finish

type diff_hunk = {
  lines : Jsont.json list;
  novel_lhs : int list;
  novel_rhs : int list;
}

let diff_hunk_jsont =
  Jsont.Object.map ~kind:"Diff_hunk"
    (fun _typ lines novel_lhs novel_rhs -> { lines; novel_lhs; novel_rhs })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.defs#diffHunk") ~enc:(fun _ -> "org.tangled.git.defs#diffHunk")
  |> Jsont.Object.mem "lines" (Jsont.list Jsont.json) ~enc:(fun r -> r.lines)
  |> Jsont.Object.mem "novelLhs" (Jsont.list Jsont.int) ~enc:(fun r -> r.novel_lhs)
  |> Jsont.Object.mem "novelRhs" (Jsont.list Jsont.int) ~enc:(fun r -> r.novel_rhs)
  |> Jsont.Object.finish

type file_diff = {
  has_byte_changes : Jsont.json option;
  has_syntactic_changes : bool;
  hunks : Jsont.json list;
  lhs_src : Jsont.json;
  rhs_src : Jsont.json;
}

let file_diff_jsont =
  Jsont.Object.map ~kind:"File_diff"
    (fun _typ has_byte_changes has_syntactic_changes hunks lhs_src rhs_src -> { has_byte_changes; has_syntactic_changes; hunks; lhs_src; rhs_src })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.defs#fileDiff") ~enc:(fun _ -> "org.tangled.git.defs#fileDiff")
  |> Jsont.Object.opt_mem "hasByteChanges" Jsont.json ~enc:(fun r -> r.has_byte_changes)
  |> Jsont.Object.mem "hasSyntacticChanges" Jsont.bool ~enc:(fun r -> r.has_syntactic_changes)
  |> Jsont.Object.mem "hunks" (Jsont.list Jsont.json) ~enc:(fun r -> r.hunks)
  |> Jsont.Object.mem "lhsSrc" Jsont.json ~enc:(fun r -> r.lhs_src)
  |> Jsont.Object.mem "rhsSrc" Jsont.json ~enc:(fun r -> r.rhs_src)
  |> Jsont.Object.finish

      end
      module ListTags = struct
type annotation = {
  message : string;
  tagger : Jsont.json option;
}

let annotation_jsont =
  Jsont.Object.map ~kind:"Annotation"
    (fun _typ message tagger -> { message; tagger })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.listTags#annotation") ~enc:(fun _ -> "org.tangled.git.listTags#annotation")
  |> Jsont.Object.mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.opt_mem "tagger" Jsont.json ~enc:(fun r -> r.tagger)
  |> Jsont.Object.finish

type tag_info = {
  annotation : Jsont.json option;
  author : Jsont.json;
  message : string;
  name : string;
  oid : string;
}

let tag_info_jsont =
  Jsont.Object.map ~kind:"Tag_info"
    (fun _typ annotation author message name oid -> { annotation; author; message; name; oid })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.listTags#tagInfo") ~enc:(fun _ -> "org.tangled.git.listTags#tagInfo")
  |> Jsont.Object.opt_mem "annotation" Jsont.json ~enc:(fun r -> r.annotation)
  |> Jsont.Object.mem "author" Jsont.json ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "oid" Jsont.string ~enc:(fun r -> r.oid)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit repo -> {
      cursor;
      limit;
      repo;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  tags : Jsont.json list;
  total : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor tags total -> { cursor; tags; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.listTags#output") ~enc:(fun _ -> "org.tangled.git.listTags#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "tags" (Jsont.list Jsont.json) ~enc:(fun r -> r.tags)
  |> Jsont.Object.mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module ListCommits = struct
type params = {
  cursor : string option;
  limit : int option;
  path : string option;
  ref_ : string;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit path ref_ repo -> {
      cursor;
      limit;
      path;
      ref_;
      repo;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "path" Jsont.string
       ~enc:(fun r -> r.path)
  |> Jsont.Object.mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  commits : Jsont.json list;
  cursor : string option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ commits cursor -> { commits; cursor })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.listCommits#output") ~enc:(fun _ -> "org.tangled.git.listCommits#output")
  |> Jsont.Object.mem "commits" (Jsont.list Jsont.json) ~enc:(fun r -> r.commits)
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.finish

      end
      module ListBranches = struct
type branch_info = {
  author : Jsont.json;
  committer : Jsont.json;
  message : string;
  name : string;
  oid : string;
}

let branch_info_jsont =
  Jsont.Object.map ~kind:"Branch_info"
    (fun _typ author committer message name oid -> { author; committer; message; name; oid })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.listBranches#branchInfo") ~enc:(fun _ -> "org.tangled.git.listBranches#branchInfo")
  |> Jsont.Object.mem "author" Jsont.json ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "committer" Jsont.json ~enc:(fun r -> r.committer)
  |> Jsont.Object.mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "oid" Jsont.string ~enc:(fun r -> r.oid)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit repo -> {
      cursor;
      limit;
      repo;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  branches : Jsont.json list;
  cursor : string option;
  default_branch : string option;
  total : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ branches cursor default_branch total -> { branches; cursor; default_branch; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.listBranches#output") ~enc:(fun _ -> "org.tangled.git.listBranches#output")
  |> Jsont.Object.mem "branches" (Jsont.list Jsont.json) ~enc:(fun r -> r.branches)
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "defaultBranch" Jsont.string ~enc:(fun r -> r.default_branch)
  |> Jsont.Object.mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module GetTree = struct
type tree_entry = {
  last_commit : Jsont.json option;
  mode : string;
  name : string;
  size : int;
}

let tree_entry_jsont =
  Jsont.Object.map ~kind:"Tree_entry"
    (fun _typ last_commit mode name size -> { last_commit; mode; name; size })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.getTree#treeEntry") ~enc:(fun _ -> "org.tangled.git.getTree#treeEntry")
  |> Jsont.Object.opt_mem "lastCommit" Jsont.json ~enc:(fun r -> r.last_commit)
  |> Jsont.Object.mem "mode" Jsont.string ~enc:(fun r -> r.mode)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "size" Jsont.int ~enc:(fun r -> r.size)
  |> Jsont.Object.finish

type params = {
  path : string option;
  ref_ : string option;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun path ref_ repo -> {
      path;
      ref_;
      repo;
    })
  |> Jsont.Object.opt_mem "path" Jsont.string
       ~enc:(fun r -> r.path)
  |> Jsont.Object.opt_mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  entries : Jsont.json list;
  last_commit : Jsont.json option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ entries last_commit -> { entries; last_commit })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.getTree#output") ~enc:(fun _ -> "org.tangled.git.getTree#output")
  |> Jsont.Object.mem "entries" (Jsont.list Jsont.json) ~enc:(fun r -> r.entries)
  |> Jsont.Object.opt_mem "lastCommit" Jsont.json ~enc:(fun r -> r.last_commit)
  |> Jsont.Object.finish

      end
      module GetEntry = struct
type params = {
  path : string;
  ref_ : string option;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun path ref_ repo -> {
      path;
      ref_;
      repo;
    })
  |> Jsont.Object.mem "path" Jsont.string
       ~enc:(fun r -> r.path)
  |> Jsont.Object.opt_mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  last_commit : Jsont.json option;
  mode : string;
  name : string;
  oid : string;
  size : int;
  submodule : Jsont.json option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ last_commit mode name oid size submodule -> { last_commit; mode; name; oid; size; submodule })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.getEntry#output") ~enc:(fun _ -> "org.tangled.git.getEntry#output")
  |> Jsont.Object.opt_mem "lastCommit" Jsont.json ~enc:(fun r -> r.last_commit)
  |> Jsont.Object.mem "mode" Jsont.string ~enc:(fun r -> r.mode)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "oid" Jsont.string ~enc:(fun r -> r.oid)
  |> Jsont.Object.mem "size" Jsont.int ~enc:(fun r -> r.size)
  |> Jsont.Object.opt_mem "submodule" Jsont.json ~enc:(fun r -> r.submodule)
  |> Jsont.Object.finish

      end
      module GetDiff = struct
type params = {
  base : string;
  head : string;
  paths : string option;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun base head paths repo -> {
      base;
      head;
      paths;
      repo;
    })
  |> Jsont.Object.mem "base" Jsont.string
       ~enc:(fun r -> r.base)
  |> Jsont.Object.mem "head" Jsont.string
       ~enc:(fun r -> r.head)
  |> Jsont.Object.opt_mem "paths" Jsont.string
       ~enc:(fun r -> r.paths)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  diffs : Jsont.json list;
  stats : Jsont.json option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ diffs stats -> { diffs; stats })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.getDiff#output") ~enc:(fun _ -> "org.tangled.git.getDiff#output")
  |> Jsont.Object.mem "diffs" (Jsont.list Jsont.json) ~enc:(fun r -> r.diffs)
  |> Jsont.Object.opt_mem "stats" Jsont.json ~enc:(fun r -> r.stats)
  |> Jsont.Object.finish

      end
      module GetBranch = struct
type params = {
  name : string;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun name repo -> {
      name;
      repo;
    })
  |> Jsont.Object.mem "name" Jsont.string
       ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  author : Jsont.json;
  hash : string;
  message : string;
  name : string;
  total_commits : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ author hash message name total_commits -> { author; hash; message; name; total_commits })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.getBranch#output") ~enc:(fun _ -> "org.tangled.git.getBranch#output")
  |> Jsont.Object.mem "author" Jsont.json ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "hash" Jsont.string ~enc:(fun r -> r.hash)
  |> Jsont.Object.mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "totalCommits" Jsont.int ~enc:(fun r -> r.total_commits)
  |> Jsont.Object.finish

      end
      module GetTag = struct
type params = {
  repo : string;
  tag : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun repo tag -> {
      repo;
      tag;
    })
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "tag" Jsont.string
       ~enc:(fun r -> r.tag)
  |> Jsont.Object.finish

type output = {
  annotation : Jsont.json option;
  author : Jsont.json;
  message : string;
  name : string;
  oid : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ annotation author message name oid -> { annotation; author; message; name; oid })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.git.getTag#output") ~enc:(fun _ -> "org.tangled.git.getTag#output")
  |> Jsont.Object.opt_mem "annotation" Jsont.json ~enc:(fun r -> r.annotation)
  |> Jsont.Object.mem "author" Jsont.json ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "oid" Jsont.string ~enc:(fun r -> r.oid)
  |> Jsont.Object.finish

      end
    end
    module Event = struct
      module Push = struct
type main = {
  actor : string option;
  new_sha : string;
  old_sha : string;
  push_options : string list option;
  ref_ : string;
  repo : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ actor new_sha old_sha push_options ref_ repo -> { actor; new_sha; old_sha; push_options; ref_; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.event.push") ~enc:(fun _ -> "org.tangled.event.push")
  |> Jsont.Object.opt_mem "actor" Jsont.string ~enc:(fun r -> r.actor)
  |> Jsont.Object.mem "newSha" Jsont.string ~enc:(fun r -> r.new_sha)
  |> Jsont.Object.mem "oldSha" Jsont.string ~enc:(fun r -> r.old_sha)
  |> Jsont.Object.opt_mem "pushOptions" (Jsont.list Jsont.string) ~enc:(fun r -> r.push_options)
  |> Jsont.Object.mem "ref" Jsont.string ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

      end
      module PullRequest = struct
type main = {
  action : string option;
  pull : string option;
  source_branch : string option;
  source_repo : string option;
  source_sha : string option;
  target_branch : string option;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ action pull source_branch source_repo source_sha target_branch -> { action; pull; source_branch; source_repo; source_sha; target_branch })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.event.pullRequest") ~enc:(fun _ -> "org.tangled.event.pullRequest")
  |> Jsont.Object.opt_mem "action" Jsont.string ~enc:(fun r -> r.action)
  |> Jsont.Object.opt_mem "pull" Jsont.string ~enc:(fun r -> r.pull)
  |> Jsont.Object.opt_mem "sourceBranch" Jsont.string ~enc:(fun r -> r.source_branch)
  |> Jsont.Object.opt_mem "sourceRepo" Jsont.string ~enc:(fun r -> r.source_repo)
  |> Jsont.Object.opt_mem "sourceSha" Jsont.string ~enc:(fun r -> r.source_sha)
  |> Jsont.Object.opt_mem "targetBranch" Jsont.string ~enc:(fun r -> r.target_branch)
  |> Jsont.Object.finish

      end
      module NotifyEvent = struct
type ci_output = {
  message : string option;
  pipelines : string list option;
}

let ci_output_jsont =
  Jsont.Object.map ~kind:"Ci_output"
    (fun _typ message pipelines -> { message; pipelines })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.event.notifyEvent#ciOutput") ~enc:(fun _ -> "org.tangled.event.notifyEvent#ciOutput")
  |> Jsont.Object.opt_mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.opt_mem "pipelines" (Jsont.list Jsont.string) ~enc:(fun r -> r.pipelines)
  |> Jsont.Object.finish

type input = {
  event : Jsont.json;
  repo : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ event repo -> { event; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.event.notifyEvent#input") ~enc:(fun _ -> "org.tangled.event.notifyEvent#input")
  |> Jsont.Object.mem "event" Jsont.json ~enc:(fun r -> r.event)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  ci : Jsont.json option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ ci -> { ci })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.event.notifyEvent#output") ~enc:(fun _ -> "org.tangled.event.notifyEvent#output")
  |> Jsont.Object.opt_mem "ci" Jsont.json ~enc:(fun r -> r.ci)
  |> Jsont.Object.finish

      end
    end
    module Ci = struct
      module Trigger = struct
        module Schedule = struct
type main = {
  commit : string;
  ref_ : string;
  repo : string;
  scheduled_at : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ commit ref_ repo scheduled_at -> { commit; ref_; repo; scheduled_at })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.ci.trigger.schedule") ~enc:(fun _ -> "org.tangled.ci.trigger.schedule")
  |> Jsont.Object.mem "commit" Jsont.string ~enc:(fun r -> r.commit)
  |> Jsont.Object.mem "ref" Jsont.string ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "scheduledAt" Jsont.string ~enc:(fun r -> r.scheduled_at)
  |> Jsont.Object.finish

        end
        module Manual = struct
type pair = {
  key : string;
  value : string;
}

let pair_jsont =
  Jsont.Object.map ~kind:"Pair"
    (fun _typ key value -> { key; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.ci.trigger.manual#pair") ~enc:(fun _ -> "org.tangled.ci.trigger.manual#pair")
  |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
  |> Jsont.Object.mem "value" Jsont.string ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type main = {
  commit : string;
  inputs : Jsont.json list option;
  ref_ : string option;
  repo : string;
  workflows : string list option;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ commit inputs ref_ repo workflows -> { commit; inputs; ref_; repo; workflows })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.ci.trigger.manual") ~enc:(fun _ -> "org.tangled.ci.trigger.manual")
  |> Jsont.Object.mem "commit" Jsont.string ~enc:(fun r -> r.commit)
  |> Jsont.Object.opt_mem "inputs" (Jsont.list Jsont.json) ~enc:(fun r -> r.inputs)
  |> Jsont.Object.opt_mem "ref" Jsont.string ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.opt_mem "workflows" (Jsont.list Jsont.string) ~enc:(fun r -> r.workflows)
  |> Jsont.Object.finish

        end
      end
      module SubscribePipelineLogs = struct
type control = {
  command : string option;
  content : string;
  kind : string option;
  outcome : string option;
  status : string option;
  step : int;
  time : string;
  workflow : string;
}

let control_jsont =
  Jsont.Object.map ~kind:"Control"
    (fun _typ command content kind outcome status step time workflow -> { command; content; kind; outcome; status; step; time; workflow })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.ci.subscribePipelineLogs#control") ~enc:(fun _ -> "org.tangled.ci.subscribePipelineLogs#control")
  |> Jsont.Object.opt_mem "command" Jsont.string ~enc:(fun r -> r.command)
  |> Jsont.Object.mem "content" Jsont.string ~enc:(fun r -> r.content)
  |> Jsont.Object.opt_mem "kind" Jsont.string ~enc:(fun r -> r.kind)
  |> Jsont.Object.opt_mem "outcome" Jsont.string ~enc:(fun r -> r.outcome)
  |> Jsont.Object.opt_mem "status" Jsont.string ~enc:(fun r -> r.status)
  |> Jsont.Object.mem "step" Jsont.int ~enc:(fun r -> r.step)
  |> Jsont.Object.mem "time" Jsont.string ~enc:(fun r -> r.time)
  |> Jsont.Object.mem "workflow" Jsont.string ~enc:(fun r -> r.workflow)
  |> Jsont.Object.finish

type data = {
  content : string;
  step : int;
  stream : string;
  time : string;
  workflow : string;
}

let data_jsont =
  Jsont.Object.map ~kind:"Data"
    (fun _typ content step stream time workflow -> { content; step; stream; time; workflow })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.ci.subscribePipelineLogs#data") ~enc:(fun _ -> "org.tangled.ci.subscribePipelineLogs#data")
  |> Jsont.Object.mem "content" Jsont.string ~enc:(fun r -> r.content)
  |> Jsont.Object.mem "step" Jsont.int ~enc:(fun r -> r.step)
  |> Jsont.Object.mem "stream" Jsont.string ~enc:(fun r -> r.stream)
  |> Jsont.Object.mem "time" Jsont.string ~enc:(fun r -> r.time)
  |> Jsont.Object.mem "workflow" Jsont.string ~enc:(fun r -> r.workflow)
  |> Jsont.Object.finish

type params = {
  pipeline : string;
  repo : string;
  workflows : string list option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun pipeline repo workflows -> {
      pipeline;
      repo;
      workflows;
    })
  |> Jsont.Object.mem "pipeline" Jsont.string
       ~enc:(fun r -> r.pipeline)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.opt_mem "workflows" (Jsont.list Jsont.string)
       ~enc:(fun r -> r.workflows)
  |> Jsont.Object.finish

type message = Jsont.json

let message_jsont = Jsont.json

      end
      module RetryPipeline = struct
type input = {
  pipeline : string;
  repo : string;
  workflows : string list option;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ pipeline repo workflows -> { pipeline; repo; workflows })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.ci.retryPipeline#input") ~enc:(fun _ -> "org.tangled.ci.retryPipeline#input")
  |> Jsont.Object.mem "pipeline" Jsont.string ~enc:(fun r -> r.pipeline)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.opt_mem "workflows" (Jsont.list Jsont.string) ~enc:(fun r -> r.workflows)
  |> Jsont.Object.finish

type output = {
  pipeline : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ pipeline -> { pipeline })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.ci.retryPipeline#output") ~enc:(fun _ -> "org.tangled.ci.retryPipeline#output")
  |> Jsont.Object.mem "pipeline" Jsont.string ~enc:(fun r -> r.pipeline)
  |> Jsont.Object.finish

      end
      module CancelPipeline = struct
type input = {
  pipeline : string;
  repo : string;
  workflows : string list option;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ pipeline repo workflows -> { pipeline; repo; workflows })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.ci.cancelPipeline#input") ~enc:(fun _ -> "org.tangled.ci.cancelPipeline#input")
  |> Jsont.Object.mem "pipeline" Jsont.string ~enc:(fun r -> r.pipeline)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.opt_mem "workflows" (Jsont.list Jsont.string) ~enc:(fun r -> r.workflows)
  |> Jsont.Object.finish

      end
      module ApprovePipeline = struct
type input = {
  pipeline : string;
  repo : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ pipeline repo -> { pipeline; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.ci.approvePipeline#input") ~enc:(fun _ -> "org.tangled.ci.approvePipeline#input")
  |> Jsont.Object.mem "pipeline" Jsont.string ~enc:(fun r -> r.pipeline)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

      end
      module TriggerPipeline = struct
type input = {
  repo : string;
  trigger : Jsont.json;
  workflows : string list option;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ repo trigger workflows -> { repo; trigger; workflows })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.ci.triggerPipeline#input") ~enc:(fun _ -> "org.tangled.ci.triggerPipeline#input")
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "trigger" Jsont.json ~enc:(fun r -> r.trigger)
  |> Jsont.Object.opt_mem "workflows" (Jsont.list Jsont.string) ~enc:(fun r -> r.workflows)
  |> Jsont.Object.finish

type output = {
  pipeline : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ pipeline -> { pipeline })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.ci.triggerPipeline#output") ~enc:(fun _ -> "org.tangled.ci.triggerPipeline#output")
  |> Jsont.Object.mem "pipeline" Jsont.string ~enc:(fun r -> r.pipeline)
  |> Jsont.Object.finish

      end
      module Defs = struct
type external_source = {
  link : string;
  name : string option;
}

let external_source_jsont =
  Jsont.Object.map ~kind:"External_source"
    (fun _typ link name -> { link; name })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.ci.defs#externalSource") ~enc:(fun _ -> "org.tangled.ci.defs#externalSource")
  |> Jsont.Object.mem "link" Jsont.string ~enc:(fun r -> r.link)
  |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.finish

type line_range = {
  end_ : int;
  start : int;
}

let line_range_jsont =
  Jsont.Object.map ~kind:"Line_range"
    (fun _typ end_ start -> { end_; start })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.ci.defs#lineRange") ~enc:(fun _ -> "org.tangled.ci.defs#lineRange")
  |> Jsont.Object.mem "end" Jsont.int ~enc:(fun r -> r.end_)
  |> Jsont.Object.mem "start" Jsont.int ~enc:(fun r -> r.start)
  |> Jsont.Object.finish

type workflow = {
  hash : string option;
  id : string;
  name : string option;
  source : Jsont.json;
  triggers : string list;
}

let workflow_jsont =
  Jsont.Object.map ~kind:"Workflow"
    (fun _typ hash id name source triggers -> { hash; id; name; source; triggers })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.ci.defs#workflow") ~enc:(fun _ -> "org.tangled.ci.defs#workflow")
  |> Jsont.Object.opt_mem "hash" Jsont.string ~enc:(fun r -> r.hash)
  |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
  |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "source" Jsont.json ~enc:(fun r -> r.source)
  |> Jsont.Object.mem "triggers" (Jsont.list Jsont.string) ~enc:(fun r -> r.triggers)
  |> Jsont.Object.finish

type file_source = {
  commit : string;
  lines : Jsont.json option;
  path : string;
  repo : string;
}

let file_source_jsont =
  Jsont.Object.map ~kind:"File_source"
    (fun _typ commit lines path repo -> { commit; lines; path; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.ci.defs#fileSource") ~enc:(fun _ -> "org.tangled.ci.defs#fileSource")
  |> Jsont.Object.mem "commit" Jsont.string ~enc:(fun r -> r.commit)
  |> Jsont.Object.opt_mem "lines" Jsont.json ~enc:(fun r -> r.lines)
  |> Jsont.Object.mem "path" Jsont.string ~enc:(fun r -> r.path)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type workflow_run = {
  definition : Jsont.json;
  error : string option;
  finished_at : string option;
  id : string;
  name : string option;
  started_at : string option;
  status : string;
}

let workflow_run_jsont =
  Jsont.Object.map ~kind:"Workflow_run"
    (fun _typ definition error finished_at id name started_at status -> { definition; error; finished_at; id; name; started_at; status })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.ci.defs#workflowRun") ~enc:(fun _ -> "org.tangled.ci.defs#workflowRun")
  |> Jsont.Object.mem "definition" Jsont.json ~enc:(fun r -> r.definition)
  |> Jsont.Object.opt_mem "error" Jsont.string ~enc:(fun r -> r.error)
  |> Jsont.Object.opt_mem "finishedAt" Jsont.string ~enc:(fun r -> r.finished_at)
  |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
  |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.opt_mem "startedAt" Jsont.string ~enc:(fun r -> r.started_at)
  |> Jsont.Object.mem "status" Jsont.string ~enc:(fun r -> r.status)
  |> Jsont.Object.finish

type pipeline = {
  commit : string;
  created_at : string option;
  id : string;
  repo : string;
  source_repo : string option;
  trigger : Jsont.json;
  workflows : Jsont.json list;
}

let pipeline_jsont =
  Jsont.Object.map ~kind:"Pipeline"
    (fun _typ commit created_at id repo source_repo trigger workflows -> { commit; created_at; id; repo; source_repo; trigger; workflows })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.ci.defs#pipeline") ~enc:(fun _ -> "org.tangled.ci.defs#pipeline")
  |> Jsont.Object.mem "commit" Jsont.string ~enc:(fun r -> r.commit)
  |> Jsont.Object.opt_mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.opt_mem "sourceRepo" Jsont.string ~enc:(fun r -> r.source_repo)
  |> Jsont.Object.mem "trigger" Jsont.json ~enc:(fun r -> r.trigger)
  |> Jsont.Object.mem "workflows" (Jsont.list Jsont.json) ~enc:(fun r -> r.workflows)
  |> Jsont.Object.finish

      end
      module QueryPipelines = struct
type params = {
  commits : string list option;
  cursor : string option;
  limit : int option;
  repo : string;
  triggers : string list option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun commits cursor limit repo triggers -> {
      commits;
      cursor;
      limit;
      repo;
      triggers;
    })
  |> Jsont.Object.opt_mem "commits" (Jsont.list Jsont.string)
       ~enc:(fun r -> r.commits)
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.opt_mem "triggers" (Jsont.list Jsont.string)
       ~enc:(fun r -> r.triggers)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  pipelines : Jsont.json list;
  total : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor pipelines total -> { cursor; pipelines; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.ci.queryPipelines#output") ~enc:(fun _ -> "org.tangled.ci.queryPipelines#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "pipelines" (Jsont.list Jsont.json) ~enc:(fun r -> r.pipelines)
  |> Jsont.Object.mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module ListWorkflowDefinitions = struct
type params = {
  ref_ : string option;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun ref_ repo -> {
      ref_;
      repo;
    })
  |> Jsont.Object.opt_mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  workflows : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ workflows -> { workflows })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.ci.listWorkflowDefinitions#output") ~enc:(fun _ -> "org.tangled.ci.listWorkflowDefinitions#output")
  |> Jsont.Object.mem "workflows" (Jsont.list Jsont.json) ~enc:(fun r -> r.workflows)
  |> Jsont.Object.finish

      end
      module GetWorkflowDefinition = struct
type params = {
  repo : string;
  workflow : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun repo workflow -> {
      repo;
      workflow;
    })
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "workflow" Jsont.string
       ~enc:(fun r -> r.workflow)
  |> Jsont.Object.finish

type output = {
  workflow : Jsont.json;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ workflow -> { workflow })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.ci.getWorkflowDefinition#output") ~enc:(fun _ -> "org.tangled.ci.getWorkflowDefinition#output")
  |> Jsont.Object.mem "workflow" Jsont.json ~enc:(fun r -> r.workflow)
  |> Jsont.Object.finish

      end
      module GetPipeline = struct
type params = {
  pipeline : string;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun pipeline repo -> {
      pipeline;
      repo;
    })
  |> Jsont.Object.mem "pipeline" Jsont.string
       ~enc:(fun r -> r.pipeline)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = Jsont.json

let output_jsont = Jsont.json

      end
    end
    module Bobbin = struct
      module AwaitRecord = struct
type params = {
  cid : string option;
  uri : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cid uri -> {
      cid;
      uri;
    })
  |> Jsont.Object.opt_mem "cid" Jsont.string
       ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string
       ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type output = {
  cid : string option;
  detail : string option;
  reason : string option;
  status : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cid detail reason status -> { cid; detail; reason; status })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.bobbin.awaitRecord#output") ~enc:(fun _ -> "org.tangled.bobbin.awaitRecord#output")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.opt_mem "detail" Jsont.string ~enc:(fun r -> r.detail)
  |> Jsont.Object.opt_mem "reason" Jsont.string ~enc:(fun r -> r.reason)
  |> Jsont.Object.mem "status" Jsont.string ~enc:(fun r -> r.status)
  |> Jsont.Object.finish

      end
    end
    module Actor = struct
      module SearchActorsTypeahead = struct
type actor = {
  did : string;
  handle : string;
  uri : string;
}

let actor_jsont =
  Jsont.Object.map ~kind:"Actor"
    (fun _typ did handle uri -> { did; handle; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.actor.searchActorsTypeahead#actor") ~enc:(fun _ -> "org.tangled.actor.searchActorsTypeahead#actor")
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.mem "handle" Jsont.string ~enc:(fun r -> r.handle)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type params = {
  limit : int option;
  q : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun limit q -> {
      limit;
      q;
    })
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "q" Jsont.string
       ~enc:(fun r -> r.q)
  |> Jsont.Object.finish

type output = {
  actors : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ actors -> { actors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.actor.searchActorsTypeahead#output") ~enc:(fun _ -> "org.tangled.actor.searchActorsTypeahead#output")
  |> Jsont.Object.mem "actors" (Jsont.list Jsont.json) ~enc:(fun r -> r.actors)
  |> Jsont.Object.finish

      end
      module Profile = struct
type main = {
  avatar : Atp.Blob_ref.t option;
  description : string option;
  is_organization : bool option;
  labels : Jsont.json option;
  links : string list option;
  location : string option;
  pinned_repositories : string list option;
  preferred_handle : string option;
  pronouns : string option;
  stats : string list option;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ avatar description is_organization labels links location pinned_repositories preferred_handle pronouns stats -> { avatar; description; is_organization; labels; links; location; pinned_repositories; preferred_handle; pronouns; stats })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.actor.profile") ~enc:(fun _ -> "org.tangled.actor.profile")
  |> Jsont.Object.opt_mem "avatar" Atp.Blob_ref.jsont ~enc:(fun r -> r.avatar)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
  |> Jsont.Object.opt_mem "isOrganization" Jsont.bool ~enc:(fun r -> r.is_organization)
  |> Jsont.Object.opt_mem "labels" Jsont.json ~enc:(fun r -> r.labels)
  |> Jsont.Object.opt_mem "links" (Jsont.list Jsont.string) ~enc:(fun r -> r.links)
  |> Jsont.Object.opt_mem "location" Jsont.string ~enc:(fun r -> r.location)
  |> Jsont.Object.opt_mem "pinnedRepositories" (Jsont.list Jsont.string) ~enc:(fun r -> r.pinned_repositories)
  |> Jsont.Object.opt_mem "preferredHandle" Jsont.string ~enc:(fun r -> r.preferred_handle)
  |> Jsont.Object.opt_mem "pronouns" Jsont.string ~enc:(fun r -> r.pronouns)
  |> Jsont.Object.opt_mem "stats" (Jsont.list Jsont.string) ~enc:(fun r -> r.stats)
  |> Jsont.Object.finish

      end
      module GetProfiles = struct
type record_view = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let record_view_jsont =
  Jsont.Object.map ~kind:"Record_view"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.actor.getProfiles#recordView") ~enc:(fun _ -> "org.tangled.actor.getProfiles#recordView")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  actors : string list;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun actors -> {
      actors;
    })
  |> Jsont.Object.mem "actors" (Jsont.list Jsont.string)
       ~enc:(fun r -> r.actors)
  |> Jsont.Object.finish

type output = {
  items : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ items -> { items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.actor.getProfiles#output") ~enc:(fun _ -> "org.tangled.actor.getProfiles#output")
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

      end
      module GetProfileActivity = struct
type commit_activity = {
  committed_at : string;
  sha : string;
  subject : string;
}

let commit_activity_jsont =
  Jsont.Object.map ~kind:"Commit_activity"
    (fun _typ committed_at sha subject -> { committed_at; sha; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.actor.getProfileActivity#commitActivity") ~enc:(fun _ -> "org.tangled.actor.getProfileActivity#commitActivity")
  |> Jsont.Object.mem "committedAt" Jsont.string ~enc:(fun r -> r.committed_at)
  |> Jsont.Object.mem "sha" Jsont.string ~enc:(fun r -> r.sha)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type repo_ref = {
  did : string option;
  name : string;
  owner_did : string;
  owner_handle : string;
  uri : string;
}

let repo_ref_jsont =
  Jsont.Object.map ~kind:"Repo_ref"
    (fun _typ did name owner_did owner_handle uri -> { did; name; owner_did; owner_handle; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.actor.getProfileActivity#repoRef") ~enc:(fun _ -> "org.tangled.actor.getProfileActivity#repoRef")
  |> Jsont.Object.opt_mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "ownerDid" Jsont.string ~enc:(fun r -> r.owner_did)
  |> Jsont.Object.mem "ownerHandle" Jsont.string ~enc:(fun r -> r.owner_handle)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type commit_repo_activity = {
  commits : int;
  recent : Jsont.json list;
  repo : Jsont.json;
}

let commit_repo_activity_jsont =
  Jsont.Object.map ~kind:"Commit_repo_activity"
    (fun _typ commits recent repo -> { commits; recent; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.actor.getProfileActivity#commitRepoActivity") ~enc:(fun _ -> "org.tangled.actor.getProfileActivity#commitRepoActivity")
  |> Jsont.Object.mem "commits" Jsont.int ~enc:(fun r -> r.commits)
  |> Jsont.Object.mem "recent" (Jsont.list Jsont.json) ~enc:(fun r -> r.recent)
  |> Jsont.Object.mem "repo" Jsont.json ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type issue_activity = {
  repo : Jsont.json;
  state : string;
  title : string;
  uri : string;
}

let issue_activity_jsont =
  Jsont.Object.map ~kind:"Issue_activity"
    (fun _typ repo state title uri -> { repo; state; title; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.actor.getProfileActivity#issueActivity") ~enc:(fun _ -> "org.tangled.actor.getProfileActivity#issueActivity")
  |> Jsont.Object.mem "repo" Jsont.json ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "state" Jsont.string ~enc:(fun r -> r.state)
  |> Jsont.Object.mem "title" Jsont.string ~enc:(fun r -> r.title)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type pull_activity = {
  repo : Jsont.json;
  state : string;
  title : string;
  uri : string;
}

let pull_activity_jsont =
  Jsont.Object.map ~kind:"Pull_activity"
    (fun _typ repo state title uri -> { repo; state; title; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.actor.getProfileActivity#pullActivity") ~enc:(fun _ -> "org.tangled.actor.getProfileActivity#pullActivity")
  |> Jsont.Object.mem "repo" Jsont.json ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "state" Jsont.string ~enc:(fun r -> r.state)
  |> Jsont.Object.mem "title" Jsont.string ~enc:(fun r -> r.title)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type repo_activity = {
  description : string option;
  kind : string;
  repo : Jsont.json;
}

let repo_activity_jsont =
  Jsont.Object.map ~kind:"Repo_activity"
    (fun _typ description kind repo -> { description; kind; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.actor.getProfileActivity#repoActivity") ~enc:(fun _ -> "org.tangled.actor.getProfileActivity#repoActivity")
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
  |> Jsont.Object.mem "kind" Jsont.string ~enc:(fun r -> r.kind)
  |> Jsont.Object.mem "repo" Jsont.json ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type activity_month = {
  commit_repos : Jsont.json list option;
  commits : int;
  issues : Jsont.json list;
  pulls : Jsont.json list;
  repos : Jsont.json list;
  start : string;
}

let activity_month_jsont =
  Jsont.Object.map ~kind:"Activity_month"
    (fun _typ commit_repos commits issues pulls repos start -> { commit_repos; commits; issues; pulls; repos; start })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.actor.getProfileActivity#activityMonth") ~enc:(fun _ -> "org.tangled.actor.getProfileActivity#activityMonth")
  |> Jsont.Object.opt_mem "commitRepos" (Jsont.list Jsont.json) ~enc:(fun r -> r.commit_repos)
  |> Jsont.Object.mem "commits" Jsont.int ~enc:(fun r -> r.commits)
  |> Jsont.Object.mem "issues" (Jsont.list Jsont.json) ~enc:(fun r -> r.issues)
  |> Jsont.Object.mem "pulls" (Jsont.list Jsont.json) ~enc:(fun r -> r.pulls)
  |> Jsont.Object.mem "repos" (Jsont.list Jsont.json) ~enc:(fun r -> r.repos)
  |> Jsont.Object.mem "start" Jsont.string ~enc:(fun r -> r.start)
  |> Jsont.Object.finish

type params = {
  actor : string;
  months : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun actor months -> {
      actor;
      months;
    })
  |> Jsont.Object.mem "actor" Jsont.string
       ~enc:(fun r -> r.actor)
  |> Jsont.Object.opt_mem "months" Jsont.int
       ~enc:(fun r -> r.months)
  |> Jsont.Object.finish

type output = {
  months : Jsont.json list;
  truncated : bool;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ months truncated -> { months; truncated })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.actor.getProfileActivity#output") ~enc:(fun _ -> "org.tangled.actor.getProfileActivity#output")
  |> Jsont.Object.mem "months" (Jsont.list Jsont.json) ~enc:(fun r -> r.months)
  |> Jsont.Object.mem "truncated" Jsont.bool ~enc:(fun r -> r.truncated)
  |> Jsont.Object.finish

      end
      module GetProfile = struct
type params = {
  actor : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun actor -> {
      actor;
    })
  |> Jsont.Object.mem "actor" Jsont.string
       ~enc:(fun r -> r.actor)
  |> Jsont.Object.finish

type output = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.actor.getProfile#output") ~enc:(fun _ -> "org.tangled.actor.getProfile#output")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

      end
      module Defs = struct
type viewer_state = {
  followed_by : string option;
  following : string option;
}

let viewer_state_jsont =
  Jsont.Object.map ~kind:"Viewer_state"
    (fun _typ followed_by following -> { followed_by; following })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.actor.defs#viewerState") ~enc:(fun _ -> "org.tangled.actor.defs#viewerState")
  |> Jsont.Object.opt_mem "followedBy" Jsont.string ~enc:(fun r -> r.followed_by)
  |> Jsont.Object.opt_mem "following" Jsont.string ~enc:(fun r -> r.following)
  |> Jsont.Object.finish

type profile_view_basic = {
  avatar : string option;
  avatar_cid : string option;
  did : string;
  handle : string;
  viewer : Jsont.json option;
}

let profile_view_basic_jsont =
  Jsont.Object.map ~kind:"Profile_view_basic"
    (fun _typ avatar avatar_cid did handle viewer -> { avatar; avatar_cid; did; handle; viewer })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.actor.defs#profileViewBasic") ~enc:(fun _ -> "org.tangled.actor.defs#profileViewBasic")
  |> Jsont.Object.opt_mem "avatar" Jsont.string ~enc:(fun r -> r.avatar)
  |> Jsont.Object.opt_mem "avatarCid" Jsont.string ~enc:(fun r -> r.avatar_cid)
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.mem "handle" Jsont.string ~enc:(fun r -> r.handle)
  |> Jsont.Object.opt_mem "viewer" Jsont.json ~enc:(fun r -> r.viewer)
  |> Jsont.Object.finish

type profile_view_detailed = {
  avatar : string option;
  avatar_cid : string option;
  did : string;
  followers_count : int option;
  follows_count : int option;
  handle : string;
  viewer : Jsont.json option;
}

let profile_view_detailed_jsont =
  Jsont.Object.map ~kind:"Profile_view_detailed"
    (fun _typ avatar avatar_cid did followers_count follows_count handle viewer -> { avatar; avatar_cid; did; followers_count; follows_count; handle; viewer })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.actor.defs#profileViewDetailed") ~enc:(fun _ -> "org.tangled.actor.defs#profileViewDetailed")
  |> Jsont.Object.opt_mem "avatar" Jsont.string ~enc:(fun r -> r.avatar)
  |> Jsont.Object.opt_mem "avatarCid" Jsont.string ~enc:(fun r -> r.avatar_cid)
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.opt_mem "followersCount" Jsont.int ~enc:(fun r -> r.followers_count)
  |> Jsont.Object.opt_mem "followsCount" Jsont.int ~enc:(fun r -> r.follows_count)
  |> Jsont.Object.mem "handle" Jsont.string ~enc:(fun r -> r.handle)
  |> Jsont.Object.opt_mem "viewer" Jsont.json ~enc:(fun r -> r.viewer)
  |> Jsont.Object.finish

      end
      module GetTrending = struct
type output = {
  people : Jsont.json list;
  repos : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ people repos -> { people; repos })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.actor.getTrending#output") ~enc:(fun _ -> "org.tangled.actor.getTrending#output")
  |> Jsont.Object.mem "people" (Jsont.list Jsont.json) ~enc:(fun r -> r.people)
  |> Jsont.Object.mem "repos" (Jsont.list Jsont.json) ~enc:(fun r -> r.repos)
  |> Jsont.Object.finish

      end
    end
    module Review = struct
      module Unspecced = struct
        module Defs = struct
type review_state = string
let review_state_jsont = Jsont.string

        end
      end
      module SubmitPatch = struct
type patch = {
  base : Jsont.json;
  head : Jsont.json;
  source_branch : string option;
  source_repo : string;
  ticket : string;
}

let patch_jsont =
  Jsont.Object.map ~kind:"Patch"
    (fun _typ base head source_branch source_repo ticket -> { base; head; source_branch; source_repo; ticket })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.review.submitPatch#patch") ~enc:(fun _ -> "org.tangled.review.submitPatch#patch")
  |> Jsont.Object.mem "base" Jsont.json ~enc:(fun r -> r.base)
  |> Jsont.Object.mem "head" Jsont.json ~enc:(fun r -> r.head)
  |> Jsont.Object.opt_mem "sourceBranch" Jsont.string ~enc:(fun r -> r.source_branch)
  |> Jsont.Object.mem "sourceRepo" Jsont.string ~enc:(fun r -> r.source_repo)
  |> Jsont.Object.mem "ticket" Jsont.string ~enc:(fun r -> r.ticket)
  |> Jsont.Object.finish

type input = {
  patch : Jsont.json;
  repo : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ patch repo -> { patch; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.review.submitPatch#input") ~enc:(fun _ -> "org.tangled.review.submitPatch#input")
  |> Jsont.Object.mem "patch" Jsont.json ~enc:(fun r -> r.patch)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  cid : string;
  uri : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cid uri -> { cid; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.review.submitPatch#output") ~enc:(fun _ -> "org.tangled.review.submitPatch#output")
  |> Jsont.Object.mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

      end
      module Patch = struct
type source = {
  branch : string;
  repo : string;
}

let source_jsont =
  Jsont.Object.map ~kind:"Source"
    (fun _typ branch repo -> { branch; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.review.patch#source") ~enc:(fun _ -> "org.tangled.review.patch#source")
  |> Jsont.Object.mem "branch" Jsont.string ~enc:(fun r -> r.branch)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type main = {
  base : Jsont.json;
  created_at : string;
  head : Jsont.json;
  source : Jsont.json option;
  ticket : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ base created_at head source ticket -> { base; created_at; head; source; ticket })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.review.patch") ~enc:(fun _ -> "org.tangled.review.patch")
  |> Jsont.Object.mem "base" Jsont.json ~enc:(fun r -> r.base)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "head" Jsont.json ~enc:(fun r -> r.head)
  |> Jsont.Object.opt_mem "source" Jsont.json ~enc:(fun r -> r.source)
  |> Jsont.Object.mem "ticket" Jsont.string ~enc:(fun r -> r.ticket)
  |> Jsont.Object.finish

      end
    end
    module Embed = struct
      module Unspecced = struct
        module File = struct
type main = {
  commit : Jsont.json;
  path : string;
  repo : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ commit path repo -> { commit; path; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.embed.unspecced.file") ~enc:(fun _ -> "org.tangled.embed.unspecced.file")
  |> Jsont.Object.mem "commit" Jsont.json ~enc:(fun r -> r.commit)
  |> Jsont.Object.mem "path" Jsont.string ~enc:(fun r -> r.path)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

        end
      end
      module Commit = struct
type main = {
  commit : Jsont.json;
  repo : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ commit repo -> { commit; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.embed.commit") ~enc:(fun _ -> "org.tangled.embed.commit")
  |> Jsont.Object.mem "commit" Jsont.json ~enc:(fun r -> r.commit)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type view = {
  change : string option;
  commit : string;
  repo : string;
}

let view_jsont =
  Jsont.Object.map ~kind:"View"
    (fun _typ change commit repo -> { change; commit; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.embed.commit#view") ~enc:(fun _ -> "org.tangled.embed.commit#view")
  |> Jsont.Object.opt_mem "change" Jsont.string ~enc:(fun r -> r.change)
  |> Jsont.Object.mem "commit" Jsont.string ~enc:(fun r -> r.commit)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

      end
    end
    module Repo = struct
      module Review = struct
        module Unspecced = struct
          module Round = struct
type main = {
  commit : Jsont.json;
  created_at : string;
  labels : Jsont.json option;
  state : Jsont.json;
  subject : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ commit created_at labels state subject -> { commit; created_at; labels; state; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.review.unspecced.round") ~enc:(fun _ -> "org.tangled.repo.review.unspecced.round")
  |> Jsont.Object.mem "commit" Jsont.json ~enc:(fun r -> r.commit)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "labels" Jsont.json ~enc:(fun r -> r.labels)
  |> Jsont.Object.mem "state" Jsont.json ~enc:(fun r -> r.state)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

          end
        end
      end
      module ReserveKey = struct
type input = {
  repo : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ repo -> { repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.reserveKey#input") ~enc:(fun _ -> "org.tangled.repo.reserveKey#input")
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  key : string;
  repo : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ key repo -> { key; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.reserveKey#output") ~enc:(fun _ -> "org.tangled.repo.reserveKey#output")
  |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

      end
      module MintToken = struct
type input = {
  aud : string;
  exp : int;
  lxm : string;
  repo : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ aud exp lxm repo -> { aud; exp; lxm; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.mintToken#input") ~enc:(fun _ -> "org.tangled.repo.mintToken#input")
  |> Jsont.Object.mem "aud" Jsont.string ~enc:(fun r -> r.aud)
  |> Jsont.Object.mem "exp" Jsont.int ~enc:(fun r -> r.exp)
  |> Jsont.Object.mem "lxm" Jsont.string ~enc:(fun r -> r.lxm)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  token : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ token -> { token })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.mintToken#output") ~enc:(fun _ -> "org.tangled.repo.mintToken#output")
  |> Jsont.Object.mem "token" Jsont.string ~enc:(fun r -> r.token)
  |> Jsont.Object.finish

      end
      module ListRepos = struct
type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.listRepos#listItem") ~enc:(fun _ -> "org.tangled.repo.listRepos#listItem")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order subject -> {
      cursor;
      limit;
      offset;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.listRepos#output") ~enc:(fun _ -> "org.tangled.repo.listRepos#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module ListRecordVersions = struct
type version = {
  change_id : string;
  cid : string option;
  created_at : string;
  editor : string;
}

let version_jsont =
  Jsont.Object.map ~kind:"Version"
    (fun _typ change_id cid created_at editor -> { change_id; cid; created_at; editor })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.listRecordVersions#version") ~enc:(fun _ -> "org.tangled.repo.listRecordVersions#version")
  |> Jsont.Object.mem "changeId" Jsont.string ~enc:(fun r -> r.change_id)
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "editor" Jsont.string ~enc:(fun r -> r.editor)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  order : string option;
  uri : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit order uri -> {
      cursor;
      limit;
      order;
      uri;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "uri" Jsont.string
       ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  uri : string;
  versions : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor uri versions -> { cursor; uri; versions })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.listRecordVersions#output") ~enc:(fun _ -> "org.tangled.repo.listRecordVersions#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "versions" (Jsont.list Jsont.json) ~enc:(fun r -> r.versions)
  |> Jsont.Object.finish

      end
      module ListPulls = struct
type pull_list_item = {
  author : string option;
  cid : string option;
  comment_count : int;
  state : string;
  state_updated_at : string option;
  uri : string;
  value : Jsont.json;
}

let pull_list_item_jsont =
  Jsont.Object.map ~kind:"Pull_list_item"
    (fun _typ author cid comment_count state state_updated_at uri value -> { author; cid; comment_count; state; state_updated_at; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.listPulls#pullListItem") ~enc:(fun _ -> "org.tangled.repo.listPulls#pullListItem")
  |> Jsont.Object.opt_mem "author" Jsont.string ~enc:(fun r -> r.author)
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "commentCount" Jsont.int ~enc:(fun r -> r.comment_count)
  |> Jsont.Object.mem "state" Jsont.string ~enc:(fun r -> r.state)
  |> Jsont.Object.opt_mem "stateUpdatedAt" Jsont.string ~enc:(fun r -> r.state_updated_at)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  author : string option;
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  status : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun author cursor limit offset order status subject -> {
      author;
      cursor;
      limit;
      offset;
      order;
      status;
      subject;
    })
  |> Jsont.Object.opt_mem "author" Jsont.string
       ~enc:(fun r -> r.author)
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.opt_mem "status" Jsont.string
       ~enc:(fun r -> r.status)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.listPulls#output") ~enc:(fun _ -> "org.tangled.repo.listPulls#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module ListIssues = struct
type issue_list_item = {
  author : string option;
  cid : string option;
  comment_count : int;
  state : string;
  state_updated_at : string option;
  uri : string;
  value : Jsont.json;
}

let issue_list_item_jsont =
  Jsont.Object.map ~kind:"Issue_list_item"
    (fun _typ author cid comment_count state state_updated_at uri value -> { author; cid; comment_count; state; state_updated_at; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.listIssues#issueListItem") ~enc:(fun _ -> "org.tangled.repo.listIssues#issueListItem")
  |> Jsont.Object.opt_mem "author" Jsont.string ~enc:(fun r -> r.author)
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "commentCount" Jsont.int ~enc:(fun r -> r.comment_count)
  |> Jsont.Object.mem "state" Jsont.string ~enc:(fun r -> r.state)
  |> Jsont.Object.opt_mem "stateUpdatedAt" Jsont.string ~enc:(fun r -> r.state_updated_at)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  author : string option;
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  state : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun author cursor limit offset order state subject -> {
      author;
      cursor;
      limit;
      offset;
      order;
      state;
      subject;
    })
  |> Jsont.Object.opt_mem "author" Jsont.string
       ~enc:(fun r -> r.author)
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.opt_mem "state" Jsont.string
       ~enc:(fun r -> r.state)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.listIssues#output") ~enc:(fun _ -> "org.tangled.repo.listIssues#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module ListForks = struct
type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.listForks#listItem") ~enc:(fun _ -> "org.tangled.repo.listForks#listItem")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order subject -> {
      cursor;
      limit;
      offset;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.listForks#output") ~enc:(fun _ -> "org.tangled.repo.listForks#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module ListCollaborators = struct
type list_item = {
  added_by : string;
  cid : string option;
  created_at : string;
  effective_since : string;
  subject : string;
  uri : string option;
  verified_at : string option;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ added_by cid created_at effective_since subject uri verified_at -> { added_by; cid; created_at; effective_since; subject; uri; verified_at })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.listCollaborators#listItem") ~enc:(fun _ -> "org.tangled.repo.listCollaborators#listItem")
  |> Jsont.Object.mem "addedBy" Jsont.string ~enc:(fun r -> r.added_by)
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "effectiveSince" Jsont.string ~enc:(fun r -> r.effective_since)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.opt_mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.opt_mem "verifiedAt" Jsont.string ~enc:(fun r -> r.verified_at)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order subject -> {
      cursor;
      limit;
      offset;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.listCollaborators#output") ~enc:(fun _ -> "org.tangled.repo.listCollaborators#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module ListCollaboratorInvitesBy = struct
type invite_item = {
  added_by : string;
  created_at : string;
  knot : string;
  repo : string;
  uri : string;
}

let invite_item_jsont =
  Jsont.Object.map ~kind:"Invite_item"
    (fun _typ added_by created_at knot repo uri -> { added_by; created_at; knot; repo; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.listCollaboratorInvitesBy#inviteItem") ~enc:(fun _ -> "org.tangled.repo.listCollaboratorInvitesBy#inviteItem")
  |> Jsont.Object.mem "addedBy" Jsont.string ~enc:(fun r -> r.added_by)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "knot" Jsont.string ~enc:(fun r -> r.knot)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  items : Jsont.json list;
  pending : string list;
  truncated : bool;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ items pending truncated -> { items; pending; truncated })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.listCollaboratorInvitesBy#output") ~enc:(fun _ -> "org.tangled.repo.listCollaboratorInvitesBy#output")
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.mem "pending" (Jsont.list Jsont.string) ~enc:(fun r -> r.pending)
  |> Jsont.Object.mem "truncated" Jsont.bool ~enc:(fun r -> r.truncated)
  |> Jsont.Object.finish

      end
      module ListArtifacts = struct
type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.listArtifacts#listItem") ~enc:(fun _ -> "org.tangled.repo.listArtifacts#listItem")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order subject -> {
      cursor;
      limit;
      offset;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.listArtifacts#output") ~enc:(fun _ -> "org.tangled.repo.listArtifacts#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module GetReposByRepoDids = struct
type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.getReposByRepoDids#listItem") ~enc:(fun _ -> "org.tangled.repo.getReposByRepoDids#listItem")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  dids : string list;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun dids -> {
      dids;
    })
  |> Jsont.Object.mem "dids" (Jsont.list Jsont.string)
       ~enc:(fun r -> r.dids)
  |> Jsont.Object.finish

type output = {
  items : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ items -> { items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.getReposByRepoDids#output") ~enc:(fun _ -> "org.tangled.repo.getReposByRepoDids#output")
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

      end
      module GetRepos = struct
type record_view = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let record_view_jsont =
  Jsont.Object.map ~kind:"Record_view"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.getRepos#recordView") ~enc:(fun _ -> "org.tangled.repo.getRepos#recordView")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  repos : string list;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun repos -> {
      repos;
    })
  |> Jsont.Object.mem "repos" (Jsont.list Jsont.string)
       ~enc:(fun r -> r.repos)
  |> Jsont.Object.finish

type output = {
  items : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ items -> { items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.getRepos#output") ~enc:(fun _ -> "org.tangled.repo.getRepos#output")
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

      end
      module GetRepoByRepoDid = struct
type params = {
  repo_did : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun repo_did -> {
      repo_did;
    })
  |> Jsont.Object.mem "repoDid" Jsont.string
       ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.finish

type output = {
  cid : string option;
  knot : string option;
  uri : string;
  value : Jsont.json;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cid knot uri value -> { cid; knot; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.getRepoByRepoDid#output") ~enc:(fun _ -> "org.tangled.repo.getRepoByRepoDid#output")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.opt_mem "knot" Jsont.string ~enc:(fun r -> r.knot)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

      end
      module GetRepoByName = struct
type params = {
  name : string;
  owner : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun name owner -> {
      name;
      owner;
    })
  |> Jsont.Object.mem "name" Jsont.string
       ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "owner" Jsont.string
       ~enc:(fun r -> r.owner)
  |> Jsont.Object.finish

type output = {
  cid : string option;
  knot : string option;
  uri : string;
  value : Jsont.json;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cid knot uri value -> { cid; knot; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.getRepoByName#output") ~enc:(fun _ -> "org.tangled.repo.getRepoByName#output")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.opt_mem "knot" Jsont.string ~enc:(fun r -> r.knot)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

      end
      module GetRepo = struct
type params = {
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun repo -> {
      repo;
    })
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  cid : string option;
  knot : string option;
  uri : string;
  value : Jsont.json;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cid knot uri value -> { cid; knot; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.getRepo#output") ~enc:(fun _ -> "org.tangled.repo.getRepo#output")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.opt_mem "knot" Jsont.string ~enc:(fun r -> r.knot)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

      end
      module GetImportStatus = struct
type params = {
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun repo -> {
      repo;
    })
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  reason : string option;
  status : string option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ reason status -> { reason; status })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.getImportStatus#output") ~enc:(fun _ -> "org.tangled.repo.getImportStatus#output")
  |> Jsont.Object.opt_mem "reason" Jsont.string ~enc:(fun r -> r.reason)
  |> Jsont.Object.opt_mem "status" Jsont.string ~enc:(fun r -> r.status)
  |> Jsont.Object.finish

      end
      module ForkSync = struct
type input = {
  branch : string;
  repo : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ branch repo -> { branch; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.forkSync#input") ~enc:(fun _ -> "org.tangled.repo.forkSync#input")
  |> Jsont.Object.mem "branch" Jsont.string ~enc:(fun r -> r.branch)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

      end
      module DeleteRepo = struct
type input = {
  force : bool option;
  repo : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ force repo -> { force; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.deleteRepo#input") ~enc:(fun _ -> "org.tangled.repo.deleteRepo#input")
  |> Jsont.Object.opt_mem "force" Jsont.bool ~enc:(fun r -> r.force)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

      end
      module Defs = struct
type language = {
  name : string;
  size : int;
}

let language_jsont =
  Jsont.Object.map ~kind:"Language"
    (fun _typ name size -> { name; size })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.defs#language") ~enc:(fun _ -> "org.tangled.repo.defs#language")
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "size" Jsont.int ~enc:(fun r -> r.size)
  |> Jsont.Object.finish

type source_external = {
  uri : string;
}

let source_external_jsont =
  Jsont.Object.map ~kind:"Source_external"
    (fun _typ uri -> { uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.defs#sourceExternal") ~enc:(fun _ -> "org.tangled.repo.defs#sourceExternal")
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type source_tangled = {
  did : string;
}

let source_tangled_jsont =
  Jsont.Object.map ~kind:"Source_tangled"
    (fun _typ did -> { did })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.defs#sourceTangled") ~enc:(fun _ -> "org.tangled.repo.defs#sourceTangled")
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.finish

type viewer_state = {
  fork : string option;
  star : string option;
}

let viewer_state_jsont =
  Jsont.Object.map ~kind:"Viewer_state"
    (fun _typ fork star -> { fork; star })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.defs#viewerState") ~enc:(fun _ -> "org.tangled.repo.defs#viewerState")
  |> Jsont.Object.opt_mem "fork" Jsont.string ~enc:(fun r -> r.fork)
  |> Jsont.Object.opt_mem "star" Jsont.string ~enc:(fun r -> r.star)
  |> Jsont.Object.finish

type repo_view_basic = {
  created_at : string;
  description : string option;
  did : string;
  fork_count : int option;
  is_starred : bool option;
  open_issue_count : int option;
  open_pull_count : int option;
  owner : Jsont.json;
  primary_language : string option;
  slug : string;
  star_count : int option;
  viewer : Jsont.json option;
}

let repo_view_basic_jsont =
  Jsont.Object.map ~kind:"Repo_view_basic"
    (fun _typ created_at description did fork_count is_starred open_issue_count open_pull_count owner primary_language slug star_count viewer -> { created_at; description; did; fork_count; is_starred; open_issue_count; open_pull_count; owner; primary_language; slug; star_count; viewer })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.defs#repoViewBasic") ~enc:(fun _ -> "org.tangled.repo.defs#repoViewBasic")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.opt_mem "forkCount" Jsont.int ~enc:(fun r -> r.fork_count)
  |> Jsont.Object.opt_mem "isStarred" Jsont.bool ~enc:(fun r -> r.is_starred)
  |> Jsont.Object.opt_mem "openIssueCount" Jsont.int ~enc:(fun r -> r.open_issue_count)
  |> Jsont.Object.opt_mem "openPullCount" Jsont.int ~enc:(fun r -> r.open_pull_count)
  |> Jsont.Object.mem "owner" Jsont.json ~enc:(fun r -> r.owner)
  |> Jsont.Object.opt_mem "primaryLanguage" Jsont.string ~enc:(fun r -> r.primary_language)
  |> Jsont.Object.mem "slug" Jsont.string ~enc:(fun r -> r.slug)
  |> Jsont.Object.opt_mem "starCount" Jsont.int ~enc:(fun r -> r.star_count)
  |> Jsont.Object.opt_mem "viewer" Jsont.json ~enc:(fun r -> r.viewer)
  |> Jsont.Object.finish

      end
      module Declaration = struct
type main = {
  created_at : string;
  repo : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at repo -> { created_at; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.declaration") ~enc:(fun _ -> "org.tangled.repo.declaration")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

      end
      module CountRepos = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.countRepos#output") ~enc:(fun _ -> "org.tangled.repo.countRepos#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module CountPulls = struct
type params = {
  author : string option;
  status : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun author status subject -> {
      author;
      status;
      subject;
    })
  |> Jsont.Object.opt_mem "author" Jsont.string
       ~enc:(fun r -> r.author)
  |> Jsont.Object.opt_mem "status" Jsont.string
       ~enc:(fun r -> r.status)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.countPulls#output") ~enc:(fun _ -> "org.tangled.repo.countPulls#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module CountIssues = struct
type params = {
  author : string option;
  state : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun author state subject -> {
      author;
      state;
      subject;
    })
  |> Jsont.Object.opt_mem "author" Jsont.string
       ~enc:(fun r -> r.author)
  |> Jsont.Object.opt_mem "state" Jsont.string
       ~enc:(fun r -> r.state)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.countIssues#output") ~enc:(fun _ -> "org.tangled.repo.countIssues#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module CountForks = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.countForks#output") ~enc:(fun _ -> "org.tangled.repo.countForks#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module CountCollaboratorsBy = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.countCollaboratorsBy#output") ~enc:(fun _ -> "org.tangled.repo.countCollaboratorsBy#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module CountCollaborators = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.countCollaborators#output") ~enc:(fun _ -> "org.tangled.repo.countCollaborators#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module CountArtifactsBy = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.countArtifactsBy#output") ~enc:(fun _ -> "org.tangled.repo.countArtifactsBy#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module CountArtifacts = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.countArtifacts#output") ~enc:(fun _ -> "org.tangled.repo.countArtifacts#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module Artifact = struct
type main = {
  artifact : Atp.Blob_ref.t;
  created_at : string;
  name : string;
  subject : Jsont.json;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ artifact created_at name subject -> { artifact; created_at; name; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.artifact") ~enc:(fun _ -> "org.tangled.repo.artifact")
  |> Jsont.Object.mem "artifact" Atp.Blob_ref.jsont ~enc:(fun r -> r.artifact)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "subject" Jsont.json ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module Manifest = struct
type declaration = {
  name : string;
  owner : string;
}

let declaration_jsont =
  Jsont.Object.map ~kind:"Declaration"
    (fun _typ name owner -> { name; owner })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.manifest#declaration") ~enc:(fun _ -> "org.tangled.repo.manifest#declaration")
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "owner" Jsont.string ~enc:(fun r -> r.owner)
  |> Jsont.Object.finish

type main = {
  contribution_policy : string option;
  created_at : string;
  declaration : Jsont.json;
  description : string option;
  source : Jsont.json option;
  spindle : string option;
  topics : string list option;
  website : string option;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ contribution_policy created_at declaration description source spindle topics website -> { contribution_policy; created_at; declaration; description; source; spindle; topics; website })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.manifest") ~enc:(fun _ -> "org.tangled.repo.manifest")
  |> Jsont.Object.opt_mem "contributionPolicy" Jsont.string ~enc:(fun r -> r.contribution_policy)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "declaration" Jsont.json ~enc:(fun r -> r.declaration)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
  |> Jsont.Object.opt_mem "source" Jsont.json ~enc:(fun r -> r.source)
  |> Jsont.Object.opt_mem "spindle" Jsont.string ~enc:(fun r -> r.spindle)
  |> Jsont.Object.opt_mem "topics" (Jsont.list Jsont.string) ~enc:(fun r -> r.topics)
  |> Jsont.Object.opt_mem "website" Jsont.string ~enc:(fun r -> r.website)
  |> Jsont.Object.finish

      end
      module ListPullsBy = struct
type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  status : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order status subject -> {
      cursor;
      limit;
      offset;
      order;
      status;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.opt_mem "status" Jsont.string
       ~enc:(fun r -> r.status)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.listPullsBy#output") ~enc:(fun _ -> "org.tangled.repo.listPullsBy#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module ListIssuesBy = struct
type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  state : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order state subject -> {
      cursor;
      limit;
      offset;
      order;
      state;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.opt_mem "state" Jsont.string
       ~enc:(fun r -> r.state)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.listIssuesBy#output") ~enc:(fun _ -> "org.tangled.repo.listIssuesBy#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module ListCollaboratorsBy = struct
type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order subject -> {
      cursor;
      limit;
      offset;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.listCollaboratorsBy#output") ~enc:(fun _ -> "org.tangled.repo.listCollaboratorsBy#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module ListArtifactsBy = struct
type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order subject -> {
      cursor;
      limit;
      offset;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.listArtifactsBy#output") ~enc:(fun _ -> "org.tangled.repo.listArtifactsBy#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module CreateRepo = struct
type input = {
  created_at : string option;
  default_branch : string option;
  description : string option;
  did : string option;
  name : string;
  source : Jsont.json option;
  spindle : string option;
  topics : string list option;
  website : string option;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ created_at default_branch description did name source spindle topics website -> { created_at; default_branch; description; did; name; source; spindle; topics; website })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.createRepo#input") ~enc:(fun _ -> "org.tangled.repo.createRepo#input")
  |> Jsont.Object.opt_mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "defaultBranch" Jsont.string ~enc:(fun r -> r.default_branch)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
  |> Jsont.Object.opt_mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.opt_mem "source" Jsont.json ~enc:(fun r -> r.source)
  |> Jsont.Object.opt_mem "spindle" Jsont.string ~enc:(fun r -> r.spindle)
  |> Jsont.Object.opt_mem "topics" (Jsont.list Jsont.string) ~enc:(fun r -> r.topics)
  |> Jsont.Object.opt_mem "website" Jsont.string ~enc:(fun r -> r.website)
  |> Jsont.Object.finish

type output = {
  did : string option;
  key : string option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ did key -> { did; key })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.repo.createRepo#output") ~enc:(fun _ -> "org.tangled.repo.createRepo#output")
  |> Jsont.Object.opt_mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.opt_mem "key" Jsont.string ~enc:(fun r -> r.key)
  |> Jsont.Object.finish

      end
    end
    module Track = struct
      module Unspecced = struct
        module Milestone = struct
type main = unit

let main_jsont = Jsont.ignore

        end
      end
      module Defs = struct
(* Enum: open, closed, completed *)
type ticket_state = string
let ticket_state_jsont = Jsont.string

type ticket_view_detailed = {
  author : Jsont.json;
  body : Jsont.json option;
  cid : string;
  created_at : string;
  repo : Jsont.json;
  state : Jsont.json;
  tangled_labels : Jsont.json list option;
  target_branch : string option;
  title : Jsont.json;
  uri : string;
}

let ticket_view_detailed_jsont =
  Jsont.Object.map ~kind:"Ticket_view_detailed"
    (fun _typ author body cid created_at repo state tangled_labels target_branch title uri -> { author; body; cid; created_at; repo; state; tangled_labels; target_branch; title; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.track.defs#ticketViewDetailed") ~enc:(fun _ -> "org.tangled.track.defs#ticketViewDetailed")
  |> Jsont.Object.mem "author" Jsont.json ~enc:(fun r -> r.author)
  |> Jsont.Object.opt_mem "body" Jsont.json ~enc:(fun r -> r.body)
  |> Jsont.Object.mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "repo" Jsont.json ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "state" Jsont.json ~enc:(fun r -> r.state)
  |> Jsont.Object.opt_mem "tangledLabels" (Jsont.list Jsont.json) ~enc:(fun r -> r.tangled_labels)
  |> Jsont.Object.opt_mem "targetBranch" Jsont.string ~enc:(fun r -> r.target_branch)
  |> Jsont.Object.mem "title" Jsont.json ~enc:(fun r -> r.title)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

      end
      module Ticket = struct
type main = {
  assignees : string list option;
  body : Jsont.json option;
  created_at : string;
  labels : Jsont.json option;
  state : Jsont.json;
  tangled_labels : Jsont.json list option;
  target_branch : string option;
  title : Jsont.json;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ assignees body created_at labels state tangled_labels target_branch title -> { assignees; body; created_at; labels; state; tangled_labels; target_branch; title })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.track.ticket") ~enc:(fun _ -> "org.tangled.track.ticket")
  |> Jsont.Object.opt_mem "assignees" (Jsont.list Jsont.string) ~enc:(fun r -> r.assignees)
  |> Jsont.Object.opt_mem "body" Jsont.json ~enc:(fun r -> r.body)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "labels" Jsont.json ~enc:(fun r -> r.labels)
  |> Jsont.Object.mem "state" Jsont.json ~enc:(fun r -> r.state)
  |> Jsont.Object.opt_mem "tangledLabels" (Jsont.list Jsont.json) ~enc:(fun r -> r.tangled_labels)
  |> Jsont.Object.opt_mem "targetBranch" Jsont.string ~enc:(fun r -> r.target_branch)
  |> Jsont.Object.mem "title" Jsont.json ~enc:(fun r -> r.title)
  |> Jsont.Object.finish

      end
      module GetTicketView = struct
type params = {
  uri : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun uri -> {
      uri;
    })
  |> Jsont.Object.mem "uri" Jsont.string
       ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type output = Jsont.json

let output_jsont = Jsont.json

      end
    end
    module Temp = struct
      module Webhook = struct
        module UpdateWebhook = struct
type input = {
  active : bool option;
  events : string list option;
  id : int;
  repo_did : string;
  secret : string option;
  url : string option;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ active events id repo_did secret url -> { active; events; id; repo_did; secret; url })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.webhook.updateWebhook#input") ~enc:(fun _ -> "org.tangled.temp.webhook.updateWebhook#input")
  |> Jsont.Object.opt_mem "active" Jsont.bool ~enc:(fun r -> r.active)
  |> Jsont.Object.opt_mem "events" (Jsont.list Jsont.string) ~enc:(fun r -> r.events)
  |> Jsont.Object.mem "id" Jsont.int ~enc:(fun r -> r.id)
  |> Jsont.Object.mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.opt_mem "secret" Jsont.string ~enc:(fun r -> r.secret)
  |> Jsont.Object.opt_mem "url" Jsont.string ~enc:(fun r -> r.url)
  |> Jsont.Object.finish

        end
        module ToggleWebhook = struct
type input = {
  id : int;
  repo_did : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ id repo_did -> { id; repo_did })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.webhook.toggleWebhook#input") ~enc:(fun _ -> "org.tangled.temp.webhook.toggleWebhook#input")
  |> Jsont.Object.mem "id" Jsont.int ~enc:(fun r -> r.id)
  |> Jsont.Object.mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.finish

type output = {
  active : bool;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ active -> { active })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.webhook.toggleWebhook#output") ~enc:(fun _ -> "org.tangled.temp.webhook.toggleWebhook#output")
  |> Jsont.Object.mem "active" Jsont.bool ~enc:(fun r -> r.active)
  |> Jsont.Object.finish

        end
        module RetryDelivery = struct
type input = {
  delivery_id : string;
  repo_did : string;
  webhook_id : int;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ delivery_id repo_did webhook_id -> { delivery_id; repo_did; webhook_id })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.webhook.retryDelivery#input") ~enc:(fun _ -> "org.tangled.temp.webhook.retryDelivery#input")
  |> Jsont.Object.mem "deliveryId" Jsont.string ~enc:(fun r -> r.delivery_id)
  |> Jsont.Object.mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.mem "webhookId" Jsont.int ~enc:(fun r -> r.webhook_id)
  |> Jsont.Object.finish

        end
        module ListWebhooks = struct
type webhook = {
  active : bool;
  created_at : string;
  events : string list;
  id : int;
  updated_at : string option;
  url : string;
}

let webhook_jsont =
  Jsont.Object.map ~kind:"Webhook"
    (fun _typ active created_at events id updated_at url -> { active; created_at; events; id; updated_at; url })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.webhook.listWebhooks#webhook") ~enc:(fun _ -> "org.tangled.temp.webhook.listWebhooks#webhook")
  |> Jsont.Object.mem "active" Jsont.bool ~enc:(fun r -> r.active)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "events" (Jsont.list Jsont.string) ~enc:(fun r -> r.events)
  |> Jsont.Object.mem "id" Jsont.int ~enc:(fun r -> r.id)
  |> Jsont.Object.opt_mem "updatedAt" Jsont.string ~enc:(fun r -> r.updated_at)
  |> Jsont.Object.mem "url" Jsont.string ~enc:(fun r -> r.url)
  |> Jsont.Object.finish

type params = {
  repo_did : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun repo_did -> {
      repo_did;
    })
  |> Jsont.Object.mem "repoDid" Jsont.string
       ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.finish

type output = {
  webhooks : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ webhooks -> { webhooks })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.webhook.listWebhooks#output") ~enc:(fun _ -> "org.tangled.temp.webhook.listWebhooks#output")
  |> Jsont.Object.mem "webhooks" (Jsont.list Jsont.json) ~enc:(fun r -> r.webhooks)
  |> Jsont.Object.finish

        end
        module GetDeliveriesForWebhooks = struct
type delivery = {
  created_at : string;
  delivery_id : string;
  event : string;
  id : int;
  request_body : string option;
  response_body : string option;
  response_code : int option;
  success : bool;
  url : string;
  webhook_id : int;
}

let delivery_jsont =
  Jsont.Object.map ~kind:"Delivery"
    (fun _typ created_at delivery_id event id request_body response_body response_code success url webhook_id -> { created_at; delivery_id; event; id; request_body; response_body; response_code; success; url; webhook_id })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.webhook.getDeliveriesForWebhooks#delivery") ~enc:(fun _ -> "org.tangled.temp.webhook.getDeliveriesForWebhooks#delivery")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "deliveryId" Jsont.string ~enc:(fun r -> r.delivery_id)
  |> Jsont.Object.mem "event" Jsont.string ~enc:(fun r -> r.event)
  |> Jsont.Object.mem "id" Jsont.int ~enc:(fun r -> r.id)
  |> Jsont.Object.opt_mem "requestBody" Jsont.string ~enc:(fun r -> r.request_body)
  |> Jsont.Object.opt_mem "responseBody" Jsont.string ~enc:(fun r -> r.response_body)
  |> Jsont.Object.opt_mem "responseCode" Jsont.int ~enc:(fun r -> r.response_code)
  |> Jsont.Object.mem "success" Jsont.bool ~enc:(fun r -> r.success)
  |> Jsont.Object.mem "url" Jsont.string ~enc:(fun r -> r.url)
  |> Jsont.Object.mem "webhookId" Jsont.int ~enc:(fun r -> r.webhook_id)
  |> Jsont.Object.finish

type params = {
  ids : int list;
  limit : int option;
  repo_did : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun ids limit repo_did -> {
      ids;
      limit;
      repo_did;
    })
  |> Jsont.Object.mem "ids" (Jsont.list Jsont.int)
       ~enc:(fun r -> r.ids)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "repoDid" Jsont.string
       ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.finish

type output = {
  deliveries : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ deliveries -> { deliveries })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.webhook.getDeliveriesForWebhooks#output") ~enc:(fun _ -> "org.tangled.temp.webhook.getDeliveriesForWebhooks#output")
  |> Jsont.Object.mem "deliveries" (Jsont.list Jsont.json) ~enc:(fun r -> r.deliveries)
  |> Jsont.Object.finish

        end
        module DeleteWebhook = struct
type input = {
  id : int;
  repo_did : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ id repo_did -> { id; repo_did })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.webhook.deleteWebhook#input") ~enc:(fun _ -> "org.tangled.temp.webhook.deleteWebhook#input")
  |> Jsont.Object.mem "id" Jsont.int ~enc:(fun r -> r.id)
  |> Jsont.Object.mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.finish

        end
        module CreateWebhook = struct
type input = {
  active : bool option;
  events : string list;
  repo_did : string;
  secret : string option;
  url : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ active events repo_did secret url -> { active; events; repo_did; secret; url })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.webhook.createWebhook#input") ~enc:(fun _ -> "org.tangled.temp.webhook.createWebhook#input")
  |> Jsont.Object.opt_mem "active" Jsont.bool ~enc:(fun r -> r.active)
  |> Jsont.Object.mem "events" (Jsont.list Jsont.string) ~enc:(fun r -> r.events)
  |> Jsont.Object.mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.opt_mem "secret" Jsont.string ~enc:(fun r -> r.secret)
  |> Jsont.Object.mem "url" Jsont.string ~enc:(fun r -> r.url)
  |> Jsont.Object.finish

type output = {
  id : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ id -> { id })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.webhook.createWebhook#output") ~enc:(fun _ -> "org.tangled.temp.webhook.createWebhook#output")
  |> Jsont.Object.mem "id" Jsont.int ~enc:(fun r -> r.id)
  |> Jsont.Object.finish

        end
      end
      module Spindle = struct
        module Quota = struct
          module Unset = struct
type input = {
  did : string;
  resource : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ did resource -> { did; resource })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.spindle.quota.unset#input") ~enc:(fun _ -> "org.tangled.temp.spindle.quota.unset#input")
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.mem "resource" Jsont.string ~enc:(fun r -> r.resource)
  |> Jsont.Object.finish

          end
          module Set = struct
type input = {
  did : string;
  limit : int option;
  resource : string;
  unlimited : bool option;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ did limit resource unlimited -> { did; limit; resource; unlimited })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.spindle.quota.set#input") ~enc:(fun _ -> "org.tangled.temp.spindle.quota.set#input")
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.opt_mem "limit" Jsont.int ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "resource" Jsont.string ~enc:(fun r -> r.resource)
  |> Jsont.Object.opt_mem "unlimited" Jsont.bool ~enc:(fun r -> r.unlimited)
  |> Jsont.Object.finish

          end
          module ReleaseNars = struct
type input = {
  nars : string list;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ nars -> { nars })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.spindle.quota.releaseNars#input") ~enc:(fun _ -> "org.tangled.temp.spindle.quota.releaseNars#input")
  |> Jsont.Object.mem "nars" (Jsont.list Jsont.string) ~enc:(fun r -> r.nars)
  |> Jsont.Object.finish

type output = {
  bytes_ : int;
  released : int;
  unmatched : string list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ bytes_ released unmatched -> { bytes_; released; unmatched })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.spindle.quota.releaseNars#output") ~enc:(fun _ -> "org.tangled.temp.spindle.quota.releaseNars#output")
  |> Jsont.Object.mem "bytes" Jsont.int ~enc:(fun r -> r.bytes_)
  |> Jsont.Object.mem "released" Jsont.int ~enc:(fun r -> r.released)
  |> Jsont.Object.mem "unmatched" (Jsont.list Jsont.string) ~enc:(fun r -> r.unmatched)
  |> Jsont.Object.finish

          end
          module Defs = struct
type limit = {
  did : string;
  limit : int;
  resource : string;
}

let limit_jsont =
  Jsont.Object.map ~kind:"Limit"
    (fun _typ did limit resource -> { did; limit; resource })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.spindle.quota.defs#limit") ~enc:(fun _ -> "org.tangled.temp.spindle.quota.defs#limit")
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.mem "limit" Jsont.int ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "resource" Jsont.string ~enc:(fun r -> r.resource)
  |> Jsont.Object.finish

type usage = {
  did : string;
  resource : string;
  scope : string;
  used : int;
}

let usage_jsont =
  Jsont.Object.map ~kind:"Usage"
    (fun _typ did resource scope used -> { did; resource; scope; used })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.spindle.quota.defs#usage") ~enc:(fun _ -> "org.tangled.temp.spindle.quota.defs#usage")
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.mem "resource" Jsont.string ~enc:(fun r -> r.resource)
  |> Jsont.Object.mem "scope" Jsont.string ~enc:(fun r -> r.scope)
  |> Jsont.Object.mem "used" Jsont.int ~enc:(fun r -> r.used)
  |> Jsont.Object.finish

          end
          module Usage = struct
type params = {
  did : string option;
  scope : string option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun did scope -> {
      did;
      scope;
    })
  |> Jsont.Object.opt_mem "did" Jsont.string
       ~enc:(fun r -> r.did)
  |> Jsont.Object.opt_mem "scope" Jsont.string
       ~enc:(fun r -> r.scope)
  |> Jsont.Object.finish

type output = {
  usages : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ usages -> { usages })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.spindle.quota.usage#output") ~enc:(fun _ -> "org.tangled.temp.spindle.quota.usage#output")
  |> Jsont.Object.mem "usages" (Jsont.list Jsont.json) ~enc:(fun r -> r.usages)
  |> Jsont.Object.finish

          end
          module List = struct
type output = {
  limits : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ limits -> { limits })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.spindle.quota.list#output") ~enc:(fun _ -> "org.tangled.temp.spindle.quota.list#output")
  |> Jsont.Object.mem "limits" (Jsont.list Jsont.json) ~enc:(fun r -> r.limits)
  |> Jsont.Object.finish

          end
          module Get = struct
type params = {
  did : string;
  resource : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun did resource -> {
      did;
      resource;
    })
  |> Jsont.Object.mem "did" Jsont.string
       ~enc:(fun r -> r.did)
  |> Jsont.Object.mem "resource" Jsont.string
       ~enc:(fun r -> r.resource)
  |> Jsont.Object.finish

type output = {
  limit : Jsont.json;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ limit -> { limit })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.spindle.quota.get#output") ~enc:(fun _ -> "org.tangled.temp.spindle.quota.get#output")
  |> Jsont.Object.mem "limit" Jsont.json ~enc:(fun r -> r.limit)
  |> Jsont.Object.finish

          end
        end
        module Moderation = struct
          module Unban = struct
type input = {
  did : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ did -> { did })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.spindle.moderation.unban#input") ~enc:(fun _ -> "org.tangled.temp.spindle.moderation.unban#input")
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.finish

          end
          module Defs = struct
type ban = {
  created_at : string;
  did : string;
}

let ban_jsont =
  Jsont.Object.map ~kind:"Ban"
    (fun _typ created_at did -> { created_at; did })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.spindle.moderation.defs#ban") ~enc:(fun _ -> "org.tangled.temp.spindle.moderation.defs#ban")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.finish

          end
          module Ban = struct
type input = {
  did : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ did -> { did })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.spindle.moderation.ban#input") ~enc:(fun _ -> "org.tangled.temp.spindle.moderation.ban#input")
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.finish

          end
          module ListBans = struct
type output = {
  bans : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ bans -> { bans })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.spindle.moderation.listBans#output") ~enc:(fun _ -> "org.tangled.temp.spindle.moderation.listBans#output")
  |> Jsont.Object.mem "bans" (Jsont.list Jsont.json) ~enc:(fun r -> r.bans)
  |> Jsont.Object.finish

          end
          module GetBan = struct
type params = {
  did : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun did -> {
      did;
    })
  |> Jsont.Object.mem "did" Jsont.string
       ~enc:(fun r -> r.did)
  |> Jsont.Object.finish

type output = {
  ban : Jsont.json;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ ban -> { ban })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.spindle.moderation.getBan#output") ~enc:(fun _ -> "org.tangled.temp.spindle.moderation.getBan#output")
  |> Jsont.Object.mem "ban" Jsont.json ~enc:(fun r -> r.ban)
  |> Jsont.Object.finish

          end
        end
        module AllowList = struct
          module Remove = struct
type input = {
  did : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ did -> { did })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.spindle.allowList.remove#input") ~enc:(fun _ -> "org.tangled.temp.spindle.allowList.remove#input")
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.finish

          end
          module List = struct
type output = {
  dids : string list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ dids -> { dids })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.spindle.allowList.list#output") ~enc:(fun _ -> "org.tangled.temp.spindle.allowList.list#output")
  |> Jsont.Object.mem "dids" (Jsont.list Jsont.string) ~enc:(fun r -> r.dids)
  |> Jsont.Object.finish

          end
          module Add = struct
type input = {
  did : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ did -> { did })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.spindle.allowList.add#input") ~enc:(fun _ -> "org.tangled.temp.spindle.allowList.add#input")
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.finish

          end
        end
      end
      module Site = struct
        module ReleaseDomain = struct
type input = {
  domain : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ domain -> { domain })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.site.releaseDomain#input") ~enc:(fun _ -> "org.tangled.temp.site.releaseDomain#input")
  |> Jsont.Object.mem "domain" Jsont.string ~enc:(fun r -> r.domain)
  |> Jsont.Object.finish

        end
        module GetDomainClaim = struct
type output = {
  domain : string option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ domain -> { domain })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.site.getDomainClaim#output") ~enc:(fun _ -> "org.tangled.temp.site.getDomainClaim#output")
  |> Jsont.Object.opt_mem "domain" Jsont.string ~enc:(fun r -> r.domain)
  |> Jsont.Object.finish

        end
        module ClaimDomain = struct
type input = {
  domain : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ domain -> { domain })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.site.claimDomain#input") ~enc:(fun _ -> "org.tangled.temp.site.claimDomain#input")
  |> Jsont.Object.mem "domain" Jsont.string ~enc:(fun r -> r.domain)
  |> Jsont.Object.finish

        end
      end
      module Server = struct
        module UploadBlob = struct
type params = {
  repo_did : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun repo_did -> {
      repo_did;
    })
  |> Jsont.Object.mem "repoDid" Jsont.string
       ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.finish

type input = unit
let input_jsont = Jsont.ignore

type output = {
  blob : Atp.Blob_ref.t;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ blob -> { blob })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.server.uploadBlob#output") ~enc:(fun _ -> "org.tangled.temp.server.uploadBlob#output")
  |> Jsont.Object.mem "blob" Atp.Blob_ref.jsont ~enc:(fun r -> r.blob)
  |> Jsont.Object.finish

        end
        module SeedRepo = struct
type record_result = {
  collection : string;
  error : string option;
  rkey : string;
}

let record_result_jsont =
  Jsont.Object.map ~kind:"Record_result"
    (fun _typ collection error rkey -> { collection; error; rkey })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.server.seedRepo#recordResult") ~enc:(fun _ -> "org.tangled.temp.server.seedRepo#recordResult")
  |> Jsont.Object.mem "collection" Jsont.string ~enc:(fun r -> r.collection)
  |> Jsont.Object.opt_mem "error" Jsont.string ~enc:(fun r -> r.error)
  |> Jsont.Object.mem "rkey" Jsont.string ~enc:(fun r -> r.rkey)
  |> Jsont.Object.finish

type seed_record = {
  author : string;
  collection : string;
  created_at : int;
  record : Jsont.json;
  rkey : string;
}

let seed_record_jsont =
  Jsont.Object.map ~kind:"Seed_record"
    (fun _typ author collection created_at record rkey -> { author; collection; created_at; record; rkey })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.server.seedRepo#seedRecord") ~enc:(fun _ -> "org.tangled.temp.server.seedRepo#seedRecord")
  |> Jsont.Object.mem "author" Jsont.string ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "collection" Jsont.string ~enc:(fun r -> r.collection)
  |> Jsont.Object.mem "createdAt" Jsont.int ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "record" Jsont.json ~enc:(fun r -> r.record)
  |> Jsont.Object.mem "rkey" Jsont.string ~enc:(fun r -> r.rkey)
  |> Jsont.Object.finish

type input = {
  manifest : Jsont.json;
  records : Jsont.json list;
  repo_did : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ manifest records repo_did -> { manifest; records; repo_did })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.server.seedRepo#input") ~enc:(fun _ -> "org.tangled.temp.server.seedRepo#input")
  |> Jsont.Object.mem "manifest" Jsont.json ~enc:(fun r -> r.manifest)
  |> Jsont.Object.mem "records" (Jsont.list Jsont.json) ~enc:(fun r -> r.records)
  |> Jsont.Object.mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.finish

type output = {
  did : string;
  key : string;
  results : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ did key results -> { did; key; results })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.server.seedRepo#output") ~enc:(fun _ -> "org.tangled.temp.server.seedRepo#output")
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
  |> Jsont.Object.mem "results" (Jsont.list Jsont.json) ~enc:(fun r -> r.results)
  |> Jsont.Object.finish

        end
        module RedeemInviteCode = struct
type input = {
  code : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ code -> { code })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.server.redeemInviteCode#input") ~enc:(fun _ -> "org.tangled.temp.server.redeemInviteCode#input")
  |> Jsont.Object.mem "code" Jsont.string ~enc:(fun r -> r.code)
  |> Jsont.Object.finish

        end
        module HandoverRepo = struct
type input = {
  foster_url : string;
  repo_did : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ foster_url repo_did -> { foster_url; repo_did })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.server.handoverRepo#input") ~enc:(fun _ -> "org.tangled.temp.server.handoverRepo#input")
  |> Jsont.Object.mem "fosterUrl" Jsont.string ~enc:(fun r -> r.foster_url)
  |> Jsont.Object.mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.finish

type output = {
  repo_did : string;
  state : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ repo_did state -> { repo_did; state })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.server.handoverRepo#output") ~enc:(fun _ -> "org.tangled.temp.server.handoverRepo#output")
  |> Jsont.Object.mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.mem "state" Jsont.string ~enc:(fun r -> r.state)
  |> Jsont.Object.finish

        end
        module CreateInviteCode = struct
type input = {
  for_account : string option;
  use_count : int;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ for_account use_count -> { for_account; use_count })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.server.createInviteCode#input") ~enc:(fun _ -> "org.tangled.temp.server.createInviteCode#input")
  |> Jsont.Object.opt_mem "forAccount" Jsont.string ~enc:(fun r -> r.for_account)
  |> Jsont.Object.mem "useCount" Jsont.int ~enc:(fun r -> r.use_count)
  |> Jsont.Object.finish

type output = {
  code : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ code -> { code })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.server.createInviteCode#output") ~enc:(fun _ -> "org.tangled.temp.server.createInviteCode#output")
  |> Jsont.Object.mem "code" Jsont.string ~enc:(fun r -> r.code)
  |> Jsont.Object.finish

        end
        module CancelHandover = struct
type input = {
  repo_did : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ repo_did -> { repo_did })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.server.cancelHandover#input") ~enc:(fun _ -> "org.tangled.temp.server.cancelHandover#input")
  |> Jsont.Object.mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.finish

type output = {
  repo_did : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ repo_did -> { repo_did })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.server.cancelHandover#output") ~enc:(fun _ -> "org.tangled.temp.server.cancelHandover#output")
  |> Jsont.Object.mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.finish

        end
        module AckHandover = struct
type input = {
  acked_at : int;
  records : int;
  repo_did : string;
  signature : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ acked_at records repo_did signature -> { acked_at; records; repo_did; signature })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.server.ackHandover#input") ~enc:(fun _ -> "org.tangled.temp.server.ackHandover#input")
  |> Jsont.Object.mem "ackedAt" Jsont.int ~enc:(fun r -> r.acked_at)
  |> Jsont.Object.mem "records" Jsont.int ~enc:(fun r -> r.records)
  |> Jsont.Object.mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.mem "signature" Jsont.string ~enc:(fun r -> r.signature)
  |> Jsont.Object.finish

        end
      end
      module Search = struct
        module SearchCode = struct
type highlight = {
  end_ : int;
  start : int;
}

let highlight_jsont =
  Jsont.Object.map ~kind:"Highlight"
    (fun _typ end_ start -> { end_; start })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.search.searchCode#highlight") ~enc:(fun _ -> "org.tangled.temp.search.searchCode#highlight")
  |> Jsont.Object.mem "end" Jsont.int ~enc:(fun r -> r.end_)
  |> Jsont.Object.mem "start" Jsont.int ~enc:(fun r -> r.start)
  |> Jsont.Object.finish

type chunk = {
  content : string;
  highlights : Jsont.json list option;
  line_start : int;
}

let chunk_jsont =
  Jsont.Object.map ~kind:"Chunk"
    (fun _typ content highlights line_start -> { content; highlights; line_start })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.search.searchCode#chunk") ~enc:(fun _ -> "org.tangled.temp.search.searchCode#chunk")
  |> Jsont.Object.mem "content" Jsont.string ~enc:(fun r -> r.content)
  |> Jsont.Object.opt_mem "highlights" (Jsont.list Jsont.json) ~enc:(fun r -> r.highlights)
  |> Jsont.Object.mem "lineStart" Jsont.int ~enc:(fun r -> r.line_start)
  |> Jsont.Object.finish

type file_result = {
  branches : string list option;
  chunks : Jsont.json list option;
  commit : string;
  language : string option;
  path : string;
  ranges : Jsont.json list option;
  repo : Jsont.json;
}

let file_result_jsont =
  Jsont.Object.map ~kind:"File_result"
    (fun _typ branches chunks commit language path ranges repo -> { branches; chunks; commit; language; path; ranges; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.search.searchCode#fileResult") ~enc:(fun _ -> "org.tangled.temp.search.searchCode#fileResult")
  |> Jsont.Object.opt_mem "branches" (Jsont.list Jsont.string) ~enc:(fun r -> r.branches)
  |> Jsont.Object.opt_mem "chunks" (Jsont.list Jsont.json) ~enc:(fun r -> r.chunks)
  |> Jsont.Object.mem "commit" Jsont.string ~enc:(fun r -> r.commit)
  |> Jsont.Object.opt_mem "language" Jsont.string ~enc:(fun r -> r.language)
  |> Jsont.Object.mem "path" Jsont.string ~enc:(fun r -> r.path)
  |> Jsont.Object.opt_mem "ranges" (Jsont.list Jsont.json) ~enc:(fun r -> r.ranges)
  |> Jsont.Object.mem "repo" Jsont.json ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  q : string;
  repo : string option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit q repo -> {
      cursor;
      limit;
      q;
      repo;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "q" Jsont.string
       ~enc:(fun r -> r.q)
  |> Jsont.Object.opt_mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  results : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor results -> { cursor; results })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.search.searchCode#output") ~enc:(fun _ -> "org.tangled.temp.search.searchCode#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "results" (Jsont.list Jsont.json) ~enc:(fun r -> r.results)
  |> Jsont.Object.finish

        end
      end
      module Review = struct
        module ListPatches = struct
type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.review.listPatches#listItem") ~enc:(fun _ -> "org.tangled.temp.review.listPatches#listItem")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  ticket : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order ticket -> {
      cursor;
      limit;
      offset;
      order;
      ticket;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "ticket" Jsont.string
       ~enc:(fun r -> r.ticket)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.review.listPatches#output") ~enc:(fun _ -> "org.tangled.temp.review.listPatches#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

        end
      end
      module Repo = struct
        module UpdateSiteConfig = struct
type input = {
  branch : string;
  dir : string;
  is_index : bool option;
  knot : string;
  name : string;
  repo_did : string;
  rkey : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ branch dir is_index knot name repo_did rkey -> { branch; dir; is_index; knot; name; repo_did; rkey })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.repo.updateSiteConfig#input") ~enc:(fun _ -> "org.tangled.temp.repo.updateSiteConfig#input")
  |> Jsont.Object.mem "branch" Jsont.string ~enc:(fun r -> r.branch)
  |> Jsont.Object.mem "dir" Jsont.string ~enc:(fun r -> r.dir)
  |> Jsont.Object.opt_mem "isIndex" Jsont.bool ~enc:(fun r -> r.is_index)
  |> Jsont.Object.mem "knot" Jsont.string ~enc:(fun r -> r.knot)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.mem "rkey" Jsont.string ~enc:(fun r -> r.rkey)
  |> Jsont.Object.finish

        end
        module ListRecordOrigins = struct
type origin = {
  author : string;
  change_id : string;
  created_at : string;
  rkey : string;
}

let origin_jsont =
  Jsont.Object.map ~kind:"Origin"
    (fun _typ author change_id created_at rkey -> { author; change_id; created_at; rkey })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.repo.listRecordOrigins#origin") ~enc:(fun _ -> "org.tangled.temp.repo.listRecordOrigins#origin")
  |> Jsont.Object.mem "author" Jsont.string ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "changeId" Jsont.string ~enc:(fun r -> r.change_id)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "rkey" Jsont.string ~enc:(fun r -> r.rkey)
  |> Jsont.Object.finish

type params = {
  collection : string;
  cursor : string option;
  limit : int option;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun collection cursor limit repo -> {
      collection;
      cursor;
      limit;
      repo;
    })
  |> Jsont.Object.mem "collection" Jsont.string
       ~enc:(fun r -> r.collection)
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  collection : string;
  cursor : string option;
  origins : Jsont.json list;
  repo : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ collection cursor origins repo -> { collection; cursor; origins; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.repo.listRecordOrigins#output") ~enc:(fun _ -> "org.tangled.temp.repo.listRecordOrigins#output")
  |> Jsont.Object.mem "collection" Jsont.string ~enc:(fun r -> r.collection)
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "origins" (Jsont.list Jsont.json) ~enc:(fun r -> r.origins)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

        end
        module GetSiteConfig = struct
type site_config = {
  branch : string;
  dir : string;
  is_index : bool;
}

let site_config_jsont =
  Jsont.Object.map ~kind:"Site_config"
    (fun _typ branch dir is_index -> { branch; dir; is_index })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.repo.getSiteConfig#siteConfig") ~enc:(fun _ -> "org.tangled.temp.repo.getSiteConfig#siteConfig")
  |> Jsont.Object.mem "branch" Jsont.string ~enc:(fun r -> r.branch)
  |> Jsont.Object.mem "dir" Jsont.string ~enc:(fun r -> r.dir)
  |> Jsont.Object.mem "isIndex" Jsont.bool ~enc:(fun r -> r.is_index)
  |> Jsont.Object.finish

type params = {
  repo_did : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun repo_did -> {
      repo_did;
    })
  |> Jsont.Object.mem "repoDid" Jsont.string
       ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.finish

type output = {
  config : Jsont.json option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ config -> { config })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.repo.getSiteConfig#output") ~enc:(fun _ -> "org.tangled.temp.repo.getSiteConfig#output")
  |> Jsont.Object.opt_mem "config" Jsont.json ~enc:(fun r -> r.config)
  |> Jsont.Object.finish

        end
        module GetRepoByName = struct
type git_info = {
  default_branch : string;
  is_empty : bool;
  languages : Jsont.json list option;
}

let git_info_jsont =
  Jsont.Object.map ~kind:"Git_info"
    (fun _typ default_branch is_empty languages -> { default_branch; is_empty; languages })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.repo.getRepoByName#gitInfo") ~enc:(fun _ -> "org.tangled.temp.repo.getRepoByName#gitInfo")
  |> Jsont.Object.mem "defaultBranch" Jsont.string ~enc:(fun r -> r.default_branch)
  |> Jsont.Object.mem "isEmpty" Jsont.bool ~enc:(fun r -> r.is_empty)
  |> Jsont.Object.opt_mem "languages" (Jsont.list Jsont.json) ~enc:(fun r -> r.languages)
  |> Jsont.Object.finish

type params = {
  name : string;
  owner : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun name owner -> {
      name;
      owner;
    })
  |> Jsont.Object.mem "name" Jsont.string
       ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "owner" Jsont.string
       ~enc:(fun r -> r.owner)
  |> Jsont.Object.finish

type output = {
  cid : string option;
  git : Jsont.json option;
  knot : string option;
  uri : string;
  value : Jsont.json;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cid git knot uri value -> { cid; git; knot; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.repo.getRepoByName#output") ~enc:(fun _ -> "org.tangled.temp.repo.getRepoByName#output")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.opt_mem "git" Jsont.json ~enc:(fun r -> r.git)
  |> Jsont.Object.opt_mem "knot" Jsont.string ~enc:(fun r -> r.knot)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

        end
        module GetDeployHistory = struct
type deploy = {
  branch : string;
  commit_sha : string;
  created_at : string;
  dir : string;
  error : string;
  status : string;
  trigger : string;
}

let deploy_jsont =
  Jsont.Object.map ~kind:"Deploy"
    (fun _typ branch commit_sha created_at dir error status trigger -> { branch; commit_sha; created_at; dir; error; status; trigger })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.repo.getDeployHistory#deploy") ~enc:(fun _ -> "org.tangled.temp.repo.getDeployHistory#deploy")
  |> Jsont.Object.mem "branch" Jsont.string ~enc:(fun r -> r.branch)
  |> Jsont.Object.mem "commitSha" Jsont.string ~enc:(fun r -> r.commit_sha)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "dir" Jsont.string ~enc:(fun r -> r.dir)
  |> Jsont.Object.mem "error" Jsont.string ~enc:(fun r -> r.error)
  |> Jsont.Object.mem "status" Jsont.string ~enc:(fun r -> r.status)
  |> Jsont.Object.mem "trigger" Jsont.string ~enc:(fun r -> r.trigger)
  |> Jsont.Object.finish

type params = {
  repo_did : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun repo_did -> {
      repo_did;
    })
  |> Jsont.Object.mem "repoDid" Jsont.string
       ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.finish

type output = {
  deploys : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ deploys -> { deploys })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.repo.getDeployHistory#output") ~enc:(fun _ -> "org.tangled.temp.repo.getDeployHistory#output")
  |> Jsont.Object.mem "deploys" (Jsont.list Jsont.json) ~enc:(fun r -> r.deploys)
  |> Jsont.Object.finish

        end
        module DisableSite = struct
type input = {
  repo_did : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ repo_did -> { repo_did })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.repo.disableSite#input") ~enc:(fun _ -> "org.tangled.temp.repo.disableSite#input")
  |> Jsont.Object.mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.finish

        end
        module DeploySite = struct
type input = {
  repo_did : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ repo_did -> { repo_did })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.repo.deploySite#input") ~enc:(fun _ -> "org.tangled.temp.repo.deploySite#input")
  |> Jsont.Object.mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.finish

        end
      end
      module Notification = struct
        module UpdateSeen = struct
type input = {
  read : bool;
  uri : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ read uri -> { read; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.notification.updateSeen#input") ~enc:(fun _ -> "org.tangled.temp.notification.updateSeen#input")
  |> Jsont.Object.mem "read" Jsont.bool ~enc:(fun r -> r.read)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

        end
        module UpdatePreferences = struct
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

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ email_notifications followed issue_closed issue_commented issue_created pull_commented pull_created pull_merged repo_starred user_mentioned -> { email_notifications; followed; issue_closed; issue_commented; issue_created; pull_commented; pull_created; pull_merged; repo_starred; user_mentioned })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.notification.updatePreferences#input") ~enc:(fun _ -> "org.tangled.temp.notification.updatePreferences#input")
  |> Jsont.Object.opt_mem "emailNotifications" Jsont.bool ~enc:(fun r -> r.email_notifications)
  |> Jsont.Object.opt_mem "followed" Jsont.bool ~enc:(fun r -> r.followed)
  |> Jsont.Object.opt_mem "issueClosed" Jsont.bool ~enc:(fun r -> r.issue_closed)
  |> Jsont.Object.opt_mem "issueCommented" Jsont.bool ~enc:(fun r -> r.issue_commented)
  |> Jsont.Object.opt_mem "issueCreated" Jsont.bool ~enc:(fun r -> r.issue_created)
  |> Jsont.Object.opt_mem "pullCommented" Jsont.bool ~enc:(fun r -> r.pull_commented)
  |> Jsont.Object.opt_mem "pullCreated" Jsont.bool ~enc:(fun r -> r.pull_created)
  |> Jsont.Object.opt_mem "pullMerged" Jsont.bool ~enc:(fun r -> r.pull_merged)
  |> Jsont.Object.opt_mem "repoStarred" Jsont.bool ~enc:(fun r -> r.repo_starred)
  |> Jsont.Object.opt_mem "userMentioned" Jsont.bool ~enc:(fun r -> r.user_mentioned)
  |> Jsont.Object.finish

        end
        module MarkEntityRead = struct
type input = {
  entity_at : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ entity_at -> { entity_at })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.notification.markEntityRead#input") ~enc:(fun _ -> "org.tangled.temp.notification.markEntityRead#input")
  |> Jsont.Object.mem "entityAt" Jsont.string ~enc:(fun r -> r.entity_at)
  |> Jsont.Object.finish

        end
        module MarkAllRead = struct
        end
        module ListRecipients = struct
type params = {
  collection : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun collection subject -> {
      collection;
      subject;
    })
  |> Jsont.Object.opt_mem "collection" Jsont.string
       ~enc:(fun r -> r.collection)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  dids : string list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ dids -> { dids })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.notification.listRecipients#output") ~enc:(fun _ -> "org.tangled.temp.notification.listRecipients#output")
  |> Jsont.Object.mem "dids" (Jsont.list Jsont.string) ~enc:(fun r -> r.dids)
  |> Jsont.Object.finish

        end
        module ListNotifications = struct
type notification = {
  actor_did : string;
  category : string;
  created_at : string;
  issue_at : string option;
  pull_at : string option;
  read : bool;
  repo_did : string option;
  type_ : string;
  uri : string;
}

let notification_jsont =
  Jsont.Object.map ~kind:"Notification"
    (fun _typ actor_did category created_at issue_at pull_at read repo_did type_ uri -> { actor_did; category; created_at; issue_at; pull_at; read; repo_did; type_; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.notification.listNotifications#notification") ~enc:(fun _ -> "org.tangled.temp.notification.listNotifications#notification")
  |> Jsont.Object.mem "actorDid" Jsont.string ~enc:(fun r -> r.actor_did)
  |> Jsont.Object.mem "category" Jsont.string ~enc:(fun r -> r.category)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "issueAt" Jsont.string ~enc:(fun r -> r.issue_at)
  |> Jsont.Object.opt_mem "pullAt" Jsont.string ~enc:(fun r -> r.pull_at)
  |> Jsont.Object.mem "read" Jsont.bool ~enc:(fun r -> r.read)
  |> Jsont.Object.opt_mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.mem "type" Jsont.string ~enc:(fun r -> r.type_)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type params = {
  category : string option;
  cursor : string option;
  limit : int option;
  read : string option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun category cursor limit read -> {
      category;
      cursor;
      limit;
      read;
    })
  |> Jsont.Object.opt_mem "category" Jsont.string
       ~enc:(fun r -> r.category)
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "read" Jsont.string
       ~enc:(fun r -> r.read)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  notifications : Jsont.json list;
  social_unread_count : int;
  work_unread_count : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor notifications social_unread_count work_unread_count -> { cursor; notifications; social_unread_count; work_unread_count })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.notification.listNotifications#output") ~enc:(fun _ -> "org.tangled.temp.notification.listNotifications#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "notifications" (Jsont.list Jsont.json) ~enc:(fun r -> r.notifications)
  |> Jsont.Object.mem "socialUnreadCount" Jsont.int ~enc:(fun r -> r.social_unread_count)
  |> Jsont.Object.mem "workUnreadCount" Jsont.int ~enc:(fun r -> r.work_unread_count)
  |> Jsont.Object.finish

        end
        module GetUnreadCount = struct
type output = {
  count : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count -> { count })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.notification.getUnreadCount#output") ~enc:(fun _ -> "org.tangled.temp.notification.getUnreadCount#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.finish

        end
        module GetPreferences = struct
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

let preferences_jsont =
  Jsont.Object.map ~kind:"Preferences"
    (fun _typ email_notifications followed issue_closed issue_commented issue_created pull_commented pull_created pull_merged repo_starred user_mentioned -> { email_notifications; followed; issue_closed; issue_commented; issue_created; pull_commented; pull_created; pull_merged; repo_starred; user_mentioned })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.notification.getPreferences#preferences") ~enc:(fun _ -> "org.tangled.temp.notification.getPreferences#preferences")
  |> Jsont.Object.mem "emailNotifications" Jsont.bool ~enc:(fun r -> r.email_notifications)
  |> Jsont.Object.mem "followed" Jsont.bool ~enc:(fun r -> r.followed)
  |> Jsont.Object.mem "issueClosed" Jsont.bool ~enc:(fun r -> r.issue_closed)
  |> Jsont.Object.mem "issueCommented" Jsont.bool ~enc:(fun r -> r.issue_commented)
  |> Jsont.Object.mem "issueCreated" Jsont.bool ~enc:(fun r -> r.issue_created)
  |> Jsont.Object.mem "pullCommented" Jsont.bool ~enc:(fun r -> r.pull_commented)
  |> Jsont.Object.mem "pullCreated" Jsont.bool ~enc:(fun r -> r.pull_created)
  |> Jsont.Object.mem "pullMerged" Jsont.bool ~enc:(fun r -> r.pull_merged)
  |> Jsont.Object.mem "repoStarred" Jsont.bool ~enc:(fun r -> r.repo_starred)
  |> Jsont.Object.mem "userMentioned" Jsont.bool ~enc:(fun r -> r.user_mentioned)
  |> Jsont.Object.finish

type output = Jsont.json

let output_jsont = Jsont.json

        end
        module DeleteNotification = struct
type input = {
  uri : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ uri -> { uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.notification.deleteNotification#input") ~enc:(fun _ -> "org.tangled.temp.notification.deleteNotification#input")
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

        end
      end
      module Newsletter = struct
        module Signup = struct
type input = {
  email : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ email -> { email })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.newsletter.signup#input") ~enc:(fun _ -> "org.tangled.temp.newsletter.signup#input")
  |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
  |> Jsont.Object.finish

        end
      end
      module Migrator = struct
        module DescribeSource = struct
type params = {
  source_url : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun source_url -> {
      source_url;
    })
  |> Jsont.Object.mem "sourceUrl" Jsont.string
       ~enc:(fun r -> r.source_url)
  |> Jsont.Object.finish

type output = {
  default_branch : string;
  name : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ default_branch name -> { default_branch; name })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.migrator.describeSource#output") ~enc:(fun _ -> "org.tangled.temp.migrator.describeSource#output")
  |> Jsont.Object.mem "defaultBranch" Jsont.string ~enc:(fun r -> r.default_branch)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.finish

        end
        module Defs = struct
type job = {
  attempts : int;
  error : string option;
  id : string;
  knot_did : string;
  name : string;
  private_ : bool;
  repo_did : string option;
  source_url : string;
  status : string;
}

let job_jsont =
  Jsont.Object.map ~kind:"Job"
    (fun _typ attempts error id knot_did name private_ repo_did source_url status -> { attempts; error; id; knot_did; name; private_; repo_did; source_url; status })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.migrator.defs#job") ~enc:(fun _ -> "org.tangled.temp.migrator.defs#job")
  |> Jsont.Object.mem "attempts" Jsont.int ~enc:(fun r -> r.attempts)
  |> Jsont.Object.opt_mem "error" Jsont.string ~enc:(fun r -> r.error)
  |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
  |> Jsont.Object.mem "knotDid" Jsont.string ~enc:(fun r -> r.knot_did)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "private" Jsont.bool ~enc:(fun r -> r.private_)
  |> Jsont.Object.opt_mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.mem "sourceUrl" Jsont.string ~enc:(fun r -> r.source_url)
  |> Jsont.Object.mem "status" Jsont.string ~enc:(fun r -> r.status)
  |> Jsont.Object.finish

type new_job = {
  description : string option;
  knot_did : string;
  name : string;
  private_ : bool option;
  source_url : string;
}

let new_job_jsont =
  Jsont.Object.map ~kind:"New_job"
    (fun _typ description knot_did name private_ source_url -> { description; knot_did; name; private_; source_url })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.migrator.defs#newJob") ~enc:(fun _ -> "org.tangled.temp.migrator.defs#newJob")
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
  |> Jsont.Object.mem "knotDid" Jsont.string ~enc:(fun r -> r.knot_did)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.opt_mem "private" Jsont.bool ~enc:(fun r -> r.private_)
  |> Jsont.Object.mem "sourceUrl" Jsont.string ~enc:(fun r -> r.source_url)
  |> Jsont.Object.finish

type task = {
  created_at : string;
  id : string;
  jobs : Jsont.json list;
  owner_did : string;
  updated_at : string option;
}

let task_jsont =
  Jsont.Object.map ~kind:"Task"
    (fun _typ created_at id jobs owner_did updated_at -> { created_at; id; jobs; owner_did; updated_at })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.migrator.defs#task") ~enc:(fun _ -> "org.tangled.temp.migrator.defs#task")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
  |> Jsont.Object.mem "jobs" (Jsont.list Jsont.json) ~enc:(fun r -> r.jobs)
  |> Jsont.Object.mem "ownerDid" Jsont.string ~enc:(fun r -> r.owner_did)
  |> Jsont.Object.opt_mem "updatedAt" Jsont.string ~enc:(fun r -> r.updated_at)
  |> Jsont.Object.finish

        end
        module RetryJob = struct
type input = {
  github_token : string option;
  job_id : string;
  task_id : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ github_token job_id task_id -> { github_token; job_id; task_id })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.migrator.retryJob#input") ~enc:(fun _ -> "org.tangled.temp.migrator.retryJob#input")
  |> Jsont.Object.opt_mem "githubToken" Jsont.string ~enc:(fun r -> r.github_token)
  |> Jsont.Object.mem "jobId" Jsont.string ~enc:(fun r -> r.job_id)
  |> Jsont.Object.mem "taskId" Jsont.string ~enc:(fun r -> r.task_id)
  |> Jsont.Object.finish

type output = Jsont.json

let output_jsont = Jsont.json

        end
        module ListTasks = struct
type params = {
  limit : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun limit -> {
      limit;
    })
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.finish

type output = {
  tasks : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ tasks -> { tasks })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.migrator.listTasks#output") ~enc:(fun _ -> "org.tangled.temp.migrator.listTasks#output")
  |> Jsont.Object.mem "tasks" (Jsont.list Jsont.json) ~enc:(fun r -> r.tasks)
  |> Jsont.Object.finish

        end
        module GetTask = struct
type params = {
  task_id : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun task_id -> {
      task_id;
    })
  |> Jsont.Object.mem "taskId" Jsont.string
       ~enc:(fun r -> r.task_id)
  |> Jsont.Object.finish

type output = Jsont.json

let output_jsont = Jsont.json

        end
        module CreateTask = struct
type input = {
  github_token : string option;
  jobs : Jsont.json list;
  request_id : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ github_token jobs request_id -> { github_token; jobs; request_id })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.migrator.createTask#input") ~enc:(fun _ -> "org.tangled.temp.migrator.createTask#input")
  |> Jsont.Object.opt_mem "githubToken" Jsont.string ~enc:(fun r -> r.github_token)
  |> Jsont.Object.mem "jobs" (Jsont.list Jsont.json) ~enc:(fun r -> r.jobs)
  |> Jsont.Object.mem "requestId" Jsont.string ~enc:(fun r -> r.request_id)
  |> Jsont.Object.finish

type output = Jsont.json

let output_jsont = Jsont.json

        end
      end
      module Git = struct
        module SyncFork = struct
type input = {
  branch : string;
  repo : string;
  source : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ branch repo source -> { branch; repo; source })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.git.syncFork#input") ~enc:(fun _ -> "org.tangled.temp.git.syncFork#input")
  |> Jsont.Object.mem "branch" Jsont.string ~enc:(fun r -> r.branch)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "source" Jsont.string ~enc:(fun r -> r.source)
  |> Jsont.Object.finish

        end
        module MergeCommit = struct
type merge_commit_content = {
  author : Jsont.json option;
  message : string option;
}

let merge_commit_content_jsont =
  Jsont.Object.map ~kind:"Merge_commit_content"
    (fun _typ author message -> { author; message })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.git.mergeCommit#mergeCommitContent") ~enc:(fun _ -> "org.tangled.temp.git.mergeCommit#mergeCommitContent")
  |> Jsont.Object.opt_mem "author" Jsont.json ~enc:(fun r -> r.author)
  |> Jsont.Object.opt_mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.finish

type source = {
  commit : string;
  repo : string;
}

let source_jsont =
  Jsont.Object.map ~kind:"Source"
    (fun _typ commit repo -> { commit; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.git.mergeCommit#source") ~enc:(fun _ -> "org.tangled.temp.git.mergeCommit#source")
  |> Jsont.Object.mem "commit" Jsont.string ~enc:(fun r -> r.commit)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type target = {
  branch : string;
  commit : string;
  repo : string;
}

let target_jsont =
  Jsont.Object.map ~kind:"Target"
    (fun _typ branch commit repo -> { branch; commit; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.git.mergeCommit#target") ~enc:(fun _ -> "org.tangled.temp.git.mergeCommit#target")
  |> Jsont.Object.mem "branch" Jsont.string ~enc:(fun r -> r.branch)
  |> Jsont.Object.mem "commit" Jsont.string ~enc:(fun r -> r.commit)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type input = {
  merge_commit : Jsont.json option;
  source : Jsont.json;
  style : string;
  target : Jsont.json;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ merge_commit source style target -> { merge_commit; source; style; target })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.git.mergeCommit#input") ~enc:(fun _ -> "org.tangled.temp.git.mergeCommit#input")
  |> Jsont.Object.opt_mem "mergeCommit" Jsont.json ~enc:(fun r -> r.merge_commit)
  |> Jsont.Object.mem "source" Jsont.json ~enc:(fun r -> r.source)
  |> Jsont.Object.mem "style" Jsont.string ~enc:(fun r -> r.style)
  |> Jsont.Object.mem "target" Jsont.json ~enc:(fun r -> r.target)
  |> Jsont.Object.finish

        end
        module ListTags = struct
type annotation = {
  message : string;
  tagger : Jsont.json option;
}

let annotation_jsont =
  Jsont.Object.map ~kind:"Annotation"
    (fun _typ message tagger -> { message; tagger })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.git.listTags#annotation") ~enc:(fun _ -> "org.tangled.temp.git.listTags#annotation")
  |> Jsont.Object.mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.opt_mem "tagger" Jsont.json ~enc:(fun r -> r.tagger)
  |> Jsont.Object.finish

type tag_info = {
  annotation : Jsont.json option;
  artifacts : Jsont.json list option;
  author : Jsont.json;
  message : string;
  name : string;
  oid : string;
}

let tag_info_jsont =
  Jsont.Object.map ~kind:"Tag_info"
    (fun _typ annotation artifacts author message name oid -> { annotation; artifacts; author; message; name; oid })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.git.listTags#tagInfo") ~enc:(fun _ -> "org.tangled.temp.git.listTags#tagInfo")
  |> Jsont.Object.opt_mem "annotation" Jsont.json ~enc:(fun r -> r.annotation)
  |> Jsont.Object.opt_mem "artifacts" (Jsont.list Jsont.json) ~enc:(fun r -> r.artifacts)
  |> Jsont.Object.mem "author" Jsont.json ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "oid" Jsont.string ~enc:(fun r -> r.oid)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit repo -> {
      cursor;
      limit;
      repo;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  tags : Jsont.json list;
  total : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor tags total -> { cursor; tags; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.git.listTags#output") ~enc:(fun _ -> "org.tangled.temp.git.listTags#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "tags" (Jsont.list Jsont.json) ~enc:(fun r -> r.tags)
  |> Jsont.Object.mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

        end
        module ListCommits = struct
type header = {
  key : string;
  value : string;
}

let header_jsont =
  Jsont.Object.map ~kind:"Header"
    (fun _typ key value -> { key; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.git.listCommits#header") ~enc:(fun _ -> "org.tangled.temp.git.listCommits#header")
  |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
  |> Jsont.Object.mem "value" Jsont.string ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type ssh_signature = {
  fingerprint : string;
}

let ssh_signature_jsont =
  Jsont.Object.map ~kind:"Ssh_signature"
    (fun _typ fingerprint -> { fingerprint })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.git.listCommits#sshSignature") ~enc:(fun _ -> "org.tangled.temp.git.listCommits#sshSignature")
  |> Jsont.Object.mem "fingerprint" Jsont.string ~enc:(fun r -> r.fingerprint)
  |> Jsont.Object.finish

type commit = {
  author : Jsont.json;
  committer : Jsont.json;
  extra_headers : Jsont.json list;
  message : string;
  oid : string;
  parents : string list;
  signature : Jsont.json option;
  stat : Jsont.json option;
  tree : string;
}

let commit_jsont =
  Jsont.Object.map ~kind:"Commit"
    (fun _typ author committer extra_headers message oid parents signature stat tree -> { author; committer; extra_headers; message; oid; parents; signature; stat; tree })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.git.listCommits#commit") ~enc:(fun _ -> "org.tangled.temp.git.listCommits#commit")
  |> Jsont.Object.mem "author" Jsont.json ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "committer" Jsont.json ~enc:(fun r -> r.committer)
  |> Jsont.Object.mem "extraHeaders" (Jsont.list Jsont.json) ~enc:(fun r -> r.extra_headers)
  |> Jsont.Object.mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.mem "oid" Jsont.string ~enc:(fun r -> r.oid)
  |> Jsont.Object.mem "parents" (Jsont.list Jsont.string) ~enc:(fun r -> r.parents)
  |> Jsont.Object.opt_mem "signature" Jsont.json ~enc:(fun r -> r.signature)
  |> Jsont.Object.opt_mem "stat" Jsont.json ~enc:(fun r -> r.stat)
  |> Jsont.Object.mem "tree" Jsont.string ~enc:(fun r -> r.tree)
  |> Jsont.Object.finish

type params = {
  include_stats : bool option;
  limit : int option;
  ranges : string list option;
  repos : string list;
  skip : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun include_stats limit ranges repos skip -> {
      include_stats;
      limit;
      ranges;
      repos;
      skip;
    })
  |> Jsont.Object.opt_mem "includeStats" Jsont.bool
       ~enc:(fun r -> r.include_stats)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "ranges" (Jsont.list Jsont.string)
       ~enc:(fun r -> r.ranges)
  |> Jsont.Object.mem "repos" (Jsont.list Jsont.string)
       ~enc:(fun r -> r.repos)
  |> Jsont.Object.opt_mem "skip" Jsont.int
       ~enc:(fun r -> r.skip)
  |> Jsont.Object.finish

type output = {
  commits : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ commits -> { commits })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.git.listCommits#output") ~enc:(fun _ -> "org.tangled.temp.git.listCommits#output")
  |> Jsont.Object.mem "commits" (Jsont.list Jsont.json) ~enc:(fun r -> r.commits)
  |> Jsont.Object.finish

        end
        module ListBranches = struct
type branch_info = {
  author : Jsont.json;
  committer : Jsont.json;
  message : string;
  name : string;
  oid : string;
}

let branch_info_jsont =
  Jsont.Object.map ~kind:"Branch_info"
    (fun _typ author committer message name oid -> { author; committer; message; name; oid })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.git.listBranches#branchInfo") ~enc:(fun _ -> "org.tangled.temp.git.listBranches#branchInfo")
  |> Jsont.Object.mem "author" Jsont.json ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "committer" Jsont.json ~enc:(fun r -> r.committer)
  |> Jsont.Object.mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "oid" Jsont.string ~enc:(fun r -> r.oid)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit repo -> {
      cursor;
      limit;
      repo;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  branches : Jsont.json list;
  cursor : string option;
  default_branch : string option;
  total : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ branches cursor default_branch total -> { branches; cursor; default_branch; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.git.listBranches#output") ~enc:(fun _ -> "org.tangled.temp.git.listBranches#output")
  |> Jsont.Object.mem "branches" (Jsont.list Jsont.json) ~enc:(fun r -> r.branches)
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "defaultBranch" Jsont.string ~enc:(fun r -> r.default_branch)
  |> Jsont.Object.mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

        end
        module GetTree = struct
type tree_entry = {
  last_commit : Jsont.json option;
  mode : string;
  name : string;
  size : int;
}

let tree_entry_jsont =
  Jsont.Object.map ~kind:"Tree_entry"
    (fun _typ last_commit mode name size -> { last_commit; mode; name; size })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.git.getTree#treeEntry") ~enc:(fun _ -> "org.tangled.temp.git.getTree#treeEntry")
  |> Jsont.Object.opt_mem "lastCommit" Jsont.json ~enc:(fun r -> r.last_commit)
  |> Jsont.Object.mem "mode" Jsont.string ~enc:(fun r -> r.mode)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "size" Jsont.int ~enc:(fun r -> r.size)
  |> Jsont.Object.finish

type params = {
  path : string option;
  ref_ : string option;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun path ref_ repo -> {
      path;
      ref_;
      repo;
    })
  |> Jsont.Object.opt_mem "path" Jsont.string
       ~enc:(fun r -> r.path)
  |> Jsont.Object.opt_mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  entries : Jsont.json list;
  last_commit : Jsont.json option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ entries last_commit -> { entries; last_commit })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.git.getTree#output") ~enc:(fun _ -> "org.tangled.temp.git.getTree#output")
  |> Jsont.Object.mem "entries" (Jsont.list Jsont.json) ~enc:(fun r -> r.entries)
  |> Jsont.Object.opt_mem "lastCommit" Jsont.json ~enc:(fun r -> r.last_commit)
  |> Jsont.Object.finish

        end
        module GetMergeBase = struct
type params = {
  base : string;
  head : string;
  repos : string list;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun base head repos -> {
      base;
      head;
      repos;
    })
  |> Jsont.Object.mem "base" Jsont.string
       ~enc:(fun r -> r.base)
  |> Jsont.Object.mem "head" Jsont.string
       ~enc:(fun r -> r.head)
  |> Jsont.Object.mem "repos" (Jsont.list Jsont.string)
       ~enc:(fun r -> r.repos)
  |> Jsont.Object.finish

type output = {
  commit : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ commit -> { commit })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.git.getMergeBase#output") ~enc:(fun _ -> "org.tangled.temp.git.getMergeBase#output")
  |> Jsont.Object.mem "commit" Jsont.string ~enc:(fun r -> r.commit)
  |> Jsont.Object.finish

        end
        module GetEntry = struct
type params = {
  path : string;
  ref_ : string option;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun path ref_ repo -> {
      path;
      ref_;
      repo;
    })
  |> Jsont.Object.mem "path" Jsont.string
       ~enc:(fun r -> r.path)
  |> Jsont.Object.opt_mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  last_commit : Jsont.json option;
  mode : string;
  name : string;
  oid : string;
  size : int;
  submodule : Jsont.json option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ last_commit mode name oid size submodule -> { last_commit; mode; name; oid; size; submodule })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.git.getEntry#output") ~enc:(fun _ -> "org.tangled.temp.git.getEntry#output")
  |> Jsont.Object.opt_mem "lastCommit" Jsont.json ~enc:(fun r -> r.last_commit)
  |> Jsont.Object.mem "mode" Jsont.string ~enc:(fun r -> r.mode)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "oid" Jsont.string ~enc:(fun r -> r.oid)
  |> Jsont.Object.mem "size" Jsont.int ~enc:(fun r -> r.size)
  |> Jsont.Object.opt_mem "submodule" Jsont.json ~enc:(fun r -> r.submodule)
  |> Jsont.Object.finish

        end
        module GetBranch = struct
type params = {
  name : string;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun name repo -> {
      name;
      repo;
    })
  |> Jsont.Object.mem "name" Jsont.string
       ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  author : Jsont.json;
  hash : string;
  message : string;
  name : string;
  total_commits : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ author hash message name total_commits -> { author; hash; message; name; total_commits })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.git.getBranch#output") ~enc:(fun _ -> "org.tangled.temp.git.getBranch#output")
  |> Jsont.Object.mem "author" Jsont.json ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "hash" Jsont.string ~enc:(fun r -> r.hash)
  |> Jsont.Object.mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "totalCommits" Jsont.int ~enc:(fun r -> r.total_commits)
  |> Jsont.Object.finish

        end
        module GetBlob = struct
type params = {
  download : bool option;
  path : string;
  ref_ : string option;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun download path ref_ repo -> {
      download;
      path;
      ref_;
      repo;
    })
  |> Jsont.Object.opt_mem "download" Jsont.bool
       ~enc:(fun r -> r.download)
  |> Jsont.Object.mem "path" Jsont.string
       ~enc:(fun r -> r.path)
  |> Jsont.Object.opt_mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = unit
let output_jsont = Jsont.ignore

        end
        module GetArchive = struct
type params = {
  format : string option;
  prefix : string;
  ref_ : string;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun format prefix ref_ repo -> {
      format;
      prefix;
      ref_;
      repo;
    })
  |> Jsont.Object.opt_mem "format" Jsont.string
       ~enc:(fun r -> r.format)
  |> Jsont.Object.mem "prefix" Jsont.string
       ~enc:(fun r -> r.prefix)
  |> Jsont.Object.mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = unit
let output_jsont = Jsont.ignore

        end
        module FormatPatch = struct
type params = {
  from : string;
  repo : string;
  to_ : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun from repo to_ -> {
      from;
      repo;
      to_;
    })
  |> Jsont.Object.mem "from" Jsont.string
       ~enc:(fun r -> r.from)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "to" Jsont.string
       ~enc:(fun r -> r.to_)
  |> Jsont.Object.finish

type output = unit
let output_jsont = Jsont.ignore

        end
        module DeleteBranch = struct
type input = {
  branch : string;
  repo : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ branch repo -> { branch; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.git.deleteBranch#input") ~enc:(fun _ -> "org.tangled.temp.git.deleteBranch#input")
  |> Jsont.Object.mem "branch" Jsont.string ~enc:(fun r -> r.branch)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

        end
        module ListCommitsByRef = struct
type params = {
  include_stats : bool option;
  limit : int option;
  ranges : string list option;
  repo : string;
  skip : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun include_stats limit ranges repo skip -> {
      include_stats;
      limit;
      ranges;
      repo;
      skip;
    })
  |> Jsont.Object.opt_mem "includeStats" Jsont.bool
       ~enc:(fun r -> r.include_stats)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "ranges" (Jsont.list Jsont.string)
       ~enc:(fun r -> r.ranges)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.opt_mem "skip" Jsont.int
       ~enc:(fun r -> r.skip)
  |> Jsont.Object.finish

type output = {
  commits : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ commits -> { commits })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.git.listCommitsByRef#output") ~enc:(fun _ -> "org.tangled.temp.git.listCommitsByRef#output")
  |> Jsont.Object.mem "commits" (Jsont.list Jsont.json) ~enc:(fun r -> r.commits)
  |> Jsont.Object.finish

        end
        module GetTag = struct
type params = {
  repo : string;
  tag : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun repo tag -> {
      repo;
      tag;
    })
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "tag" Jsont.string
       ~enc:(fun r -> r.tag)
  |> Jsont.Object.finish

type output = {
  annotation : Jsont.json option;
  author : Jsont.json;
  message : string;
  name : string;
  oid : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ annotation author message name oid -> { annotation; author; message; name; oid })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.git.getTag#output") ~enc:(fun _ -> "org.tangled.temp.git.getTag#output")
  |> Jsont.Object.opt_mem "annotation" Jsont.json ~enc:(fun r -> r.annotation)
  |> Jsont.Object.mem "author" Jsont.json ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "oid" Jsont.string ~enc:(fun r -> r.oid)
  |> Jsont.Object.finish

        end
        module GetRef = struct
type params = {
  ref_ : string;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun ref_ repo -> {
      ref_;
      repo;
    })
  |> Jsont.Object.mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  annotation : Jsont.json option;
  author : Jsont.json;
  message : string;
  name : string;
  oid : string;
  total_commits : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ annotation author message name oid total_commits -> { annotation; author; message; name; oid; total_commits })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.git.getRef#output") ~enc:(fun _ -> "org.tangled.temp.git.getRef#output")
  |> Jsont.Object.opt_mem "annotation" Jsont.json ~enc:(fun r -> r.annotation)
  |> Jsont.Object.mem "author" Jsont.json ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "oid" Jsont.string ~enc:(fun r -> r.oid)
  |> Jsont.Object.mem "totalCommits" Jsont.int ~enc:(fun r -> r.total_commits)
  |> Jsont.Object.finish

        end
      end
      module Focus = struct
        module NextItem = struct
type input = {
  current_id : int;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ current_id -> { current_id })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.focus.nextItem#input") ~enc:(fun _ -> "org.tangled.temp.focus.nextItem#input")
  |> Jsont.Object.mem "currentId" Jsont.int ~enc:(fun r -> r.current_id)
  |> Jsont.Object.finish

type output = {
  issue_at : string option;
  notification_id : int option;
  pull_at : string option;
  repo_did : string option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ issue_at notification_id pull_at repo_did -> { issue_at; notification_id; pull_at; repo_did })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.focus.nextItem#output") ~enc:(fun _ -> "org.tangled.temp.focus.nextItem#output")
  |> Jsont.Object.opt_mem "issueAt" Jsont.string ~enc:(fun r -> r.issue_at)
  |> Jsont.Object.opt_mem "notificationId" Jsont.int ~enc:(fun r -> r.notification_id)
  |> Jsont.Object.opt_mem "pullAt" Jsont.string ~enc:(fun r -> r.pull_at)
  |> Jsont.Object.opt_mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.finish

        end
        module EndSession = struct
        end
        module BeginSession = struct
type output = {
  issue_at : string option;
  notification_id : int option;
  pull_at : string option;
  repo_did : string option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ issue_at notification_id pull_at repo_did -> { issue_at; notification_id; pull_at; repo_did })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.focus.beginSession#output") ~enc:(fun _ -> "org.tangled.temp.focus.beginSession#output")
  |> Jsont.Object.opt_mem "issueAt" Jsont.string ~enc:(fun r -> r.issue_at)
  |> Jsont.Object.opt_mem "notificationId" Jsont.int ~enc:(fun r -> r.notification_id)
  |> Jsont.Object.opt_mem "pullAt" Jsont.string ~enc:(fun r -> r.pull_at)
  |> Jsont.Object.opt_mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.finish

        end
      end
      module Actor = struct
        module Defs = struct
type onboarding_pref = {
  completed_at : string option;
  nudge_dismissed_at : string option;
  started_at : string;
}

let onboarding_pref_jsont =
  Jsont.Object.map ~kind:"Onboarding_pref"
    (fun _typ completed_at nudge_dismissed_at started_at -> { completed_at; nudge_dismissed_at; started_at })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.actor.defs#onboardingPref") ~enc:(fun _ -> "org.tangled.temp.actor.defs#onboardingPref")
  |> Jsont.Object.opt_mem "completedAt" Jsont.string ~enc:(fun r -> r.completed_at)
  |> Jsont.Object.opt_mem "nudgeDismissedAt" Jsont.string ~enc:(fun r -> r.nudge_dismissed_at)
  |> Jsont.Object.mem "startedAt" Jsont.string ~enc:(fun r -> r.started_at)
  |> Jsont.Object.finish

type recent_issue = {
  open_ : bool;
  owner_handle : string;
  repo_name : string;
  rkey : string;
  title : string;
  visited : string;
}

let recent_issue_jsont =
  Jsont.Object.map ~kind:"Recent_issue"
    (fun _typ open_ owner_handle repo_name rkey title visited -> { open_; owner_handle; repo_name; rkey; title; visited })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.actor.defs#recentIssue") ~enc:(fun _ -> "org.tangled.temp.actor.defs#recentIssue")
  |> Jsont.Object.mem "open" Jsont.bool ~enc:(fun r -> r.open_)
  |> Jsont.Object.mem "ownerHandle" Jsont.string ~enc:(fun r -> r.owner_handle)
  |> Jsont.Object.mem "repoName" Jsont.string ~enc:(fun r -> r.repo_name)
  |> Jsont.Object.mem "rkey" Jsont.string ~enc:(fun r -> r.rkey)
  |> Jsont.Object.mem "title" Jsont.string ~enc:(fun r -> r.title)
  |> Jsont.Object.mem "visited" Jsont.string ~enc:(fun r -> r.visited)
  |> Jsont.Object.finish

type recent_pull = {
  owner_handle : string;
  repo_name : string;
  rkey : string;
  state : string;
  title : string;
  visited : string;
}

let recent_pull_jsont =
  Jsont.Object.map ~kind:"Recent_pull"
    (fun _typ owner_handle repo_name rkey state title visited -> { owner_handle; repo_name; rkey; state; title; visited })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.actor.defs#recentPull") ~enc:(fun _ -> "org.tangled.temp.actor.defs#recentPull")
  |> Jsont.Object.mem "ownerHandle" Jsont.string ~enc:(fun r -> r.owner_handle)
  |> Jsont.Object.mem "repoName" Jsont.string ~enc:(fun r -> r.repo_name)
  |> Jsont.Object.mem "rkey" Jsont.string ~enc:(fun r -> r.rkey)
  |> Jsont.Object.mem "state" Jsont.string ~enc:(fun r -> r.state)
  |> Jsont.Object.mem "title" Jsont.string ~enc:(fun r -> r.title)
  |> Jsont.Object.mem "visited" Jsont.string ~enc:(fun r -> r.visited)
  |> Jsont.Object.finish

type recent_repo = {
  name : string;
  owner_did : string option;
  owner_handle : string;
  visited : string;
}

let recent_repo_jsont =
  Jsont.Object.map ~kind:"Recent_repo"
    (fun _typ name owner_did owner_handle visited -> { name; owner_did; owner_handle; visited })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.actor.defs#recentRepo") ~enc:(fun _ -> "org.tangled.temp.actor.defs#recentRepo")
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.opt_mem "ownerDid" Jsont.string ~enc:(fun r -> r.owner_did)
  |> Jsont.Object.mem "ownerHandle" Jsont.string ~enc:(fun r -> r.owner_handle)
  |> Jsont.Object.mem "visited" Jsont.string ~enc:(fun r -> r.visited)
  |> Jsont.Object.finish

type recents_pref = {
  items : Jsont.json list;
}

let recents_pref_jsont =
  Jsont.Object.map ~kind:"Recents_pref"
    (fun _typ items -> { items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.actor.defs#recentsPref") ~enc:(fun _ -> "org.tangled.temp.actor.defs#recentsPref")
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

type saved_service = {
  id : string;
  value : string;
}

let saved_service_jsont =
  Jsont.Object.map ~kind:"Saved_service"
    (fun _typ id value -> { id; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.actor.defs#savedService") ~enc:(fun _ -> "org.tangled.temp.actor.defs#savedService")
  |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
  |> Jsont.Object.mem "value" Jsont.string ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type saved_knot_pref = {
  items : Jsont.json list;
}

let saved_knot_pref_jsont =
  Jsont.Object.map ~kind:"Saved_knot_pref"
    (fun _typ items -> { items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.actor.defs#savedKnotPref") ~enc:(fun _ -> "org.tangled.temp.actor.defs#savedKnotPref")
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

type saved_spindle_pref = {
  items : Jsont.json list;
}

let saved_spindle_pref_jsont =
  Jsont.Object.map ~kind:"Saved_spindle_pref"
    (fun _typ items -> { items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.actor.defs#savedSpindlePref") ~enc:(fun _ -> "org.tangled.temp.actor.defs#savedSpindlePref")
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

        end
      end
      module Account = struct
        module VerifyEmail = struct
type input = {
  token : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ token -> { token })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.account.verifyEmail#input") ~enc:(fun _ -> "org.tangled.temp.account.verifyEmail#input")
  |> Jsont.Object.mem "token" Jsont.string ~enc:(fun r -> r.token)
  |> Jsont.Object.finish

type output = {
  email : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ email -> { email })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.account.verifyEmail#output") ~enc:(fun _ -> "org.tangled.temp.account.verifyEmail#output")
  |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
  |> Jsont.Object.finish

        end
        module SubscribeNewsletter = struct
        end
        module SetPrimaryEmail = struct
type input = {
  email : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ email -> { email })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.account.setPrimaryEmail#input") ~enc:(fun _ -> "org.tangled.temp.account.setPrimaryEmail#input")
  |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
  |> Jsont.Object.finish

        end
        module ResendSignup = struct
type input = {
  email : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ email -> { email })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.account.resendSignup#input") ~enc:(fun _ -> "org.tangled.temp.account.resendSignup#input")
  |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
  |> Jsont.Object.finish

        end
        module ListEmails = struct
type email = {
  address : string;
  created_at : string;
  primary : bool;
  verified : bool;
}

let email_jsont =
  Jsont.Object.map ~kind:"Email"
    (fun _typ address created_at primary verified -> { address; created_at; primary; verified })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.account.listEmails#email") ~enc:(fun _ -> "org.tangled.temp.account.listEmails#email")
  |> Jsont.Object.mem "address" Jsont.string ~enc:(fun r -> r.address)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "primary" Jsont.bool ~enc:(fun r -> r.primary)
  |> Jsont.Object.mem "verified" Jsont.bool ~enc:(fun r -> r.verified)
  |> Jsont.Object.finish

type output = {
  emails : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ emails -> { emails })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.account.listEmails#output") ~enc:(fun _ -> "org.tangled.temp.account.listEmails#output")
  |> Jsont.Object.mem "emails" (Jsont.list Jsont.json) ~enc:(fun r -> r.emails)
  |> Jsont.Object.finish

        end
        module GetSignup = struct
type params = {
  token : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun token -> {
      token;
    })
  |> Jsont.Object.mem "token" Jsont.string
       ~enc:(fun r -> r.token)
  |> Jsont.Object.finish

type output = {
  email : string;
  pds : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ email pds -> { email; pds })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.account.getSignup#output") ~enc:(fun _ -> "org.tangled.temp.account.getSignup#output")
  |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
  |> Jsont.Object.mem "pds" Jsont.string ~enc:(fun r -> r.pds)
  |> Jsont.Object.finish

        end
        module GetNewsletterPreference = struct
type output = {
  status : string option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ status -> { status })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.account.getNewsletterPreference#output") ~enc:(fun _ -> "org.tangled.temp.account.getNewsletterPreference#output")
  |> Jsont.Object.opt_mem "status" Jsont.string ~enc:(fun r -> r.status)
  |> Jsont.Object.finish

        end
        module DismissNewsletter = struct
        end
        module DeleteEmail = struct
type input = {
  email : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ email -> { email })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.account.deleteEmail#input") ~enc:(fun _ -> "org.tangled.temp.account.deleteEmail#input")
  |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
  |> Jsont.Object.finish

        end
        module CompleteSignup = struct
type authorization = {
  code : string;
  iss : string;
  state : string;
}

let authorization_jsont =
  Jsont.Object.map ~kind:"Authorization"
    (fun _typ code iss state -> { code; iss; state })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.account.completeSignup#authorization") ~enc:(fun _ -> "org.tangled.temp.account.completeSignup#authorization")
  |> Jsont.Object.mem "code" Jsont.string ~enc:(fun r -> r.code)
  |> Jsont.Object.mem "iss" Jsont.string ~enc:(fun r -> r.iss)
  |> Jsont.Object.mem "state" Jsont.string ~enc:(fun r -> r.state)
  |> Jsont.Object.finish

type input = {
  password : string;
  request_uri : string option;
  token : string;
  username : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ password request_uri token username -> { password; request_uri; token; username })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.account.completeSignup#input") ~enc:(fun _ -> "org.tangled.temp.account.completeSignup#input")
  |> Jsont.Object.mem "password" Jsont.string ~enc:(fun r -> r.password)
  |> Jsont.Object.opt_mem "requestUri" Jsont.string ~enc:(fun r -> r.request_uri)
  |> Jsont.Object.mem "token" Jsont.string ~enc:(fun r -> r.token)
  |> Jsont.Object.mem "username" Jsont.string ~enc:(fun r -> r.username)
  |> Jsont.Object.finish

type output = {
  authorization : Jsont.json option;
  did : string;
  handle : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ authorization did handle -> { authorization; did; handle })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.account.completeSignup#output") ~enc:(fun _ -> "org.tangled.temp.account.completeSignup#output")
  |> Jsont.Object.opt_mem "authorization" Jsont.json ~enc:(fun r -> r.authorization)
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.mem "handle" Jsont.string ~enc:(fun r -> r.handle)
  |> Jsont.Object.finish

        end
        module BeginSignup = struct
type input = {
  email : string;
  turnstile_token : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ email turnstile_token -> { email; turnstile_token })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.account.beginSignup#input") ~enc:(fun _ -> "org.tangled.temp.account.beginSignup#input")
  |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
  |> Jsont.Object.mem "turnstileToken" Jsont.string ~enc:(fun r -> r.turnstile_token)
  |> Jsont.Object.finish

        end
        module AddEmail = struct
type input = {
  email : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ email -> { email })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.account.addEmail#input") ~enc:(fun _ -> "org.tangled.temp.account.addEmail#input")
  |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
  |> Jsont.Object.finish

        end
      end
    end
    module Feed = struct
      module Subscription = struct
type record = {
  uri : string;
}

let record_jsont =
  Jsont.Object.map ~kind:"Record"
    (fun _typ uri -> { uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.feed.subscription#record") ~enc:(fun _ -> "org.tangled.feed.subscription#record")
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type repo = {
  did : string;
}

let repo_jsont =
  Jsont.Object.map ~kind:"Repo"
    (fun _typ did -> { did })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.feed.subscription#repo") ~enc:(fun _ -> "org.tangled.feed.subscription#repo")
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.finish

type main = {
  created_at : string;
  ignore : bool option;
  subject : Jsont.json;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at ignore subject -> { created_at; ignore; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.feed.subscription") ~enc:(fun _ -> "org.tangled.feed.subscription")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "ignore" Jsont.bool ~enc:(fun r -> r.ignore)
  |> Jsont.Object.mem "subject" Jsont.json ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module Star = struct
type record = {
  record : Jsont.json;
}

let record_jsont =
  Jsont.Object.map ~kind:"Record"
    (fun _typ record -> { record })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.feed.star#record") ~enc:(fun _ -> "org.tangled.feed.star#record")
  |> Jsont.Object.mem "record" Jsont.json ~enc:(fun r -> r.record)
  |> Jsont.Object.finish

type repo = {
  identity : string;
}

let repo_jsont =
  Jsont.Object.map ~kind:"Repo"
    (fun _typ identity -> { identity })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.feed.star#repo") ~enc:(fun _ -> "org.tangled.feed.star#repo")
  |> Jsont.Object.mem "identity" Jsont.string ~enc:(fun r -> r.identity)
  |> Jsont.Object.finish

type main = {
  created_at : string;
  subject : Jsont.json;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at subject -> { created_at; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.feed.star") ~enc:(fun _ -> "org.tangled.feed.star")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "subject" Jsont.json ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module Reaction = struct
type main = {
  created_at : string;
  reaction : string;
  subject : Jsont.json;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at reaction subject -> { created_at; reaction; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.feed.reaction") ~enc:(fun _ -> "org.tangled.feed.reaction")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "reaction" Jsont.string ~enc:(fun r -> r.reaction)
  |> Jsont.Object.mem "subject" Jsont.json ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module ListStars = struct
type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.feed.listStars#listItem") ~enc:(fun _ -> "org.tangled.feed.listStars#listItem")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order subject -> {
      cursor;
      limit;
      offset;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.feed.listStars#output") ~enc:(fun _ -> "org.tangled.feed.listStars#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module ListReactions = struct
type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.feed.listReactions#listItem") ~enc:(fun _ -> "org.tangled.feed.listReactions#listItem")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order subject -> {
      cursor;
      limit;
      offset;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.feed.listReactions#output") ~enc:(fun _ -> "org.tangled.feed.listReactions#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module GetTimeline = struct
type follow_event = {
  actor : Jsont.json;
  cid : string;
  subject : Jsont.json;
  uri : string;
}

let follow_event_jsont =
  Jsont.Object.map ~kind:"Follow_event"
    (fun _typ actor cid subject uri -> { actor; cid; subject; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.feed.getTimeline#followEvent") ~enc:(fun _ -> "org.tangled.feed.getTimeline#followEvent")
  |> Jsont.Object.mem "actor" Jsont.json ~enc:(fun r -> r.actor)
  |> Jsont.Object.mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "subject" Jsont.json ~enc:(fun r -> r.subject)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type repo_event = {
  cid : string;
  repo : Jsont.json;
  source : Jsont.json option;
  uri : string;
}

let repo_event_jsont =
  Jsont.Object.map ~kind:"Repo_event"
    (fun _typ cid repo source uri -> { cid; repo; source; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.feed.getTimeline#repoEvent") ~enc:(fun _ -> "org.tangled.feed.getTimeline#repoEvent")
  |> Jsont.Object.mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "repo" Jsont.json ~enc:(fun r -> r.repo)
  |> Jsont.Object.opt_mem "source" Jsont.json ~enc:(fun r -> r.source)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type star_event = {
  actor : Jsont.json;
  cid : string;
  repo : Jsont.json;
  uri : string;
}

let star_event_jsont =
  Jsont.Object.map ~kind:"Star_event"
    (fun _typ actor cid repo uri -> { actor; cid; repo; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.feed.getTimeline#starEvent") ~enc:(fun _ -> "org.tangled.feed.getTimeline#starEvent")
  |> Jsont.Object.mem "actor" Jsont.json ~enc:(fun r -> r.actor)
  |> Jsont.Object.mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "repo" Jsont.json ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type timeline_item = {
  event : Jsont.json;
  event_at : string;
}

let timeline_item_jsont =
  Jsont.Object.map ~kind:"Timeline_item"
    (fun _typ event event_at -> { event; event_at })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.feed.getTimeline#timelineItem") ~enc:(fun _ -> "org.tangled.feed.getTimeline#timelineItem")
  |> Jsont.Object.mem "event" Jsont.json ~enc:(fun r -> r.event)
  |> Jsont.Object.mem "eventAt" Jsont.string ~enc:(fun r -> r.event_at)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  following_only : bool option;
  limit : int option;
  viewer : string option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor following_only limit viewer -> {
      cursor;
      following_only;
      limit;
      viewer;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "followingOnly" Jsont.bool
       ~enc:(fun r -> r.following_only)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "viewer" Jsont.string
       ~enc:(fun r -> r.viewer)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  feed : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor feed -> { cursor; feed })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.feed.getTimeline#output") ~enc:(fun _ -> "org.tangled.feed.getTimeline#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "feed" (Jsont.list Jsont.json) ~enc:(fun r -> r.feed)
  |> Jsont.Object.finish

      end
      module GetStar = struct
type params = {
  actor : string;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun actor subject -> {
      actor;
      subject;
    })
  |> Jsont.Object.mem "actor" Jsont.string
       ~enc:(fun r -> r.actor)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  uri : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ uri -> { uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.feed.getStar#output") ~enc:(fun _ -> "org.tangled.feed.getStar#output")
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

      end
      module Defs = struct
type comment_view = {
  body : Jsont.json;
  cid : string;
  created_at : string;
  editor : string option;
  embed : Jsont.json option;
  reply_to : Jsont.json option;
  subject : Jsont.json;
  uri : string;
}

let comment_view_jsont =
  Jsont.Object.map ~kind:"Comment_view"
    (fun _typ body cid created_at editor embed reply_to subject uri -> { body; cid; created_at; editor; embed; reply_to; subject; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.feed.defs#commentView") ~enc:(fun _ -> "org.tangled.feed.defs#commentView")
  |> Jsont.Object.mem "body" Jsont.json ~enc:(fun r -> r.body)
  |> Jsont.Object.mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "editor" Jsont.string ~enc:(fun r -> r.editor)
  |> Jsont.Object.opt_mem "embed" Jsont.json ~enc:(fun r -> r.embed)
  |> Jsont.Object.opt_mem "replyTo" Jsont.json ~enc:(fun r -> r.reply_to)
  |> Jsont.Object.mem "subject" Jsont.json ~enc:(fun r -> r.subject)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

      end
      module CountStarsBy = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.feed.countStarsBy#output") ~enc:(fun _ -> "org.tangled.feed.countStarsBy#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module CountStars = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.feed.countStars#output") ~enc:(fun _ -> "org.tangled.feed.countStars#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module CountReactionsBy = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.feed.countReactionsBy#output") ~enc:(fun _ -> "org.tangled.feed.countReactionsBy#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module CountReactions = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.feed.countReactions#output") ~enc:(fun _ -> "org.tangled.feed.countReactions#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module CountCommentsBy = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.feed.countCommentsBy#output") ~enc:(fun _ -> "org.tangled.feed.countCommentsBy#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module CountComments = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.feed.countComments#output") ~enc:(fun _ -> "org.tangled.feed.countComments#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module Comment = struct
type main = {
  body : Jsont.json;
  created_at : string;
  embed : Jsont.json option;
  reply_to : Jsont.json option;
  subject : Jsont.json;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ body created_at embed reply_to subject -> { body; created_at; embed; reply_to; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.feed.comment") ~enc:(fun _ -> "org.tangled.feed.comment")
  |> Jsont.Object.mem "body" Jsont.json ~enc:(fun r -> r.body)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "embed" Jsont.json ~enc:(fun r -> r.embed)
  |> Jsont.Object.opt_mem "replyTo" Jsont.json ~enc:(fun r -> r.reply_to)
  |> Jsont.Object.mem "subject" Jsont.json ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module ListStarsBy = struct
type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order subject -> {
      cursor;
      limit;
      offset;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.feed.listStarsBy#output") ~enc:(fun _ -> "org.tangled.feed.listStarsBy#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module ListReactionsBy = struct
type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order subject -> {
      cursor;
      limit;
      offset;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.feed.listReactionsBy#output") ~enc:(fun _ -> "org.tangled.feed.listReactionsBy#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module ListComments = struct
type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.feed.listComments#listItem") ~enc:(fun _ -> "org.tangled.feed.listComments#listItem")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order subject -> {
      cursor;
      limit;
      offset;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.feed.listComments#output") ~enc:(fun _ -> "org.tangled.feed.listComments#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module ListCommentsBy = struct
type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order subject -> {
      cursor;
      limit;
      offset;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : Jsont.json list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.feed.listCommentsBy#output") ~enc:(fun _ -> "org.tangled.feed.listCommentsBy#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
    end
    module Search = struct
      module Query = struct
type hit = {
  author : string option;
  cid : string option;
  comment_count : int option;
  nsid : string;
  score : Jsont.json;
  state : Jsont.json option;
  uri : string;
  value : Jsont.json;
}

let hit_jsont =
  Jsont.Object.map ~kind:"Hit"
    (fun _typ author cid comment_count nsid score state uri value -> { author; cid; comment_count; nsid; score; state; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.search.query#hit") ~enc:(fun _ -> "org.tangled.search.query#hit")
  |> Jsont.Object.opt_mem "author" Jsont.string ~enc:(fun r -> r.author)
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.opt_mem "commentCount" Jsont.int ~enc:(fun r -> r.comment_count)
  |> Jsont.Object.mem "nsid" Jsont.string ~enc:(fun r -> r.nsid)
  |> Jsont.Object.mem "score" Jsont.json ~enc:(fun r -> r.score)
  |> Jsont.Object.opt_mem "state" Jsont.json ~enc:(fun r -> r.state)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type state_counts = {
  closed : int;
  completed : int;
  open_ : int;
}

let state_counts_jsont =
  Jsont.Object.map ~kind:"State_counts"
    (fun _typ closed completed open_ -> { closed; completed; open_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.search.query#stateCounts") ~enc:(fun _ -> "org.tangled.search.query#stateCounts")
  |> Jsont.Object.mem "closed" Jsont.int ~enc:(fun r -> r.closed)
  |> Jsont.Object.mem "completed" Jsont.int ~enc:(fun r -> r.completed)
  |> Jsont.Object.mem "open" Jsont.int ~enc:(fun r -> r.open_)
  |> Jsont.Object.finish

type params = {
  author : string option;
  cursor : string option;
  kind : string option;
  label : string list option;
  limit : int option;
  nsid : string option;
  offset : int option;
  q : string option;
  repo : string option;
  since : string option;
  sort : string option;
  until : string option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun author cursor kind label limit nsid offset q repo since sort until -> {
      author;
      cursor;
      kind;
      label;
      limit;
      nsid;
      offset;
      q;
      repo;
      since;
      sort;
      until;
    })
  |> Jsont.Object.opt_mem "author" Jsont.string
       ~enc:(fun r -> r.author)
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "kind" Jsont.string
       ~enc:(fun r -> r.kind)
  |> Jsont.Object.opt_mem "label" (Jsont.list Jsont.string)
       ~enc:(fun r -> r.label)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "nsid" Jsont.string
       ~enc:(fun r -> r.nsid)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "q" Jsont.string
       ~enc:(fun r -> r.q)
  |> Jsont.Object.opt_mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.opt_mem "since" Jsont.string
       ~enc:(fun r -> r.since)
  |> Jsont.Object.opt_mem "sort" Jsont.string
       ~enc:(fun r -> r.sort)
  |> Jsont.Object.opt_mem "until" Jsont.string
       ~enc:(fun r -> r.until)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  hits : Jsont.json list;
  state_counts : Jsont.json option;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor hits state_counts total -> { cursor; hits; state_counts; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.search.query#output") ~enc:(fun _ -> "org.tangled.search.query#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "hits" (Jsont.list Jsont.json) ~enc:(fun r -> r.hits)
  |> Jsont.Object.opt_mem "stateCounts" Jsont.json ~enc:(fun r -> r.state_counts)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
    end
  end
end
module Com = struct
  module Bad_example = struct
    module Pocket = struct
      module Defs = struct
type preferences = Jsont.json list
let preferences_jsont = (Jsont.list Jsont.json)

      end
      module PutPreferences = struct
type input = {
  preferences : Jsont.json;
  swap_rev : string option option;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ preferences swap_rev -> { preferences; swap_rev })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "com.bad-example.pocket.putPreferences#input") ~enc:(fun _ -> "com.bad-example.pocket.putPreferences#input")
  |> Jsont.Object.mem "preferences" Jsont.json ~enc:(fun r -> r.preferences)
  |> Jsont.Object.opt_mem "swapRev" (Jsont.option Jsont.string) ~enc:(fun r -> r.swap_rev)
  |> Jsont.Object.finish

      end
      module GetPreferences = struct
type params = unit

let params_jsont = Jsont.ignore

type output = {
  preferences : Jsont.json;
  rev : string option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ preferences rev -> { preferences; rev })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "com.bad-example.pocket.getPreferences#output") ~enc:(fun _ -> "com.bad-example.pocket.getPreferences#output")
  |> Jsont.Object.mem "preferences" Jsont.json ~enc:(fun r -> r.preferences)
  |> Jsont.Object.opt_mem "rev" Jsont.string ~enc:(fun r -> r.rev)
  |> Jsont.Object.finish

      end
    end
  end
  module Atproto = struct
    module Sync = struct
      module SubscribeRepos = struct
type account = {
  active : bool;
  did : string;
  seq : int;
  status : string option;
  time : string;
}

let account_jsont =
  Jsont.Object.map ~kind:"Account"
    (fun _typ active did seq status time -> { active; did; seq; status; time })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "com.atproto.sync.subscribeRepos#account") ~enc:(fun _ -> "com.atproto.sync.subscribeRepos#account")
  |> Jsont.Object.mem "active" Jsont.bool ~enc:(fun r -> r.active)
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.mem "seq" Jsont.int ~enc:(fun r -> r.seq)
  |> Jsont.Object.opt_mem "status" Jsont.string ~enc:(fun r -> r.status)
  |> Jsont.Object.mem "time" Jsont.string ~enc:(fun r -> r.time)
  |> Jsont.Object.finish

type identity = {
  did : string;
  handle : string option;
  seq : int;
  time : string;
}

let identity_jsont =
  Jsont.Object.map ~kind:"Identity"
    (fun _typ did handle seq time -> { did; handle; seq; time })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "com.atproto.sync.subscribeRepos#identity") ~enc:(fun _ -> "com.atproto.sync.subscribeRepos#identity")
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.opt_mem "handle" Jsont.string ~enc:(fun r -> r.handle)
  |> Jsont.Object.mem "seq" Jsont.int ~enc:(fun r -> r.seq)
  |> Jsont.Object.mem "time" Jsont.string ~enc:(fun r -> r.time)
  |> Jsont.Object.finish

type info = {
  message : string option;
  name : string;
}

let info_jsont =
  Jsont.Object.map ~kind:"Info"
    (fun _typ message name -> { message; name })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "com.atproto.sync.subscribeRepos#info") ~enc:(fun _ -> "com.atproto.sync.subscribeRepos#info")
  |> Jsont.Object.opt_mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.finish

type repo_op = {
  action : string;
  cid : Atp.Cid.t option;
  path : string;
  prev : Atp.Cid.t option;
}

let repo_op_jsont =
  Jsont.Object.map ~kind:"Repo_op"
    (fun _typ action cid path prev -> { action; cid; path; prev })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "com.atproto.sync.subscribeRepos#repoOp") ~enc:(fun _ -> "com.atproto.sync.subscribeRepos#repoOp")
  |> Jsont.Object.mem "action" Jsont.string ~enc:(fun r -> r.action)
  |> Jsont.Object.mem "cid" (Jsont.option Atp.Cid.jsont) ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "path" Jsont.string ~enc:(fun r -> r.path)
  |> Jsont.Object.opt_mem "prev" Atp.Cid.jsont ~enc:(fun r -> r.prev)
  |> Jsont.Object.finish

type sync = {
  blocks : string;
  did : string;
  rev : string;
  seq : int;
  time : string;
}

let sync_jsont =
  Jsont.Object.map ~kind:"Sync"
    (fun _typ blocks did rev seq time -> { blocks; did; rev; seq; time })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "com.atproto.sync.subscribeRepos#sync") ~enc:(fun _ -> "com.atproto.sync.subscribeRepos#sync")
  |> Jsont.Object.mem "blocks" Atp.Lex.bytes_jsont ~enc:(fun r -> r.blocks)
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.mem "rev" Jsont.string ~enc:(fun r -> r.rev)
  |> Jsont.Object.mem "seq" Jsont.int ~enc:(fun r -> r.seq)
  |> Jsont.Object.mem "time" Jsont.string ~enc:(fun r -> r.time)
  |> Jsont.Object.finish

type commit = {
  blobs : Atp.Cid.t list;
  blocks : string;
  commit : Atp.Cid.t;
  ops : Jsont.json list;
  prev_data : Atp.Cid.t option;
  rebase : bool;
  repo : string;
  rev : string;
  seq : int;
  since : string option;
  time : string;
  too_big : bool;
}

let commit_jsont =
  Jsont.Object.map ~kind:"Commit"
    (fun _typ blobs blocks commit ops prev_data rebase repo rev seq since time too_big -> { blobs; blocks; commit; ops; prev_data; rebase; repo; rev; seq; since; time; too_big })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "com.atproto.sync.subscribeRepos#commit") ~enc:(fun _ -> "com.atproto.sync.subscribeRepos#commit")
  |> Jsont.Object.mem "blobs" (Jsont.list Atp.Cid.jsont) ~enc:(fun r -> r.blobs)
  |> Jsont.Object.mem "blocks" Atp.Lex.bytes_jsont ~enc:(fun r -> r.blocks)
  |> Jsont.Object.mem "commit" Atp.Cid.jsont ~enc:(fun r -> r.commit)
  |> Jsont.Object.mem "ops" (Jsont.list Jsont.json) ~enc:(fun r -> r.ops)
  |> Jsont.Object.opt_mem "prevData" Atp.Cid.jsont ~enc:(fun r -> r.prev_data)
  |> Jsont.Object.mem "rebase" Jsont.bool ~enc:(fun r -> r.rebase)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "rev" Jsont.string ~enc:(fun r -> r.rev)
  |> Jsont.Object.mem "seq" Jsont.int ~enc:(fun r -> r.seq)
  |> Jsont.Object.mem "since" (Jsont.option Jsont.string) ~enc:(fun r -> r.since)
  |> Jsont.Object.mem "time" Jsont.string ~enc:(fun r -> r.time)
  |> Jsont.Object.mem "tooBig" Jsont.bool ~enc:(fun r -> r.too_big)
  |> Jsont.Object.finish

type params = {
  cursor : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor -> {
      cursor;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.int
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.finish

type message = Jsont.json

let message_jsont = Jsont.json

      end
    end
    module Repo = struct
      module StrongRef = struct
type main = {
  cid : string;
  uri : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ cid uri -> { cid; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "com.atproto.repo.strongRef") ~enc:(fun _ -> "com.atproto.repo.strongRef")
  |> Jsont.Object.mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

      end
    end
    module Label = struct
      module Defs = struct
type label = {
  cid : string option;
  cts : string;
  exp : string option;
  neg : bool option;
  sig_ : string option;
  src : string;
  uri : string;
  val_ : string;
  ver : int option;
}

let label_jsont =
  Jsont.Object.map ~kind:"Label"
    (fun _typ cid cts exp neg sig_ src uri val_ ver -> { cid; cts; exp; neg; sig_; src; uri; val_; ver })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "com.atproto.label.defs#label") ~enc:(fun _ -> "com.atproto.label.defs#label")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "cts" Jsont.string ~enc:(fun r -> r.cts)
  |> Jsont.Object.opt_mem "exp" Jsont.string ~enc:(fun r -> r.exp)
  |> Jsont.Object.opt_mem "neg" Jsont.bool ~enc:(fun r -> r.neg)
  |> Jsont.Object.opt_mem "sig" Atp.Lex.bytes_jsont ~enc:(fun r -> r.sig_)
  |> Jsont.Object.mem "src" Jsont.string ~enc:(fun r -> r.src)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "val" Jsont.string ~enc:(fun r -> r.val_)
  |> Jsont.Object.opt_mem "ver" Jsont.int ~enc:(fun r -> r.ver)
  |> Jsont.Object.finish

type label_value = string
let label_value_jsont = Jsont.string

type label_value_definition_strings = {
  description : string;
  lang : string;
  name : string;
}

let label_value_definition_strings_jsont =
  Jsont.Object.map ~kind:"Label_value_definition_strings"
    (fun _typ description lang name -> { description; lang; name })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "com.atproto.label.defs#labelValueDefinitionStrings") ~enc:(fun _ -> "com.atproto.label.defs#labelValueDefinitionStrings")
  |> Jsont.Object.mem "description" Jsont.string ~enc:(fun r -> r.description)
  |> Jsont.Object.mem "lang" Jsont.string ~enc:(fun r -> r.lang)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.finish

type self_label = {
  val_ : string;
}

let self_label_jsont =
  Jsont.Object.map ~kind:"Self_label"
    (fun _typ val_ -> { val_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "com.atproto.label.defs#selfLabel") ~enc:(fun _ -> "com.atproto.label.defs#selfLabel")
  |> Jsont.Object.mem "val" Jsont.string ~enc:(fun r -> r.val_)
  |> Jsont.Object.finish

type label_value_definition = {
  adult_only : bool option;
  blurs : string;
  default_setting : string option;
  identifier : string;
  locales : Jsont.json list;
  severity : string;
}

let label_value_definition_jsont =
  Jsont.Object.map ~kind:"Label_value_definition"
    (fun _typ adult_only blurs default_setting identifier locales severity -> { adult_only; blurs; default_setting; identifier; locales; severity })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "com.atproto.label.defs#labelValueDefinition") ~enc:(fun _ -> "com.atproto.label.defs#labelValueDefinition")
  |> Jsont.Object.opt_mem "adultOnly" Jsont.bool ~enc:(fun r -> r.adult_only)
  |> Jsont.Object.mem "blurs" Jsont.string ~enc:(fun r -> r.blurs)
  |> Jsont.Object.opt_mem "defaultSetting" Jsont.string ~enc:(fun r -> r.default_setting)
  |> Jsont.Object.mem "identifier" Jsont.string ~enc:(fun r -> r.identifier)
  |> Jsont.Object.mem "locales" (Jsont.list Jsont.json) ~enc:(fun r -> r.locales)
  |> Jsont.Object.mem "severity" Jsont.string ~enc:(fun r -> r.severity)
  |> Jsont.Object.finish

type self_labels = {
  values : Jsont.json list;
}

let self_labels_jsont =
  Jsont.Object.map ~kind:"Self_labels"
    (fun _typ values -> { values })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "com.atproto.label.defs#selfLabels") ~enc:(fun _ -> "com.atproto.label.defs#selfLabels")
  |> Jsont.Object.mem "values" (Jsont.list Jsont.json) ~enc:(fun r -> r.values)
  |> Jsont.Object.finish

      end
      module SubscribeLabels = struct
type info = {
  message : string option;
  name : string;
}

let info_jsont =
  Jsont.Object.map ~kind:"Info"
    (fun _typ message name -> { message; name })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "com.atproto.label.subscribeLabels#info") ~enc:(fun _ -> "com.atproto.label.subscribeLabels#info")
  |> Jsont.Object.opt_mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.finish

type labels = {
  labels : Jsont.json list;
  seq : int;
}

let labels_jsont =
  Jsont.Object.map ~kind:"Labels"
    (fun _typ labels seq -> { labels; seq })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "com.atproto.label.subscribeLabels#labels") ~enc:(fun _ -> "com.atproto.label.subscribeLabels#labels")
  |> Jsont.Object.mem "labels" (Jsont.list Jsont.json) ~enc:(fun r -> r.labels)
  |> Jsont.Object.mem "seq" Jsont.int ~enc:(fun r -> r.seq)
  |> Jsont.Object.finish

type params = {
  cursor : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor -> {
      cursor;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.int
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.finish

type message = Jsont.json

let message_jsont = Jsont.json

      end
    end
  end
end
module Sh = struct
  module Tangled = struct
    module Sync = struct
      module RequestCrawl = struct
type input = {
  ensure_repo : string option;
  hostname : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ ensure_repo hostname -> { ensure_repo; hostname })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.sync.requestCrawl#input") ~enc:(fun _ -> "sh.tangled.sync.requestCrawl#input")
  |> Jsont.Object.opt_mem "ensureRepo" Jsont.string ~enc:(fun r -> r.ensure_repo)
  |> Jsont.Object.mem "hostname" Jsont.string ~enc:(fun r -> r.hostname)
  |> Jsont.Object.finish

      end
      module ListRepos = struct
type default_branch = {
  head : string option;
  ref_ : string;
}

let default_branch_jsont =
  Jsont.Object.map ~kind:"Default_branch"
    (fun _typ head ref_ -> { head; ref_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.sync.listRepos#defaultBranch") ~enc:(fun _ -> "sh.tangled.sync.listRepos#defaultBranch")
  |> Jsont.Object.opt_mem "head" Jsont.string ~enc:(fun r -> r.head)
  |> Jsont.Object.mem "ref" Jsont.string ~enc:(fun r -> r.ref_)
  |> Jsont.Object.finish

type repo = {
  default_branch : default_branch option;
  repo : string;
  status : string;
}

let repo_jsont =
  Jsont.Object.map ~kind:"Repo"
    (fun _typ default_branch repo status -> { default_branch; repo; status })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.sync.listRepos#repo") ~enc:(fun _ -> "sh.tangled.sync.listRepos#repo")
  |> Jsont.Object.opt_mem "defaultBranch" default_branch_jsont ~enc:(fun r -> r.default_branch)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "status" Jsont.string ~enc:(fun r -> r.status)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  order : string option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit order -> {
      cursor;
      limit;
      order;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  repos : repo list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor repos -> { cursor; repos })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.sync.listRepos#output") ~enc:(fun _ -> "sh.tangled.sync.listRepos#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "repos" (Jsont.list repo_jsont) ~enc:(fun r -> r.repos)
  |> Jsont.Object.finish

      end
    end
    module String = struct
type main = {
  contents : string;
  created_at : string;
  description : string;
  filename : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ contents created_at description filename -> { contents; created_at; description; filename })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.string") ~enc:(fun _ -> "sh.tangled.string")
  |> Jsont.Object.mem "contents" Jsont.string ~enc:(fun r -> r.contents)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "description" Jsont.string ~enc:(fun r -> r.description)
  |> Jsont.Object.mem "filename" Jsont.string ~enc:(fun r -> r.filename)
  |> Jsont.Object.finish

    end
    module Spindle = struct
type main = {
  created_at : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at -> { created_at })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.spindle") ~enc:(fun _ -> "sh.tangled.spindle")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.finish

      module Member = struct
type main = {
  created_at : string;
  instance : string;
  subject : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at instance subject -> { created_at; instance; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.spindle.member") ~enc:(fun _ -> "sh.tangled.spindle.member")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "instance" Jsont.string ~enc:(fun r -> r.instance)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module ListSpindles = struct
type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.spindle.listSpindles#listItem") ~enc:(fun _ -> "sh.tangled.spindle.listSpindles#listItem")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order subject -> {
      cursor;
      limit;
      offset;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : list_item list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.spindle.listSpindles#output") ~enc:(fun _ -> "sh.tangled.spindle.listSpindles#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module ListMembers = struct
type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.spindle.listMembers#listItem") ~enc:(fun _ -> "sh.tangled.spindle.listMembers#listItem")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order subject -> {
      cursor;
      limit;
      offset;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : list_item list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.spindle.listMembers#output") ~enc:(fun _ -> "sh.tangled.spindle.listMembers#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module CountSpindles = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.spindle.countSpindles#output") ~enc:(fun _ -> "sh.tangled.spindle.countSpindles#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module CountMembersBy = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.spindle.countMembersBy#output") ~enc:(fun _ -> "sh.tangled.spindle.countMembersBy#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module CountMembers = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.spindle.countMembers#output") ~enc:(fun _ -> "sh.tangled.spindle.countMembers#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module ListMembersBy = struct
type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order subject -> {
      cursor;
      limit;
      offset;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : ListMembers.list_item list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.spindle.listMembersBy#output") ~enc:(fun _ -> "sh.tangled.spindle.listMembersBy#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list ListMembers.list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
    end
    module PublicKey = struct
type main = {
  created_at : string;
  key : string;
  name : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at key name -> { created_at; key; name })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.publicKey") ~enc:(fun _ -> "sh.tangled.publicKey")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.finish

    end
    module Pipeline = struct
type clone_opts = {
  depth : int;
  skip : bool;
  submodules : bool;
  tags : bool;
}

let clone_opts_jsont =
  Jsont.Object.map ~kind:"Clone_opts"
    (fun _typ depth skip submodules tags -> { depth; skip; submodules; tags })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pipeline#cloneOpts") ~enc:(fun _ -> "sh.tangled.pipeline#cloneOpts")
  |> Jsont.Object.mem "depth" Jsont.int ~enc:(fun r -> r.depth)
  |> Jsont.Object.mem "skip" Jsont.bool ~enc:(fun r -> r.skip)
  |> Jsont.Object.mem "submodules" Jsont.bool ~enc:(fun r -> r.submodules)
  |> Jsont.Object.mem "tags" Jsont.bool ~enc:(fun r -> r.tags)
  |> Jsont.Object.finish

type pair = {
  key : string;
  value : string;
}

let pair_jsont =
  Jsont.Object.map ~kind:"Pair"
    (fun _typ key value -> { key; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pipeline#pair") ~enc:(fun _ -> "sh.tangled.pipeline#pair")
  |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
  |> Jsont.Object.mem "value" Jsont.string ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type pull_request_trigger_data = {
  action : string option;
  pull : string option;
  source_branch : string;
  source_sha : string;
  target_branch : string;
}

let pull_request_trigger_data_jsont =
  Jsont.Object.map ~kind:"Pull_request_trigger_data"
    (fun _typ action pull source_branch source_sha target_branch -> { action; pull; source_branch; source_sha; target_branch })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pipeline#pullRequestTriggerData") ~enc:(fun _ -> "sh.tangled.pipeline#pullRequestTriggerData")
  |> Jsont.Object.opt_mem "action" Jsont.string ~enc:(fun r -> r.action)
  |> Jsont.Object.opt_mem "pull" Jsont.string ~enc:(fun r -> r.pull)
  |> Jsont.Object.mem "sourceBranch" Jsont.string ~enc:(fun r -> r.source_branch)
  |> Jsont.Object.mem "sourceSha" Jsont.string ~enc:(fun r -> r.source_sha)
  |> Jsont.Object.mem "targetBranch" Jsont.string ~enc:(fun r -> r.target_branch)
  |> Jsont.Object.finish

type push_trigger_data = {
  new_sha : string;
  old_sha : string;
  ref_ : string;
}

let push_trigger_data_jsont =
  Jsont.Object.map ~kind:"Push_trigger_data"
    (fun _typ new_sha old_sha ref_ -> { new_sha; old_sha; ref_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pipeline#pushTriggerData") ~enc:(fun _ -> "sh.tangled.pipeline#pushTriggerData")
  |> Jsont.Object.mem "newSha" Jsont.string ~enc:(fun r -> r.new_sha)
  |> Jsont.Object.mem "oldSha" Jsont.string ~enc:(fun r -> r.old_sha)
  |> Jsont.Object.mem "ref" Jsont.string ~enc:(fun r -> r.ref_)
  |> Jsont.Object.finish

type schedule_trigger_data = {
  ref_ : string;
  scheduled_at : string;
  sha : string;
}

let schedule_trigger_data_jsont =
  Jsont.Object.map ~kind:"Schedule_trigger_data"
    (fun _typ ref_ scheduled_at sha -> { ref_; scheduled_at; sha })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pipeline#scheduleTriggerData") ~enc:(fun _ -> "sh.tangled.pipeline#scheduleTriggerData")
  |> Jsont.Object.mem "ref" Jsont.string ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "scheduledAt" Jsont.string ~enc:(fun r -> r.scheduled_at)
  |> Jsont.Object.mem "sha" Jsont.string ~enc:(fun r -> r.sha)
  |> Jsont.Object.finish

type trigger_repo = {
  default_branch : string;
  did : string;
  knot : string;
  repo : string option;
  repo_did : string option;
}

let trigger_repo_jsont =
  Jsont.Object.map ~kind:"Trigger_repo"
    (fun _typ default_branch did knot repo repo_did -> { default_branch; did; knot; repo; repo_did })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pipeline#triggerRepo") ~enc:(fun _ -> "sh.tangled.pipeline#triggerRepo")
  |> Jsont.Object.mem "defaultBranch" Jsont.string ~enc:(fun r -> r.default_branch)
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.mem "knot" Jsont.string ~enc:(fun r -> r.knot)
  |> Jsont.Object.opt_mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.opt_mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.finish

type manual_trigger_data = {
  inputs : pair list option;
  ref_ : string option;
  sha : string;
}

let manual_trigger_data_jsont =
  Jsont.Object.map ~kind:"Manual_trigger_data"
    (fun _typ inputs ref_ sha -> { inputs; ref_; sha })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pipeline#manualTriggerData") ~enc:(fun _ -> "sh.tangled.pipeline#manualTriggerData")
  |> Jsont.Object.opt_mem "inputs" (Jsont.list pair_jsont) ~enc:(fun r -> r.inputs)
  |> Jsont.Object.opt_mem "ref" Jsont.string ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "sha" Jsont.string ~enc:(fun r -> r.sha)
  |> Jsont.Object.finish

type workflow = {
  audience : string option;
  clone : clone_opts;
  engine : string;
  name : string;
  raw : string;
  runs_on : string list option;
}

let workflow_jsont =
  Jsont.Object.map ~kind:"Workflow"
    (fun _typ audience clone engine name raw runs_on -> { audience; clone; engine; name; raw; runs_on })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pipeline#workflow") ~enc:(fun _ -> "sh.tangled.pipeline#workflow")
  |> Jsont.Object.opt_mem "audience" Jsont.string ~enc:(fun r -> r.audience)
  |> Jsont.Object.mem "clone" clone_opts_jsont ~enc:(fun r -> r.clone)
  |> Jsont.Object.mem "engine" Jsont.string ~enc:(fun r -> r.engine)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "raw" Jsont.string ~enc:(fun r -> r.raw)
  |> Jsont.Object.opt_mem "runsOn" (Jsont.list Jsont.string) ~enc:(fun r -> r.runs_on)
  |> Jsont.Object.finish

type trigger_metadata = {
  actor : string option;
  kind : string;
  manual : manual_trigger_data option;
  pull_request : pull_request_trigger_data option;
  push : push_trigger_data option;
  repo : trigger_repo;
  schedule : schedule_trigger_data option;
  source_repo : string option;
}

let trigger_metadata_jsont =
  Jsont.Object.map ~kind:"Trigger_metadata"
    (fun _typ actor kind manual pull_request push repo schedule source_repo -> { actor; kind; manual; pull_request; push; repo; schedule; source_repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pipeline#triggerMetadata") ~enc:(fun _ -> "sh.tangled.pipeline#triggerMetadata")
  |> Jsont.Object.opt_mem "actor" Jsont.string ~enc:(fun r -> r.actor)
  |> Jsont.Object.mem "kind" Jsont.string ~enc:(fun r -> r.kind)
  |> Jsont.Object.opt_mem "manual" manual_trigger_data_jsont ~enc:(fun r -> r.manual)
  |> Jsont.Object.opt_mem "pullRequest" pull_request_trigger_data_jsont ~enc:(fun r -> r.pull_request)
  |> Jsont.Object.opt_mem "push" push_trigger_data_jsont ~enc:(fun r -> r.push)
  |> Jsont.Object.mem "repo" trigger_repo_jsont ~enc:(fun r -> r.repo)
  |> Jsont.Object.opt_mem "schedule" schedule_trigger_data_jsont ~enc:(fun r -> r.schedule)
  |> Jsont.Object.opt_mem "sourceRepo" Jsont.string ~enc:(fun r -> r.source_repo)
  |> Jsont.Object.finish

type main = {
  trigger_metadata : trigger_metadata;
  workflows : workflow list;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ trigger_metadata workflows -> { trigger_metadata; workflows })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pipeline") ~enc:(fun _ -> "sh.tangled.pipeline")
  |> Jsont.Object.mem "triggerMetadata" trigger_metadata_jsont ~enc:(fun r -> r.trigger_metadata)
  |> Jsont.Object.mem "workflows" (Jsont.list workflow_jsont) ~enc:(fun r -> r.workflows)
  |> Jsont.Object.finish

      module Status = struct
type main = {
  created_at : string;
  error : string option;
  exit_code : int option;
  pipeline : string;
  status : string;
  workflow : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at error exit_code pipeline status workflow -> { created_at; error; exit_code; pipeline; status; workflow })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pipeline.status") ~enc:(fun _ -> "sh.tangled.pipeline.status")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "error" Jsont.string ~enc:(fun r -> r.error)
  |> Jsont.Object.opt_mem "exitCode" Jsont.int ~enc:(fun r -> r.exit_code)
  |> Jsont.Object.mem "pipeline" Jsont.string ~enc:(fun r -> r.pipeline)
  |> Jsont.Object.mem "status" Jsont.string ~enc:(fun r -> r.status)
  |> Jsont.Object.mem "workflow" Jsont.string ~enc:(fun r -> r.workflow)
  |> Jsont.Object.finish

      end
      module ListStatuses = struct
type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pipeline.listStatuses#listItem") ~enc:(fun _ -> "sh.tangled.pipeline.listStatuses#listItem")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order subject -> {
      cursor;
      limit;
      offset;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : list_item list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pipeline.listStatuses#output") ~enc:(fun _ -> "sh.tangled.pipeline.listStatuses#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module ListPipelines = struct
type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pipeline.listPipelines#listItem") ~enc:(fun _ -> "sh.tangled.pipeline.listPipelines#listItem")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order subject -> {
      cursor;
      limit;
      offset;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : list_item list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pipeline.listPipelines#output") ~enc:(fun _ -> "sh.tangled.pipeline.listPipelines#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module CountStatusesBy = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pipeline.countStatusesBy#output") ~enc:(fun _ -> "sh.tangled.pipeline.countStatusesBy#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module CountStatuses = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pipeline.countStatuses#output") ~enc:(fun _ -> "sh.tangled.pipeline.countStatuses#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module CountPipelinesBy = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pipeline.countPipelinesBy#output") ~enc:(fun _ -> "sh.tangled.pipeline.countPipelinesBy#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module CountPipelines = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pipeline.countPipelines#output") ~enc:(fun _ -> "sh.tangled.pipeline.countPipelines#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module CancelPipeline = struct
type input = {
  pipeline : string;
  repo : string;
  workflow : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ pipeline repo workflow -> { pipeline; repo; workflow })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pipeline.cancelPipeline#input") ~enc:(fun _ -> "sh.tangled.pipeline.cancelPipeline#input")
  |> Jsont.Object.mem "pipeline" Jsont.string ~enc:(fun r -> r.pipeline)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "workflow" Jsont.string ~enc:(fun r -> r.workflow)
  |> Jsont.Object.finish

      end
      module ListStatusesBy = struct
type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order subject -> {
      cursor;
      limit;
      offset;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : ListStatuses.list_item list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pipeline.listStatusesBy#output") ~enc:(fun _ -> "sh.tangled.pipeline.listStatusesBy#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list ListStatuses.list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module ListPipelinesBy = struct
type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order subject -> {
      cursor;
      limit;
      offset;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : ListPipelines.list_item list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pipeline.listPipelinesBy#output") ~enc:(fun _ -> "sh.tangled.pipeline.listPipelinesBy#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list ListPipelines.list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
    end
    module Owner = struct
type output = {
  owner : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ owner -> { owner })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.owner#output") ~enc:(fun _ -> "sh.tangled.owner#output")
  |> Jsont.Object.mem "owner" Jsont.string ~enc:(fun r -> r.owner)
  |> Jsont.Object.finish

    end
    module Markup = struct
      module Markdown = struct
type main = {
  blobs : Atp.Blob_ref.t list option;
  original : string option;
  text : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ blobs original text -> { blobs; original; text })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.markup.markdown") ~enc:(fun _ -> "sh.tangled.markup.markdown")
  |> Jsont.Object.opt_mem "blobs" (Jsont.list Atp.Blob_ref.jsont) ~enc:(fun r -> r.blobs)
  |> Jsont.Object.opt_mem "original" Jsont.string ~enc:(fun r -> r.original)
  |> Jsont.Object.mem "text" Jsont.string ~enc:(fun r -> r.text)
  |> Jsont.Object.finish

      end
    end
    module Label = struct
      module Op = struct
type operand = {
  key : string;
  value : string;
}

let operand_jsont =
  Jsont.Object.map ~kind:"Operand"
    (fun _typ key value -> { key; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.label.op#operand") ~enc:(fun _ -> "sh.tangled.label.op#operand")
  |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
  |> Jsont.Object.mem "value" Jsont.string ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type main = {
  add : operand list;
  delete : operand list;
  performed_at : string;
  subject : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ add delete performed_at subject -> { add; delete; performed_at; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.label.op") ~enc:(fun _ -> "sh.tangled.label.op")
  |> Jsont.Object.mem "add" (Jsont.list operand_jsont) ~enc:(fun r -> r.add)
  |> Jsont.Object.mem "delete" (Jsont.list operand_jsont) ~enc:(fun r -> r.delete)
  |> Jsont.Object.mem "performedAt" Jsont.string ~enc:(fun r -> r.performed_at)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module ListOps = struct
type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.label.listOps#listItem") ~enc:(fun _ -> "sh.tangled.label.listOps#listItem")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order subject -> {
      cursor;
      limit;
      offset;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : list_item list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.label.listOps#output") ~enc:(fun _ -> "sh.tangled.label.listOps#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module Definition = struct
type value_type = {
  enum : string list option;
  format : string;
  type_ : string;
}

let value_type_jsont =
  Jsont.Object.map ~kind:"Value_type"
    (fun _typ enum format type_ -> { enum; format; type_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.label.definition#valueType") ~enc:(fun _ -> "sh.tangled.label.definition#valueType")
  |> Jsont.Object.opt_mem "enum" (Jsont.list Jsont.string) ~enc:(fun r -> r.enum)
  |> Jsont.Object.mem "format" Jsont.string ~enc:(fun r -> r.format)
  |> Jsont.Object.mem "type" Jsont.string ~enc:(fun r -> r.type_)
  |> Jsont.Object.finish

type main = {
  color : string option;
  created_at : string;
  multiple : bool option;
  name : string;
  scope : string list;
  value_type : value_type;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ color created_at multiple name scope value_type -> { color; created_at; multiple; name; scope; value_type })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.label.definition") ~enc:(fun _ -> "sh.tangled.label.definition")
  |> Jsont.Object.opt_mem "color" Jsont.string ~enc:(fun r -> r.color)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "multiple" Jsont.bool ~enc:(fun r -> r.multiple)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "scope" (Jsont.list Jsont.string) ~enc:(fun r -> r.scope)
  |> Jsont.Object.mem "valueType" value_type_jsont ~enc:(fun r -> r.value_type)
  |> Jsont.Object.finish

      end
      module CountOpsBy = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.label.countOpsBy#output") ~enc:(fun _ -> "sh.tangled.label.countOpsBy#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module CountOps = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.label.countOps#output") ~enc:(fun _ -> "sh.tangled.label.countOps#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module ListOpsBy = struct
type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order subject -> {
      cursor;
      limit;
      offset;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : ListOps.list_item list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.label.listOpsBy#output") ~enc:(fun _ -> "sh.tangled.label.listOpsBy#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list ListOps.list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
    end
    module Knot = struct
type main = {
  created_at : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at -> { created_at })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.knot") ~enc:(fun _ -> "sh.tangled.knot")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.finish

      module Version = struct
type output = {
  capabilities : string list option;
  version : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ capabilities version -> { capabilities; version })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.knot.version#output") ~enc:(fun _ -> "sh.tangled.knot.version#output")
  |> Jsont.Object.opt_mem "capabilities" (Jsont.list Jsont.string) ~enc:(fun r -> r.capabilities)
  |> Jsont.Object.mem "version" Jsont.string ~enc:(fun r -> r.version)
  |> Jsont.Object.finish

      end
      module Unban = struct
type input = {
  subject : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ subject -> { subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.knot.unban#input") ~enc:(fun _ -> "sh.tangled.knot.unban#input")
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module RemoveMember = struct
type input = {
  subject : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ subject -> { subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.knot.removeMember#input") ~enc:(fun _ -> "sh.tangled.knot.removeMember#input")
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module MemberInvite = struct
type main = {
  created_at : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at -> { created_at })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.knot.memberInvite") ~enc:(fun _ -> "sh.tangled.knot.memberInvite")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.finish

      end
      module MemberAcceptance = struct
type main = {
  created_at : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at -> { created_at })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.knot.memberAcceptance") ~enc:(fun _ -> "sh.tangled.knot.memberAcceptance")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.finish

      end
      module Member = struct
type main = {
  created_at : string;
  domain : string;
  subject : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at domain subject -> { created_at; domain; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.knot.member") ~enc:(fun _ -> "sh.tangled.knot.member")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "domain" Jsont.string ~enc:(fun r -> r.domain)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module ListMembers = struct
type list_item = {
  added_by : string;
  cid : string option;
  created_at : string;
  subject : string;
  uri : string option;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ added_by cid created_at subject uri -> { added_by; cid; created_at; subject; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.knot.listMembers#listItem") ~enc:(fun _ -> "sh.tangled.knot.listMembers#listItem")
  |> Jsont.Object.mem "addedBy" Jsont.string ~enc:(fun r -> r.added_by)
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.opt_mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order subject -> {
      cursor;
      limit;
      offset;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : list_item list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.knot.listMembers#output") ~enc:(fun _ -> "sh.tangled.knot.listMembers#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module ListKnots = struct
type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.knot.listKnots#listItem") ~enc:(fun _ -> "sh.tangled.knot.listKnots#listItem")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order subject -> {
      cursor;
      limit;
      offset;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : list_item list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.knot.listKnots#output") ~enc:(fun _ -> "sh.tangled.knot.listKnots#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module ListKeys = struct
type public_key = {
  created_at : string;
  did : string;
  key : string;
}

let public_key_jsont =
  Jsont.Object.map ~kind:"Public_key"
    (fun _typ created_at did key -> { created_at; did; key })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.knot.listKeys#publicKey") ~enc:(fun _ -> "sh.tangled.knot.listKeys#publicKey")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit -> {
      cursor;
      limit;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  keys : public_key list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor keys -> { cursor; keys })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.knot.listKeys#output") ~enc:(fun _ -> "sh.tangled.knot.listKeys#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "keys" (Jsont.list public_key_jsont) ~enc:(fun r -> r.keys)
  |> Jsont.Object.finish

      end
      module CountMembersBy = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.knot.countMembersBy#output") ~enc:(fun _ -> "sh.tangled.knot.countMembersBy#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module CountMembers = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.knot.countMembers#output") ~enc:(fun _ -> "sh.tangled.knot.countMembers#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module CountKnots = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.knot.countKnots#output") ~enc:(fun _ -> "sh.tangled.knot.countKnots#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module Ban = struct
type input = {
  subject : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ subject -> { subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.knot.ban#input") ~enc:(fun _ -> "sh.tangled.knot.ban#input")
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module AddMember = struct
type input = {
  subject : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ subject -> { subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.knot.addMember#input") ~enc:(fun _ -> "sh.tangled.knot.addMember#input")
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module AcceptMembership = struct
type input = {
  acceptance : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ acceptance -> { acceptance })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.knot.acceptMembership#input") ~enc:(fun _ -> "sh.tangled.knot.acceptMembership#input")
  |> Jsont.Object.mem "acceptance" Jsont.string ~enc:(fun r -> r.acceptance)
  |> Jsont.Object.finish

      end
    end
    module Identity = struct
      module ResolveCommitters = struct
type input = {
  actor : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ actor -> { actor })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.identity.resolveCommitters#input") ~enc:(fun _ -> "sh.tangled.identity.resolveCommitters#input")
  |> Jsont.Object.mem "actor" Jsont.string ~enc:(fun r -> r.actor)
  |> Jsont.Object.finish

type output = {
  committers : string list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ committers -> { committers })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.identity.resolveCommitters#output") ~enc:(fun _ -> "sh.tangled.identity.resolveCommitters#output")
  |> Jsont.Object.mem "committers" (Jsont.list Jsont.string) ~enc:(fun r -> r.committers)
  |> Jsont.Object.finish

      end
    end
    module Graph = struct
      module Vouch = struct
type main = {
  created_at : string;
  evidences : string list option;
  kind : string;
  reason : string option;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at evidences kind reason -> { created_at; evidences; kind; reason })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.graph.vouch") ~enc:(fun _ -> "sh.tangled.graph.vouch")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "evidences" (Jsont.list Jsont.string) ~enc:(fun r -> r.evidences)
  |> Jsont.Object.mem "kind" Jsont.string ~enc:(fun r -> r.kind)
  |> Jsont.Object.opt_mem "reason" Jsont.string ~enc:(fun r -> r.reason)
  |> Jsont.Object.finish

      end
      module ListVouches = struct
type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.graph.listVouches#listItem") ~enc:(fun _ -> "sh.tangled.graph.listVouches#listItem")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order subject -> {
      cursor;
      limit;
      offset;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : list_item list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.graph.listVouches#output") ~enc:(fun _ -> "sh.tangled.graph.listVouches#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module Follow = struct
type main = {
  created_at : string;
  subject : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at subject -> { created_at; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.graph.follow") ~enc:(fun _ -> "sh.tangled.graph.follow")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module CountVouchesBy = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.graph.countVouchesBy#output") ~enc:(fun _ -> "sh.tangled.graph.countVouchesBy#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module CountVouches = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.graph.countVouches#output") ~enc:(fun _ -> "sh.tangled.graph.countVouches#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module ListVouchesBy = struct
type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order subject -> {
      cursor;
      limit;
      offset;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : ListVouches.list_item list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.graph.listVouchesBy#output") ~enc:(fun _ -> "sh.tangled.graph.listVouchesBy#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list ListVouches.list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
    end
    module Git = struct
      module Temp = struct
        module ListTags = struct
type params = {
  cursor : string option;
  limit : int option;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit repo -> {
      cursor;
      limit;
      repo;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = unit
let output_jsont = Jsont.ignore

        end
        module ListLanguages = struct
type language = {
  name : string;
  size : int;
}

let language_jsont =
  Jsont.Object.map ~kind:"Language"
    (fun _typ name size -> { name; size })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp.listLanguages#language") ~enc:(fun _ -> "sh.tangled.git.temp.listLanguages#language")
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "size" Jsont.int ~enc:(fun r -> r.size)
  |> Jsont.Object.finish

type params = {
  indexed_only : bool option;
  ref_ : string option;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun indexed_only ref_ repo -> {
      indexed_only;
      ref_;
      repo;
    })
  |> Jsont.Object.opt_mem "indexedOnly" Jsont.bool
       ~enc:(fun r -> r.indexed_only)
  |> Jsont.Object.opt_mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  languages : language list;
  ref_ : string;
  total : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ languages ref_ total -> { languages; ref_; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp.listLanguages#output") ~enc:(fun _ -> "sh.tangled.git.temp.listLanguages#output")
  |> Jsont.Object.mem "languages" (Jsont.list language_jsont) ~enc:(fun r -> r.languages)
  |> Jsont.Object.mem "ref" Jsont.string ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

        end
        module ListCommits = struct
type params = {
  cursor : string option;
  limit : int option;
  ref_ : string option;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit ref_ repo -> {
      cursor;
      limit;
      ref_;
      repo;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = unit
let output_jsont = Jsont.ignore

        end
        module ListBranches = struct
type params = {
  cursor : string option;
  limit : int option;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit repo -> {
      cursor;
      limit;
      repo;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = unit
let output_jsont = Jsont.ignore

        end
        module GetTree = struct
type readme = {
  contents : string;
  filename : string;
}

let readme_jsont =
  Jsont.Object.map ~kind:"Readme"
    (fun _typ contents filename -> { contents; filename })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp.getTree#readme") ~enc:(fun _ -> "sh.tangled.git.temp.getTree#readme")
  |> Jsont.Object.mem "contents" Jsont.string ~enc:(fun r -> r.contents)
  |> Jsont.Object.mem "filename" Jsont.string ~enc:(fun r -> r.filename)
  |> Jsont.Object.finish

type signature = {
  email : string;
  name : string;
  when_ : string;
}

let signature_jsont =
  Jsont.Object.map ~kind:"Signature"
    (fun _typ email name when_ -> { email; name; when_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp.getTree#signature") ~enc:(fun _ -> "sh.tangled.git.temp.getTree#signature")
  |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "when" Jsont.string ~enc:(fun r -> r.when_)
  |> Jsont.Object.finish

type last_commit = {
  author : signature option;
  hash : string;
  message : string;
  when_ : string;
}

let last_commit_jsont =
  Jsont.Object.map ~kind:"Last_commit"
    (fun _typ author hash message when_ -> { author; hash; message; when_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp.getTree#lastCommit") ~enc:(fun _ -> "sh.tangled.git.temp.getTree#lastCommit")
  |> Jsont.Object.opt_mem "author" signature_jsont ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "hash" Jsont.string ~enc:(fun r -> r.hash)
  |> Jsont.Object.mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.mem "when" Jsont.string ~enc:(fun r -> r.when_)
  |> Jsont.Object.finish

type tree_entry = {
  last_commit : last_commit option;
  mode : string;
  name : string;
  size : int;
}

let tree_entry_jsont =
  Jsont.Object.map ~kind:"Tree_entry"
    (fun _typ last_commit mode name size -> { last_commit; mode; name; size })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp.getTree#treeEntry") ~enc:(fun _ -> "sh.tangled.git.temp.getTree#treeEntry")
  |> Jsont.Object.opt_mem "last_commit" last_commit_jsont ~enc:(fun r -> r.last_commit)
  |> Jsont.Object.mem "mode" Jsont.string ~enc:(fun r -> r.mode)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "size" Jsont.int ~enc:(fun r -> r.size)
  |> Jsont.Object.finish

type params = {
  path : string option;
  ref_ : string;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun path ref_ repo -> {
      path;
      ref_;
      repo;
    })
  |> Jsont.Object.opt_mem "path" Jsont.string
       ~enc:(fun r -> r.path)
  |> Jsont.Object.mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  dotdot : string option;
  files : tree_entry list;
  last_commit : last_commit option;
  parent : string option;
  readme : readme option;
  ref_ : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ dotdot files last_commit parent readme ref_ -> { dotdot; files; last_commit; parent; readme; ref_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp.getTree#output") ~enc:(fun _ -> "sh.tangled.git.temp.getTree#output")
  |> Jsont.Object.opt_mem "dotdot" Jsont.string ~enc:(fun r -> r.dotdot)
  |> Jsont.Object.mem "files" (Jsont.list tree_entry_jsont) ~enc:(fun r -> r.files)
  |> Jsont.Object.opt_mem "lastCommit" last_commit_jsont ~enc:(fun r -> r.last_commit)
  |> Jsont.Object.opt_mem "parent" Jsont.string ~enc:(fun r -> r.parent)
  |> Jsont.Object.opt_mem "readme" readme_jsont ~enc:(fun r -> r.readme)
  |> Jsont.Object.mem "ref" Jsont.string ~enc:(fun r -> r.ref_)
  |> Jsont.Object.finish

        end
        module GetTag = struct
type params = {
  repo : string;
  tag : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun repo tag -> {
      repo;
      tag;
    })
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "tag" Jsont.string
       ~enc:(fun r -> r.tag)
  |> Jsont.Object.finish

type output = unit
let output_jsont = Jsont.ignore

        end
        module GetMergeBase = struct
type params = {
  base : string;
  head : string;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun base head repo -> {
      base;
      head;
      repo;
    })
  |> Jsont.Object.mem "base" Jsont.string
       ~enc:(fun r -> r.base)
  |> Jsont.Object.mem "head" Jsont.string
       ~enc:(fun r -> r.head)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  commit : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ commit -> { commit })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp.getMergeBase#output") ~enc:(fun _ -> "sh.tangled.git.temp.getMergeBase#output")
  |> Jsont.Object.mem "commit" Jsont.string ~enc:(fun r -> r.commit)
  |> Jsont.Object.finish

        end
        module GetDiff = struct
type params = {
  repo : string;
  rev1 : string;
  rev2 : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun repo rev1 rev2 -> {
      repo;
      rev1;
      rev2;
    })
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "rev1" Jsont.string
       ~enc:(fun r -> r.rev1)
  |> Jsont.Object.mem "rev2" Jsont.string
       ~enc:(fun r -> r.rev2)
  |> Jsont.Object.finish

type output = unit
let output_jsont = Jsont.ignore

        end
        module GetBlob = struct
type params = {
  path : string;
  ref_ : string option;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun path ref_ repo -> {
      path;
      ref_;
      repo;
    })
  |> Jsont.Object.mem "path" Jsont.string
       ~enc:(fun r -> r.path)
  |> Jsont.Object.opt_mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = unit
let output_jsont = Jsont.ignore

        end
        module GetArchive = struct
type params = {
  format : string option;
  prefix : string option;
  ref_ : string;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun format prefix ref_ repo -> {
      format;
      prefix;
      ref_;
      repo;
    })
  |> Jsont.Object.opt_mem "format" Jsont.string
       ~enc:(fun r -> r.format)
  |> Jsont.Object.opt_mem "prefix" Jsont.string
       ~enc:(fun r -> r.prefix)
  |> Jsont.Object.mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = unit
let output_jsont = Jsont.ignore

        end
        module FormatPatch = struct
type params = {
  from : string;
  repo : string;
  to_ : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun from repo to_ -> {
      from;
      repo;
      to_;
    })
  |> Jsont.Object.mem "from" Jsont.string
       ~enc:(fun r -> r.from)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "to" Jsont.string
       ~enc:(fun r -> r.to_)
  |> Jsont.Object.finish

type output = unit
let output_jsont = Jsont.ignore

        end
        module Defs = struct
type hash = string
let hash_jsont = Jsont.string

type signature = {
  email : string;
  name : string;
  when_ : string;
}

let signature_jsont =
  Jsont.Object.map ~kind:"Signature"
    (fun _typ email name when_ -> { email; name; when_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp.defs#signature") ~enc:(fun _ -> "sh.tangled.git.temp.defs#signature")
  |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "when" Jsont.string ~enc:(fun r -> r.when_)
  |> Jsont.Object.finish

type submodule = {
  branch : string option;
  name : string;
  url : string;
}

let submodule_jsont =
  Jsont.Object.map ~kind:"Submodule"
    (fun _typ branch name url -> { branch; name; url })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp.defs#submodule") ~enc:(fun _ -> "sh.tangled.git.temp.defs#submodule")
  |> Jsont.Object.opt_mem "branch" Jsont.string ~enc:(fun r -> r.branch)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "url" Jsont.string ~enc:(fun r -> r.url)
  |> Jsont.Object.finish

type commit = {
  author : signature;
  committer : signature;
  hash : hash;
  message : string;
  tree : hash;
}

let commit_jsont =
  Jsont.Object.map ~kind:"Commit"
    (fun _typ author committer hash message tree -> { author; committer; hash; message; tree })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp.defs#commit") ~enc:(fun _ -> "sh.tangled.git.temp.defs#commit")
  |> Jsont.Object.mem "author" signature_jsont ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "committer" signature_jsont ~enc:(fun r -> r.committer)
  |> Jsont.Object.mem "hash" hash_jsont ~enc:(fun r -> r.hash)
  |> Jsont.Object.mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.mem "tree" hash_jsont ~enc:(fun r -> r.tree)
  |> Jsont.Object.finish

type tag = {
  message : string option;
  name : string;
  tagger : signature;
  target : Jsont.json;
}

let tag_jsont =
  Jsont.Object.map ~kind:"Tag"
    (fun _typ message name tagger target -> { message; name; tagger; target })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp.defs#tag") ~enc:(fun _ -> "sh.tangled.git.temp.defs#tag")
  |> Jsont.Object.opt_mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "tagger" signature_jsont ~enc:(fun r -> r.tagger)
  |> Jsont.Object.mem "target" Jsont.json ~enc:(fun r -> r.target)
  |> Jsont.Object.finish

type branch = {
  commit : commit;
  name : string;
}

let branch_jsont =
  Jsont.Object.map ~kind:"Branch"
    (fun _typ commit name -> { commit; name })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp.defs#branch") ~enc:(fun _ -> "sh.tangled.git.temp.defs#branch")
  |> Jsont.Object.mem "commit" commit_jsont ~enc:(fun r -> r.commit)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.finish

        end
        module AnalyzeMerge = struct
type conflict_info = {
  filename : string;
  reason : string;
}

let conflict_info_jsont =
  Jsont.Object.map ~kind:"Conflict_info"
    (fun _typ filename reason -> { filename; reason })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp.analyzeMerge#conflictInfo") ~enc:(fun _ -> "sh.tangled.git.temp.analyzeMerge#conflictInfo")
  |> Jsont.Object.mem "filename" Jsont.string ~enc:(fun r -> r.filename)
  |> Jsont.Object.mem "reason" Jsont.string ~enc:(fun r -> r.reason)
  |> Jsont.Object.finish

type params = {
  branch : string;
  patch : string;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun branch patch repo -> {
      branch;
      patch;
      repo;
    })
  |> Jsont.Object.mem "branch" Jsont.string
       ~enc:(fun r -> r.branch)
  |> Jsont.Object.mem "patch" Jsont.string
       ~enc:(fun r -> r.patch)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  conflicts : conflict_info list option;
  is_conflicted : bool;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ conflicts is_conflicted -> { conflicts; is_conflicted })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp.analyzeMerge#output") ~enc:(fun _ -> "sh.tangled.git.temp.analyzeMerge#output")
  |> Jsont.Object.opt_mem "conflicts" (Jsont.list conflict_info_jsont) ~enc:(fun r -> r.conflicts)
  |> Jsont.Object.mem "is_conflicted" Jsont.bool ~enc:(fun r -> r.is_conflicted)
  |> Jsont.Object.finish

        end
        module ListIndexedLanguages = struct
type repo_languages = {
  commit : string;
  languages : ListLanguages.language list;
  repo : string;
}

let repo_languages_jsont =
  Jsont.Object.map ~kind:"Repo_languages"
    (fun _typ commit languages repo -> { commit; languages; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp.listIndexedLanguages#repoLanguages") ~enc:(fun _ -> "sh.tangled.git.temp.listIndexedLanguages#repoLanguages")
  |> Jsont.Object.mem "commit" Jsont.string ~enc:(fun r -> r.commit)
  |> Jsont.Object.mem "languages" (Jsont.list ListLanguages.language_jsont) ~enc:(fun r -> r.languages)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit -> {
      cursor;
      limit;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  repos : repo_languages list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor repos -> { cursor; repos })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp.listIndexedLanguages#output") ~enc:(fun _ -> "sh.tangled.git.temp.listIndexedLanguages#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "repos" (Jsont.list repo_languages_jsont) ~enc:(fun r -> r.repos)
  |> Jsont.Object.finish

        end
        module GetHead = struct
type params = {
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun repo -> {
      repo;
    })
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = Defs.branch

let output_jsont = Defs.branch_jsont

        end
        module GetEntry = struct
type params = {
  path : string;
  ref_ : string option;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun path ref_ repo -> {
      path;
      ref_;
      repo;
    })
  |> Jsont.Object.mem "path" Jsont.string
       ~enc:(fun r -> r.path)
  |> Jsont.Object.opt_mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  last_commit : Defs.commit option;
  mode : string;
  name : string;
  oid : string;
  size : int;
  submodule : Defs.submodule option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ last_commit mode name oid size submodule -> { last_commit; mode; name; oid; size; submodule })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp.getEntry#output") ~enc:(fun _ -> "sh.tangled.git.temp.getEntry#output")
  |> Jsont.Object.opt_mem "lastCommit" Defs.commit_jsont ~enc:(fun r -> r.last_commit)
  |> Jsont.Object.mem "mode" Jsont.string ~enc:(fun r -> r.mode)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "oid" Jsont.string ~enc:(fun r -> r.oid)
  |> Jsont.Object.mem "size" Jsont.int ~enc:(fun r -> r.size)
  |> Jsont.Object.opt_mem "submodule" Defs.submodule_jsont ~enc:(fun r -> r.submodule)
  |> Jsont.Object.finish

        end
        module GetCommit = struct
type params = {
  ref_ : string;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun ref_ repo -> {
      ref_;
      repo;
    })
  |> Jsont.Object.mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = Defs.commit

let output_jsont = Defs.commit_jsont

        end
        module GetBranch = struct
type params = {
  name : string;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun name repo -> {
      name;
      repo;
    })
  |> Jsont.Object.mem "name" Jsont.string
       ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  author : Defs.signature option;
  hash : string;
  message : string option;
  name : string;
  when_ : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ author hash message name when_ -> { author; hash; message; name; when_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp.getBranch#output") ~enc:(fun _ -> "sh.tangled.git.temp.getBranch#output")
  |> Jsont.Object.opt_mem "author" Defs.signature_jsont ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "hash" Jsont.string ~enc:(fun r -> r.hash)
  |> Jsont.Object.opt_mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "when" Jsont.string ~enc:(fun r -> r.when_)
  |> Jsont.Object.finish

        end
      end
      module RefUpdate = struct
type individual_email_commit_count = {
  count : int;
  email : string;
}

let individual_email_commit_count_jsont =
  Jsont.Object.map ~kind:"Individual_email_commit_count"
    (fun _typ count email -> { count; email })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.refUpdate#individualEmailCommitCount") ~enc:(fun _ -> "sh.tangled.git.refUpdate#individualEmailCommitCount")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
  |> Jsont.Object.finish

type individual_language_size = {
  lang : string;
  size : int;
}

let individual_language_size_jsont =
  Jsont.Object.map ~kind:"Individual_language_size"
    (fun _typ lang size -> { lang; size })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.refUpdate#individualLanguageSize") ~enc:(fun _ -> "sh.tangled.git.refUpdate#individualLanguageSize")
  |> Jsont.Object.mem "lang" Jsont.string ~enc:(fun r -> r.lang)
  |> Jsont.Object.mem "size" Jsont.int ~enc:(fun r -> r.size)
  |> Jsont.Object.finish

type commit_count_breakdown = {
  by_email : individual_email_commit_count list option;
}

let commit_count_breakdown_jsont =
  Jsont.Object.map ~kind:"Commit_count_breakdown"
    (fun _typ by_email -> { by_email })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.refUpdate#commitCountBreakdown") ~enc:(fun _ -> "sh.tangled.git.refUpdate#commitCountBreakdown")
  |> Jsont.Object.opt_mem "byEmail" (Jsont.list individual_email_commit_count_jsont) ~enc:(fun r -> r.by_email)
  |> Jsont.Object.finish

type lang_breakdown = {
  inputs : individual_language_size list option;
}

let lang_breakdown_jsont =
  Jsont.Object.map ~kind:"Lang_breakdown"
    (fun _typ inputs -> { inputs })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.refUpdate#langBreakdown") ~enc:(fun _ -> "sh.tangled.git.refUpdate#langBreakdown")
  |> Jsont.Object.opt_mem "inputs" (Jsont.list individual_language_size_jsont) ~enc:(fun r -> r.inputs)
  |> Jsont.Object.finish

type meta = {
  commit_count : commit_count_breakdown;
  is_default_ref : bool;
  lang_breakdown : lang_breakdown option;
}

let meta_jsont =
  Jsont.Object.map ~kind:"Meta"
    (fun _typ commit_count is_default_ref lang_breakdown -> { commit_count; is_default_ref; lang_breakdown })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.refUpdate#meta") ~enc:(fun _ -> "sh.tangled.git.refUpdate#meta")
  |> Jsont.Object.mem "commitCount" commit_count_breakdown_jsont ~enc:(fun r -> r.commit_count)
  |> Jsont.Object.mem "isDefaultRef" Jsont.bool ~enc:(fun r -> r.is_default_ref)
  |> Jsont.Object.opt_mem "langBreakdown" lang_breakdown_jsont ~enc:(fun r -> r.lang_breakdown)
  |> Jsont.Object.finish

type main = {
  changed_files : string list option;
  committer_did : string;
  meta : meta;
  new_sha : string;
  old_sha : string;
  owner_did : string option;
  push_options : string list option;
  ref_ : string;
  repo : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ changed_files committer_did meta new_sha old_sha owner_did push_options ref_ repo -> { changed_files; committer_did; meta; new_sha; old_sha; owner_did; push_options; ref_; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.refUpdate") ~enc:(fun _ -> "sh.tangled.git.refUpdate")
  |> Jsont.Object.opt_mem "changedFiles" (Jsont.list Jsont.string) ~enc:(fun r -> r.changed_files)
  |> Jsont.Object.mem "committerDid" Jsont.string ~enc:(fun r -> r.committer_did)
  |> Jsont.Object.mem "meta" meta_jsont ~enc:(fun r -> r.meta)
  |> Jsont.Object.mem "newSha" Jsont.string ~enc:(fun r -> r.new_sha)
  |> Jsont.Object.mem "oldSha" Jsont.string ~enc:(fun r -> r.old_sha)
  |> Jsont.Object.opt_mem "ownerDid" Jsont.string ~enc:(fun r -> r.owner_did)
  |> Jsont.Object.opt_mem "pushOptions" (Jsont.list Jsont.string) ~enc:(fun r -> r.push_options)
  |> Jsont.Object.mem "ref" Jsont.string ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

      end
      module Ref = struct
type main = {
  sha : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ sha -> { sha })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.ref") ~enc:(fun _ -> "sh.tangled.git.ref")
  |> Jsont.Object.mem "sha" Jsont.string ~enc:(fun r -> r.sha)
  |> Jsont.Object.finish

      end
      module Oid = struct
type main = {
  oid : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ oid -> { oid })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.oid") ~enc:(fun _ -> "sh.tangled.git.oid")
  |> Jsont.Object.mem "oid" Jsont.string ~enc:(fun r -> r.oid)
  |> Jsont.Object.finish

      end
      module MergeCheck = struct
type conflict_info = {
  filename : string;
  reason : string;
}

let conflict_info_jsont =
  Jsont.Object.map ~kind:"Conflict_info"
    (fun _typ filename reason -> { filename; reason })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.mergeCheck#conflictInfo") ~enc:(fun _ -> "sh.tangled.git.mergeCheck#conflictInfo")
  |> Jsont.Object.mem "filename" Jsont.string ~enc:(fun r -> r.filename)
  |> Jsont.Object.mem "reason" Jsont.string ~enc:(fun r -> r.reason)
  |> Jsont.Object.finish

type source = {
  commit : string;
  repo : string;
}

let source_jsont =
  Jsont.Object.map ~kind:"Source"
    (fun _typ commit repo -> { commit; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.mergeCheck#source") ~enc:(fun _ -> "sh.tangled.git.mergeCheck#source")
  |> Jsont.Object.mem "commit" Jsont.string ~enc:(fun r -> r.commit)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type input = {
  branch : string;
  repo : string;
  source : source;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ branch repo source -> { branch; repo; source })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.mergeCheck#input") ~enc:(fun _ -> "sh.tangled.git.mergeCheck#input")
  |> Jsont.Object.mem "branch" Jsont.string ~enc:(fun r -> r.branch)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "source" source_jsont ~enc:(fun r -> r.source)
  |> Jsont.Object.finish

type output = {
  conflicts : conflict_info list option;
  is_conflicted : bool;
  message : string option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ conflicts is_conflicted message -> { conflicts; is_conflicted; message })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.mergeCheck#output") ~enc:(fun _ -> "sh.tangled.git.mergeCheck#output")
  |> Jsont.Object.opt_mem "conflicts" (Jsont.list conflict_info_jsont) ~enc:(fun r -> r.conflicts)
  |> Jsont.Object.mem "isConflicted" Jsont.bool ~enc:(fun r -> r.is_conflicted)
  |> Jsont.Object.opt_mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.finish

      end
      module ListRefs = struct
type default_branch = {
  head : string option;
  ref_ : string;
}

let default_branch_jsont =
  Jsont.Object.map ~kind:"Default_branch"
    (fun _typ head ref_ -> { head; ref_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.listRefs#defaultBranch") ~enc:(fun _ -> "sh.tangled.git.listRefs#defaultBranch")
  |> Jsont.Object.opt_mem "head" Jsont.string ~enc:(fun r -> r.head)
  |> Jsont.Object.mem "ref" Jsont.string ~enc:(fun r -> r.ref_)
  |> Jsont.Object.finish

type ref_ = {
  ref_ : string;
  sha : string;
}

let ref__jsont =
  Jsont.Object.map ~kind:"Ref_"
    (fun _typ ref_ sha -> { ref_; sha })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.listRefs#ref") ~enc:(fun _ -> "sh.tangled.git.listRefs#ref")
  |> Jsont.Object.mem "ref" Jsont.string ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "sha" Jsont.string ~enc:(fun r -> r.sha)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit repo -> {
      cursor;
      limit;
      repo;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  default_branch : default_branch option;
  refs : ref_ list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor default_branch refs -> { cursor; default_branch; refs })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.listRefs#output") ~enc:(fun _ -> "sh.tangled.git.listRefs#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "defaultBranch" default_branch_jsont ~enc:(fun r -> r.default_branch)
  |> Jsont.Object.mem "refs" (Jsont.list ref__jsont) ~enc:(fun r -> r.refs)
  |> Jsont.Object.finish

      end
      module KeepCommit = struct
type commit = {
  oid : string;
  repo : string;
}

let commit_jsont =
  Jsont.Object.map ~kind:"Commit"
    (fun _typ oid repo -> { oid; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.keepCommit#commit") ~enc:(fun _ -> "sh.tangled.git.keepCommit#commit")
  |> Jsont.Object.mem "oid" Jsont.string ~enc:(fun r -> r.oid)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type patches = {
  patches : string list;
}

let patches_jsont =
  Jsont.Object.map ~kind:"Patches"
    (fun _typ patches -> { patches })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.keepCommit#patches") ~enc:(fun _ -> "sh.tangled.git.keepCommit#patches")
  |> Jsont.Object.mem "patches" (Jsont.list Jsont.string) ~enc:(fun r -> r.patches)
  |> Jsont.Object.finish

type input = {
  record : string;
  repo : string;
  source : Jsont.json;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ record repo source -> { record; repo; source })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.keepCommit#input") ~enc:(fun _ -> "sh.tangled.git.keepCommit#input")
  |> Jsont.Object.mem "record" Jsont.string ~enc:(fun r -> r.record)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "source" Jsont.json ~enc:(fun r -> r.source)
  |> Jsont.Object.finish

type output = {
  commit : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ commit -> { commit })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.keepCommit#output") ~enc:(fun _ -> "sh.tangled.git.keepCommit#output")
  |> Jsont.Object.mem "commit" Jsont.string ~enc:(fun r -> r.commit)
  |> Jsont.Object.finish

      end
      module Defs = struct
type byte_changes = {
  lhs : int;
  rhs : int;
}

let byte_changes_jsont =
  Jsont.Object.map ~kind:"Byte_changes"
    (fun _typ lhs rhs -> { lhs; rhs })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.defs#byteChanges") ~enc:(fun _ -> "sh.tangled.git.defs#byteChanges")
  |> Jsont.Object.mem "lhs" Jsont.int ~enc:(fun r -> r.lhs)
  |> Jsont.Object.mem "rhs" Jsont.int ~enc:(fun r -> r.rhs)
  |> Jsont.Object.finish

type diff_src = {
  content : string option;
  is_binary : bool;
  is_submodule : bool;
  oid : string;
  path : string;
  size : int;
}

let diff_src_jsont =
  Jsont.Object.map ~kind:"Diff_src"
    (fun _typ content is_binary is_submodule oid path size -> { content; is_binary; is_submodule; oid; path; size })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.defs#diffSrc") ~enc:(fun _ -> "sh.tangled.git.defs#diffSrc")
  |> Jsont.Object.opt_mem "content" Jsont.string ~enc:(fun r -> r.content)
  |> Jsont.Object.mem "isBinary" Jsont.bool ~enc:(fun r -> r.is_binary)
  |> Jsont.Object.mem "isSubmodule" Jsont.bool ~enc:(fun r -> r.is_submodule)
  |> Jsont.Object.mem "oid" Jsont.string ~enc:(fun r -> r.oid)
  |> Jsont.Object.mem "path" Jsont.string ~enc:(fun r -> r.path)
  |> Jsont.Object.mem "size" Jsont.int ~enc:(fun r -> r.size)
  |> Jsont.Object.finish

type diff_stat = {
  deletions : int;
  files_changed : int;
  insertions : int;
}

let diff_stat_jsont =
  Jsont.Object.map ~kind:"Diff_stat"
    (fun _typ deletions files_changed insertions -> { deletions; files_changed; insertions })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.defs#diffStat") ~enc:(fun _ -> "sh.tangled.git.defs#diffStat")
  |> Jsont.Object.mem "deletions" Jsont.int ~enc:(fun r -> r.deletions)
  |> Jsont.Object.mem "filesChanged" Jsont.int ~enc:(fun r -> r.files_changed)
  |> Jsont.Object.mem "insertions" Jsont.int ~enc:(fun r -> r.insertions)
  |> Jsont.Object.finish

type line_pair = {
  lhs : int option;
  rhs : int option;
}

let line_pair_jsont =
  Jsont.Object.map ~kind:"Line_pair"
    (fun _typ lhs rhs -> { lhs; rhs })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.defs#linePair") ~enc:(fun _ -> "sh.tangled.git.defs#linePair")
  |> Jsont.Object.opt_mem "lhs" Jsont.int ~enc:(fun r -> r.lhs)
  |> Jsont.Object.opt_mem "rhs" Jsont.int ~enc:(fun r -> r.rhs)
  |> Jsont.Object.finish

type signature = {
  email : string;
  name : string;
  when_ : string;
}

let signature_jsont =
  Jsont.Object.map ~kind:"Signature"
    (fun _typ email name when_ -> { email; name; when_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.defs#signature") ~enc:(fun _ -> "sh.tangled.git.defs#signature")
  |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "when" Jsont.string ~enc:(fun r -> r.when_)
  |> Jsont.Object.finish

type diff_hunk = {
  lines : line_pair list;
  novel_lhs : int list;
  novel_rhs : int list;
}

let diff_hunk_jsont =
  Jsont.Object.map ~kind:"Diff_hunk"
    (fun _typ lines novel_lhs novel_rhs -> { lines; novel_lhs; novel_rhs })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.defs#diffHunk") ~enc:(fun _ -> "sh.tangled.git.defs#diffHunk")
  |> Jsont.Object.mem "lines" (Jsont.list line_pair_jsont) ~enc:(fun r -> r.lines)
  |> Jsont.Object.mem "novelLhs" (Jsont.list Jsont.int) ~enc:(fun r -> r.novel_lhs)
  |> Jsont.Object.mem "novelRhs" (Jsont.list Jsont.int) ~enc:(fun r -> r.novel_rhs)
  |> Jsont.Object.finish

type file_diff = {
  has_byte_changes : byte_changes option;
  has_syntactic_changes : bool;
  hunks : diff_hunk list;
  lhs_src : diff_src;
  rhs_src : diff_src;
}

let file_diff_jsont =
  Jsont.Object.map ~kind:"File_diff"
    (fun _typ has_byte_changes has_syntactic_changes hunks lhs_src rhs_src -> { has_byte_changes; has_syntactic_changes; hunks; lhs_src; rhs_src })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.defs#fileDiff") ~enc:(fun _ -> "sh.tangled.git.defs#fileDiff")
  |> Jsont.Object.opt_mem "hasByteChanges" byte_changes_jsont ~enc:(fun r -> r.has_byte_changes)
  |> Jsont.Object.mem "hasSyntacticChanges" Jsont.bool ~enc:(fun r -> r.has_syntactic_changes)
  |> Jsont.Object.mem "hunks" (Jsont.list diff_hunk_jsont) ~enc:(fun r -> r.hunks)
  |> Jsont.Object.mem "lhsSrc" diff_src_jsont ~enc:(fun r -> r.lhs_src)
  |> Jsont.Object.mem "rhsSrc" diff_src_jsont ~enc:(fun r -> r.rhs_src)
  |> Jsont.Object.finish

      end
      module Temp2 = struct
        module MergeCheck = struct
type conflict = {
  filename : string;
  reason : string;
}

let conflict_jsont =
  Jsont.Object.map ~kind:"Conflict"
    (fun _typ filename reason -> { filename; reason })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp2.mergeCheck#conflict") ~enc:(fun _ -> "sh.tangled.git.temp2.mergeCheck#conflict")
  |> Jsont.Object.mem "filename" Jsont.string ~enc:(fun r -> r.filename)
  |> Jsont.Object.mem "reason" Jsont.string ~enc:(fun r -> r.reason)
  |> Jsont.Object.finish

type params = {
  source_commit : string;
  source_repo : string;
  target_commit : string;
  target_repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun source_commit source_repo target_commit target_repo -> {
      source_commit;
      source_repo;
      target_commit;
      target_repo;
    })
  |> Jsont.Object.mem "sourceCommit" Jsont.string
       ~enc:(fun r -> r.source_commit)
  |> Jsont.Object.mem "sourceRepo" Jsont.string
       ~enc:(fun r -> r.source_repo)
  |> Jsont.Object.mem "targetCommit" Jsont.string
       ~enc:(fun r -> r.target_commit)
  |> Jsont.Object.mem "targetRepo" Jsont.string
       ~enc:(fun r -> r.target_repo)
  |> Jsont.Object.finish

type output = {
  conflicts : conflict list;
  is_conflicted : bool;
  message : string option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ conflicts is_conflicted message -> { conflicts; is_conflicted; message })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp2.mergeCheck#output") ~enc:(fun _ -> "sh.tangled.git.temp2.mergeCheck#output")
  |> Jsont.Object.mem "conflicts" (Jsont.list conflict_jsont) ~enc:(fun r -> r.conflicts)
  |> Jsont.Object.mem "isConflicted" Jsont.bool ~enc:(fun r -> r.is_conflicted)
  |> Jsont.Object.opt_mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.finish

        end
        module ListCommits = struct
type header = {
  key : string;
  value : string;
}

let header_jsont =
  Jsont.Object.map ~kind:"Header"
    (fun _typ key value -> { key; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp2.listCommits#header") ~enc:(fun _ -> "sh.tangled.git.temp2.listCommits#header")
  |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
  |> Jsont.Object.mem "value" Jsont.string ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

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

let commit_jsont =
  Jsont.Object.map ~kind:"Commit"
    (fun _typ author committer extra_headers message oid parents stat tree -> { author; committer; extra_headers; message; oid; parents; stat; tree })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp2.listCommits#commit") ~enc:(fun _ -> "sh.tangled.git.temp2.listCommits#commit")
  |> Jsont.Object.mem "author" Defs.signature_jsont ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "committer" Defs.signature_jsont ~enc:(fun r -> r.committer)
  |> Jsont.Object.mem "extraHeaders" (Jsont.list header_jsont) ~enc:(fun r -> r.extra_headers)
  |> Jsont.Object.mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.mem "oid" Jsont.string ~enc:(fun r -> r.oid)
  |> Jsont.Object.mem "parents" (Jsont.list Jsont.string) ~enc:(fun r -> r.parents)
  |> Jsont.Object.opt_mem "stat" Defs.diff_stat_jsont ~enc:(fun r -> r.stat)
  |> Jsont.Object.mem "tree" Jsont.string ~enc:(fun r -> r.tree)
  |> Jsont.Object.finish

type params = {
  after : string option;
  all_refs : bool option;
  before : string option;
  include_stats : bool option;
  limit : int option;
  ranges : string list option;
  repo : string;
  skip : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun after all_refs before include_stats limit ranges repo skip -> {
      after;
      all_refs;
      before;
      include_stats;
      limit;
      ranges;
      repo;
      skip;
    })
  |> Jsont.Object.opt_mem "after" Jsont.string
       ~enc:(fun r -> r.after)
  |> Jsont.Object.opt_mem "allRefs" Jsont.bool
       ~enc:(fun r -> r.all_refs)
  |> Jsont.Object.opt_mem "before" Jsont.string
       ~enc:(fun r -> r.before)
  |> Jsont.Object.opt_mem "includeStats" Jsont.bool
       ~enc:(fun r -> r.include_stats)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "ranges" (Jsont.list Jsont.string)
       ~enc:(fun r -> r.ranges)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.opt_mem "skip" Jsont.int
       ~enc:(fun r -> r.skip)
  |> Jsont.Object.finish

type output = {
  commits : commit list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ commits -> { commits })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp2.listCommits#output") ~enc:(fun _ -> "sh.tangled.git.temp2.listCommits#output")
  |> Jsont.Object.mem "commits" (Jsont.list commit_jsont) ~enc:(fun r -> r.commits)
  |> Jsont.Object.finish

        end
        module GetInterdiff = struct
type params = {
  base_commit1 : string;
  base_commit2 : string;
  base_repo : string;
  head_commit1 : string;
  head_commit2 : string;
  head_repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun base_commit1 base_commit2 base_repo head_commit1 head_commit2 head_repo -> {
      base_commit1;
      base_commit2;
      base_repo;
      head_commit1;
      head_commit2;
      head_repo;
    })
  |> Jsont.Object.mem "baseCommit1" Jsont.string
       ~enc:(fun r -> r.base_commit1)
  |> Jsont.Object.mem "baseCommit2" Jsont.string
       ~enc:(fun r -> r.base_commit2)
  |> Jsont.Object.mem "baseRepo" Jsont.string
       ~enc:(fun r -> r.base_repo)
  |> Jsont.Object.mem "headCommit1" Jsont.string
       ~enc:(fun r -> r.head_commit1)
  |> Jsont.Object.mem "headCommit2" Jsont.string
       ~enc:(fun r -> r.head_commit2)
  |> Jsont.Object.mem "headRepo" Jsont.string
       ~enc:(fun r -> r.head_repo)
  |> Jsont.Object.finish

type output = {
  diffs : Defs.file_diff list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ diffs -> { diffs })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp2.getInterdiff#output") ~enc:(fun _ -> "sh.tangled.git.temp2.getInterdiff#output")
  |> Jsont.Object.mem "diffs" (Jsont.list Defs.file_diff_jsont) ~enc:(fun r -> r.diffs)
  |> Jsont.Object.finish

        end
        module GetDiff = struct
type params = {
  base_commit : string;
  base_repo : string;
  head_commit : string;
  head_repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun base_commit base_repo head_commit head_repo -> {
      base_commit;
      base_repo;
      head_commit;
      head_repo;
    })
  |> Jsont.Object.mem "baseCommit" Jsont.string
       ~enc:(fun r -> r.base_commit)
  |> Jsont.Object.mem "baseRepo" Jsont.string
       ~enc:(fun r -> r.base_repo)
  |> Jsont.Object.mem "headCommit" Jsont.string
       ~enc:(fun r -> r.head_commit)
  |> Jsont.Object.mem "headRepo" Jsont.string
       ~enc:(fun r -> r.head_repo)
  |> Jsont.Object.finish

type output = {
  diffs : Defs.file_diff list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ diffs -> { diffs })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp2.getDiff#output") ~enc:(fun _ -> "sh.tangled.git.temp2.getDiff#output")
  |> Jsont.Object.mem "diffs" (Jsont.list Defs.file_diff_jsont) ~enc:(fun r -> r.diffs)
  |> Jsont.Object.finish

        end
        module GetCommitStats = struct
type commit = {
  committed_at : string;
  sha : string;
  subject : string;
}

let commit_jsont =
  Jsont.Object.map ~kind:"Commit"
    (fun _typ committed_at sha subject -> { committed_at; sha; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp2.getCommitStats#commit") ~enc:(fun _ -> "sh.tangled.git.temp2.getCommitStats#commit")
  |> Jsont.Object.mem "committedAt" Jsont.string ~enc:(fun r -> r.committed_at)
  |> Jsont.Object.mem "sha" Jsont.string ~enc:(fun r -> r.sha)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type repo_commits = {
  commits : int;
  recent : commit list;
  repo : string;
}

let repo_commits_jsont =
  Jsont.Object.map ~kind:"Repo_commits"
    (fun _typ commits recent repo -> { commits; recent; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp2.getCommitStats#repoCommits") ~enc:(fun _ -> "sh.tangled.git.temp2.getCommitStats#repoCommits")
  |> Jsont.Object.mem "commits" Jsont.int ~enc:(fun r -> r.commits)
  |> Jsont.Object.mem "recent" (Jsont.list commit_jsont) ~enc:(fun r -> r.recent)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type month = {
  commits : int;
  repos : repo_commits list option;
  start : string;
}

let month_jsont =
  Jsont.Object.map ~kind:"Month"
    (fun _typ commits repos start -> { commits; repos; start })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp2.getCommitStats#month") ~enc:(fun _ -> "sh.tangled.git.temp2.getCommitStats#month")
  |> Jsont.Object.mem "commits" Jsont.int ~enc:(fun r -> r.commits)
  |> Jsont.Object.opt_mem "repos" (Jsont.list repo_commits_jsont) ~enc:(fun r -> r.repos)
  |> Jsont.Object.mem "start" Jsont.string ~enc:(fun r -> r.start)
  |> Jsont.Object.finish

type params = {
  actor : string;
  months : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun actor months -> {
      actor;
      months;
    })
  |> Jsont.Object.mem "actor" Jsont.string
       ~enc:(fun r -> r.actor)
  |> Jsont.Object.opt_mem "months" Jsont.int
       ~enc:(fun r -> r.months)
  |> Jsont.Object.finish

type output = {
  months : month list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ months -> { months })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp2.getCommitStats#output") ~enc:(fun _ -> "sh.tangled.git.temp2.getCommitStats#output")
  |> Jsont.Object.mem "months" (Jsont.list month_jsont) ~enc:(fun r -> r.months)
  |> Jsont.Object.finish

        end
        module GetBlame = struct
type commit = {
  author : Defs.signature;
  message : string;
  oid : string;
}

let commit_jsont =
  Jsont.Object.map ~kind:"Commit"
    (fun _typ author message oid -> { author; message; oid })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp2.getBlame#commit") ~enc:(fun _ -> "sh.tangled.git.temp2.getBlame#commit")
  |> Jsont.Object.mem "author" Defs.signature_jsont ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.mem "oid" Jsont.string ~enc:(fun r -> r.oid)
  |> Jsont.Object.finish

type run = {
  commit : string;
  count : int;
  start : int;
}

let run_jsont =
  Jsont.Object.map ~kind:"Run"
    (fun _typ commit count start -> { commit; count; start })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp2.getBlame#run") ~enc:(fun _ -> "sh.tangled.git.temp2.getBlame#run")
  |> Jsont.Object.mem "commit" Jsont.string ~enc:(fun r -> r.commit)
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "start" Jsont.int ~enc:(fun r -> r.start)
  |> Jsont.Object.finish

type params = {
  path : string;
  ref_ : string;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun path ref_ repo -> {
      path;
      ref_;
      repo;
    })
  |> Jsont.Object.mem "path" Jsont.string
       ~enc:(fun r -> r.path)
  |> Jsont.Object.mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  commit : string;
  commits : commit list;
  runs : run list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ commit commits runs -> { commit; commits; runs })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.temp2.getBlame#output") ~enc:(fun _ -> "sh.tangled.git.temp2.getBlame#output")
  |> Jsont.Object.mem "commit" Jsont.string ~enc:(fun r -> r.commit)
  |> Jsont.Object.mem "commits" (Jsont.list commit_jsont) ~enc:(fun r -> r.commits)
  |> Jsont.Object.mem "runs" (Jsont.list run_jsont) ~enc:(fun r -> r.runs)
  |> Jsont.Object.finish

        end
      end
      module MergeCommit = struct
type merge_commit_content = {
  author : Defs.signature option;
  message : string option;
}

let merge_commit_content_jsont =
  Jsont.Object.map ~kind:"Merge_commit_content"
    (fun _typ author message -> { author; message })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.mergeCommit#mergeCommitContent") ~enc:(fun _ -> "sh.tangled.git.mergeCommit#mergeCommitContent")
  |> Jsont.Object.opt_mem "author" Defs.signature_jsont ~enc:(fun r -> r.author)
  |> Jsont.Object.opt_mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.finish

type source = {
  commit : string;
  repo : string;
}

let source_jsont =
  Jsont.Object.map ~kind:"Source"
    (fun _typ commit repo -> { commit; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.mergeCommit#source") ~enc:(fun _ -> "sh.tangled.git.mergeCommit#source")
  |> Jsont.Object.mem "commit" Jsont.string ~enc:(fun r -> r.commit)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type target = {
  branch : string;
  commit : string;
  repo : string;
}

let target_jsont =
  Jsont.Object.map ~kind:"Target"
    (fun _typ branch commit repo -> { branch; commit; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.mergeCommit#target") ~enc:(fun _ -> "sh.tangled.git.mergeCommit#target")
  |> Jsont.Object.mem "branch" Jsont.string ~enc:(fun r -> r.branch)
  |> Jsont.Object.mem "commit" Jsont.string ~enc:(fun r -> r.commit)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type input = {
  merge_commit : merge_commit_content option;
  source : source;
  style : string;
  target : target;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ merge_commit source style target -> { merge_commit; source; style; target })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.mergeCommit#input") ~enc:(fun _ -> "sh.tangled.git.mergeCommit#input")
  |> Jsont.Object.opt_mem "mergeCommit" merge_commit_content_jsont ~enc:(fun r -> r.merge_commit)
  |> Jsont.Object.mem "source" source_jsont ~enc:(fun r -> r.source)
  |> Jsont.Object.mem "style" Jsont.string ~enc:(fun r -> r.style)
  |> Jsont.Object.mem "target" target_jsont ~enc:(fun r -> r.target)
  |> Jsont.Object.finish

      end
    end
    module Ci = struct
      module Trigger = struct
type pair = {
  key : string;
  value : string;
}

let pair_jsont =
  Jsont.Object.map ~kind:"Pair"
    (fun _typ key value -> { key; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.ci.trigger#pair") ~enc:(fun _ -> "sh.tangled.ci.trigger#pair")
  |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
  |> Jsont.Object.mem "value" Jsont.string ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type pull_request = {
  action : string option;
  pull : string option;
  source_branch : string option;
  source_repo : string option;
  source_sha : string;
  target_branch : string;
}

let pull_request_jsont =
  Jsont.Object.map ~kind:"Pull_request"
    (fun _typ action pull source_branch source_repo source_sha target_branch -> { action; pull; source_branch; source_repo; source_sha; target_branch })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.ci.trigger#pullRequest") ~enc:(fun _ -> "sh.tangled.ci.trigger#pullRequest")
  |> Jsont.Object.opt_mem "action" Jsont.string ~enc:(fun r -> r.action)
  |> Jsont.Object.opt_mem "pull" Jsont.string ~enc:(fun r -> r.pull)
  |> Jsont.Object.opt_mem "sourceBranch" Jsont.string ~enc:(fun r -> r.source_branch)
  |> Jsont.Object.opt_mem "sourceRepo" Jsont.string ~enc:(fun r -> r.source_repo)
  |> Jsont.Object.mem "sourceSha" Jsont.string ~enc:(fun r -> r.source_sha)
  |> Jsont.Object.mem "targetBranch" Jsont.string ~enc:(fun r -> r.target_branch)
  |> Jsont.Object.finish

type push = {
  new_sha : string;
  old_sha : string;
  ref_ : string;
}

let push_jsont =
  Jsont.Object.map ~kind:"Push"
    (fun _typ new_sha old_sha ref_ -> { new_sha; old_sha; ref_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.ci.trigger#push") ~enc:(fun _ -> "sh.tangled.ci.trigger#push")
  |> Jsont.Object.mem "newSha" Jsont.string ~enc:(fun r -> r.new_sha)
  |> Jsont.Object.mem "oldSha" Jsont.string ~enc:(fun r -> r.old_sha)
  |> Jsont.Object.mem "ref" Jsont.string ~enc:(fun r -> r.ref_)
  |> Jsont.Object.finish

type schedule = {
  ref_ : string;
  scheduled_at : string;
  sha : string;
}

let schedule_jsont =
  Jsont.Object.map ~kind:"Schedule"
    (fun _typ ref_ scheduled_at sha -> { ref_; scheduled_at; sha })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.ci.trigger#schedule") ~enc:(fun _ -> "sh.tangled.ci.trigger#schedule")
  |> Jsont.Object.mem "ref" Jsont.string ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "scheduledAt" Jsont.string ~enc:(fun r -> r.scheduled_at)
  |> Jsont.Object.mem "sha" Jsont.string ~enc:(fun r -> r.sha)
  |> Jsont.Object.finish

type manual = {
  inputs : pair list option;
  ref_ : string option;
  sha : string;
  source_repo : string option;
}

let manual_jsont =
  Jsont.Object.map ~kind:"Manual"
    (fun _typ inputs ref_ sha source_repo -> { inputs; ref_; sha; source_repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.ci.trigger#manual") ~enc:(fun _ -> "sh.tangled.ci.trigger#manual")
  |> Jsont.Object.opt_mem "inputs" (Jsont.list pair_jsont) ~enc:(fun r -> r.inputs)
  |> Jsont.Object.opt_mem "ref" Jsont.string ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "sha" Jsont.string ~enc:(fun r -> r.sha)
  |> Jsont.Object.opt_mem "sourceRepo" Jsont.string ~enc:(fun r -> r.source_repo)
  |> Jsont.Object.finish

      end
      module SubscribePipelineLogs = struct
type control = {
  command : string option;
  content : string;
  kind : string option;
  outcome : string option;
  status : string option;
  step : int;
  time : string;
  workflow : string;
}

let control_jsont =
  Jsont.Object.map ~kind:"Control"
    (fun _typ command content kind outcome status step time workflow -> { command; content; kind; outcome; status; step; time; workflow })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.ci.subscribePipelineLogs#control") ~enc:(fun _ -> "sh.tangled.ci.subscribePipelineLogs#control")
  |> Jsont.Object.opt_mem "command" Jsont.string ~enc:(fun r -> r.command)
  |> Jsont.Object.mem "content" Jsont.string ~enc:(fun r -> r.content)
  |> Jsont.Object.opt_mem "kind" Jsont.string ~enc:(fun r -> r.kind)
  |> Jsont.Object.opt_mem "outcome" Jsont.string ~enc:(fun r -> r.outcome)
  |> Jsont.Object.opt_mem "status" Jsont.string ~enc:(fun r -> r.status)
  |> Jsont.Object.mem "step" Jsont.int ~enc:(fun r -> r.step)
  |> Jsont.Object.mem "time" Jsont.string ~enc:(fun r -> r.time)
  |> Jsont.Object.mem "workflow" Jsont.string ~enc:(fun r -> r.workflow)
  |> Jsont.Object.finish

type data = {
  content : string;
  step : int;
  stream : string;
  time : string;
  workflow : string;
}

let data_jsont =
  Jsont.Object.map ~kind:"Data"
    (fun _typ content step stream time workflow -> { content; step; stream; time; workflow })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.ci.subscribePipelineLogs#data") ~enc:(fun _ -> "sh.tangled.ci.subscribePipelineLogs#data")
  |> Jsont.Object.mem "content" Jsont.string ~enc:(fun r -> r.content)
  |> Jsont.Object.mem "step" Jsont.int ~enc:(fun r -> r.step)
  |> Jsont.Object.mem "stream" Jsont.string ~enc:(fun r -> r.stream)
  |> Jsont.Object.mem "time" Jsont.string ~enc:(fun r -> r.time)
  |> Jsont.Object.mem "workflow" Jsont.string ~enc:(fun r -> r.workflow)
  |> Jsont.Object.finish

type params = {
  pipeline : string;
  workflows : string list option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun pipeline workflows -> {
      pipeline;
      workflows;
    })
  |> Jsont.Object.mem "pipeline" Jsont.string
       ~enc:(fun r -> r.pipeline)
  |> Jsont.Object.opt_mem "workflows" (Jsont.list Jsont.string)
       ~enc:(fun r -> r.workflows)
  |> Jsont.Object.finish

type message = Jsont.json

let message_jsont = Jsont.json

      end
      module DescribeWorkflowDefinition = struct
type params = {
  repo : string;
  sha : string;
  source_repo : string option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun repo sha source_repo -> {
      repo;
      sha;
      source_repo;
    })
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "sha" Jsont.string
       ~enc:(fun r -> r.sha)
  |> Jsont.Object.opt_mem "sourceRepo" Jsont.string
       ~enc:(fun r -> r.source_repo)
  |> Jsont.Object.finish

type output = {
  derived : bool;
  hash : string option;
  workflows : string list option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ derived hash workflows -> { derived; hash; workflows })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.ci.describeWorkflowDefinition#output") ~enc:(fun _ -> "sh.tangled.ci.describeWorkflowDefinition#output")
  |> Jsont.Object.mem "derived" Jsont.bool ~enc:(fun r -> r.derived)
  |> Jsont.Object.opt_mem "hash" Jsont.string ~enc:(fun r -> r.hash)
  |> Jsont.Object.opt_mem "workflows" (Jsont.list Jsont.string) ~enc:(fun r -> r.workflows)
  |> Jsont.Object.finish

      end
      module CancelPipeline = struct
type input = {
  pipeline : string;
  repo : string;
  workflows : string list option;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ pipeline repo workflows -> { pipeline; repo; workflows })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.ci.cancelPipeline#input") ~enc:(fun _ -> "sh.tangled.ci.cancelPipeline#input")
  |> Jsont.Object.mem "pipeline" Jsont.string ~enc:(fun r -> r.pipeline)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.opt_mem "workflows" (Jsont.list Jsont.string) ~enc:(fun r -> r.workflows)
  |> Jsont.Object.finish

      end
      module TriggerPipeline = struct
type input = {
  repo : string;
  trigger : Jsont.json;
  workflows : string list option;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ repo trigger workflows -> { repo; trigger; workflows })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.ci.triggerPipeline#input") ~enc:(fun _ -> "sh.tangled.ci.triggerPipeline#input")
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "trigger" Jsont.json ~enc:(fun r -> r.trigger)
  |> Jsont.Object.opt_mem "workflows" (Jsont.list Jsont.string) ~enc:(fun r -> r.workflows)
  |> Jsont.Object.finish

type output = {
  pipeline : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ pipeline -> { pipeline })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.ci.triggerPipeline#output") ~enc:(fun _ -> "sh.tangled.ci.triggerPipeline#output")
  |> Jsont.Object.mem "pipeline" Jsont.string ~enc:(fun r -> r.pipeline)
  |> Jsont.Object.finish

      end
      module Pipeline = struct
type workflow = {
  error : string option;
  finished_at : string option;
  id : string;
  name : string;
  started_at : string option;
  status : string;
}

let workflow_jsont =
  Jsont.Object.map ~kind:"Workflow"
    (fun _typ error finished_at id name started_at status -> { error; finished_at; id; name; started_at; status })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.ci.pipeline#workflow") ~enc:(fun _ -> "sh.tangled.ci.pipeline#workflow")
  |> Jsont.Object.opt_mem "error" Jsont.string ~enc:(fun r -> r.error)
  |> Jsont.Object.opt_mem "finishedAt" Jsont.string ~enc:(fun r -> r.finished_at)
  |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.opt_mem "startedAt" Jsont.string ~enc:(fun r -> r.started_at)
  |> Jsont.Object.mem "status" Jsont.string ~enc:(fun r -> r.status)
  |> Jsont.Object.finish

type main = {
  commit : string;
  created_at : string option;
  id : string;
  repo : string;
  source_repo : string option;
  trigger : Jsont.json;
  workflows : workflow list;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ commit created_at id repo source_repo trigger workflows -> { commit; created_at; id; repo; source_repo; trigger; workflows })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.ci.pipeline") ~enc:(fun _ -> "sh.tangled.ci.pipeline")
  |> Jsont.Object.mem "commit" Jsont.string ~enc:(fun r -> r.commit)
  |> Jsont.Object.opt_mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.opt_mem "sourceRepo" Jsont.string ~enc:(fun r -> r.source_repo)
  |> Jsont.Object.mem "trigger" Jsont.json ~enc:(fun r -> r.trigger)
  |> Jsont.Object.mem "workflows" (Jsont.list workflow_jsont) ~enc:(fun r -> r.workflows)
  |> Jsont.Object.finish

      end
      module QueryPipelines = struct
type params = {
  commits : string list option;
  cursor : string option;
  kinds : string list option;
  limit : int option;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun commits cursor kinds limit repo -> {
      commits;
      cursor;
      kinds;
      limit;
      repo;
    })
  |> Jsont.Object.opt_mem "commits" (Jsont.list Jsont.string)
       ~enc:(fun r -> r.commits)
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "kinds" (Jsont.list Jsont.string)
       ~enc:(fun r -> r.kinds)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  pipelines : Pipeline.main list;
  total : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor pipelines total -> { cursor; pipelines; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.ci.queryPipelines#output") ~enc:(fun _ -> "sh.tangled.ci.queryPipelines#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "pipelines" (Jsont.list Pipeline.main_jsont) ~enc:(fun r -> r.pipelines)
  |> Jsont.Object.mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module GetPipeline = struct
type params = {
  pipeline : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun pipeline -> {
      pipeline;
    })
  |> Jsont.Object.mem "pipeline" Jsont.string
       ~enc:(fun r -> r.pipeline)
  |> Jsont.Object.finish

type output = Pipeline.main

let output_jsont = Pipeline.main_jsont

      end
    end
    module Actor = struct
      module Profile = struct
type main = {
  avatar : Atp.Blob_ref.t option;
  bluesky : bool;
  description : string option;
  is_organization : bool option;
  links : string list option;
  location : string option;
  pinned_repositories : string list option;
  preferred_handle : string option;
  pronouns : string option;
  stats : string list option;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ avatar bluesky description is_organization links location pinned_repositories preferred_handle pronouns stats -> { avatar; bluesky; description; is_organization; links; location; pinned_repositories; preferred_handle; pronouns; stats })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.actor.profile") ~enc:(fun _ -> "sh.tangled.actor.profile")
  |> Jsont.Object.opt_mem "avatar" Atp.Blob_ref.jsont ~enc:(fun r -> r.avatar)
  |> Jsont.Object.mem "bluesky" Jsont.bool ~enc:(fun r -> r.bluesky)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
  |> Jsont.Object.opt_mem "isOrganization" Jsont.bool ~enc:(fun r -> r.is_organization)
  |> Jsont.Object.opt_mem "links" (Jsont.list Jsont.string) ~enc:(fun r -> r.links)
  |> Jsont.Object.opt_mem "location" Jsont.string ~enc:(fun r -> r.location)
  |> Jsont.Object.opt_mem "pinnedRepositories" (Jsont.list Jsont.string) ~enc:(fun r -> r.pinned_repositories)
  |> Jsont.Object.opt_mem "preferredHandle" Jsont.string ~enc:(fun r -> r.preferred_handle)
  |> Jsont.Object.opt_mem "pronouns" Jsont.string ~enc:(fun r -> r.pronouns)
  |> Jsont.Object.opt_mem "stats" (Jsont.list Jsont.string) ~enc:(fun r -> r.stats)
  |> Jsont.Object.finish

      end
      module Defs = struct
type viewer_state = {
  followed_by : string option;
  following : string option;
}

let viewer_state_jsont =
  Jsont.Object.map ~kind:"Viewer_state"
    (fun _typ followed_by following -> { followed_by; following })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.actor.defs#viewerState") ~enc:(fun _ -> "sh.tangled.actor.defs#viewerState")
  |> Jsont.Object.opt_mem "followedBy" Jsont.string ~enc:(fun r -> r.followed_by)
  |> Jsont.Object.opt_mem "following" Jsont.string ~enc:(fun r -> r.following)
  |> Jsont.Object.finish

type profile_view_basic = {
  avatar : string option;
  did : string;
  handle : string;
  viewer : viewer_state option;
}

let profile_view_basic_jsont =
  Jsont.Object.map ~kind:"Profile_view_basic"
    (fun _typ avatar did handle viewer -> { avatar; did; handle; viewer })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.actor.defs#profileViewBasic") ~enc:(fun _ -> "sh.tangled.actor.defs#profileViewBasic")
  |> Jsont.Object.opt_mem "avatar" Jsont.string ~enc:(fun r -> r.avatar)
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.mem "handle" Jsont.string ~enc:(fun r -> r.handle)
  |> Jsont.Object.opt_mem "viewer" viewer_state_jsont ~enc:(fun r -> r.viewer)
  |> Jsont.Object.finish

type profile_view_detailed = {
  avatar : string option;
  did : string;
  followers_count : int option;
  follows_count : int option;
  handle : string;
  viewer : viewer_state option;
}

let profile_view_detailed_jsont =
  Jsont.Object.map ~kind:"Profile_view_detailed"
    (fun _typ avatar did followers_count follows_count handle viewer -> { avatar; did; followers_count; follows_count; handle; viewer })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.actor.defs#profileViewDetailed") ~enc:(fun _ -> "sh.tangled.actor.defs#profileViewDetailed")
  |> Jsont.Object.opt_mem "avatar" Jsont.string ~enc:(fun r -> r.avatar)
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.opt_mem "followersCount" Jsont.int ~enc:(fun r -> r.followers_count)
  |> Jsont.Object.opt_mem "followsCount" Jsont.int ~enc:(fun r -> r.follows_count)
  |> Jsont.Object.mem "handle" Jsont.string ~enc:(fun r -> r.handle)
  |> Jsont.Object.opt_mem "viewer" viewer_state_jsont ~enc:(fun r -> r.viewer)
  |> Jsont.Object.finish

      end
    end
    module Repo = struct
type main = {
  created_at : string;
  description : string option;
  knot : string;
  labels : string list option;
  name : string option;
  repo_did : string option;
  source : string option;
  spindle : string option;
  topics : string list option;
  website : string option;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at description knot labels name repo_did source spindle topics website -> { created_at; description; knot; labels; name; repo_did; source; spindle; topics; website })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo") ~enc:(fun _ -> "sh.tangled.repo")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
  |> Jsont.Object.mem "knot" Jsont.string ~enc:(fun r -> r.knot)
  |> Jsont.Object.opt_mem "labels" (Jsont.list Jsont.string) ~enc:(fun r -> r.labels)
  |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.opt_mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.opt_mem "source" Jsont.string ~enc:(fun r -> r.source)
  |> Jsont.Object.opt_mem "spindle" Jsont.string ~enc:(fun r -> r.spindle)
  |> Jsont.Object.opt_mem "topics" (Jsont.list Jsont.string) ~enc:(fun r -> r.topics)
  |> Jsont.Object.opt_mem "website" Jsont.string ~enc:(fun r -> r.website)
  |> Jsont.Object.finish

      module Tree = struct
type readme = {
  contents : string;
  filename : string;
}

let readme_jsont =
  Jsont.Object.map ~kind:"Readme"
    (fun _typ contents filename -> { contents; filename })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.tree#readme") ~enc:(fun _ -> "sh.tangled.repo.tree#readme")
  |> Jsont.Object.mem "contents" Jsont.string ~enc:(fun r -> r.contents)
  |> Jsont.Object.mem "filename" Jsont.string ~enc:(fun r -> r.filename)
  |> Jsont.Object.finish

type signature = {
  email : string;
  name : string;
  when_ : string;
}

let signature_jsont =
  Jsont.Object.map ~kind:"Signature"
    (fun _typ email name when_ -> { email; name; when_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.tree#signature") ~enc:(fun _ -> "sh.tangled.repo.tree#signature")
  |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "when" Jsont.string ~enc:(fun r -> r.when_)
  |> Jsont.Object.finish

type last_commit = {
  author : signature option;
  hash : string;
  message : string;
  when_ : string;
}

let last_commit_jsont =
  Jsont.Object.map ~kind:"Last_commit"
    (fun _typ author hash message when_ -> { author; hash; message; when_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.tree#lastCommit") ~enc:(fun _ -> "sh.tangled.repo.tree#lastCommit")
  |> Jsont.Object.opt_mem "author" signature_jsont ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "hash" Jsont.string ~enc:(fun r -> r.hash)
  |> Jsont.Object.mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.mem "when" Jsont.string ~enc:(fun r -> r.when_)
  |> Jsont.Object.finish

type tree_entry = {
  last_commit : last_commit option;
  mode : string;
  name : string;
  size : int;
}

let tree_entry_jsont =
  Jsont.Object.map ~kind:"Tree_entry"
    (fun _typ last_commit mode name size -> { last_commit; mode; name; size })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.tree#treeEntry") ~enc:(fun _ -> "sh.tangled.repo.tree#treeEntry")
  |> Jsont.Object.opt_mem "last_commit" last_commit_jsont ~enc:(fun r -> r.last_commit)
  |> Jsont.Object.mem "mode" Jsont.string ~enc:(fun r -> r.mode)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "size" Jsont.int ~enc:(fun r -> r.size)
  |> Jsont.Object.finish

type params = {
  path : string option;
  ref_ : string;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun path ref_ repo -> {
      path;
      ref_;
      repo;
    })
  |> Jsont.Object.opt_mem "path" Jsont.string
       ~enc:(fun r -> r.path)
  |> Jsont.Object.mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  dotdot : string option;
  files : tree_entry list;
  last_commit : last_commit option;
  parent : string option;
  readme : readme option;
  ref_ : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ dotdot files last_commit parent readme ref_ -> { dotdot; files; last_commit; parent; readme; ref_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.tree#output") ~enc:(fun _ -> "sh.tangled.repo.tree#output")
  |> Jsont.Object.opt_mem "dotdot" Jsont.string ~enc:(fun r -> r.dotdot)
  |> Jsont.Object.mem "files" (Jsont.list tree_entry_jsont) ~enc:(fun r -> r.files)
  |> Jsont.Object.opt_mem "lastCommit" last_commit_jsont ~enc:(fun r -> r.last_commit)
  |> Jsont.Object.opt_mem "parent" Jsont.string ~enc:(fun r -> r.parent)
  |> Jsont.Object.opt_mem "readme" readme_jsont ~enc:(fun r -> r.readme)
  |> Jsont.Object.mem "ref" Jsont.string ~enc:(fun r -> r.ref_)
  |> Jsont.Object.finish

      end
      module Tags = struct
type params = {
  cursor : string option;
  limit : int option;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit repo -> {
      cursor;
      limit;
      repo;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = unit
let output_jsont = Jsont.ignore

      end
      module Tag = struct
type params = {
  repo : string;
  tag : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun repo tag -> {
      repo;
      tag;
    })
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "tag" Jsont.string
       ~enc:(fun r -> r.tag)
  |> Jsont.Object.finish

type output = unit
let output_jsont = Jsont.ignore

      end
      module SetDefaultBranch = struct
type input = {
  default_branch : string;
  repo : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ default_branch repo -> { default_branch; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.setDefaultBranch#input") ~enc:(fun _ -> "sh.tangled.repo.setDefaultBranch#input")
  |> Jsont.Object.mem "defaultBranch" Jsont.string ~enc:(fun r -> r.default_branch)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

      end
      module SetContributionPolicy = struct
type input = {
  contribution_policy : string;
  repo : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ contribution_policy repo -> { contribution_policy; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.setContributionPolicy#input") ~enc:(fun _ -> "sh.tangled.repo.setContributionPolicy#input")
  |> Jsont.Object.mem "contributionPolicy" Jsont.string ~enc:(fun r -> r.contribution_policy)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

      end
      module ReserveKey = struct
type input = {
  repo_did : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ repo_did -> { repo_did })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.reserveKey#input") ~enc:(fun _ -> "sh.tangled.repo.reserveKey#input")
  |> Jsont.Object.mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.finish

type output = {
  key : string;
  repo_did : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ key repo_did -> { key; repo_did })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.reserveKey#output") ~enc:(fun _ -> "sh.tangled.repo.reserveKey#output")
  |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
  |> Jsont.Object.mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.finish

      end
      module Rename = struct
type input = {
  name : string;
  repo : string;
  rkey : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ name repo rkey -> { name; repo; rkey })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.rename#input") ~enc:(fun _ -> "sh.tangled.repo.rename#input")
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "rkey" Jsont.string ~enc:(fun r -> r.rkey)
  |> Jsont.Object.finish

      end
      module RemoveSecret = struct
type input = {
  key : string;
  repo : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ key repo -> { key; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.removeSecret#input") ~enc:(fun _ -> "sh.tangled.repo.removeSecret#input")
  |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

      end
      module RemoveCollaborator = struct
type input = {
  repo : string;
  subject : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ repo subject -> { repo; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.removeCollaborator#input") ~enc:(fun _ -> "sh.tangled.repo.removeCollaborator#input")
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module Push = struct
      end
      module Pull = struct
type round = {
  created_at : string;
  patch_blob : Atp.Blob_ref.t;
}

let round_jsont =
  Jsont.Object.map ~kind:"Round"
    (fun _typ created_at patch_blob -> { created_at; patch_blob })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.pull#round") ~enc:(fun _ -> "sh.tangled.repo.pull#round")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "patchBlob" Atp.Blob_ref.jsont ~enc:(fun r -> r.patch_blob)
  |> Jsont.Object.finish

type source = {
  branch : string;
  repo : string option;
}

let source_jsont =
  Jsont.Object.map ~kind:"Source"
    (fun _typ branch repo -> { branch; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.pull#source") ~enc:(fun _ -> "sh.tangled.repo.pull#source")
  |> Jsont.Object.mem "branch" Jsont.string ~enc:(fun r -> r.branch)
  |> Jsont.Object.opt_mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type target = {
  branch : string;
  repo : string;
}

let target_jsont =
  Jsont.Object.map ~kind:"Target"
    (fun _typ branch repo -> { branch; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.pull#target") ~enc:(fun _ -> "sh.tangled.repo.pull#target")
  |> Jsont.Object.mem "branch" Jsont.string ~enc:(fun r -> r.branch)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type version = {
  base : string;
  created_at : string;
  head : string;
}

let version_jsont =
  Jsont.Object.map ~kind:"Version"
    (fun _typ base created_at head -> { base; created_at; head })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.pull#version") ~enc:(fun _ -> "sh.tangled.repo.pull#version")
  |> Jsont.Object.mem "base" Jsont.string ~enc:(fun r -> r.base)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "head" Jsont.string ~enc:(fun r -> r.head)
  |> Jsont.Object.finish

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
  versions : version list;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ blobs body created_at dependent_on mentions references rounds source target title versions -> { blobs; body; created_at; dependent_on; mentions; references; rounds; source; target; title; versions })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.pull") ~enc:(fun _ -> "sh.tangled.repo.pull")
  |> Jsont.Object.opt_mem "blobs" (Jsont.list Atp.Blob_ref.jsont) ~enc:(fun r -> r.blobs)
  |> Jsont.Object.opt_mem "body" Jsont.string ~enc:(fun r -> r.body)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "dependentOn" Jsont.string ~enc:(fun r -> r.dependent_on)
  |> Jsont.Object.opt_mem "mentions" (Jsont.list Jsont.string) ~enc:(fun r -> r.mentions)
  |> Jsont.Object.opt_mem "references" (Jsont.list Jsont.string) ~enc:(fun r -> r.references)
  |> Jsont.Object.mem "rounds" (Jsont.list round_jsont) ~enc:(fun r -> r.rounds)
  |> Jsont.Object.opt_mem "source" source_jsont ~enc:(fun r -> r.source)
  |> Jsont.Object.mem "target" target_jsont ~enc:(fun r -> r.target)
  |> Jsont.Object.mem "title" Jsont.string ~enc:(fun r -> r.title)
  |> Jsont.Object.mem "versions" (Jsont.list version_jsont) ~enc:(fun r -> r.versions)
  |> Jsont.Object.finish

        module Status = struct
type main = {
  created_at : string;
  pull : string;
  status : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at pull status -> { created_at; pull; status })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.pull.status") ~enc:(fun _ -> "sh.tangled.repo.pull.status")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "pull" Jsont.string ~enc:(fun r -> r.pull)
  |> Jsont.Object.mem "status" Jsont.string ~enc:(fun r -> r.status)
  |> Jsont.Object.finish

          module Open = struct
type main = string
let main_jsont = Jsont.string

          end
          module Merged = struct
type main = string
let main_jsont = Jsont.string

          end
          module Closed = struct
type main = string
let main_jsont = Jsont.string

          end
        end
        module Merge = struct
type input = {
  repo : string;
  rkey : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ repo rkey -> { repo; rkey })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.pull.merge#input") ~enc:(fun _ -> "sh.tangled.repo.pull.merge#input")
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "rkey" Jsont.string ~enc:(fun r -> r.rkey)
  |> Jsont.Object.finish

type output = {
  merged : string list;
  tip : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ merged tip -> { merged; tip })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.pull.merge#output") ~enc:(fun _ -> "sh.tangled.repo.pull.merge#output")
  |> Jsont.Object.mem "merged" (Jsont.list Jsont.string) ~enc:(fun r -> r.merged)
  |> Jsont.Object.mem "tip" Jsont.string ~enc:(fun r -> r.tip)
  |> Jsont.Object.finish

        end
        module ListStatuses = struct
type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.pull.listStatuses#listItem") ~enc:(fun _ -> "sh.tangled.repo.pull.listStatuses#listItem")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit order subject -> {
      cursor;
      limit;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : list_item list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items -> { cursor; items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.pull.listStatuses#output") ~enc:(fun _ -> "sh.tangled.repo.pull.listStatuses#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

        end
        module CountStatusesBy = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.pull.countStatusesBy#output") ~enc:(fun _ -> "sh.tangled.repo.pull.countStatusesBy#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

        end
        module CountStatuses = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.pull.countStatuses#output") ~enc:(fun _ -> "sh.tangled.repo.pull.countStatuses#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

        end
        module Comment = struct
type main = {
  body : string;
  created_at : string;
  mentions : string list option;
  pull : string;
  references : string list option;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ body created_at mentions pull references -> { body; created_at; mentions; pull; references })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.pull.comment") ~enc:(fun _ -> "sh.tangled.repo.pull.comment")
  |> Jsont.Object.mem "body" Jsont.string ~enc:(fun r -> r.body)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "mentions" (Jsont.list Jsont.string) ~enc:(fun r -> r.mentions)
  |> Jsont.Object.mem "pull" Jsont.string ~enc:(fun r -> r.pull)
  |> Jsont.Object.opt_mem "references" (Jsont.list Jsont.string) ~enc:(fun r -> r.references)
  |> Jsont.Object.finish

        end
        module ListStatusesBy = struct
type params = {
  cursor : string option;
  limit : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit order subject -> {
      cursor;
      limit;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : ListStatuses.list_item list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items -> { cursor; items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.pull.listStatusesBy#output") ~enc:(fun _ -> "sh.tangled.repo.pull.listStatusesBy#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list ListStatuses.list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

        end
      end
      module MergeCheck = struct
type conflict_info = {
  filename : string;
  reason : string;
}

let conflict_info_jsont =
  Jsont.Object.map ~kind:"Conflict_info"
    (fun _typ filename reason -> { filename; reason })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.mergeCheck#conflictInfo") ~enc:(fun _ -> "sh.tangled.repo.mergeCheck#conflictInfo")
  |> Jsont.Object.mem "filename" Jsont.string ~enc:(fun r -> r.filename)
  |> Jsont.Object.mem "reason" Jsont.string ~enc:(fun r -> r.reason)
  |> Jsont.Object.finish

type input = {
  branch : string;
  did : string option;
  name : string option;
  patch : string;
  repo : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ branch did name patch repo -> { branch; did; name; patch; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.mergeCheck#input") ~enc:(fun _ -> "sh.tangled.repo.mergeCheck#input")
  |> Jsont.Object.mem "branch" Jsont.string ~enc:(fun r -> r.branch)
  |> Jsont.Object.opt_mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "patch" Jsont.string ~enc:(fun r -> r.patch)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  conflicts : conflict_info list option;
  error : string option;
  is_conflicted : bool;
  message : string option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ conflicts error is_conflicted message -> { conflicts; error; is_conflicted; message })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.mergeCheck#output") ~enc:(fun _ -> "sh.tangled.repo.mergeCheck#output")
  |> Jsont.Object.opt_mem "conflicts" (Jsont.list conflict_info_jsont) ~enc:(fun r -> r.conflicts)
  |> Jsont.Object.opt_mem "error" Jsont.string ~enc:(fun r -> r.error)
  |> Jsont.Object.mem "is_conflicted" Jsont.bool ~enc:(fun r -> r.is_conflicted)
  |> Jsont.Object.opt_mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.finish

      end
      module Merge = struct
type input = {
  author_email : string option;
  author_name : string option;
  branch : string;
  commit_body : string option;
  commit_message : string option;
  did : string option;
  name : string option;
  patch : string;
  repo : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ author_email author_name branch commit_body commit_message did name patch repo -> { author_email; author_name; branch; commit_body; commit_message; did; name; patch; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.merge#input") ~enc:(fun _ -> "sh.tangled.repo.merge#input")
  |> Jsont.Object.opt_mem "authorEmail" Jsont.string ~enc:(fun r -> r.author_email)
  |> Jsont.Object.opt_mem "authorName" Jsont.string ~enc:(fun r -> r.author_name)
  |> Jsont.Object.mem "branch" Jsont.string ~enc:(fun r -> r.branch)
  |> Jsont.Object.opt_mem "commitBody" Jsont.string ~enc:(fun r -> r.commit_body)
  |> Jsont.Object.opt_mem "commitMessage" Jsont.string ~enc:(fun r -> r.commit_message)
  |> Jsont.Object.opt_mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "patch" Jsont.string ~enc:(fun r -> r.patch)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

      end
      module Log = struct
type params = {
  cursor : string option;
  limit : int option;
  path : string option;
  ref_ : string;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit path ref_ repo -> {
      cursor;
      limit;
      path;
      ref_;
      repo;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "path" Jsont.string
       ~enc:(fun r -> r.path)
  |> Jsont.Object.mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = unit
let output_jsont = Jsont.ignore

      end
      module ListSecrets = struct
type secret = {
  created_at : string;
  created_by : string;
  key : string;
  repo : string;
}

let secret_jsont =
  Jsont.Object.map ~kind:"Secret"
    (fun _typ created_at created_by key repo -> { created_at; created_by; key; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.listSecrets#secret") ~enc:(fun _ -> "sh.tangled.repo.listSecrets#secret")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "createdBy" Jsont.string ~enc:(fun r -> r.created_by)
  |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type params = {
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun repo -> {
      repo;
    })
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  secrets : secret list option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ secrets -> { secrets })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.listSecrets#output") ~enc:(fun _ -> "sh.tangled.repo.listSecrets#output")
  |> Jsont.Object.mem "secrets" (Jsont.option (Jsont.list secret_jsont)) ~enc:(fun r -> r.secrets)
  |> Jsont.Object.finish

      end
      module ListCollaborators = struct
type list_item = {
  added_by : string;
  cid : string option;
  created_at : string;
  effective_since : string;
  subject : string;
  uri : string option;
  verified_at : string option;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ added_by cid created_at effective_since subject uri verified_at -> { added_by; cid; created_at; effective_since; subject; uri; verified_at })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.listCollaborators#listItem") ~enc:(fun _ -> "sh.tangled.repo.listCollaborators#listItem")
  |> Jsont.Object.mem "addedBy" Jsont.string ~enc:(fun r -> r.added_by)
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "effectiveSince" Jsont.string ~enc:(fun r -> r.effective_since)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.opt_mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.opt_mem "verifiedAt" Jsont.string ~enc:(fun r -> r.verified_at)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  offset : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit offset order subject -> {
      cursor;
      limit;
      offset;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "offset" Jsont.int
       ~enc:(fun r -> r.offset)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : list_item list;
  total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items total -> { cursor; items; total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.listCollaborators#output") ~enc:(fun _ -> "sh.tangled.repo.listCollaborators#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.finish

      end
      module ListCollaboratorInvites = struct
type invite_item = {
  added_by : string;
  created_at : string;
  subject : string;
}

let invite_item_jsont =
  Jsont.Object.map ~kind:"Invite_item"
    (fun _typ added_by created_at subject -> { added_by; created_at; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.listCollaboratorInvites#inviteItem") ~enc:(fun _ -> "sh.tangled.repo.listCollaboratorInvites#inviteItem")
  |> Jsont.Object.mem "addedBy" Jsont.string ~enc:(fun r -> r.added_by)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit order subject -> {
      cursor;
      limit;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : invite_item list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items -> { cursor; items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.listCollaboratorInvites#output") ~enc:(fun _ -> "sh.tangled.repo.listCollaboratorInvites#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list invite_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

      end
      module ListCobVersions = struct
type version = {
  change_id : string;
  cid : string option;
  created_at : string;
  editor : string;
}

let version_jsont =
  Jsont.Object.map ~kind:"Version"
    (fun _typ change_id cid created_at editor -> { change_id; cid; created_at; editor })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.listCobVersions#version") ~enc:(fun _ -> "sh.tangled.repo.listCobVersions#version")
  |> Jsont.Object.mem "changeId" Jsont.string ~enc:(fun r -> r.change_id)
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "editor" Jsont.string ~enc:(fun r -> r.editor)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  order : string option;
  uri : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit order uri -> {
      cursor;
      limit;
      order;
      uri;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "uri" Jsont.string
       ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  total : int option;
  uri : string;
  versions : version list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor total uri versions -> { cursor; total; uri; versions })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.listCobVersions#output") ~enc:(fun _ -> "sh.tangled.repo.listCobVersions#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "versions" (Jsont.list version_jsont) ~enc:(fun r -> r.versions)
  |> Jsont.Object.finish

      end
      module Languages = struct
type language = {
  color : string option;
  extensions : string list option;
  file_count : int option;
  name : string;
  percentage : int;
  size : int;
}

let language_jsont =
  Jsont.Object.map ~kind:"Language"
    (fun _typ color extensions file_count name percentage size -> { color; extensions; file_count; name; percentage; size })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.languages#language") ~enc:(fun _ -> "sh.tangled.repo.languages#language")
  |> Jsont.Object.opt_mem "color" Jsont.string ~enc:(fun r -> r.color)
  |> Jsont.Object.opt_mem "extensions" (Jsont.list Jsont.string) ~enc:(fun r -> r.extensions)
  |> Jsont.Object.opt_mem "fileCount" Jsont.int ~enc:(fun r -> r.file_count)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "percentage" Jsont.int ~enc:(fun r -> r.percentage)
  |> Jsont.Object.mem "size" Jsont.int ~enc:(fun r -> r.size)
  |> Jsont.Object.finish

type params = {
  ref_ : string option;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun ref_ repo -> {
      ref_;
      repo;
    })
  |> Jsont.Object.opt_mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  languages : language list option;
  ref_ : string;
  total_files : int option;
  total_size : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ languages ref_ total_files total_size -> { languages; ref_; total_files; total_size })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.languages#output") ~enc:(fun _ -> "sh.tangled.repo.languages#output")
  |> Jsont.Object.mem "languages" (Jsont.option (Jsont.list language_jsont)) ~enc:(fun r -> r.languages)
  |> Jsont.Object.mem "ref" Jsont.string ~enc:(fun r -> r.ref_)
  |> Jsont.Object.opt_mem "totalFiles" Jsont.int ~enc:(fun r -> r.total_files)
  |> Jsont.Object.opt_mem "totalSize" Jsont.int ~enc:(fun r -> r.total_size)
  |> Jsont.Object.finish

      end
      module Issue = struct
type main = {
  blobs : Atp.Blob_ref.t list option;
  body : string option;
  created_at : string;
  mentions : string list option;
  references : string list option;
  repo : string;
  title : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ blobs body created_at mentions references repo title -> { blobs; body; created_at; mentions; references; repo; title })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.issue") ~enc:(fun _ -> "sh.tangled.repo.issue")
  |> Jsont.Object.opt_mem "blobs" (Jsont.list Atp.Blob_ref.jsont) ~enc:(fun r -> r.blobs)
  |> Jsont.Object.opt_mem "body" Jsont.string ~enc:(fun r -> r.body)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "mentions" (Jsont.list Jsont.string) ~enc:(fun r -> r.mentions)
  |> Jsont.Object.opt_mem "references" (Jsont.list Jsont.string) ~enc:(fun r -> r.references)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "title" Jsont.string ~enc:(fun r -> r.title)
  |> Jsont.Object.finish

        module State = struct
type main = {
  created_at : string;
  issue : string;
  state : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at issue state -> { created_at; issue; state })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.issue.state") ~enc:(fun _ -> "sh.tangled.repo.issue.state")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "issue" Jsont.string ~enc:(fun r -> r.issue)
  |> Jsont.Object.mem "state" Jsont.string ~enc:(fun r -> r.state)
  |> Jsont.Object.finish

          module Open = struct
type main = string
let main_jsont = Jsont.string

          end
          module Closed = struct
type main = string
let main_jsont = Jsont.string

          end
        end
        module ListStates = struct
type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.issue.listStates#listItem") ~enc:(fun _ -> "sh.tangled.repo.issue.listStates#listItem")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit order subject -> {
      cursor;
      limit;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : list_item list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items -> { cursor; items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.issue.listStates#output") ~enc:(fun _ -> "sh.tangled.repo.issue.listStates#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

        end
        module CountStatesBy = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.issue.countStatesBy#output") ~enc:(fun _ -> "sh.tangled.repo.issue.countStatesBy#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

        end
        module CountStates = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.issue.countStates#output") ~enc:(fun _ -> "sh.tangled.repo.issue.countStates#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

        end
        module Comment = struct
type main = {
  body : string;
  created_at : string;
  issue : string;
  mentions : string list option;
  references : string list option;
  reply_to : string option;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ body created_at issue mentions references reply_to -> { body; created_at; issue; mentions; references; reply_to })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.issue.comment") ~enc:(fun _ -> "sh.tangled.repo.issue.comment")
  |> Jsont.Object.mem "body" Jsont.string ~enc:(fun r -> r.body)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "issue" Jsont.string ~enc:(fun r -> r.issue)
  |> Jsont.Object.opt_mem "mentions" (Jsont.list Jsont.string) ~enc:(fun r -> r.mentions)
  |> Jsont.Object.opt_mem "references" (Jsont.list Jsont.string) ~enc:(fun r -> r.references)
  |> Jsont.Object.opt_mem "replyTo" Jsont.string ~enc:(fun r -> r.reply_to)
  |> Jsont.Object.finish

        end
        module ListStatesBy = struct
type params = {
  cursor : string option;
  limit : int option;
  order : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit order subject -> {
      cursor;
      limit;
      order;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : ListStates.list_item list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items -> { cursor; items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.issue.listStatesBy#output") ~enc:(fun _ -> "sh.tangled.repo.issue.listStatesBy#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list ListStates.list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

        end
      end
      module HiddenRef = struct
type input = {
  fork_ref : string;
  remote_ref : string;
  repo : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ fork_ref remote_ref repo -> { fork_ref; remote_ref; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.hiddenRef#input") ~enc:(fun _ -> "sh.tangled.repo.hiddenRef#input")
  |> Jsont.Object.mem "forkRef" Jsont.string ~enc:(fun r -> r.fork_ref)
  |> Jsont.Object.mem "remoteRef" Jsont.string ~enc:(fun r -> r.remote_ref)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  error : string option;
  ref_ : string option;
  success : bool;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ error ref_ success -> { error; ref_; success })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.hiddenRef#output") ~enc:(fun _ -> "sh.tangled.repo.hiddenRef#output")
  |> Jsont.Object.opt_mem "error" Jsont.string ~enc:(fun r -> r.error)
  |> Jsont.Object.opt_mem "ref" Jsont.string ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "success" Jsont.bool ~enc:(fun r -> r.success)
  |> Jsont.Object.finish

      end
      module GetPulls = struct
type record_view = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let record_view_jsont =
  Jsont.Object.map ~kind:"Record_view"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.getPulls#recordView") ~enc:(fun _ -> "sh.tangled.repo.getPulls#recordView")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  pulls : string list;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun pulls -> {
      pulls;
    })
  |> Jsont.Object.mem "pulls" (Jsont.list Jsont.string)
       ~enc:(fun r -> r.pulls)
  |> Jsont.Object.finish

type output = {
  items : record_view list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ items -> { items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.getPulls#output") ~enc:(fun _ -> "sh.tangled.repo.getPulls#output")
  |> Jsont.Object.mem "items" (Jsont.list record_view_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

      end
      module GetPull = struct
type params = {
  pull : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun pull -> {
      pull;
    })
  |> Jsont.Object.mem "pull" Jsont.string
       ~enc:(fun r -> r.pull)
  |> Jsont.Object.finish

type output = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.getPull#output") ~enc:(fun _ -> "sh.tangled.repo.getPull#output")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

      end
      module GetIssues = struct
type record_view = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let record_view_jsont =
  Jsont.Object.map ~kind:"Record_view"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.getIssues#recordView") ~enc:(fun _ -> "sh.tangled.repo.getIssues#recordView")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  issues : string list;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun issues -> {
      issues;
    })
  |> Jsont.Object.mem "issues" (Jsont.list Jsont.string)
       ~enc:(fun r -> r.issues)
  |> Jsont.Object.finish

type output = {
  items : record_view list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ items -> { items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.getIssues#output") ~enc:(fun _ -> "sh.tangled.repo.getIssues#output")
  |> Jsont.Object.mem "items" (Jsont.list record_view_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

      end
      module GetIssue = struct
type params = {
  issue : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun issue -> {
      issue;
    })
  |> Jsont.Object.mem "issue" Jsont.string
       ~enc:(fun r -> r.issue)
  |> Jsont.Object.finish

type output = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.getIssue#output") ~enc:(fun _ -> "sh.tangled.repo.getIssue#output")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

      end
      module GetDefaultBranch = struct
type signature = {
  email : string;
  name : string;
  when_ : string;
}

let signature_jsont =
  Jsont.Object.map ~kind:"Signature"
    (fun _typ email name when_ -> { email; name; when_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.getDefaultBranch#signature") ~enc:(fun _ -> "sh.tangled.repo.getDefaultBranch#signature")
  |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "when" Jsont.string ~enc:(fun r -> r.when_)
  |> Jsont.Object.finish

type params = {
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun repo -> {
      repo;
    })
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  author : signature option;
  hash : string;
  message : string option;
  name : string;
  short_hash : string option;
  when_ : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ author hash message name short_hash when_ -> { author; hash; message; name; short_hash; when_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.getDefaultBranch#output") ~enc:(fun _ -> "sh.tangled.repo.getDefaultBranch#output")
  |> Jsont.Object.opt_mem "author" signature_jsont ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "hash" Jsont.string ~enc:(fun r -> r.hash)
  |> Jsont.Object.opt_mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.opt_mem "shortHash" Jsont.string ~enc:(fun r -> r.short_hash)
  |> Jsont.Object.mem "when" Jsont.string ~enc:(fun r -> r.when_)
  |> Jsont.Object.finish

      end
      module ForkSync = struct
type input = {
  branch : string;
  did : string option;
  name : string option;
  repo : string;
  source : string option;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ branch did name repo source -> { branch; did; name; repo; source })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.forkSync#input") ~enc:(fun _ -> "sh.tangled.repo.forkSync#input")
  |> Jsont.Object.mem "branch" Jsont.string ~enc:(fun r -> r.branch)
  |> Jsont.Object.opt_mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.opt_mem "source" Jsont.string ~enc:(fun r -> r.source)
  |> Jsont.Object.finish

      end
      module Diff = struct
type params = {
  ref_ : string;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun ref_ repo -> {
      ref_;
      repo;
    })
  |> Jsont.Object.mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = unit
let output_jsont = Jsont.ignore

      end
      module DescribeRepo = struct
type params = {
  repo_did : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun repo_did -> {
      repo_did;
    })
  |> Jsont.Object.mem "repoDid" Jsont.string
       ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.finish

type output = {
  content : string option;
  owner_did : string;
  reason : string option;
  repo_did : string;
  rkey : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ content owner_did reason repo_did rkey -> { content; owner_did; reason; repo_did; rkey })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.describeRepo#output") ~enc:(fun _ -> "sh.tangled.repo.describeRepo#output")
  |> Jsont.Object.opt_mem "content" Jsont.string ~enc:(fun r -> r.content)
  |> Jsont.Object.mem "ownerDid" Jsont.string ~enc:(fun r -> r.owner_did)
  |> Jsont.Object.opt_mem "reason" Jsont.string ~enc:(fun r -> r.reason)
  |> Jsont.Object.mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.mem "rkey" Jsont.string ~enc:(fun r -> r.rkey)
  |> Jsont.Object.finish

      end
      module DeleteBranch = struct
type input = {
  branch : string;
  repo : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ branch repo -> { branch; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.deleteBranch#input") ~enc:(fun _ -> "sh.tangled.repo.deleteBranch#input")
  |> Jsont.Object.mem "branch" Jsont.string ~enc:(fun r -> r.branch)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

      end
      module Delete = struct
type input = {
  did : string option;
  force : bool option;
  name : string option;
  repo : string;
  rkey : string option;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ did force name repo rkey -> { did; force; name; repo; rkey })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.delete#input") ~enc:(fun _ -> "sh.tangled.repo.delete#input")
  |> Jsont.Object.opt_mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.opt_mem "force" Jsont.bool ~enc:(fun r -> r.force)
  |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.opt_mem "rkey" Jsont.string ~enc:(fun r -> r.rkey)
  |> Jsont.Object.finish

      end
      module Defs = struct
type viewer_state = {
  fork : string option;
  star : string option;
}

let viewer_state_jsont =
  Jsont.Object.map ~kind:"Viewer_state"
    (fun _typ fork star -> { fork; star })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.defs#viewerState") ~enc:(fun _ -> "sh.tangled.repo.defs#viewerState")
  |> Jsont.Object.opt_mem "fork" Jsont.string ~enc:(fun r -> r.fork)
  |> Jsont.Object.opt_mem "star" Jsont.string ~enc:(fun r -> r.star)
  |> Jsont.Object.finish

type repo_view_basic = {
  created_at : string;
  description : string option;
  did : string;
  fork_count : int option;
  is_starred : bool option;
  open_issue_count : int option;
  open_pull_count : int option;
  owner : Actor.Defs.profile_view_basic;
  primary_language : string option;
  slug : string;
  star_count : int option;
  viewer : viewer_state option;
}

let repo_view_basic_jsont =
  Jsont.Object.map ~kind:"Repo_view_basic"
    (fun _typ created_at description did fork_count is_starred open_issue_count open_pull_count owner primary_language slug star_count viewer -> { created_at; description; did; fork_count; is_starred; open_issue_count; open_pull_count; owner; primary_language; slug; star_count; viewer })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.defs#repoViewBasic") ~enc:(fun _ -> "sh.tangled.repo.defs#repoViewBasic")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.opt_mem "forkCount" Jsont.int ~enc:(fun r -> r.fork_count)
  |> Jsont.Object.opt_mem "isStarred" Jsont.bool ~enc:(fun r -> r.is_starred)
  |> Jsont.Object.opt_mem "openIssueCount" Jsont.int ~enc:(fun r -> r.open_issue_count)
  |> Jsont.Object.opt_mem "openPullCount" Jsont.int ~enc:(fun r -> r.open_pull_count)
  |> Jsont.Object.mem "owner" Actor.Defs.profile_view_basic_jsont ~enc:(fun r -> r.owner)
  |> Jsont.Object.opt_mem "primaryLanguage" Jsont.string ~enc:(fun r -> r.primary_language)
  |> Jsont.Object.mem "slug" Jsont.string ~enc:(fun r -> r.slug)
  |> Jsont.Object.opt_mem "starCount" Jsont.int ~enc:(fun r -> r.star_count)
  |> Jsont.Object.opt_mem "viewer" viewer_state_jsont ~enc:(fun r -> r.viewer)
  |> Jsont.Object.finish

      end
      module Create = struct
type source = {
  kind : string;
  url : string;
}

let source_jsont =
  Jsont.Object.map ~kind:"Source"
    (fun _typ kind url -> { kind; url })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.create#source") ~enc:(fun _ -> "sh.tangled.repo.create#source")
  |> Jsont.Object.mem "kind" Jsont.string ~enc:(fun r -> r.kind)
  |> Jsont.Object.mem "url" Jsont.string ~enc:(fun r -> r.url)
  |> Jsont.Object.finish

type input = {
  default_branch : string option;
  name : string;
  repo_did : string option;
  rkey : string;
  source : source option;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ default_branch name repo_did rkey source -> { default_branch; name; repo_did; rkey; source })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.create#input") ~enc:(fun _ -> "sh.tangled.repo.create#input")
  |> Jsont.Object.opt_mem "defaultBranch" Jsont.string ~enc:(fun r -> r.default_branch)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.opt_mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.mem "rkey" Jsont.string ~enc:(fun r -> r.rkey)
  |> Jsont.Object.opt_mem "source" source_jsont ~enc:(fun r -> r.source)
  |> Jsont.Object.finish

type output = {
  key : string option;
  repo_did : string option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ key repo_did -> { key; repo_did })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.create#output") ~enc:(fun _ -> "sh.tangled.repo.create#output")
  |> Jsont.Object.opt_mem "key" Jsont.string ~enc:(fun r -> r.key)
  |> Jsont.Object.opt_mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.finish

      end
      module CountPullsBy = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.countPullsBy#output") ~enc:(fun _ -> "sh.tangled.repo.countPullsBy#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module CountIssuesBy = struct
type params = {
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun subject -> {
      subject;
    })
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  count : int;
  distinct_authors : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count distinct_authors -> { count; distinct_authors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.countIssuesBy#output") ~enc:(fun _ -> "sh.tangled.repo.countIssuesBy#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module Compare = struct
type params = {
  repo : string;
  rev1 : string;
  rev2 : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun repo rev1 rev2 -> {
      repo;
      rev1;
      rev2;
    })
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "rev1" Jsont.string
       ~enc:(fun r -> r.rev1)
  |> Jsont.Object.mem "rev2" Jsont.string
       ~enc:(fun r -> r.rev2)
  |> Jsont.Object.finish

type output = unit
let output_jsont = Jsont.ignore

      end
      module CollaboratorInvite = struct
type main = {
  created_at : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at -> { created_at })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.collaboratorInvite") ~enc:(fun _ -> "sh.tangled.repo.collaboratorInvite")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.finish

      end
      module CollaboratorAcceptance = struct
type main = {
  created_at : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at -> { created_at })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.collaboratorAcceptance") ~enc:(fun _ -> "sh.tangled.repo.collaboratorAcceptance")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.finish

      end
      module Collaborator = struct
type main = {
  created_at : string;
  repo : string;
  subject : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at repo subject -> { created_at; repo; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.collaborator") ~enc:(fun _ -> "sh.tangled.repo.collaborator")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module CheckPushAllowed = struct
type params = {
  key : string;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun key repo -> {
      key;
      repo;
    })
  |> Jsont.Object.mem "key" Jsont.string
       ~enc:(fun r -> r.key)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  allowed : bool;
  did : string option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ allowed did -> { allowed; did })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.checkPushAllowed#output") ~enc:(fun _ -> "sh.tangled.repo.checkPushAllowed#output")
  |> Jsont.Object.mem "allowed" Jsont.bool ~enc:(fun r -> r.allowed)
  |> Jsont.Object.opt_mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.finish

      end
      module Branches = struct
type params = {
  cursor : string option;
  limit : int option;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit repo -> {
      cursor;
      limit;
      repo;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = unit
let output_jsont = Jsont.ignore

      end
      module Branch = struct
type signature = {
  email : string;
  name : string;
  when_ : string;
}

let signature_jsont =
  Jsont.Object.map ~kind:"Signature"
    (fun _typ email name when_ -> { email; name; when_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.branch#signature") ~enc:(fun _ -> "sh.tangled.repo.branch#signature")
  |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "when" Jsont.string ~enc:(fun r -> r.when_)
  |> Jsont.Object.finish

type params = {
  name : string;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun name repo -> {
      name;
      repo;
    })
  |> Jsont.Object.mem "name" Jsont.string
       ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  author : signature option;
  hash : string;
  is_default : bool option;
  message : string option;
  name : string;
  short_hash : string option;
  when_ : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ author hash is_default message name short_hash when_ -> { author; hash; is_default; message; name; short_hash; when_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.branch#output") ~enc:(fun _ -> "sh.tangled.repo.branch#output")
  |> Jsont.Object.opt_mem "author" signature_jsont ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "hash" Jsont.string ~enc:(fun r -> r.hash)
  |> Jsont.Object.opt_mem "isDefault" Jsont.bool ~enc:(fun r -> r.is_default)
  |> Jsont.Object.opt_mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.opt_mem "shortHash" Jsont.string ~enc:(fun r -> r.short_hash)
  |> Jsont.Object.mem "when" Jsont.string ~enc:(fun r -> r.when_)
  |> Jsont.Object.finish

      end
      module Blob = struct
type signature = {
  email : string;
  name : string;
  when_ : string;
}

let signature_jsont =
  Jsont.Object.map ~kind:"Signature"
    (fun _typ email name when_ -> { email; name; when_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.blob#signature") ~enc:(fun _ -> "sh.tangled.repo.blob#signature")
  |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "when" Jsont.string ~enc:(fun r -> r.when_)
  |> Jsont.Object.finish

type submodule = {
  branch : string option;
  name : string;
  url : string;
}

let submodule_jsont =
  Jsont.Object.map ~kind:"Submodule"
    (fun _typ branch name url -> { branch; name; url })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.blob#submodule") ~enc:(fun _ -> "sh.tangled.repo.blob#submodule")
  |> Jsont.Object.opt_mem "branch" Jsont.string ~enc:(fun r -> r.branch)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "url" Jsont.string ~enc:(fun r -> r.url)
  |> Jsont.Object.finish

type last_commit = {
  author : signature option;
  hash : string;
  message : string;
  when_ : string;
}

let last_commit_jsont =
  Jsont.Object.map ~kind:"Last_commit"
    (fun _typ author hash message when_ -> { author; hash; message; when_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.blob#lastCommit") ~enc:(fun _ -> "sh.tangled.repo.blob#lastCommit")
  |> Jsont.Object.opt_mem "author" signature_jsont ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "hash" Jsont.string ~enc:(fun r -> r.hash)
  |> Jsont.Object.mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.mem "when" Jsont.string ~enc:(fun r -> r.when_)
  |> Jsont.Object.finish

type params = {
  path : string;
  raw : bool option;
  ref_ : string;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun path raw ref_ repo -> {
      path;
      raw;
      ref_;
      repo;
    })
  |> Jsont.Object.mem "path" Jsont.string
       ~enc:(fun r -> r.path)
  |> Jsont.Object.opt_mem "raw" Jsont.bool
       ~enc:(fun r -> r.raw)
  |> Jsont.Object.mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  content : string option;
  encoding : string option;
  file_too_large : bool option;
  is_binary : bool option;
  last_commit : last_commit option;
  mime_type : string option;
  path : string;
  ref_ : string;
  size : int option;
  submodule : submodule option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ content encoding file_too_large is_binary last_commit mime_type path ref_ size submodule -> { content; encoding; file_too_large; is_binary; last_commit; mime_type; path; ref_; size; submodule })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.blob#output") ~enc:(fun _ -> "sh.tangled.repo.blob#output")
  |> Jsont.Object.opt_mem "content" Jsont.string ~enc:(fun r -> r.content)
  |> Jsont.Object.opt_mem "encoding" Jsont.string ~enc:(fun r -> r.encoding)
  |> Jsont.Object.opt_mem "fileTooLarge" Jsont.bool ~enc:(fun r -> r.file_too_large)
  |> Jsont.Object.opt_mem "isBinary" Jsont.bool ~enc:(fun r -> r.is_binary)
  |> Jsont.Object.opt_mem "lastCommit" last_commit_jsont ~enc:(fun r -> r.last_commit)
  |> Jsont.Object.opt_mem "mimeType" Jsont.string ~enc:(fun r -> r.mime_type)
  |> Jsont.Object.mem "path" Jsont.string ~enc:(fun r -> r.path)
  |> Jsont.Object.mem "ref" Jsont.string ~enc:(fun r -> r.ref_)
  |> Jsont.Object.opt_mem "size" Jsont.int ~enc:(fun r -> r.size)
  |> Jsont.Object.opt_mem "submodule" submodule_jsont ~enc:(fun r -> r.submodule)
  |> Jsont.Object.finish

      end
      module Blame = struct
type author = {
  email : string;
  name : string;
}

let author_jsont =
  Jsont.Object.map ~kind:"Author"
    (fun _typ email name -> { email; name })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.blame#author") ~enc:(fun _ -> "sh.tangled.repo.blame#author")
  |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.finish

type run = {
  commit : string;
  count : int;
  start : int;
}

let run_jsont =
  Jsont.Object.map ~kind:"Run"
    (fun _typ commit count start -> { commit; count; start })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.blame#run") ~enc:(fun _ -> "sh.tangled.repo.blame#run")
  |> Jsont.Object.mem "commit" Jsont.string ~enc:(fun r -> r.commit)
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "start" Jsont.int ~enc:(fun r -> r.start)
  |> Jsont.Object.finish

type commit = {
  author : author;
  authored_at : string;
  sha : string;
  subject : string;
}

let commit_jsont =
  Jsont.Object.map ~kind:"Commit"
    (fun _typ author authored_at sha subject -> { author; authored_at; sha; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.blame#commit") ~enc:(fun _ -> "sh.tangled.repo.blame#commit")
  |> Jsont.Object.mem "author" author_jsont ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "authoredAt" Jsont.string ~enc:(fun r -> r.authored_at)
  |> Jsont.Object.mem "sha" Jsont.string ~enc:(fun r -> r.sha)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type params = {
  path : string;
  ref_ : string;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun path ref_ repo -> {
      path;
      ref_;
      repo;
    })
  |> Jsont.Object.mem "path" Jsont.string
       ~enc:(fun r -> r.path)
  |> Jsont.Object.mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  commit : string;
  commits : commit list;
  runs : run list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ commit commits runs -> { commit; commits; runs })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.blame#output") ~enc:(fun _ -> "sh.tangled.repo.blame#output")
  |> Jsont.Object.mem "commit" Jsont.string ~enc:(fun r -> r.commit)
  |> Jsont.Object.mem "commits" (Jsont.list commit_jsont) ~enc:(fun r -> r.commits)
  |> Jsont.Object.mem "runs" (Jsont.list run_jsont) ~enc:(fun r -> r.runs)
  |> Jsont.Object.finish

      end
      module Artifact = struct
type main = {
  artifact : Atp.Blob_ref.t;
  created_at : string;
  name : string;
  repo : string option;
  repo_did : string option;
  tag : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ artifact created_at name repo repo_did tag -> { artifact; created_at; name; repo; repo_did; tag })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.artifact") ~enc:(fun _ -> "sh.tangled.repo.artifact")
  |> Jsont.Object.mem "artifact" Atp.Blob_ref.jsont ~enc:(fun r -> r.artifact)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.opt_mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.opt_mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.mem "tag" Atp.Lex.bytes_jsont ~enc:(fun r -> r.tag)
  |> Jsont.Object.finish

      end
      module Archive = struct
type params = {
  format : string option;
  prefix : string option;
  ref_ : string;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun format prefix ref_ repo -> {
      format;
      prefix;
      ref_;
      repo;
    })
  |> Jsont.Object.opt_mem "format" Jsont.string
       ~enc:(fun r -> r.format)
  |> Jsont.Object.opt_mem "prefix" Jsont.string
       ~enc:(fun r -> r.prefix)
  |> Jsont.Object.mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = unit
let output_jsont = Jsont.ignore

      end
      module AddSecret = struct
type input = {
  key : string;
  repo : string;
  value : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ key repo value -> { key; repo; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.addSecret#input") ~enc:(fun _ -> "sh.tangled.repo.addSecret#input")
  |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "value" Jsont.string ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

      end
      module AddCollaborator = struct
type input = {
  repo : string;
  subject : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ repo subject -> { repo; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.addCollaborator#input") ~enc:(fun _ -> "sh.tangled.repo.addCollaborator#input")
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module AcceptCollaboration = struct
type input = {
  acceptance : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ acceptance -> { acceptance })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.acceptCollaboration#input") ~enc:(fun _ -> "sh.tangled.repo.acceptCollaboration#input")
  |> Jsont.Object.mem "acceptance" Jsont.string ~enc:(fun r -> r.acceptance)
  |> Jsont.Object.finish

      end
    end
    module Embed = struct
      module Commit = struct
type main = {
  commit : Git.Oid.main;
  repo : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ commit repo -> { commit; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.embed.commit") ~enc:(fun _ -> "sh.tangled.embed.commit")
  |> Jsont.Object.mem "commit" Git.Oid.main_jsont ~enc:(fun r -> r.commit)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type view = {
  change : string option;
  commit : string;
  repo : string;
}

let view_jsont =
  Jsont.Object.map ~kind:"View"
    (fun _typ change commit repo -> { change; commit; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.embed.commit#view") ~enc:(fun _ -> "sh.tangled.embed.commit#view")
  |> Jsont.Object.opt_mem "change" Jsont.string ~enc:(fun r -> r.change)
  |> Jsont.Object.mem "commit" Jsont.string ~enc:(fun r -> r.commit)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

      end
    end
    module Pull = struct
      module Defs = struct
type patch_view = {
  base : string;
  created_at : string;
  head : string;
}

let patch_view_jsont =
  Jsont.Object.map ~kind:"Patch_view"
    (fun _typ base created_at head -> { base; created_at; head })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pull.defs#patchView") ~enc:(fun _ -> "sh.tangled.pull.defs#patchView")
  |> Jsont.Object.mem "base" Jsont.string ~enc:(fun r -> r.base)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "head" Jsont.string ~enc:(fun r -> r.head)
  |> Jsont.Object.finish

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

let pull_view_jsont =
  Jsont.Object.map ~kind:"Pull_view"
    (fun _typ author cid created_at repo state target_branch title uri -> { author; cid; created_at; repo; state; target_branch; title; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pull.defs#pullView") ~enc:(fun _ -> "sh.tangled.pull.defs#pullView")
  |> Jsont.Object.mem "author" Actor.Defs.profile_view_basic_jsont ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "repo" Repo.Defs.repo_view_basic_jsont ~enc:(fun r -> r.repo)
  |> Jsont.Object.opt_mem "state" Jsont.string ~enc:(fun r -> r.state)
  |> Jsont.Object.mem "targetBranch" Jsont.string ~enc:(fun r -> r.target_branch)
  |> Jsont.Object.mem "title" Jsont.string ~enc:(fun r -> r.title)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type source = {
  branch : string;
  repo : Repo.Defs.repo_view_basic option;
}

let source_jsont =
  Jsont.Object.map ~kind:"Source"
    (fun _typ branch repo -> { branch; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pull.defs#source") ~enc:(fun _ -> "sh.tangled.pull.defs#source")
  |> Jsont.Object.mem "branch" Jsont.string ~enc:(fun r -> r.branch)
  |> Jsont.Object.opt_mem "repo" Repo.Defs.repo_view_basic_jsont ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

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
  versions : patch_view list;
}

let pull_view_detailed_jsont =
  Jsont.Object.map ~kind:"Pull_view_detailed"
    (fun _typ author body cid created_at repo source state target_branch title uri versions -> { author; body; cid; created_at; repo; source; state; target_branch; title; uri; versions })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pull.defs#pullViewDetailed") ~enc:(fun _ -> "sh.tangled.pull.defs#pullViewDetailed")
  |> Jsont.Object.mem "author" Actor.Defs.profile_view_basic_jsont ~enc:(fun r -> r.author)
  |> Jsont.Object.opt_mem "body" Markup.Markdown.main_jsont ~enc:(fun r -> r.body)
  |> Jsont.Object.mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "repo" Repo.Defs.repo_view_basic_jsont ~enc:(fun r -> r.repo)
  |> Jsont.Object.opt_mem "source" source_jsont ~enc:(fun r -> r.source)
  |> Jsont.Object.opt_mem "state" Jsont.string ~enc:(fun r -> r.state)
  |> Jsont.Object.mem "targetBranch" Jsont.string ~enc:(fun r -> r.target_branch)
  |> Jsont.Object.mem "title" Jsont.string ~enc:(fun r -> r.title)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "versions" (Jsont.list patch_view_jsont) ~enc:(fun r -> r.versions)
  |> Jsont.Object.finish

      end
      module ListPullViews = struct
type params = {
  cursor : string option;
  limit : int option;
  state : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit state subject -> {
      cursor;
      limit;
      state;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "state" Jsont.string
       ~enc:(fun r -> r.state)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : Defs.pull_view list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items -> { cursor; items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pull.listPullViews#output") ~enc:(fun _ -> "sh.tangled.pull.listPullViews#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list Defs.pull_view_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

      end
      module GetPullView = struct
type params = {
  uri : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun uri -> {
      uri;
    })
  |> Jsont.Object.mem "uri" Jsont.string
       ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type output = Defs.pull_view_detailed

let output_jsont = Defs.pull_view_detailed_jsont

      end
    end
    module Feed = struct
      module Star = struct
type repo = {
  did : string;
}

let repo_jsont =
  Jsont.Object.map ~kind:"Repo"
    (fun _typ did -> { did })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.feed.star#repo") ~enc:(fun _ -> "sh.tangled.feed.star#repo")
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.finish

type string_ = {
  uri : string;
}

let string__jsont =
  Jsont.Object.map ~kind:"String_"
    (fun _typ uri -> { uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.feed.star#string") ~enc:(fun _ -> "sh.tangled.feed.star#string")
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type main = {
  created_at : string;
  subject : Jsont.json;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at subject -> { created_at; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.feed.star") ~enc:(fun _ -> "sh.tangled.feed.star")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "subject" Jsont.json ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module Reaction = struct
type main = {
  created_at : string;
  reaction : string;
  subject : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at reaction subject -> { created_at; reaction; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.feed.reaction") ~enc:(fun _ -> "sh.tangled.feed.reaction")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "reaction" Jsont.string ~enc:(fun r -> r.reaction)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module Defs = struct
type comment_view = {
  body : Jsont.json;
  cid : string;
  created_at : string;
  editor : string option;
  embed : Embed.Commit.view option;
  reply_to : Jsont.json option;
  subject : Jsont.json;
  uri : string;
}

let comment_view_jsont =
  Jsont.Object.map ~kind:"Comment_view"
    (fun _typ body cid created_at editor embed reply_to subject uri -> { body; cid; created_at; editor; embed; reply_to; subject; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.feed.defs#commentView") ~enc:(fun _ -> "sh.tangled.feed.defs#commentView")
  |> Jsont.Object.mem "body" Jsont.json ~enc:(fun r -> r.body)
  |> Jsont.Object.mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "editor" Jsont.string ~enc:(fun r -> r.editor)
  |> Jsont.Object.opt_mem "embed" Embed.Commit.view_jsont ~enc:(fun r -> r.embed)
  |> Jsont.Object.opt_mem "replyTo" Jsont.json ~enc:(fun r -> r.reply_to)
  |> Jsont.Object.mem "subject" Jsont.json ~enc:(fun r -> r.subject)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

      end
      module Comment = struct
type main = {
  body : Markup.Markdown.main;
  created_at : string;
  embed : Embed.Commit.main option;
  pull_round_idx : int option;
  reply_to : Jsont.json option;
  subject : Jsont.json;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ body created_at embed pull_round_idx reply_to subject -> { body; created_at; embed; pull_round_idx; reply_to; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.feed.comment") ~enc:(fun _ -> "sh.tangled.feed.comment")
  |> Jsont.Object.mem "body" Markup.Markdown.main_jsont ~enc:(fun r -> r.body)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "embed" Embed.Commit.main_jsont ~enc:(fun r -> r.embed)
  |> Jsont.Object.opt_mem "pullRoundIdx" Jsont.int ~enc:(fun r -> r.pull_round_idx)
  |> Jsont.Object.opt_mem "replyTo" Jsont.json ~enc:(fun r -> r.reply_to)
  |> Jsont.Object.mem "subject" Jsont.json ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
    end
  end
end
