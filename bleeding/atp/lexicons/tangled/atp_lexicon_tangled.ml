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
    module Temp = struct
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
  usages : Defs.usage list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ usages -> { usages })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.spindle.quota.usage#output") ~enc:(fun _ -> "org.tangled.temp.spindle.quota.usage#output")
  |> Jsont.Object.mem "usages" (Jsont.list Defs.usage_jsont) ~enc:(fun r -> r.usages)
  |> Jsont.Object.finish

          end
          module List = struct
type output = {
  limits : Defs.limit list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ limits -> { limits })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.spindle.quota.list#output") ~enc:(fun _ -> "org.tangled.temp.spindle.quota.list#output")
  |> Jsont.Object.mem "limits" (Jsont.list Defs.limit_jsont) ~enc:(fun r -> r.limits)
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
  limit : Defs.limit;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ limit -> { limit })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.spindle.quota.get#output") ~enc:(fun _ -> "org.tangled.temp.spindle.quota.get#output")
  |> Jsont.Object.mem "limit" Defs.limit_jsont ~enc:(fun r -> r.limit)
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
  bans : Defs.ban list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ bans -> { bans })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.spindle.moderation.listBans#output") ~enc:(fun _ -> "org.tangled.temp.spindle.moderation.listBans#output")
  |> Jsont.Object.mem "bans" (Jsont.list Defs.ban_jsont) ~enc:(fun r -> r.bans)
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
  ban : Defs.ban;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ ban -> { ban })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.spindle.moderation.getBan#output") ~enc:(fun _ -> "org.tangled.temp.spindle.moderation.getBan#output")
  |> Jsont.Object.mem "ban" Defs.ban_jsont ~enc:(fun r -> r.ban)
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
  subdomain : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ subdomain -> { subdomain })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.site.claimDomain#input") ~enc:(fun _ -> "org.tangled.temp.site.claimDomain#input")
  |> Jsont.Object.mem "subdomain" Jsont.string ~enc:(fun r -> r.subdomain)
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
  highlights : highlight list option;
  line_start : int;
}

let chunk_jsont =
  Jsont.Object.map ~kind:"Chunk"
    (fun _typ content highlights line_start -> { content; highlights; line_start })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.search.searchCode#chunk") ~enc:(fun _ -> "org.tangled.temp.search.searchCode#chunk")
  |> Jsont.Object.mem "content" Jsont.string ~enc:(fun r -> r.content)
  |> Jsont.Object.opt_mem "highlights" (Jsont.list highlight_jsont) ~enc:(fun r -> r.highlights)
  |> Jsont.Object.mem "lineStart" Jsont.int ~enc:(fun r -> r.line_start)
  |> Jsont.Object.finish

type file_result = {
  chunks : chunk list;
  language : string option;
  path : string;
  repo_did : string;
}

let file_result_jsont =
  Jsont.Object.map ~kind:"File_result"
    (fun _typ chunks language path repo_did -> { chunks; language; path; repo_did })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.search.searchCode#fileResult") ~enc:(fun _ -> "org.tangled.temp.search.searchCode#fileResult")
  |> Jsont.Object.mem "chunks" (Jsont.list chunk_jsont) ~enc:(fun r -> r.chunks)
  |> Jsont.Object.opt_mem "language" Jsont.string ~enc:(fun r -> r.language)
  |> Jsont.Object.mem "path" Jsont.string ~enc:(fun r -> r.path)
  |> Jsont.Object.mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  lang : string option;
  limit : int option;
  q : string;
  repo_did : string option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor lang limit q repo_did -> {
      cursor;
      lang;
      limit;
      q;
      repo_did;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "lang" Jsont.string
       ~enc:(fun r -> r.lang)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "q" Jsont.string
       ~enc:(fun r -> r.q)
  |> Jsont.Object.opt_mem "repoDid" Jsont.string
       ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  results : file_result list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor results -> { cursor; results })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.search.searchCode#output") ~enc:(fun _ -> "org.tangled.temp.search.searchCode#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "results" (Jsont.list file_result_jsont) ~enc:(fun r -> r.results)
  |> Jsont.Object.finish

        end
      end
      module Repo = struct
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.repo.updateWebhook#input") ~enc:(fun _ -> "org.tangled.temp.repo.updateWebhook#input")
  |> Jsont.Object.opt_mem "active" Jsont.bool ~enc:(fun r -> r.active)
  |> Jsont.Object.opt_mem "events" (Jsont.list Jsont.string) ~enc:(fun r -> r.events)
  |> Jsont.Object.mem "id" Jsont.int ~enc:(fun r -> r.id)
  |> Jsont.Object.mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.opt_mem "secret" Jsont.string ~enc:(fun r -> r.secret)
  |> Jsont.Object.opt_mem "url" Jsont.string ~enc:(fun r -> r.url)
  |> Jsont.Object.finish

        end
        module UpdateSiteConfig = struct
type input = {
  branch : string;
  dir : string;
  is_index : bool option;
  repo_did : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ branch dir is_index repo_did -> { branch; dir; is_index; repo_did })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.repo.updateSiteConfig#input") ~enc:(fun _ -> "org.tangled.temp.repo.updateSiteConfig#input")
  |> Jsont.Object.mem "branch" Jsont.string ~enc:(fun r -> r.branch)
  |> Jsont.Object.mem "dir" Jsont.string ~enc:(fun r -> r.dir)
  |> Jsont.Object.opt_mem "isIndex" Jsont.bool ~enc:(fun r -> r.is_index)
  |> Jsont.Object.mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.repo.toggleWebhook#input") ~enc:(fun _ -> "org.tangled.temp.repo.toggleWebhook#input")
  |> Jsont.Object.mem "id" Jsont.int ~enc:(fun r -> r.id)
  |> Jsont.Object.mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.finish

type output = {
  active : bool;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ active -> { active })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.repo.toggleWebhook#output") ~enc:(fun _ -> "org.tangled.temp.repo.toggleWebhook#output")
  |> Jsont.Object.mem "active" Jsont.bool ~enc:(fun r -> r.active)
  |> Jsont.Object.finish

        end
        module RetryWebhookDelivery = struct
type input = {
  delivery_id : string;
  repo_did : string;
  webhook_id : int;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ delivery_id repo_did webhook_id -> { delivery_id; repo_did; webhook_id })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.repo.retryWebhookDelivery#input") ~enc:(fun _ -> "org.tangled.temp.repo.retryWebhookDelivery#input")
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.repo.listWebhooks#webhook") ~enc:(fun _ -> "org.tangled.temp.repo.listWebhooks#webhook")
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
  webhooks : webhook list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ webhooks -> { webhooks })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.repo.listWebhooks#output") ~enc:(fun _ -> "org.tangled.temp.repo.listWebhooks#output")
  |> Jsont.Object.mem "webhooks" (Jsont.list webhook_jsont) ~enc:(fun r -> r.webhooks)
  |> Jsont.Object.finish

        end
        module ListWebhookDeliveries = struct
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
}

let delivery_jsont =
  Jsont.Object.map ~kind:"Delivery"
    (fun _typ created_at delivery_id event id request_body response_body response_code success url -> { created_at; delivery_id; event; id; request_body; response_body; response_code; success; url })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.repo.listWebhookDeliveries#delivery") ~enc:(fun _ -> "org.tangled.temp.repo.listWebhookDeliveries#delivery")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "deliveryId" Jsont.string ~enc:(fun r -> r.delivery_id)
  |> Jsont.Object.mem "event" Jsont.string ~enc:(fun r -> r.event)
  |> Jsont.Object.mem "id" Jsont.int ~enc:(fun r -> r.id)
  |> Jsont.Object.opt_mem "requestBody" Jsont.string ~enc:(fun r -> r.request_body)
  |> Jsont.Object.opt_mem "responseBody" Jsont.string ~enc:(fun r -> r.response_body)
  |> Jsont.Object.opt_mem "responseCode" Jsont.int ~enc:(fun r -> r.response_code)
  |> Jsont.Object.mem "success" Jsont.bool ~enc:(fun r -> r.success)
  |> Jsont.Object.mem "url" Jsont.string ~enc:(fun r -> r.url)
  |> Jsont.Object.finish

type params = {
  id : int;
  limit : int option;
  repo_did : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun id limit repo_did -> {
      id;
      limit;
      repo_did;
    })
  |> Jsont.Object.mem "id" Jsont.int
       ~enc:(fun r -> r.id)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "repoDid" Jsont.string
       ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.finish

type output = {
  deliveries : delivery list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ deliveries -> { deliveries })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.repo.listWebhookDeliveries#output") ~enc:(fun _ -> "org.tangled.temp.repo.listWebhookDeliveries#output")
  |> Jsont.Object.mem "deliveries" (Jsont.list delivery_jsont) ~enc:(fun r -> r.deliveries)
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
  config : site_config option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ config -> { config })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.repo.getSiteConfig#output") ~enc:(fun _ -> "org.tangled.temp.repo.getSiteConfig#output")
  |> Jsont.Object.opt_mem "config" site_config_jsont ~enc:(fun r -> r.config)
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
        module DeleteWebhook = struct
type input = {
  id : int;
  repo_did : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ id repo_did -> { id; repo_did })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.repo.deleteWebhook#input") ~enc:(fun _ -> "org.tangled.temp.repo.deleteWebhook#input")
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.repo.createWebhook#input") ~enc:(fun _ -> "org.tangled.temp.repo.createWebhook#input")
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.repo.createWebhook#output") ~enc:(fun _ -> "org.tangled.temp.repo.createWebhook#output")
  |> Jsont.Object.mem "id" Jsont.int ~enc:(fun r -> r.id)
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
  notifications : notification list;
  social_unread_count : int;
  work_unread_count : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor notifications social_unread_count work_unread_count -> { cursor; notifications; social_unread_count; work_unread_count })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.notification.listNotifications#output") ~enc:(fun _ -> "org.tangled.temp.notification.listNotifications#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "notifications" (Jsont.list notification_jsont) ~enc:(fun r -> r.notifications)
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

type output = preferences

let output_jsont = preferences_jsont

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
      module Account = struct
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
  emails : email list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ emails -> { emails })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.account.listEmails#output") ~enc:(fun _ -> "org.tangled.temp.account.listEmails#output")
  |> Jsont.Object.mem "emails" (Jsont.list email_jsont) ~enc:(fun r -> r.emails)
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
type input = {
  code : string;
  password : string;
  username : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ code password username -> { code; password; username })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.account.completeSignup#input") ~enc:(fun _ -> "org.tangled.temp.account.completeSignup#input")
  |> Jsont.Object.mem "code" Jsont.string ~enc:(fun r -> r.code)
  |> Jsont.Object.mem "password" Jsont.string ~enc:(fun r -> r.password)
  |> Jsont.Object.mem "username" Jsont.string ~enc:(fun r -> r.username)
  |> Jsont.Object.finish

type output = {
  did : string;
  handle : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ did handle -> { did; handle })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "org.tangled.temp.account.completeSignup#output") ~enc:(fun _ -> "org.tangled.temp.account.completeSignup#output")
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
      end
    end
  end
end
module Com = struct
  module Atproto = struct
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

      module ListStrings = struct
type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.string.listStrings#listItem") ~enc:(fun _ -> "sh.tangled.string.listStrings#listItem")
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.string.listStrings#output") ~enc:(fun _ -> "sh.tangled.string.listStrings#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list list_item_jsont) ~enc:(fun r -> r.items)
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.string.countStrings#output") ~enc:(fun _ -> "sh.tangled.string.countStrings#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.spindle.listSpindles#output") ~enc:(fun _ -> "sh.tangled.spindle.listSpindles#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list list_item_jsont) ~enc:(fun r -> r.items)
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.spindle.listMembers#output") ~enc:(fun _ -> "sh.tangled.spindle.listMembers#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list list_item_jsont) ~enc:(fun r -> r.items)
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
  items : ListMembers.list_item list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items -> { cursor; items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.spindle.listMembersBy#output") ~enc:(fun _ -> "sh.tangled.spindle.listMembersBy#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list ListMembers.list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

      end
    end
    module Search = struct
      module Query = struct
type hit = {
  cid : string option;
  nsid : string;
  score : Jsont.json;
  uri : string;
  value : Jsont.json;
}

let hit_jsont =
  Jsont.Object.map ~kind:"Hit"
    (fun _typ cid nsid score uri value -> { cid; nsid; score; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.search.query#hit") ~enc:(fun _ -> "sh.tangled.search.query#hit")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "nsid" Jsont.string ~enc:(fun r -> r.nsid)
  |> Jsont.Object.mem "score" Jsont.json ~enc:(fun r -> r.score)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  author : string option;
  cursor : string option;
  limit : int option;
  nsid : string option;
  q : string;
  repo : string option;
  since : string option;
  until : string option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun author cursor limit nsid q repo since until -> {
      author;
      cursor;
      limit;
      nsid;
      q;
      repo;
      since;
      until;
    })
  |> Jsont.Object.opt_mem "author" Jsont.string
       ~enc:(fun r -> r.author)
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "nsid" Jsont.string
       ~enc:(fun r -> r.nsid)
  |> Jsont.Object.mem "q" Jsont.string
       ~enc:(fun r -> r.q)
  |> Jsont.Object.opt_mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.opt_mem "since" Jsont.string
       ~enc:(fun r -> r.since)
  |> Jsont.Object.opt_mem "until" Jsont.string
       ~enc:(fun r -> r.until)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  hits : hit list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor hits -> { cursor; hits })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.search.query#output") ~enc:(fun _ -> "sh.tangled.search.query#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "hits" (Jsont.list hit_jsont) ~enc:(fun r -> r.hits)
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

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ blobs body created_at dependent_on mentions references rounds source target title -> { blobs; body; created_at; dependent_on; mentions; references; rounds; source; target; title })
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
      module ListRepos = struct
type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.listRepos#listItem") ~enc:(fun _ -> "sh.tangled.repo.listRepos#listItem")
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.listRepos#output") ~enc:(fun _ -> "sh.tangled.repo.listRepos#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

      end
      module ListPulls = struct
type pull_list_item = {
  cid : string option;
  comment_count : int;
  state : string;
  state_updated_at : string option;
  uri : string;
  value : Jsont.json;
}

let pull_list_item_jsont =
  Jsont.Object.map ~kind:"Pull_list_item"
    (fun _typ cid comment_count state state_updated_at uri value -> { cid; comment_count; state; state_updated_at; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.listPulls#pullListItem") ~enc:(fun _ -> "sh.tangled.repo.listPulls#pullListItem")
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
  order : string option;
  status : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun author cursor limit order status subject -> {
      author;
      cursor;
      limit;
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
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.opt_mem "status" Jsont.string
       ~enc:(fun r -> r.status)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : pull_list_item list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items -> { cursor; items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.listPulls#output") ~enc:(fun _ -> "sh.tangled.repo.listPulls#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list pull_list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

      end
      module ListIssues = struct
type issue_list_item = {
  cid : string option;
  comment_count : int;
  state : string;
  state_updated_at : string option;
  uri : string;
  value : Jsont.json;
}

let issue_list_item_jsont =
  Jsont.Object.map ~kind:"Issue_list_item"
    (fun _typ cid comment_count state state_updated_at uri value -> { cid; comment_count; state; state_updated_at; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.listIssues#issueListItem") ~enc:(fun _ -> "sh.tangled.repo.listIssues#issueListItem")
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
  order : string option;
  state : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun author cursor limit order state subject -> {
      author;
      cursor;
      limit;
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
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.opt_mem "state" Jsont.string
       ~enc:(fun r -> r.state)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : issue_list_item list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items -> { cursor; items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.listIssues#output") ~enc:(fun _ -> "sh.tangled.repo.listIssues#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list issue_list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

      end
      module ListCollaborators = struct
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.listCollaborators#listItem") ~enc:(fun _ -> "sh.tangled.repo.listCollaborators#listItem")
  |> Jsont.Object.mem "addedBy" Jsont.string ~enc:(fun r -> r.added_by)
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.opt_mem "uri" Jsont.string ~enc:(fun r -> r.uri)
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.listCollaborators#output") ~enc:(fun _ -> "sh.tangled.repo.listCollaborators#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list list_item_jsont) ~enc:(fun r -> r.items)
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.listArtifacts#listItem") ~enc:(fun _ -> "sh.tangled.repo.listArtifacts#listItem")
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.listArtifacts#output") ~enc:(fun _ -> "sh.tangled.repo.listArtifacts#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list list_item_jsont) ~enc:(fun r -> r.items)
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
      module GetRepos = struct
type record_view = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let record_view_jsont =
  Jsont.Object.map ~kind:"Record_view"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.getRepos#recordView") ~enc:(fun _ -> "sh.tangled.repo.getRepos#recordView")
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
  items : record_view list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ items -> { items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.getRepos#output") ~enc:(fun _ -> "sh.tangled.repo.getRepos#output")
  |> Jsont.Object.mem "items" (Jsont.list record_view_jsont) ~enc:(fun r -> r.items)
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
  uri : string;
  value : Jsont.json;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.getRepoByRepoDid#output") ~enc:(fun _ -> "sh.tangled.repo.getRepoByRepoDid#output")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
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
  uri : string;
  value : Jsont.json;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.getRepo#output") ~enc:(fun _ -> "sh.tangled.repo.getRepo#output")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
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
  owner_did : string;
  repo_did : string;
  rkey : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ owner_did repo_did rkey -> { owner_did; repo_did; rkey })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.describeRepo#output") ~enc:(fun _ -> "sh.tangled.repo.describeRepo#output")
  |> Jsont.Object.mem "ownerDid" Jsont.string ~enc:(fun r -> r.owner_did)
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
      module Create = struct
type input = {
  default_branch : string option;
  name : string;
  repo_did : string option;
  rkey : string;
  source : string option;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ default_branch name repo_did rkey source -> { default_branch; name; repo_did; rkey; source })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.create#input") ~enc:(fun _ -> "sh.tangled.repo.create#input")
  |> Jsont.Object.opt_mem "defaultBranch" Jsont.string ~enc:(fun r -> r.default_branch)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.opt_mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.mem "rkey" Jsont.string ~enc:(fun r -> r.rkey)
  |> Jsont.Object.opt_mem "source" Jsont.string ~enc:(fun r -> r.source)
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.countRepos#output") ~enc:(fun _ -> "sh.tangled.repo.countRepos#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
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
      module CountPulls = struct
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.countPulls#output") ~enc:(fun _ -> "sh.tangled.repo.countPulls#output")
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
      module CountIssues = struct
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.countIssues#output") ~enc:(fun _ -> "sh.tangled.repo.countIssues#output")
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.countCollaboratorsBy#output") ~enc:(fun _ -> "sh.tangled.repo.countCollaboratorsBy#output")
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.countCollaborators#output") ~enc:(fun _ -> "sh.tangled.repo.countCollaborators#output")
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.countArtifactsBy#output") ~enc:(fun _ -> "sh.tangled.repo.countArtifactsBy#output")
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.countArtifacts#output") ~enc:(fun _ -> "sh.tangled.repo.countArtifacts#output")
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
      module ListPullsBy = struct
type params = {
  cursor : string option;
  limit : int option;
  order : string option;
  status : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit order status subject -> {
      cursor;
      limit;
      order;
      status;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.opt_mem "status" Jsont.string
       ~enc:(fun r -> r.status)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : ListPulls.pull_list_item list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items -> { cursor; items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.listPullsBy#output") ~enc:(fun _ -> "sh.tangled.repo.listPullsBy#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list ListPulls.pull_list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

      end
      module ListIssuesBy = struct
type params = {
  cursor : string option;
  limit : int option;
  order : string option;
  state : string option;
  subject : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit order state subject -> {
      cursor;
      limit;
      order;
      state;
      subject;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "order" Jsont.string
       ~enc:(fun r -> r.order)
  |> Jsont.Object.opt_mem "state" Jsont.string
       ~enc:(fun r -> r.state)
  |> Jsont.Object.mem "subject" Jsont.string
       ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : ListIssues.issue_list_item list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items -> { cursor; items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.listIssuesBy#output") ~enc:(fun _ -> "sh.tangled.repo.listIssuesBy#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list ListIssues.issue_list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

      end
      module ListCollaboratorsBy = struct
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
  items : ListCollaborators.list_item list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items -> { cursor; items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.listCollaboratorsBy#output") ~enc:(fun _ -> "sh.tangled.repo.listCollaboratorsBy#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list ListCollaborators.list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

      end
      module ListArtifactsBy = struct
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
  items : ListArtifacts.list_item list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items -> { cursor; items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.repo.listArtifactsBy#output") ~enc:(fun _ -> "sh.tangled.repo.listArtifactsBy#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list ListArtifacts.list_item_jsont) ~enc:(fun r -> r.items)
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

      module ListKeys = struct
type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.publicKey.listKeys#listItem") ~enc:(fun _ -> "sh.tangled.publicKey.listKeys#listItem")
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.publicKey.listKeys#output") ~enc:(fun _ -> "sh.tangled.publicKey.listKeys#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list list_item_jsont) ~enc:(fun r -> r.items)
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.publicKey.countKeys#output") ~enc:(fun _ -> "sh.tangled.publicKey.countKeys#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
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
  clone : clone_opts;
  engine : string;
  name : string;
  raw : string;
  runs_on : string list option;
}

let workflow_jsont =
  Jsont.Object.map ~kind:"Workflow"
    (fun _typ clone engine name raw runs_on -> { clone; engine; name; raw; runs_on })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pipeline#workflow") ~enc:(fun _ -> "sh.tangled.pipeline#workflow")
  |> Jsont.Object.mem "clone" clone_opts_jsont ~enc:(fun r -> r.clone)
  |> Jsont.Object.mem "engine" Jsont.string ~enc:(fun r -> r.engine)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "raw" Jsont.string ~enc:(fun r -> r.raw)
  |> Jsont.Object.opt_mem "runsOn" (Jsont.list Jsont.string) ~enc:(fun r -> r.runs_on)
  |> Jsont.Object.finish

type trigger_metadata = {
  kind : string;
  manual : manual_trigger_data option;
  pull_request : pull_request_trigger_data option;
  push : push_trigger_data option;
  repo : trigger_repo;
  source_repo : string option;
}

let trigger_metadata_jsont =
  Jsont.Object.map ~kind:"Trigger_metadata"
    (fun _typ kind manual pull_request push repo source_repo -> { kind; manual; pull_request; push; repo; source_repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pipeline#triggerMetadata") ~enc:(fun _ -> "sh.tangled.pipeline#triggerMetadata")
  |> Jsont.Object.mem "kind" Jsont.string ~enc:(fun r -> r.kind)
  |> Jsont.Object.opt_mem "manual" manual_trigger_data_jsont ~enc:(fun r -> r.manual)
  |> Jsont.Object.opt_mem "pullRequest" pull_request_trigger_data_jsont ~enc:(fun r -> r.pull_request)
  |> Jsont.Object.opt_mem "push" push_trigger_data_jsont ~enc:(fun r -> r.push)
  |> Jsont.Object.mem "repo" trigger_repo_jsont ~enc:(fun r -> r.repo)
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pipeline.listStatuses#output") ~enc:(fun _ -> "sh.tangled.pipeline.listStatuses#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list list_item_jsont) ~enc:(fun r -> r.items)
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pipeline.listPipelines#output") ~enc:(fun _ -> "sh.tangled.pipeline.listPipelines#output")
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pipeline.listStatusesBy#output") ~enc:(fun _ -> "sh.tangled.pipeline.listStatusesBy#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list ListStatuses.list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

      end
      module ListPipelinesBy = struct
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
  items : ListPipelines.list_item list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items -> { cursor; items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.pipeline.listPipelinesBy#output") ~enc:(fun _ -> "sh.tangled.pipeline.listPipelinesBy#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list ListPipelines.list_item_jsont) ~enc:(fun r -> r.items)
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.label.listOps#output") ~enc:(fun _ -> "sh.tangled.label.listOps#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

      end
      module ListDefinitions = struct
type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.label.listDefinitions#listItem") ~enc:(fun _ -> "sh.tangled.label.listDefinitions#listItem")
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.label.listDefinitions#output") ~enc:(fun _ -> "sh.tangled.label.listDefinitions#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list list_item_jsont) ~enc:(fun r -> r.items)
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.label.countDefinitions#output") ~enc:(fun _ -> "sh.tangled.label.countDefinitions#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module ListOpsBy = struct
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
  items : ListOps.list_item list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items -> { cursor; items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.label.listOpsBy#output") ~enc:(fun _ -> "sh.tangled.label.listOpsBy#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list ListOps.list_item_jsont) ~enc:(fun r -> r.items)
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
      module SubscribeRepos = struct
type git_sync1 = {
  did : string;
  seq : int;
}

let git_sync1_jsont =
  Jsont.Object.map ~kind:"Git_sync1"
    (fun _typ did seq -> { did; seq })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.knot.subscribeRepos#gitSync1") ~enc:(fun _ -> "sh.tangled.knot.subscribeRepos#gitSync1")
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.mem "seq" Jsont.int ~enc:(fun r -> r.seq)
  |> Jsont.Object.finish

type git_sync2 = {
  repo : string;
  seq : int;
}

let git_sync2_jsont =
  Jsont.Object.map ~kind:"Git_sync2"
    (fun _typ repo seq -> { repo; seq })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.knot.subscribeRepos#gitSync2") ~enc:(fun _ -> "sh.tangled.knot.subscribeRepos#gitSync2")
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "seq" Jsont.int ~enc:(fun r -> r.seq)
  |> Jsont.Object.finish

type identity = {
  did : string;
  seq : int;
  time : string;
}

let identity_jsont =
  Jsont.Object.map ~kind:"Identity"
    (fun _typ did seq time -> { did; seq; time })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.knot.subscribeRepos#identity") ~enc:(fun _ -> "sh.tangled.knot.subscribeRepos#identity")
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.mem "seq" Jsont.int ~enc:(fun r -> r.seq)
  |> Jsont.Object.mem "time" Jsont.string ~enc:(fun r -> r.time)
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.knot.listMembers#output") ~enc:(fun _ -> "sh.tangled.knot.listMembers#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list list_item_jsont) ~enc:(fun r -> r.items)
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.knot.listKnots#output") ~enc:(fun _ -> "sh.tangled.knot.listKnots#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list list_item_jsont) ~enc:(fun r -> r.items)
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
      module ListMembersBy = struct
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
  items : ListMembers.list_item list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items -> { cursor; items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.knot.listMembersBy#output") ~enc:(fun _ -> "sh.tangled.knot.listMembersBy#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list ListMembers.list_item_jsont) ~enc:(fun r -> r.items)
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.graph.listVouches#output") ~enc:(fun _ -> "sh.tangled.graph.listVouches#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

      end
      module ListFollows = struct
type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.graph.listFollows#listItem") ~enc:(fun _ -> "sh.tangled.graph.listFollows#listItem")
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.graph.listFollows#output") ~enc:(fun _ -> "sh.tangled.graph.listFollows#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list list_item_jsont) ~enc:(fun r -> r.items)
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.graph.countFollowsBy#output") ~enc:(fun _ -> "sh.tangled.graph.countFollowsBy#output")
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.graph.countFollows#output") ~enc:(fun _ -> "sh.tangled.graph.countFollows#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module ListVouchesBy = struct
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
  items : ListVouches.list_item list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items -> { cursor; items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.graph.listVouchesBy#output") ~enc:(fun _ -> "sh.tangled.graph.listVouchesBy#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list ListVouches.list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

      end
      module ListFollowsBy = struct
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
  items : ListFollows.list_item list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items -> { cursor; items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.graph.listFollowsBy#output") ~enc:(fun _ -> "sh.tangled.graph.listFollowsBy#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list ListFollows.list_item_jsont) ~enc:(fun r -> r.items)
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
      module ListRefUpdates = struct
type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.listRefUpdates#listItem") ~enc:(fun _ -> "sh.tangled.git.listRefUpdates#listItem")
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.listRefUpdates#output") ~enc:(fun _ -> "sh.tangled.git.listRefUpdates#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

      end
      module CountRefUpdatesBy = struct
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.countRefUpdatesBy#output") ~enc:(fun _ -> "sh.tangled.git.countRefUpdatesBy#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module CountRefUpdates = struct
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.countRefUpdates#output") ~enc:(fun _ -> "sh.tangled.git.countRefUpdates#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module ListRefUpdatesBy = struct
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
  items : ListRefUpdates.list_item list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items -> { cursor; items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.git.listRefUpdatesBy#output") ~enc:(fun _ -> "sh.tangled.git.listRefUpdatesBy#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list ListRefUpdates.list_item_jsont) ~enc:(fun r -> r.items)
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
  status : string option;
  step : int;
  time : string;
  workflow : string;
}

let control_jsont =
  Jsont.Object.map ~kind:"Control"
    (fun _typ command content kind status step time workflow -> { command; content; kind; status; step; time; workflow })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.ci.subscribePipelineLogs#control") ~enc:(fun _ -> "sh.tangled.ci.subscribePipelineLogs#control")
  |> Jsont.Object.opt_mem "command" Jsont.string ~enc:(fun r -> r.command)
  |> Jsont.Object.mem "content" Jsont.string ~enc:(fun r -> r.content)
  |> Jsont.Object.opt_mem "kind" Jsont.string ~enc:(fun r -> r.kind)
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
  repo : string option;
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
  |> Jsont.Object.opt_mem "repo" Jsont.string ~enc:(fun r -> r.repo)
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
  links : string list option;
  location : string option;
  pinned_repositories : string list option;
  preferred_handle : string option;
  pronouns : string option;
  stats : string list option;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ avatar bluesky description links location pinned_repositories preferred_handle pronouns stats -> { avatar; bluesky; description; links; location; pinned_repositories; preferred_handle; pronouns; stats })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.actor.profile") ~enc:(fun _ -> "sh.tangled.actor.profile")
  |> Jsont.Object.opt_mem "avatar" Atp.Blob_ref.jsont ~enc:(fun r -> r.avatar)
  |> Jsont.Object.mem "bluesky" Jsont.bool ~enc:(fun r -> r.bluesky)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.actor.getProfiles#recordView") ~enc:(fun _ -> "sh.tangled.actor.getProfiles#recordView")
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
  items : record_view list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ items -> { items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.actor.getProfiles#output") ~enc:(fun _ -> "sh.tangled.actor.getProfiles#output")
  |> Jsont.Object.mem "items" (Jsont.list record_view_jsont) ~enc:(fun r -> r.items)
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.actor.getProfile#output") ~enc:(fun _ -> "sh.tangled.actor.getProfile#output")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

      end
    end
    module Feed = struct
      module Subscription = struct
type repo = {
  did : string;
}

let repo_jsont =
  Jsont.Object.map ~kind:"Repo"
    (fun _typ did -> { did })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.feed.subscription#repo") ~enc:(fun _ -> "sh.tangled.feed.subscription#repo")
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.finish

type uri = {
  uri : string;
}

let uri_jsont =
  Jsont.Object.map ~kind:"Uri"
    (fun _typ uri -> { uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.feed.subscription#uri") ~enc:(fun _ -> "sh.tangled.feed.subscription#uri")
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type main = {
  collections : string list option;
  created_at : string;
  subject : Jsont.json;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ collections created_at subject -> { collections; created_at; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.feed.subscription") ~enc:(fun _ -> "sh.tangled.feed.subscription")
  |> Jsont.Object.opt_mem "collections" (Jsont.list Jsont.string) ~enc:(fun r -> r.collections)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "subject" Jsont.json ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
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
      module ListStars = struct
type list_item = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let list_item_jsont =
  Jsont.Object.map ~kind:"List_item"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.feed.listStars#listItem") ~enc:(fun _ -> "sh.tangled.feed.listStars#listItem")
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.feed.listStars#output") ~enc:(fun _ -> "sh.tangled.feed.listStars#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list list_item_jsont) ~enc:(fun r -> r.items)
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.feed.listReactions#listItem") ~enc:(fun _ -> "sh.tangled.feed.listReactions#listItem")
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.feed.listReactions#output") ~enc:(fun _ -> "sh.tangled.feed.listReactions#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list list_item_jsont) ~enc:(fun r -> r.items)
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.feed.listComments#listItem") ~enc:(fun _ -> "sh.tangled.feed.listComments#listItem")
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.feed.listComments#output") ~enc:(fun _ -> "sh.tangled.feed.listComments#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list list_item_jsont) ~enc:(fun r -> r.items)
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.feed.countStarsBy#output") ~enc:(fun _ -> "sh.tangled.feed.countStarsBy#output")
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.feed.countStars#output") ~enc:(fun _ -> "sh.tangled.feed.countStars#output")
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.feed.countReactionsBy#output") ~enc:(fun _ -> "sh.tangled.feed.countReactionsBy#output")
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.feed.countReactions#output") ~enc:(fun _ -> "sh.tangled.feed.countReactions#output")
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.feed.countCommentsBy#output") ~enc:(fun _ -> "sh.tangled.feed.countCommentsBy#output")
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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.feed.countComments#output") ~enc:(fun _ -> "sh.tangled.feed.countComments#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "distinctAuthors" Jsont.int ~enc:(fun r -> r.distinct_authors)
  |> Jsont.Object.finish

      end
      module Comment = struct
type main = {
  body : Markup.Markdown.main;
  created_at : string;
  pull_round_idx : int option;
  reply_to : Com.Atproto.Repo.StrongRef.main option;
  subject : Com.Atproto.Repo.StrongRef.main;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ body created_at pull_round_idx reply_to subject -> { body; created_at; pull_round_idx; reply_to; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.feed.comment") ~enc:(fun _ -> "sh.tangled.feed.comment")
  |> Jsont.Object.mem "body" Markup.Markdown.main_jsont ~enc:(fun r -> r.body)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "pullRoundIdx" Jsont.int ~enc:(fun r -> r.pull_round_idx)
  |> Jsont.Object.opt_mem "replyTo" Com.Atproto.Repo.StrongRef.main_jsont ~enc:(fun r -> r.reply_to)
  |> Jsont.Object.mem "subject" Com.Atproto.Repo.StrongRef.main_jsont ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module ListStarsBy = struct
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
  items : ListStars.list_item list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items -> { cursor; items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.feed.listStarsBy#output") ~enc:(fun _ -> "sh.tangled.feed.listStarsBy#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list ListStars.list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

      end
      module ListReactionsBy = struct
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
  items : ListReactions.list_item list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items -> { cursor; items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.feed.listReactionsBy#output") ~enc:(fun _ -> "sh.tangled.feed.listReactionsBy#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list ListReactions.list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

      end
      module ListCommentsBy = struct
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
  items : ListComments.list_item list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items -> { cursor; items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:(fun () -> "sh.tangled.feed.listCommentsBy#output") ~enc:(fun _ -> "sh.tangled.feed.listCommentsBy#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list ListComments.list_item_jsont) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

      end
    end
  end
end
