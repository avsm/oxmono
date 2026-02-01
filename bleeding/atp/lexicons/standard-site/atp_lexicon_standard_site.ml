(* Atp_lexicon_standard_site - generated from atproto lexicons *)

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
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"com.atproto.repo.strongRef" ~enc:(fun _ -> "com.atproto.repo.strongRef")
  |> Jsont.Object.mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

      end
    end
  end
end
module Site = struct
  module Standard = struct
    module Theme = struct
      module Color = struct
type rgb = {
  b : int;
  g : int;
  r : int;
}

let rgb_jsont =
  Jsont.Object.map ~kind:"Rgb"
    (fun _typ b g r -> { b; g; r })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"site.standard.theme.color#rgb" ~enc:(fun _ -> "site.standard.theme.color#rgb")
  |> Jsont.Object.mem "b" Jsont.int ~enc:(fun r -> r.b)
  |> Jsont.Object.mem "g" Jsont.int ~enc:(fun r -> r.g)
  |> Jsont.Object.mem "r" Jsont.int ~enc:(fun r -> r.r)
  |> Jsont.Object.finish

type rgba = {
  a : int;
  b : int;
  g : int;
  r : int;
}

let rgba_jsont =
  Jsont.Object.map ~kind:"Rgba"
    (fun _typ a b g r -> { a; b; g; r })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"site.standard.theme.color#rgba" ~enc:(fun _ -> "site.standard.theme.color#rgba")
  |> Jsont.Object.mem "a" Jsont.int ~enc:(fun r -> r.a)
  |> Jsont.Object.mem "b" Jsont.int ~enc:(fun r -> r.b)
  |> Jsont.Object.mem "g" Jsont.int ~enc:(fun r -> r.g)
  |> Jsont.Object.mem "r" Jsont.int ~enc:(fun r -> r.r)
  |> Jsont.Object.finish

      end
      module Basic = struct
type main = {
  accent : Color.rgb;
  accent_foreground : Color.rgb;
  background : Color.rgb;
  foreground : Color.rgb;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ accent accent_foreground background foreground -> { accent; accent_foreground; background; foreground })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"site.standard.theme.basic" ~enc:(fun _ -> "site.standard.theme.basic")
  |> Jsont.Object.mem "accent" Color.rgb_jsont ~enc:(fun r -> r.accent)
  |> Jsont.Object.mem "accentForeground" Color.rgb_jsont ~enc:(fun r -> r.accent_foreground)
  |> Jsont.Object.mem "background" Color.rgb_jsont ~enc:(fun r -> r.background)
  |> Jsont.Object.mem "foreground" Color.rgb_jsont ~enc:(fun r -> r.foreground)
  |> Jsont.Object.finish

      end
    end
    module Graph = struct
      module Subscription = struct
type main = {
  publication : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ publication -> { publication })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"site.standard.graph.subscription" ~enc:(fun _ -> "site.standard.graph.subscription")
  |> Jsont.Object.mem "publication" Jsont.string ~enc:(fun r -> r.publication)
  |> Jsont.Object.finish

      end
    end
    module Document = struct
type main = {
  bsky_post_ref : Com.Atproto.Repo.StrongRef.main option;
  content : Jsont.json option;
  cover_image : Atp.Blob_ref.t option;
  description : string option;
  path : string option;
  published_at : string;
  site : string;
  tags : string list option;
  text_content : string option;
  title : string;
  updated_at : string option;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ bsky_post_ref content cover_image description path published_at site tags text_content title updated_at -> { bsky_post_ref; content; cover_image; description; path; published_at; site; tags; text_content; title; updated_at })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"site.standard.document" ~enc:(fun _ -> "site.standard.document")
  |> Jsont.Object.opt_mem "bskyPostRef" Com.Atproto.Repo.StrongRef.main_jsont ~enc:(fun r -> r.bsky_post_ref)
  |> Jsont.Object.opt_mem "content" Jsont.json ~enc:(fun r -> r.content)
  |> Jsont.Object.opt_mem "coverImage" Atp.Blob_ref.jsont ~enc:(fun r -> r.cover_image)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
  |> Jsont.Object.opt_mem "path" Jsont.string ~enc:(fun r -> r.path)
  |> Jsont.Object.mem "publishedAt" Jsont.string ~enc:(fun r -> r.published_at)
  |> Jsont.Object.mem "site" Jsont.string ~enc:(fun r -> r.site)
  |> Jsont.Object.opt_mem "tags" (Jsont.list Jsont.string) ~enc:(fun r -> r.tags)
  |> Jsont.Object.opt_mem "textContent" Jsont.string ~enc:(fun r -> r.text_content)
  |> Jsont.Object.mem "title" Jsont.string ~enc:(fun r -> r.title)
  |> Jsont.Object.opt_mem "updatedAt" Jsont.string ~enc:(fun r -> r.updated_at)
  |> Jsont.Object.finish

    end
    module Publication = struct
type preferences = {
  show_in_discover : bool option;
}

let preferences_jsont =
  Jsont.Object.map ~kind:"Preferences"
    (fun _typ show_in_discover -> { show_in_discover })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"site.standard.publication#preferences" ~enc:(fun _ -> "site.standard.publication#preferences")
  |> Jsont.Object.opt_mem "showInDiscover" Jsont.bool ~enc:(fun r -> r.show_in_discover)
  |> Jsont.Object.finish

type main = {
  basic_theme : Theme.Basic.main option;
  description : string option;
  icon : Atp.Blob_ref.t option;
  name : string;
  preferences : preferences option;
  url : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ basic_theme description icon name preferences url -> { basic_theme; description; icon; name; preferences; url })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"site.standard.publication" ~enc:(fun _ -> "site.standard.publication")
  |> Jsont.Object.opt_mem "basicTheme" Theme.Basic.main_jsont ~enc:(fun r -> r.basic_theme)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
  |> Jsont.Object.opt_mem "icon" Atp.Blob_ref.jsont ~enc:(fun r -> r.icon)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.opt_mem "preferences" preferences_jsont ~enc:(fun r -> r.preferences)
  |> Jsont.Object.mem "url" Jsont.string ~enc:(fun r -> r.url)
  |> Jsont.Object.finish

    end
  end
end
