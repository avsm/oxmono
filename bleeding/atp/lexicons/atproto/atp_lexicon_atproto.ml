(* Atp_lexicon_atproto - generated from atproto lexicons *)

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
      module ListRecords = struct
type record = {
  cid : string;
  uri : string;
  value : Jsont.json;
}

let record_jsont =
  Jsont.Object.map ~kind:"Record"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"com.atproto.repo.listRecords#record" ~enc:(fun _ -> "com.atproto.repo.listRecords#record")
  |> Jsont.Object.mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  collection : string;
  cursor : string option;
  limit : int option;
  repo : string;
  reverse : bool option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun collection cursor limit repo reverse -> {
      collection;
      cursor;
      limit;
      repo;
      reverse;
    })
  |> Jsont.Object.mem "collection" Jsont.string
       ~enc:(fun r -> r.collection)
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.opt_mem "reverse" Jsont.bool
       ~enc:(fun r -> r.reverse)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  records : record list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor records -> { cursor; records })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"com.atproto.repo.listRecords#output" ~enc:(fun _ -> "com.atproto.repo.listRecords#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "records" (Jsont.list record_jsont) ~enc:(fun r -> r.records)
  |> Jsont.Object.finish

      end
      module GetRecord = struct
type params = {
  cid : string option;
  collection : string;
  repo : string;
  rkey : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cid collection repo rkey -> {
      cid;
      collection;
      repo;
      rkey;
    })
  |> Jsont.Object.opt_mem "cid" Jsont.string
       ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "collection" Jsont.string
       ~enc:(fun r -> r.collection)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "rkey" Jsont.string
       ~enc:(fun r -> r.rkey)
  |> Jsont.Object.finish

type output = {
  cid : string option;
  uri : string;
  value : Jsont.json;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cid uri value -> { cid; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"com.atproto.repo.getRecord#output" ~enc:(fun _ -> "com.atproto.repo.getRecord#output")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

      end
      module Defs = struct
type commit_meta = {
  cid : string;
  rev : string;
}

let commit_meta_jsont =
  Jsont.Object.map ~kind:"Commit_meta"
    (fun _typ cid rev -> { cid; rev })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"com.atproto.repo.defs#commitMeta" ~enc:(fun _ -> "com.atproto.repo.defs#commitMeta")
  |> Jsont.Object.mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "rev" Jsont.string ~enc:(fun r -> r.rev)
  |> Jsont.Object.finish

      end
      module PutRecord = struct
type input = {
  collection : string;
  record : Jsont.json;
  repo : string;
  rkey : string;
  swap_commit : string option;
  swap_record : string option;
  validate : bool option;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ collection record repo rkey swap_commit swap_record validate -> { collection; record; repo; rkey; swap_commit; swap_record; validate })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"com.atproto.repo.putRecord#input" ~enc:(fun _ -> "com.atproto.repo.putRecord#input")
  |> Jsont.Object.mem "collection" Jsont.string ~enc:(fun r -> r.collection)
  |> Jsont.Object.mem "record" Jsont.json ~enc:(fun r -> r.record)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "rkey" Jsont.string ~enc:(fun r -> r.rkey)
  |> Jsont.Object.opt_mem "swapCommit" Jsont.string ~enc:(fun r -> r.swap_commit)
  |> Jsont.Object.opt_mem "swapRecord" Jsont.string ~enc:(fun r -> r.swap_record)
  |> Jsont.Object.opt_mem "validate" Jsont.bool ~enc:(fun r -> r.validate)
  |> Jsont.Object.finish

type output = {
  cid : string;
  commit : Defs.commit_meta option;
  uri : string;
  validation_status : string option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cid commit uri validation_status -> { cid; commit; uri; validation_status })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"com.atproto.repo.putRecord#output" ~enc:(fun _ -> "com.atproto.repo.putRecord#output")
  |> Jsont.Object.mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.opt_mem "commit" Defs.commit_meta_jsont ~enc:(fun r -> r.commit)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.opt_mem "validationStatus" Jsont.string ~enc:(fun r -> r.validation_status)
  |> Jsont.Object.finish

      end
      module DeleteRecord = struct
type input = {
  collection : string;
  repo : string;
  rkey : string;
  swap_commit : string option;
  swap_record : string option;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ collection repo rkey swap_commit swap_record -> { collection; repo; rkey; swap_commit; swap_record })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"com.atproto.repo.deleteRecord#input" ~enc:(fun _ -> "com.atproto.repo.deleteRecord#input")
  |> Jsont.Object.mem "collection" Jsont.string ~enc:(fun r -> r.collection)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "rkey" Jsont.string ~enc:(fun r -> r.rkey)
  |> Jsont.Object.opt_mem "swapCommit" Jsont.string ~enc:(fun r -> r.swap_commit)
  |> Jsont.Object.opt_mem "swapRecord" Jsont.string ~enc:(fun r -> r.swap_record)
  |> Jsont.Object.finish

type output = {
  commit : Defs.commit_meta option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ commit -> { commit })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"com.atproto.repo.deleteRecord#output" ~enc:(fun _ -> "com.atproto.repo.deleteRecord#output")
  |> Jsont.Object.opt_mem "commit" Defs.commit_meta_jsont ~enc:(fun r -> r.commit)
  |> Jsont.Object.finish

      end
      module CreateRecord = struct
type input = {
  collection : string;
  record : Jsont.json;
  repo : string;
  rkey : string option;
  swap_commit : string option;
  validate : bool option;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ collection record repo rkey swap_commit validate -> { collection; record; repo; rkey; swap_commit; validate })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"com.atproto.repo.createRecord#input" ~enc:(fun _ -> "com.atproto.repo.createRecord#input")
  |> Jsont.Object.mem "collection" Jsont.string ~enc:(fun r -> r.collection)
  |> Jsont.Object.mem "record" Jsont.json ~enc:(fun r -> r.record)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.opt_mem "rkey" Jsont.string ~enc:(fun r -> r.rkey)
  |> Jsont.Object.opt_mem "swapCommit" Jsont.string ~enc:(fun r -> r.swap_commit)
  |> Jsont.Object.opt_mem "validate" Jsont.bool ~enc:(fun r -> r.validate)
  |> Jsont.Object.finish

type output = {
  cid : string;
  commit : Defs.commit_meta option;
  uri : string;
  validation_status : string option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cid commit uri validation_status -> { cid; commit; uri; validation_status })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"com.atproto.repo.createRecord#output" ~enc:(fun _ -> "com.atproto.repo.createRecord#output")
  |> Jsont.Object.mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.opt_mem "commit" Defs.commit_meta_jsont ~enc:(fun r -> r.commit)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.opt_mem "validationStatus" Jsont.string ~enc:(fun r -> r.validation_status)
  |> Jsont.Object.finish

      end
    end
  end
end
