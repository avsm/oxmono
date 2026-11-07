(* Helpers shared by the knowledge store and image tests. *)

(* [S] wraps the store so status and search results are compared as JSON. *)
module S = struct
  include Bushel_knowledge_store

  let encode = Retrieval.Json.encode Response.status_codec
  let status store = encode (status store)
  let image_status store = encode (image_status store)

  let index ?on_progress store ~check ~max_batches =
    encode (index ?on_progress store ~check ~max_batches)

  let index_all ?on_progress store ~check =
    encode (index_all ?on_progress store ~check)

  let images_search ?mode ?offset store ~check ~query =
    Response.encode
      (Response.Search (images_search ?mode ?offset store ~check ~query))

  let images_get store ~check ~id ~offset =
    Response.encode (Response.Source (images_get store ~check ~id ~offset))
end

module J = Retrieval.Json

let check name value = if not value then failwith name
let noop () = ()

let write path text =
  Out_channel.with_open_bin path (fun oc -> output_string oc text)

let rec remove path =
  if (Unix.lstat path).st_kind = Unix.S_DIR then (
    Array.iter
      (fun name -> remove (Filename.concat path name))
      (Sys.readdir path);
    Unix.rmdir path)
  else Sys.remove path

let field name codec =
  Jsont.Object.map Fun.id
  |> Jsont.Object.mem name codec ~enc:Fun.id
  |> Jsont.Object.finish

let integer name json = J.decode (field name Jsont.int) json

(* [setenv name value] sets [name], or unsets it when [value] is [None]. The
   tests call it before starting Eio or any worker domains. *)
let setenv = Testenv.set
