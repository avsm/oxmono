module M = Siftbot_model

type provider = Retrieval.Embedding.provider = {
  model : string;
  dimensions : int;
  max_batch : int;
  embed :
    task:Retrieval.Embedding.task ->
    check:(unit -> unit) -> string list -> float list list;
}

let backend db =
  Retrieval.Embedding.
    {
      documents = (fun () -> Store.messages db);
      key = (fun m -> M.key m.M.source);
      revision = (fun m -> m.M.revision);
      passage = (fun m -> clip 8192 (m.M.subject ^ "\n" ^ m.text));
      index = Store.vector_index db;
      current =
        (fun ~index ~document:m ~input_hash ->
          Store.vector_current db ~index ~source:m.M.source ~revision:m.revision
            ~input_hash);
      put =
        (fun ~index ~document:m ~input_hash vector ->
          Store.vector_put db ~index ~source:m.M.source ~revision:m.revision
            ~input_hash vector);
      nearest = Store.vector_search db;
      lexical = Store.search db;
    }

let index db = Retrieval.Embedding.index (backend db)
let search db = Retrieval.Embedding.search (backend db)
let hybrid db = Retrieval.Embedding.hybrid (backend db)
let http = Retrieval.Embedding.http
