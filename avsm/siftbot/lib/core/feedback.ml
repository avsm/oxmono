module M = Siftbot_model
module Words = Set.Make (String)

type example = { feedback : M.feedback; reasons : string list; score : float }

type entry = {
  feedback : M.feedback;
  words : Words.t;
  vector : float list option;
}

type t = { entries : entry list; provider : Embedding.provider option }

let words text =
  String.lowercase_ascii text
  |> String.map (function ('a' .. 'z' | '0' .. '9') as c -> c | _ -> ' ')
  |> String.split_on_char ' '
  |> List.filter (fun s ->
         String.length s > 2
         && not
              (List.mem s
                 [
                   "the";
                   "and";
                   "for";
                   "that";
                   "this";
                   "with";
                   "from";
                   "you";
                   "your";
                   "are";
                   "was";
                   "have";
                 ]))
  |> Words.of_list

let passage (m : M.message) = Agentkit.Memo.clip 8192 (m.subject ^ "\n" ^ m.text)
let hash text = Digestif.SHA256.(to_hex (digest_string text))
let normalized text = Words.elements (words text) |> String.concat " "

let validate (p : Embedding.provider) vector =
  if
    List.length vector <> p.dimensions
    || p.dimensions < 1
    || List.exists (fun f -> not (Float.is_finite f)) vector
    || not (List.exists (fun f -> f <> 0.) vector)
  then invalid_arg "Invalid feedback embedding vector";
  let scale = List.fold_left (fun n x -> max n (abs_float x)) 0. vector in
  let scaled = List.map (fun x -> x /. scale) vector in
  let norm = sqrt (List.fold_left (fun n x -> n +. (x *. x)) 0. scaled) in
  List.map (fun x -> x /. norm) scaled

let infer p task input =
  Eio.Fiber.check ();
  match p.Embedding.embed ~task ~check:Eio.Fiber.check [ input ] with
  | [ vector ] -> validate p vector
  | _ -> invalid_arg "Feedback embedding response must contain one vector"

let prepare ?provider db =
  let entries =
    Store.feedback db
    |> List.map (fun (f : M.feedback) ->
           let input =
             Agentkit.Memo.clip 6144 (passage f.message)
             ^ "\nCorrection: "
             ^ M.dimension f.correction.dimension
             ^ " = " ^ f.correction.value ^ "\n"
             ^ Agentkit.Memo.clip 1024 f.correction.explanation
           in
           let vector =
             Option.map
               (fun (p : Embedding.provider) ->
                 let model = p.model ^ ":" ^ string_of_int p.dimensions in
                 let input_hash = hash input in
                 match
                   Store.feedback_vector db ~key:f.id ~model ~input_hash
                 with
                 | Some vector -> validate p vector
                 | None ->
                     let vector = infer p `Document input in
                     Store.save_feedback_vector db ~key:f.id ~model ~input_hash
                       vector;
                     vector)
               provider
           in
           { feedback = f; words = words input; vector })
  in
  { entries; provider }

let cosine a b =
  let dot = List.fold_left2 (fun n a b -> n +. (a *. b)) 0. a b in
  let norm xs = sqrt (List.fold_left (fun n x -> n +. (x *. x)) 0. xs) in
  dot /. (norm a *. norm b)

let same scope a b =
  try M.rule_key scope a = M.rule_key scope b with Invalid_argument _ -> false

let eligible ?(chronological = false) (m : M.message) (f : M.feedback) =
  let source = f.message in
  source.source.service = m.source.service
  && source.source.account = m.source.account
  && source.source <> m.source
  && (source.thread = "" || m.thread = "" || source.thread <> m.thread)
  && (not
        (List.exists (fun id -> List.mem id m.message_ids) source.message_ids))
  && (String.trim source.text = ""
     || normalized (Agentkit.Memo.clip 8192 source.text)
        <> normalized (Agentkit.Memo.clip 8192 m.text))
  && ((not chronological) || source.date < m.date)

let select ?(chronological = false) t ~limit (m : M.message) =
  if limit < 0 then invalid_arg "Negative feedback limit";
  let candidates =
    List.filter (fun e -> eligible ~chronological m e.feedback) t.entries
  in
  let query_words = words (passage m) in
  let query_vector =
    if candidates = [] || limit = 0 then None
    else Option.map (fun p -> infer p `Query (passage m)) t.provider
  in
  candidates
  |> List.filter_map (fun e ->
         let sender = same M.Sender m e.feedback.message in
         let list = same M.Mailing_list m e.feedback.message in
         let overlap = Words.cardinal (Words.inter query_words e.words) in
         let lexical =
           if overlap = 0 then 0.
           else
             float overlap
             /. sqrt
                  (float
                     (max 1
                        (Words.cardinal query_words * Words.cardinal e.words)))
         in
         let semantic =
           match (query_vector, e.vector) with
           | Some q, Some v -> max 0. (cosine q v)
           | _ -> 0.
         in
         let reasons =
           (if sender then [ "same sender" ] else [])
           @ (if list then [ "same mailing list" ] else [])
           @ (if lexical > 0. then [ "similar wording" ] else [])
           @ if semantic >= 0.35 then [ "semantic similarity" ] else []
         in
         if reasons = [] then None
         else
           Some
             {
               feedback = e.feedback;
               reasons;
               score =
                 (if sender then 3. else 0.)
                 +. (if list then 2. else 0.)
                 +. lexical +. semantic;
             })
  |> List.sort (fun a b ->
         match Float.compare b.score a.score with
         | 0 -> String.compare a.feedback.id b.feedback.id
         | n -> n)
  |> List.filteri (fun i _ -> i < limit)

let trace examples =
  if examples = [] then "No relevant manual corrections were supplied."
  else
    "Manual corrections supplied as examples (not proof of model reasoning):\n"
    ^ String.concat "\n"
        (List.map
           (fun (e : example) ->
             let f = e.feedback in
             Printf.sprintf "- %s (%s): %s = %s. %s. Selected by %s."
               f.message.subject f.message.sender
               (M.dimension f.correction.dimension)
               f.correction.value
               (Agentkit.Memo.clip 1024 f.correction.explanation)
               (String.concat ", " e.reasons))
           examples)
