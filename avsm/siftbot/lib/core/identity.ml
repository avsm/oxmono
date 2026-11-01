module C = Sortal.Contact
module M = Siftbot_model

type person = { uid : string; contact : C.t }
type match_ = { uid : string; name : string; affiliations : string list }

let load ~fs ~root =
  let store = Sortal.Store.create_at fs root in
  Sortal.Store.list store
  |> List.map (fun contact ->
         let filename = Sortal.Store.filename store (C.handle contact) in
         {
           uid = Filename.remove_extension (Filename.basename filename);
           contact;
         })

let normalize s =
  let s = String.trim s in
  match String.rindex_opt s '@' with
  | None -> s
  | Some i ->
      String.sub s 0 (i + 1)
      ^ String.lowercase_ascii (String.sub s (i + 1) (String.length s - i - 1))

let resolve people ~aliases ~address ~date =
  let address = normalize address in
  let reviewed =
    List.filter_map
      (fun (a, uid) -> if normalize a = address then Some uid else None)
      aliases
  in
  List.filter_map
    (fun (p : person) ->
      let matched =
        if reviewed <> [] then List.mem p.uid reviewed
        else List.exists (fun a -> normalize a = address) (C.emails p.contact)
      in
      if not matched then None
      else
        let affiliations =
          C.affiliations p.contact
          |> List.filter (fun (a : C.affiliation) ->
                 Option.fold ~none:true
                   ~some:(fun start -> compare start date <= 0)
                   a.from
                 && Option.fold ~none:true
                      ~some:(fun finish -> compare date finish < 0)
                      a.until)
          |> List.map (fun (a : C.affiliation) ->
                 a.org ^ Option.fold ~none:"" ~some:(fun x -> ": " ^ x) a.title)
        in
        Some { uid = p.uid; name = C.name p.contact; affiliations })
    people

let duplicates ms =
  let rec loop acc = function
    | [] -> List.rev acc
    | (m : M.message) :: rest ->
        let candidates =
          List.filter
            (fun (n : M.message) ->
              m.source <> n.source && m.sender = n.sender && m.text = n.text
              && List.exists (fun id -> List.mem id n.message_ids) m.message_ids)
            rest
        in
        loop
          (List.map (fun (n : M.message) -> (m.source, n.source)) candidates
          @ acc)
          rest
  in
  loop [] ms
