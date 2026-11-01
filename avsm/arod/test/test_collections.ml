let check name condition = if not condition then failwith name

let entries =
  Bushel.Entry.v ~papers:[] ~notes:[] ~projects:[] ~ideas:[] ~videos:[]
    ~contacts:[] ~data_dir:"." ()

let context = Arod.Ctx.of_entries ~config:Arod.Config.default entries
let items = [: 0; 1; 2; 3; 4 :]

let portable : (unit -> bool) @ portable = fun () ->
  let snapshot = Arod.Ctx.collections context in
  Arod.Collections.length (Arod.Collections.links snapshot) = 0
  && Arod.Collections.page items ~offset:1 ~limit:2 = [1; 2]

let () =
  check "immutable context and arrays cross portable closures" (portable ());
  let page = Arod.Collections.page items in
  check "negative offset" (page ~offset:(-2) ~limit:2 = [0; 1]);
  check "empty limit" (page ~offset:1 ~limit:0 = []);
  check "negative limit" (page ~offset:1 ~limit:(-1) = []);
  check "end of array" (page ~offset:5 ~limit:2 = []);
  check "oversized offset" (page ~offset:max_int ~limit:max_int = []);
  check "oversized limit" (page ~offset:3 ~limit:max_int = [3; 4]);
  check "all items" (page ~offset:0 ~limit:5 = [0; 1; 2; 3; 4]);
  let contact =
    Sortal_schema.Contact.make ~handle:"www" ~names:["WWW"]
      ~links:[{ url = "https://www./"; label = None }] ()
  in
  let domains = Arod.Collections.build_contact_by_domain [contact] in
  check "the exact www. host keeps its domain key"
    (Arod.Collections.find_contact_for_url domains "www." "/" = Some contact);
  print_endline "collection bounds and portability passed"
