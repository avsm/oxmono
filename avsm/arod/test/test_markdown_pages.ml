(* The list pages served as markdown follow the HTML pages: the same order,
   the same grouping and the same facts for each entry. *)

let checks = ref 0

let check name cond =
  incr checks;
  if not cond then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let find hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i =
    if i + n > h then None
    else if String.sub hay i n = needle then Some i
    else go (i + 1)
  in
  go 0

let contains hay needle = find hay needle <> None

let before hay a b =
  match (find hay a, find hay b) with Some i, Some j -> i < j | _ -> false

let cfg : Arod.Config.t =
  { Arod.Config.default with
    site = { Arod.Config.default.site with base_url = "https://example.com" } }

let ctx_of ?(papers = []) ?(notes = []) ?(projects = []) ?(ideas = [])
    ?(videos = []) ?releases () =
  Arod.Ctx.of_entries ~config:cfg ?releases
    (Bushel.Entry.v ~papers ~notes ~projects ~ideas ~videos ~contacts:[]
       ~data_dir:"." ())

(* {1 Projects} *)

let project ?(finish = None) ?(tags = []) ~slug ~title ~start body :
    Bushel.Project.t =
  { Bushel.Project.slug; title; start; finish; tags; ideas = ""; body;
    social = None }

(* Given in no useful order, so that the order of the list is the order of the
   HTML cards and not the order of loading. *)
let projects =
  [ project ~slug:"zeta" ~title:"Zeta" ~start:2020 ~finish:(Some 2022)
      ~tags:[ "alpha"; "beta" ] "Zeta opens here.\n\nA second paragraph.";
    project ~slug:"alpha" ~title:"Alpha" ~start:2024 "Alpha opens here.";
    project ~slug:"mid" ~title:"Mid" ~start:2021 "" ]

let () =
  let md =
    Arod_component.Markdown_export.projects_list_md
      ~ctx:(ctx_of ~projects ())
  in
  let expected =
    List.sort Bushel.Project.compare projects |> List.map Bushel.Project.title
  in
  let positions =
    List.map (fun t -> find md ("[" ^ t ^ "](https://example.com/projects/")) expected
  in
  check "every project is listed" (List.for_all (fun p -> p <> None) positions);
  check "projects come in the order of the HTML cards"
    (let ps = List.filter_map Fun.id positions in
     ps = List.sort compare ps);
  check "the introduction of the HTML page opens the list"
    (before md "I work on a number of research projects" "[Zeta]"
    && contains md "[EEG Zulip](https://eeg.zulipchat.com)");
  check "years are given as the card gives them"
    (contains md "(2020\xE2\x80\x932022)" && contains md "(2024\xE2\x80\x93now)");
  check "a card opens with its summary"
    (contains md "Zeta opens here." && contains md "Alpha opens here.");
  check "tags are listed" (contains md "Tags: alpha, beta");
  check "a project with no body is still listed" (contains md "[Mid]");
  Printf.printf "test_markdown_pages: %d checks passed\n" !checks
