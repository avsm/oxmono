(* The context carries the registered releases so that pages can show them. *)

let check name cond =
  if not cond then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let cfg : Arod.Config.t =
  { Arod.Config.default with
    site = { Arod.Config.default.site with base_url = "https://example.com" } }

let entries =
  Bushel.Entry.v ~papers:[] ~notes:[] ~projects:[] ~ideas:[] ~videos:[]
    ~contacts:[] ~data_dir:"." ()

let release =
  {
    Bushel.Release.repo = "a/b";
    forge = Bushel.Release.Github;
    project = None;
    releases =
      [
        {
          Bushel.Release.version = "1.0.0";
          tag = None;
          date = (2026, 5, 1);
          summary = "s";
          url = "https://x";
          registries = [];
        };
      ];
  }

let () =
  check "no releases by default"
    (Arod.Ctx.releases (Arod.Ctx.of_entries ~config:cfg entries) = []);
  check "releases are exposed"
    (Arod.Ctx.releases
       (Arod.Ctx.of_entries ~config:cfg ~releases:[ release ] entries)
    = [ release ]);
  print_endline "ok"
