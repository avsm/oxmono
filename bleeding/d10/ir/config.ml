type t = {
  build_parallelism : int;
  keep_staging : bool;
  log_dir : string option;
  inject_env : (string * string) list;
}

let domain_count_default () =
  try int_of_string (Sys.getenv "OI_DOMAINS")
  with Not_found | Failure _ -> Domain.recommended_domain_count ()

let default =
  let p = max 1 (min 8 (domain_count_default ())) in
  {
    build_parallelism = p;
    keep_staging = false;
    log_dir = None;
    inject_env = [];
  }

let with_env_overrides t =
  let parallel =
    match Sys.getenv_opt "OI_BUILD_PARALLELISM" with
    | Some s -> (
        try max 1 (int_of_string s) with Failure _ -> t.build_parallelism)
    | None -> t.build_parallelism
  in
  let keep =
    match Sys.getenv_opt "OI_KEEP_STAGING" with
    | Some "" | None -> t.keep_staging
    | Some _ -> true
  in
  { t with build_parallelism = parallel; keep_staging = keep }

let pp ppf t =
  Fmt.pf ppf "@[<h>{ parallelism = %d; keep_staging = %b; log_dir = %a }@]"
    t.build_parallelism t.keep_staging
    Fmt.(option ~none:(any "default") string)
    t.log_dir
