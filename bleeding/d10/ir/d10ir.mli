(** Build recipes and executors for d10 layers.

    A {!Plan.t} describes packages, dependency hashes, prepared sources, shell
    scripts and environments. Producers resolve packages and fetch and patch
    sources before execution. Hashes and cache identities belong to the
    producer. Plans contain executable code and must come from trusted sources.

    {!Direct.run} schedules a plan. {!Direct.run_node} executes a node whose
    dependencies are already available. Both support temporary staging prefixes
    and permanent prefixes for artifacts that embed their installation paths.
    Callers serialize cache writes. {!Plan.validate} checks a serialized plan
    and its source archives before execution.

    {!Makefile} exports a standalone build from unpacked sources. Its bundles
    build without the d10 libraries. {!Registry} optionally downloads prepared
    archives from an explicitly configured HTTP server. *)

module Layer_hash = Layer_hash
module Archive = Archive
module Plan = Plan
module Config = Config
module Direct = Direct
module Registry = Registry
module Install_file = Install_file
module Makefile = Makefile
