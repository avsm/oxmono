# Open work

Package READMEs describe current behavior. These links track outstanding work,
not promises that historical test runs still apply.

- [IMAP acceptance gates](wip/imap/PRODUCTION-GATES.md): crash coverage,
  concurrency, resource bounds and independent-server verification.
- [Siftbot roadmap](u/avsm/siftbot/DESIGN.md#remaining-work): discovery, model
  evaluation, reviewed identities and attention workflows.
- [Matrix interoperability audit](wip/matrix/TODO.md#verification-interoperability-audit):
  run the independent-client checklist on a Docker host.
- [ocaml-ci-spindle plans](wip/ocaml-ci-spindle/PLAN.md): estimated build
  times from earlier runs, and approved candidate fixes from diagnosis.
- Multi-domain HTTP serving: `Proffer_httpz.run` currently serves connections
  as fibers on one domain. Arod's log database and other capabilities need an
  explicit ownership and dispatch design before adding domains. Start with
  [the backend contract](wip/proffer/httpz/proffer_httpz.mli) and
  [Arod's server](u/avsm/arod/lib/server/arod_server.ml).
- Replace Sortal's handwritten HTML builders with Htmlit if the reduction
  justifies the change. Preserve escaping and rendering behavior with explicit
  comparisons, including `&#39;` versus `&apos;`. See
  [the current builder](u/avsm/sortal/lib/web/html.ml).

The old porting transcript and allocation measurements remain in Git history.
Re-probe current interfaces and benchmarks before proposing further mode or
allocation changes. In particular, vendored Jsont now supports portable codecs.
