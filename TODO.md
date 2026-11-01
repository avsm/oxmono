# Open work

Package READMEs describe current behavior. These links track outstanding work,
not promises that historical test runs still apply.

- [IMAP acceptance gates](bleeding/imap/PRODUCTION-GATES.md): crash coverage,
  concurrency, resource bounds and independent-server verification.
- [Siftbot roadmap](avsm/siftbot/DESIGN.md#remaining-work): discovery, model
  evaluation, reviewed identities and attention workflows.
- [Matrix interoperability audit](bleeding/matrix/TODO.md#verification-interoperability-audit):
  run the independent-client checklist on a Docker host.
- Multi-domain HTTP serving: `Proffer_httpz.run` currently serves connections
  as fibers on one domain. Arod's log database and other capabilities need an
  explicit ownership and dispatch design before adding domains. Start with
  [the backend contract](bleeding/proffer/httpz/proffer_httpz.mli) and
  [Arod's server](avsm/arod/lib/server/arod_server.ml).
- Replace Sortal's handwritten HTML builders with Htmlit if the reduction
  justifies the change. Preserve escaping and rendering behavior with explicit
  comparisons, including `&#39;` versus `&apos;`. See
  [the current builder](avsm/sortal/lib/web/html.ml).

The old porting transcript and allocation measurements remain in Git history.
Re-probe current interfaces and benchmarks before proposing further mode or
allocation changes. In particular, vendored Jsont now supports portable codecs.
