## Unreleased

- Search ranks results in tiers: pages and tags the query names, then papers,
  notes, projects, ideas and talks by relevance, then the links they cite.
  Links are deduplicated and links to this site are left out.
- Search moves from a modal to a `/search` page with facets, a year
  histogram and a links rail. Search URLs can be shared. `Cmd-K` opens it.
- `arod search` prints results by tier with scores.
- `arod search`, which opens the index read-only, now drops the site's own
  host from links and offers project and tag go-to hits, matching the
  server's in-memory index. It reports "No results." for an empty query
  instead of a blank line.
- A tag-only search (`#tag`) returns hits with their tags and a non-empty
  tag facet.
- A search snippet HTML-escapes its body text, so page markup or a
  third-party link summary under a match can no longer inject HTML.
- A tag go-to hit links straight to the search page instead of a `/#tag=`
  hash that bounced through the home page first.
- The search page's "Show N more" button stops growing past the server's
  100-hit limit, and its year histogram ignores a corrupt year instead of
  rendering thousands of bars.
