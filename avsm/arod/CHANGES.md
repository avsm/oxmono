## Unreleased

- The ideas page is rewritten for a student browsing for a project. An idea
  still open for takers is a card bordered in the colour of its status, with
  its picture washed in behind the words and a sentence saying the level it
  suits, the year and who co-supervises it. Cards sit under
  a header naming the project that owns them, in two columns filled left to
  right, so the first row holds the first two projects. The ideas offered
  previously fold to one line each under the same header, carrying a link to
  the idea so reaching it costs no extra click, and the chevron beside the
  project opens the lot. A keyword box and a row per academic level filter
  both kinds of entry at once, and the keyword reaches the text inside a
  folded line. The per-project status bar graph is gone.
- The ideas index stylesheet sits outside `@layer components`, as the search
  page's already does. Tailwind's preflight is unlayered, so a layered rule
  loses `border-width` to `*` and a layered button loses its padding, whatever
  the specificity.
- An idea takes its thumbnail from the first picture in its own body, falling
  back to the logo of its project as before. This shows on the ideas index,
  in a sidenote and in the `og:image` of an idea page.
- An image with no generated variants is rendered without a `srcset`, rather
  than with an empty one and a `sizes` that selects among candidates that are
  not there. This covers an animated GIF, which srcsetter passes through
  whole, and any picture already narrower than the smallest target width.
- A summary drawn out of rendered HTML has its entities decoded, so a card, a
  sidenote or a feed line reads `"close"` rather than `&quot;close&quot;`.
- The search page's facet row no longer reserves empty space when a
  browse has no tag facet, and the sort-toggle links percent-encode the
  query correctly.
- Search ranks results in tiers: pages and tags the query names, then papers,
  notes, projects, ideas and talks by relevance, then the links they cite.
  Links are deduplicated and links to this site are left out.
- Search moves from a modal to a `/search` page with facets, a year
  histogram and a links rail. Search URLs can be shared. `Cmd-K` opens it.
- `arod search` prints results by tier with scores.
- The server starts serving as soon as its content is loaded and builds
  the search index on a background fibre, answering empty until it is
  ready. A rebuild also reuses its insert statements, which roughly
  halves the indexing time.
- Weeknotes are indexed as their own search kind, so `kind:weekly` and
  the search page's facets filter them apart from notes.
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
