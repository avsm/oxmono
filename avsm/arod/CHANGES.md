## Unreleased

- The ideas page is rewritten for a student browsing for a project. The page
  opens on the ground rules rather than on a description of itself: who the
  ideas are open to, and that a short specific enquiry about something
  concrete stands a far better chance of an answer than one of the many
  applications that read as machine written. An idea still going is a card
  bordered in the colour of its status, with a sentence saying the level it
  suits, the year and who co-supervises it. The picture washes across the
  project heading rather than behind the words of each card, so one bar
  carries the look of the work for the whole block instead of repeating it
  down the page under ideas that share a project. That covers work already
  under way as well as work open for takers, so an idea being done now
  carries its summary without the reader opening anything, and its sentence
  says who is doing it and since when instead of when it was proposed. A
  project heading counts what can be taken on and what is under way
  separately, since the two are not the same offer. The sentence of facts
  sits in a narrow column down the right of a wide card, where the card was
  blank before, and the title and the summary keep the left to themselves.
  The summary is longer for the room this frees. Below the width of two
  columns the three stack in the order they read. A project that sets an
  ideas line in its own entry shows it as a subtitle within the heading bar,
  under the title and in a smaller face, which is where a student weighing
  the project up reads what working on it involves. The counts beside the
  title are chips drawn in the colour of the status each one counts, so a
  heading reads against the same key as the filter row and the card borders,
  and the chevron takes the same shape in a neutral colour since it is an
  action rather than a count. Projects run from the one with the most ideas
  going spare down to the one with the fewest, then by how much work is
  under way, then by how much history they have, so what a student can act
  on is at the top of the page. The contents follows the same order. Cards
  sit under a header naming the project that owns them, in one column held
  to a reading measure so a summary does not run the width of the screen.
  The header is a filled bar that reaches past the cards on both sides, and
  its left edge is drawn in the text colour rather than in one from the
  status palette, so a heading cannot be read as a card. Finished ideas sit
  under the same header as one line each, over the same tracks, so a line
  and a card put their facts in the same place and read down the page as one
  thing. The line is the title with the level, who took it and when beside
  it, and the whole of it links straight to the idea. Nothing opens in place
  any more. The summary and the co-supervisors are on the idea's own page,
  and a panel here only put a click in front of them. The students named in
  the line are still links to them, and a click on one goes to the student
  rather than to the idea. The status dot that used to sit before the title
  is gone, since the line is already bordered in the colour of its status,
  and the chevron that opened a whole block has gone with the panels it
  opened. The count it carried is now a chip on the heading beside the other
  two, in no status colour, since finished work covers both completed and
  expired. The filter is a rail down the side of the ideas, in room the
  reading measure used to leave blank, and falls back to a band above them
  when the window is too narrow to hold both. It is two compact rows of
  checkboxes, one per status and then one per academic level, and then a
  contents listing every project as a bar of its ideas stacked by status. A
  bar is scaled against the largest project on the page, so its length
  compares across the contents. Within a row the ticked boxes are a union,
  and the two rows narrow each other. Levels are counted over every idea
  rather than only the open ones, which is what brings PhD and Postdoc into
  the list at all. A status box is drawn in the colour its bands are drawn
  in, filled when ticked and outlined when not, so the row is the key to the
  bars as well as a filter. A line of the contents jumps to that project,
  and a filter that empties a project greys its line rather than dropping
  it, so the contents keeps its shape. There is no keyword search and no
  count of what the filter matched. The status bar graph that used to sit on
  every project block is gone, and the contents carries one line of it
  instead.
- An idea page carries a way back to the list it belongs to, above the
  title. It lands on the project the idea sits under rather than the top of
  the page, since that is where the rest of the work on the same subject is.
  An idea reached from a search, a feed or a link had no way back before.
- The people named on an idea card on a project page are links to them, as
  they already were on the ideas index. Both the students who took the idea
  on and the co-supervisors were plain text there.
- An idea open to any level of study reads as an internship project, which
  is what those ideas are in practice. It reads that way on a card, on a
  folded line, in the filter row and in the feeds, where it used to say it
  suited any level. The token the filter matches on is unchanged, so this is
  wording only and the same ideas are selected.
- Completed is violet and expired is grey in the status palette, where
  completed was grey and expired was red. A status is coloured on the ideas
  index, on an idea page and in the sidebar, so the change shows in all
  three.
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
