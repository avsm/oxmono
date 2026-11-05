# Changes

## Unreleased

- Add `breadcrumbs`, a trail down to the current page with a house on its
  first link and chevrons drawn by the stylesheet, and `page ~crumbs`, which
  shows one above the title.

- Make `account` a menu opened from the account's picture, or its initial
  without one, at the end of the header. It lists who is signed in, the new
  `?links`, and Log out, opens without the script, and closes on a click
  elsewhere or on Escape. Floating panels take the new `--sk-float` shadow.

- Add `avatar` and `person`, an account's picture and its name after it, and
  give `account` an optional picture. `account` now takes a final unit.

- Add `terminal`, which renders a program's terminal output as HTML with
  its colours and attributes as classes, after `Console_text.strip` has
  removed every other escape code.
- Add `follow`, a link that Sitekit's script follows when the page loads,
  for sending a form's result to another site under `form-action 'self'`.
- Add `post_form`, a form posting fields and hidden values such as a CSRF
  token.
- Add Sitekit, a page shell and Htmlit components for Proffer sites that
  take only validated URLs and no raw HTML, served with content-hashed
  assets under a Content Security Policy without `'unsafe-inline'`.
