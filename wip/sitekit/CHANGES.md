# Changes

## Unreleased

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
