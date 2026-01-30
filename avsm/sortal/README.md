# Sortal - Contact Metadata Management Library

Sortal is an OCaml library that provides a comprehensive system for managing
contact metadata with temporal validity tracking. It stores data in
XDG-compliant locations using the YAML format and optionally versions all changes
with git.

## Features

- **Temporal Support**: Track how contact information changes over time (emails, organizations, URLs)
- **XDG-compliant storage**: Contact metadata stored in standard XDG data directories
- **YAML format**: Human-readable YAML files with type-safe encoding/decoding using yamlt
- **Rich metadata**: Support for multiple names, emails (typed), organizations, services (GitHub, social media), ORCID, URLs, and Atom feeds
- **Git Versioning**: Optional automatic git commits for all changes with descriptive messages
- **CLI Interface**: Full command-line interface for CRUD operations on contacts
- **Simple API**: Easy-to-use functions for saving, loading, searching, and deleting contacts

## Metadata Fields

Each contact can include:

- `handle`: Unique identifier/username (required)
- `names`: List of full names with primary name first (required)
- `email`: Email address
- `icon`: Avatar/icon URL
- `thumbnail`: Path to a local thumbnail image file
- `github`: GitHub username
- `twitter`: Twitter/X username
- `bluesky`: Bluesky handle
- `mastodon`: Mastodon handle (with instance)
- `orcid`: ORCID identifier
- `url`: Personal/professional website
- `atom_feeds`: List of Atom/RSS feed URLs

## Storage

Contact data is stored as individual YAML files in the XDG data directory:

- Default location: `$HOME/.local/share/sortal/`
- Override with: `SORTAL_DATA_DIR` or `XDG_DATA_HOME`
- Each contact stored as: `{handle}.yaml`
- Format: Human-readable YAML with temporal data support

## Usage Example

### Basic Usage

```ocaml
(* Create a contact store from filesystem *)
let store = Sortal.create env#fs "myapp" in

(* Or create from an existing XDG context (recommended when using eiocmd) *)
let store = Sortal.create_from_xdg xdg in

(* Create a new contact *)
let contact = Sortal.Contact.make
  ~handle:"avsm"
  ~names:["Anil Madhavapeddy"]
  ~email:"anil@recoil.org"
  ~github:"avsm"
  ~orcid:"0000-0002-7890-1234"
  () in

(* Save the contact *)
Sortal.save store contact;

(* Lookup by handle *)
match Sortal.lookup store "avsm" with
| Some c -> Printf.printf "Found: %s\n" (Sortal.Contact.name c)
| None -> Printf.printf "Not found\n"

(* Search for contacts by name *)
let matches = Sortal.search_all store "Anil" in
List.iter (fun c ->
  Printf.printf "%s: %s\n"
    (Sortal.Contact.handle c)
    (Sortal.Contact.name c)
) matches

(* List all contacts *)
let all_contacts = Sortal.list store in
List.iter (fun c ->
  Printf.printf "%s: %s\n"
    (Sortal.Contact.handle c)
    (Sortal.Contact.name c)
) all_contacts
```

## CLI Tool

The library includes a standalone `sortal` CLI tool with full CRUD functionality:

```bash
# Initialize git versioning (optional)
sortal git-init

# List all contacts
sortal list

# Show details for a specific contact
sortal show avsm

# Search for contacts
sortal search "Anil"

# Show database statistics
sortal stats

# Add a new contact
sortal add jsmith --name "John Smith" --email "john@example.com" --kind person

# Add metadata to contacts
sortal add-org jsmith "Acme Corp" --title "Software Engineer" --from 2020-01
sortal add-service jsmith "https://github.com/jsmith" --kind github --handle jsmith
sortal add-email jsmith "john.work@example.com" --type work --from 2020-01
sortal add-url jsmith "https://jsmith.example.com" --label "Personal website"

# Remove metadata
sortal remove-email jsmith "old@example.com"
sortal remove-service jsmith "https://old-service.com"
sortal remove-org jsmith "Old Company"
sortal remove-url jsmith "https://old-url.com"

# Delete a contact
sortal delete jsmith

# Synchronize data (convert thumbnails to PNG)
sortal sync
```

## Git Versioning

Sortal includes a `Sortal_git_store` module that provides automatic git commits
for all contact modifications:

```ocaml
open Sortal

(* Create a git-backed store *)
let git_store = Git_store.create store env in

(* Initialize git repository *)
let () = match Git_store.init git_store with
  | Ok () -> Logs.app (fun m -> m "Git initialized")
  | Error msg -> Logs.err (fun m -> m "Error: %s" msg)
in

(* Save a contact - automatically commits with descriptive message *)
let contact = Contact.make ~handle:"jsmith" ~names:["John Smith"] () in
match Git_store.save git_store contact with
| Ok () -> Logs.app (fun m -> m "Contact saved and committed")
| Error msg -> Logs.err (fun m -> m "Error: %s" msg)
```

**Commit Messages**: All git store operations create descriptive commit messages:
- `save`: "Add contact @handle (Name)" or "Update contact @handle (Name)"
- `delete`: "Delete contact @handle (Name)"
- `add_email`: "Update @handle: add email address@example.com"
- `remove_email`: "Update @handle: remove email address@example.com"
- `add_service`: "Update @handle: add service Kind (url)"
- `add_organization`: "Update @handle: add organization Org Name"
- And similar for all other operations

## Project Status

Still very much just used by Anil Madhavapeddy. You're welcome to try it, but let me know...

## License

ISC License - see [LICENSE.md](LICENSE.md) for details.
