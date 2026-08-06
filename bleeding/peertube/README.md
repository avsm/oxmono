# ocaml-peertube

An OCaml client library for the PeerTube video platform API, built on Eio for effect-based I/O.

## Features

- Browse, search, and fetch videos from any PeerTube instance
- Access channels, accounts, and playlists
- Retrieve server configuration and statistics
- Automatic pagination support
- Full JSON serialization with Jsont
- Command-line client (`opeertube`) included

## Installation

### From source

```bash
git clone https://git.recoil.org/anil.recoil.org/ocaml-peertube.git
cd ocaml-peertube
opam install . --deps-only
dune build
```

## Usage

### Library

```ocaml
open Peertube

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let session = Requests.create ~sw env in
  let client = Client.create ~session ~base_url:"https://video.example.com" in
  let videos = Client.list_videos client ~count:10 () in
  List.iter (fun v ->
    Printf.printf "%s: %s\n" (Video.uuid v) (Video.name v)
  ) (Paginated.data videos)
```

### Command-line client

```bash
# Browse videos
opeertube browse -u https://video.example.com

# Search videos
opeertube search -u https://video.example.com "search query"

# Get video details
opeertube get -u https://video.example.com <uuid>

# List channels
opeertube channels list -u https://video.example.com

# Get server info
opeertube server info -u https://video.example.com

# JSON output
opeertube browse -u https://video.example.com --json
```

## API Coverage

### Videos
- `Client.list_videos` - Browse videos with filtering and sorting
- `Client.search_videos` - Full-text search with date/duration filters
- `Client.fetch_video_details` - Get details for a specific video
- `Client.fetch_channel_videos` - List videos from a channel
- `Client.fetch_all_channel_videos` - Auto-paginate all channel videos

### Channels & Accounts
- `Client.list_channels` / `Client.search_channels` / `Client.get_channel`
- `Client.list_accounts` / `Client.get_account`
- `Client.get_account_videos` / `Client.get_account_channels`

### Playlists
- `Client.list_playlists` / `Client.search_playlists` / `Client.get_playlist`
- `Client.get_playlist_videos` / `Client.get_account_playlists`

### Server
- `Client.get_config` - Server configuration
- `Client.get_stats` - Server statistics

## Documentation

Build locally with:

```bash
dune build @doc
open _build/default/_doc/_html/index.html
```

## Development

### Building

```bash
dune build
```

### Formatting

```bash
dune fmt
```

## License

ISC License. See [LICENSE.md](LICENSE.md) for details.
