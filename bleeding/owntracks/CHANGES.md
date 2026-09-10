# Unreleased

Preserve optional location SSID, BSSID and report creation time through message
and Recorder codecs. Display them in location output when present.

Configure Crow's OpenStreetMap interpreter through `[owntracks.overpass]`,
defaulting to `https://overpass-api.de/api/interpreter`.

Expose shared configuration-path and device-alias resolution for applications
reusing the CLI's OwnTracks configuration.

Port OwnTracks to mqttz and Jsont with borrowed payload decoding, separate
Recorder I/O, strict TOML and bounded GeoJSON exports grouped by device.
