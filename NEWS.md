# tinyspotifyr 0.2.3.3

* `get_spotify_authorization_code()` now falls back to a fresh login when a
  legacy or cached refresh token is revoked, instead of hard-erroring. The
  legacy `.httr-oauth` file seeds the tinyoauth cache once, best-effort.
* `add_latest_to_playlist()` and `add_items_to_playlist()` skip phantom `NA`
  episode rows (which Spotify sometimes returns) instead of posting `uris=NA`
  and 400ing.

# tinyspotifyr 0.2.3.2

* Fixed `get_shows_episodes()` sending `market` twice (in the URL and via the
  query), producing a malformed query and a Spotify "Invalid market parameter"
  400. Another migration leftover, like the get_artist()/get_album() fixes in
  0.2.3.1.

# tinyspotifyr 0.2.3.1

* Replaced the httr backend with tinyoauth (curl + jsonlite + serverSocket); Imports trimmed to tinyoauth alone. Existing httr .httr-oauth caches carry over via oauth_import_httr() (no re-login).
* Fixed pre-existing get_artist() and get_album() bugs surfaced during the migration.

