#' tinyspotifyr: A Quick and Easy Wrapper for Spotify's Web API
#'
#' See the README on
#' \href{https://github.com/troyhernandez/tinyspotifyr#readme}{GitHub}
#'
#' @keywords internal
"_PACKAGE"

globalVars <- c("album_name", "album_name_", "album_release_year_",
                "album_rank", "album_release_date", "album_release_year",
                "album_uri", "analysis_url", "base_album",
                "base_album_name", "key", "name", "num_albums",
                "num_base_albums", "playlist_img", "playlist_name",
                "playlist_uri", "preview_url", "href", "album_id",
                "images", "release_date", "release_date_precision",
                "track_href", "track_uri", "type", "uri",
                "album_name_lower", "album_type", "possible_album",
                "possible_lyrics", "disco_audio_feats", "data",
                "future_map_df", "map_chr", "is_collaboration", "key_name",
                "mode_name", "lyrics", "na.omit", "parse_playlist_to_df",
                "release_date", "selected_artist", "track_n", "track_title",
                "track_url", "volume", "primary_color", "track_id",
                "track.id", "playlist_id", "playlist_owner_name",
                "playlist_owner_id", "added_at", "artist_names", "artists",
                "available_markets", "duration_ms", "genius_album",
                "popularity", "track_number", ".")

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
    utils::globalVariables(globalVars)
}
