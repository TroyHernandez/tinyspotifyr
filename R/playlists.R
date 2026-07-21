#' Add one or more tracks to a user’s playlist.
#'
#' @param playlist_id Required. The \href{https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids}{Spotify ID} for the playlist.
#' @param uris Optional. A character vector of \href{https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids}{Spotify track URIs} to add. For example \cr
#' \code{uris = "spotify:track:4iV5W9uYEdYUVa79Axb7Rh", "spotify:track:1301WleyT98MSxVHPZCA6M"} \cr
#' A maximum of 100 tracks can be added in one request.
#' @param position Optional. Integer indicating the position to insert the tracks, a zero-based index. For example, to insert the tracks in the first position: \code{position = 0}; to insert the tracks in the third position: \code{position = 2} . If omitted, the tracks will be appended to the playlist. Tracks are added in the order they are listed in the query string or request body.
#' @param market Optional for music. Required for podcasts episodes. \cr
#' An ISO 3166-1 alpha-2 country code or the string \code{"from_token"}. Provide this parameter if you want to apply \href{https://developer.spotify.com/documentation/general/guides/track-relinking-guide/}{Track Relinking}
#' @param authorization Required. A valid access token from the Spotify Accounts service. See the \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/}{Web API authorization Guide} for more details. Defaults to \code{spotifyr::get_spotify_authorization_code()}. The access token must have been issued on behalf of the current user. \cr
#' Adding tracks to the current user’s public playlists requires authorization of the \code{playlist-modify-public} scope; adding tracks to the current user’s private playlist (including collaborative playlists) requires the \code{playlist-modify-private} scope. See \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/#list-of-scopes}{Using Scopes}.
#' @return
#' No return value. Items added to playlist.
#' @export

add_items_to_playlist <- function(playlist_id, uris, position = NULL,
                                  market = "US",
                                  authorization = get_spotify_authorization_code()) {
    base_url <- 'https://api.spotify.com/v1/playlists'

    # Drop NA URIs so a caller passing a phantom episode row (which Spotify
    # sometimes returns) can't build a "uris=NA" request that 400s.
    uris <- uris[!is.na(uris)]

    if (is.null(position)) {
        url <- paste0(base_url, "/", playlist_id, "/tracks?uris=",
                      paste0(uris, collapse = ","), "&market=", market)
    } else {
        url <- paste0(base_url, "/", playlist_id, "/tracks?uris=", paste0(uris, collapse = ","), "&market=", market, "&position=", position)
    }

    tinyoauth::oauth_request(authorization, url, "POST", flatten = TRUE)
}

#' Add one or more tracks to a user’s playlist.
#'
#' @param playlist_id Required. The \href{https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids}{Spotify ID} for the playlist.
#' @param uris Optional. A character vector of \href{https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids}{Spotify track URIs} to add. For example \cr
#' \code{uris = "spotify:track:4iV5W9uYEdYUVa79Axb7Rh", "spotify:track:1301WleyT98MSxVHPZCA6M"} \cr
#' A maximum of 100 tracks can be added in one request.
#' @param position Optional. Integer indicating the position to insert the tracks, a zero-based index. For example, to insert the tracks in the first position: \code{position = 0}; to insert the tracks in the third position: \code{position = 2} . If omitted, the tracks will be appended to the playlist. Tracks are added in the order they are listed in the query string or request body.
#' @param market Optional for music. Required for podcasts episodes. \cr
#' An ISO 3166-1 alpha-2 country code or the string \code{"from_token"}. Provide this parameter if you want to apply \href{https://developer.spotify.com/documentation/general/guides/track-relinking-guide/}{Track Relinking}
#' @param authorization Required. A valid access token from the Spotify Accounts service. See the \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/}{Web API authorization Guide} for more details. Defaults to \code{spotifyr::get_spotify_authorization_code()}. The access token must have been issued on behalf of the current user. \cr
#' Adding tracks to the current user’s public playlists requires authorization of the \code{playlist-modify-public} scope; adding tracks to the current user’s private playlist (including collaborative playlists) requires the \code{playlist-modify-private} scope. See \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/#list-of-scopes}{Using Scopes}.
#' @return
#' No return value. Tracks added to playlist.
#' @export

add_tracks_to_playlist <- function(playlist_id, uris, position = NULL,
                                   market = NULL,
                                   authorization = get_spotify_authorization_code()) {
    base_url <- 'https://api.spotify.com/v1/playlists'
    url <- paste0(base_url, "/", playlist_id, "/tracks?uris=",
                  paste0(uris, collapse = ","))
    params <- list(market = market, position = position)
    tinyoauth::oauth_request(authorization, url, "POST", body = params, flatten = TRUE)
}

#' Change a playlist’s name and public/private state. (The user must, of course, own the playlist.)
#'
#' @param playlist_id Required. The \href{https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids}{Spotify ID} for the playlist.
#' @param name Optional String containing the name for the new playlist, for example \code{"Your Coolest Playlist"}. This name does not need to be unique; a user may have several playlists with the same name.
#' @param public Optional. Boolean. If \code{TRUE} the playlist will be public. If \code{FALSE} it will be private.
#' @param collaborative Optional. Boolean. If \code{TRUE} the playlist will become collaborative and other users will be able to modify the playlist in their Spotify client.Note: you can only set \code{collaborative} to \code{TRUE} on non-public playlists.
#' @param description Optional. String containing the playlist description as displayed in Spotify Clients and in the Web API.
#' @param authorization Required. A valid access token from the Spotify Accounts service. See the \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/}{Web API authorization Guide} for more details. Defaults to \code{spotifyr::get_spotify_authorization_code()}. The access token must have been issued on behalf of the current user. \cr
#' Changing a public playlist for a user requires authorization of the \code{playlist-modify-public} scope; changing a private playlist requires the \code{playlist-modify-private} scope. See \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/#list-of-scopes}{Using Scopes}.
#' @return
#' No return value. Playlist details changed.
#' @export

change_playlist_details <- function(playlist_id, name = NULL, public = NULL,
                                    collaborative = NULL, description = NULL,
                                    authorization = get_spotify_authorization_code()) {
    base_url <- 'https://api.spotify.com/v1/playlists'
    url <- paste0(base_url, "/", playlist_id)
    params <- list(name = name, public = public,
                   collaborative = collaborative, description = description)
    tinyoauth::oauth_request(authorization, url, "PUT", body = params, flatten = TRUE)
}

#' Create a playlist for a Spotify user. (The playlist will be empty until you add tracks.)
#'
#' @param user_id Required. The user's \href{https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids}{Spotify user ID}.
#' @param name Required. String containing the name for the new playlist, for example \code{"Your Coolest Playlist"}. This name does not need to be unique; a user may have several playlists with the same name.
#' @param public Optional. Boolean. Defaults to \code{TRUE}. If \code{TRUE} the playlist will be public. If \code{FALSE} it will be private. To be able to create private playlists, the user must have granted the \code{playlist-modify-private} \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/#list-of-scopes}{scope}
#' @param collaborative Optional. Boolean. Defaults to \code{FALSE}. If \code{TRUE} the playlist will be collaborative. Note that to create a collaborative playlist you must also set \code{public} to \code{FALES}. To create collaborative playlists you must have granted \code{playlist-modify-private} and \code{playlist-modify-public} \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/#list-of-scopes}{scopes}.
#' @param description Optional. String containing the playlist description as displayed in Spotify Clients and in the Web API.
#' @param authorization Required. A valid access token from the Spotify Accounts service. See the \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/}{Web API authorization Guide} for more details. Defaults to \code{spotifyr::get_spotify_authorization_code()}. The access token must have been issued on behalf of the current user. \cr
#' Creating a public playlist for a user requires authorization of the \code{playlist-modify-public} scope; creating a private playlist requires the \code{playlist-modify-private} scope. See \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/#list-of-scopes}{Using Scopes}.
#' @return
#' Returns a list containing playlist information.
#' @export

create_playlist <- function(user_id, name, public = TRUE,
                            collaborative = FALSE, description = NULL,
                            authorization = get_spotify_authorization_code()) {
    base_url <- 'https://api.spotify.com/v1/users'
    url <- paste0(base_url, "/", user_id, "/playlists")
    params <- list(name = name, public = public,
                   collaborative = collaborative, description = description)
    tinyoauth::oauth_request(authorization, url, "POST", body = params, flatten = TRUE)
}

#' Get a list of the playlists owned or followed by the current Spotify user.
#'
#' @param limit Optional. \cr
#' Maximum number of playlists to return. \cr
#' Default: 20 \cr
#' Minimum: 1 \cr
#' Maximum: 50 \cr
#' @param offset Optional. \cr
#' The index of the first playlist to return. \cr
#' Default: 0 (the first object). Maximum offset: 100,000. Use with \code{limit} to get the next set of playlists.
#' @param authorization Required. A valid access token from the Spotify Accounts service. See the \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/}{Web API authorization Guide} for more details. Defaults to \code{spotifyr::get_spotify_authorization_code()}. The access token must have been issued on behalf of the current user. \cr
#' Private playlists are only retrievable for the current user and requires the \code{playlist-read-private} \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/#list-of-scopes}{scope} to have been authorized by the user. Note that this scope alone will not return collaborative playlists, even though they are always private. \cr
#' Collaborative playlists are only retrievable for the current user and requires the \code{playlist-read-collaborative} \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/#list-of-scopes}{scope} to have been authorized by the user.
#' @param include_meta_info Optional. Boolean indicating whether to include full result, with meta information such as \code{"total"}, and \code{"limit"}. Defaults to \code{FALSE}.
#' @return
#' Returns a data frame of results containing user profile information. See \url{https://developer.spotify.com/documentation/web-api} for more information.
#' @export

get_my_playlists <- function(limit = 20, offset = 0,
                             authorization = get_spotify_authorization_code(),
                             include_meta_info = FALSE) {
    base_url <- 'https://api.spotify.com/v1/me/playlists'
    params <- list(limit = limit, offset = offset)
    res <- tinyoauth::oauth_request(authorization, base_url, "GET",
                                    query = params, flatten = TRUE)

    if (!include_meta_info) {
        res <- res$items
    }
    return(res)
}

#' Get a list of the playlists owned or followed by a Spotify user.
#'
#' @param user_id Required. The user's \href{https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids}{Spotify user ID}.
#' @param limit Optional. \cr
#' Maximum number of playlists to return. \cr
#' Default: 20 \cr
#' Minimum: 1 \cr
#' Maximum: 50 \cr
#' @param offset Optional. \cr
#' The index of the first playlist to return. \cr
#' Default: 0 (the first object). Maximum offset: 100,000. Use with \code{limit} to get the next set of playlists.
#' @param authorization Required. A valid access token from the Spotify Accounts service. See the \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/}{Web API authorization Guide} for more details. Defaults to \code{spotifyr::get_spotify_authorization_code()}. The access token must have been issued on behalf of the current user. \cr
#' Private playlists are only retrievable for the current user and requires the \code{playlist-read-private} \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/#list-of-scopes}{scope} to have been authorized by the user. Note that this scope alone will not return collaborative playlists, even though they are always private. \cr
#' Collaborative playlists are only retrievable for the current user and requires the \code{playlist-read-collaborative} \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/#list-of-scopes}{scope} to have been authorized by the user.
#' @param include_meta_info Optional. Boolean indicating whether to include full result, with meta information such as \code{"total"}, and \code{"limit"}. Defaults to \code{FALSE}.
#' @return
#' Returns a data frame of results containing user playlist information. See the official \href{https://developer.spotify.com/documentation/web-api}{Spotify Web API documentation} for more information.
#' @export
get_user_playlists <- function(user_id, limit = 20, offset = 0,
                               authorization = get_spotify_authorization_code(),
                               include_meta_info = FALSE) {
    base_url <- 'https://api.spotify.com/v1/users'
    url <- paste0(base_url, "/", user_id, "/playlists")
    params <- list(limit = limit, offset = offset)
    res <- tinyoauth::oauth_request(authorization, url, "GET",
                                    query = params, flatten = TRUE)
    if (!include_meta_info) {
        res <- res$items
    }
    return(res)
}

#' Get the current image associated with a specific playlist.
#'
#' @param playlist_id Required. The \href{https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids}{Spotify ID} for the playlist.
#' @param authorization Required. A valid access token from the Spotify Accounts service. See the \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/}{Web API authorization Guide} for more details. Defaults to \code{spotifyr::get_spotify_authorization_code()}. The access token must have been issued on behalf of the current user. \cr
#' Current playlist image for both Public and Private playlists of any user are retrievable on provision of a valid access token.
#' @return
#' Returns a data frame of results containing playlist cover image information. See the official \href{https://developer.spotify.com/documentation/web-api}{Spotify Web API Documentation} for more information.
#' @export

get_playlist_cover_image <- function(playlist_id,
                                     authorization = get_spotify_authorization_code()) {
    base_url <- 'https://api.spotify.com/v1/playlists'
    url <- paste0(base_url, "/", playlist_id, "/images")
    tinyoauth::oauth_request(authorization, url, "GET", flatten = TRUE)
}

#' Get a playlist owned by a Spotify user.
#'
#'
#' @param playlist_id Required. The \href{https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids}{Spotify ID} for the playlist.
#' @param fields Optional. Filters for the query: a comma-separated list of the fields to return. If omitted, all fields are returned. For example, to get just the playlist’s description and URI: \cr
#' \code{fields = c("description", "uri")} \cr A dot separator can be used to specify non-reoccurring fields, while parentheses can be used to specify reoccurring fields within objects. For example, to get just the added date and user ID of the adder: \cr
#' \code{fields = "tracks.items(added_at,added_by.id)"} \cr Use multiple parentheses to drill down into nested objects, for example: \cr
#' \code{fields = "tracks.items(track(name,href,album(name,href)))"} \cr Fields can be excluded by prefixing them with an exclamation mark, for example: \cr
#' \code{fields = "tracks.items(track(name,href,album(!name,href)))"}
#' @param market Optional. \cr
#' An ISO 3166-1 alpha-2 country code or the string \code{"from_token"}. Provide this parameter if you want to apply \href{https://developer.spotify.com/documentation/general/guides/track-relinking-guide/}{Track Relinking}
#' @param authorization Required. A valid access token from the Spotify Accounts service. See the \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/}{Web API authorization guide} for more details. Both Public and Private playlists belonging to any user are retrievable on provision of a valid access token. Defaults to \code{spotifyr::get_spotify_access_token()}
#' @return
#' Returns a data frame of results containing user profile information. See \url{https://developer.spotify.com/documentation/web-api} for more information.
#' @export

get_playlist <- function(playlist_id, fields = NULL, market = NULL,
                         authorization = get_spotify_access_token()) {
    base_url <- 'https://api.spotify.com/v1/playlists'
    url <- paste0(base_url, "/", playlist_id)
    params <- list(fields = paste(fields, collapse = ','), market = market)
    tinyoauth::oauth_request(authorization, url, "GET", query = params,
                             flatten = TRUE)
}

#' Get full details of the items of a playlist owned by a Spotify user.
#'
#' @param playlist_id Required. The \href{https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids}{Spotify ID} for the playlist.
#' @param fields Optional. Filters for the query: a comma-separated list of the fields to return. If omitted, all fields are returned. For example, to get just the playlist’s creation date and album information: \code{fields = c("added_at", "track.album")}. A dot separator can be used to specify non-reoccurring fields, while parentheses can be used to specify reoccurring fields within objects. For example, to get just the added date and user ID of the adder: \cr
#' \code{fields = "tracks.items(added_at,added_by.id)"}. Use multiple parentheses to drill down into nested objects, for example: \cr
#' \code{fields = "tracks.items(track(name,href,album(name,href)))"}. Fields can be excluded by prefixing them with an exclamation mark, for example: \cr
#' \code{fields = "tracks.items(track(name,href,album(!name,href)))"}.
#' @param limit Optional. \cr
#' Maximum number of tracks to return. \cr
#' Default: 100 \cr
#' Minimum: 1 \cr
#' Maximum: 100 \cr
#' @param offset Optional. \cr
#' The index of the first track to return. \cr
#' Default: 0 (the first object). \cr
#' @param market Optional. \cr
#' An ISO 3166-1 alpha-2 country code or the string \code{"from_token"}. Provide this parameter if you want to apply \href{https://developer.spotify.com/documentation/general/guides/track-relinking-guide/}{Track Relinking}
#' @param authorization Required. A valid access token from the Spotify Accounts service. See the \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/}{Web API authorization guide} for more details. Both Public and Private playlists belonging to any user are retrievable on provision of a valid access token. Defaults to \code{spotifyr::get_spotify_access_token()}
#' @param include_meta_info Optional. Boolean indicating whether to include full result, with meta information such as \code{"total"}, and \code{"limit"}. Defaults to \code{FALSE}.
#' @return
#' Returns a data frame of results containing user profile information. See \url{https://developer.spotify.com/documentation/web-api} for more information.
#' @export

get_playlist_items <- function(playlist_id, fields = NULL, limit = 100,
                               offset = 0, market = NULL,
                               authorization = get_spotify_access_token(),
                               include_meta_info = FALSE) {
    base_url <- 'https://api.spotify.com/v1/playlists'
    url <- paste0(base_url, "/", playlist_id, "/tracks")
    params <- list(
                   fields = ifelse(!is.null(fields),
                                   paste0('items(', paste0(fields, collapse = ','), ')'),
                                   ''),
                   limit = limit,
                   offset = offset,
                   market = market
    )
    res <- tinyoauth::oauth_request(authorization, url, "GET", query = params, flatten = TRUE)

    if (!include_meta_info) {
        res <- res$items
    }
    return(res)
}

#' Get full details of the tracks of a playlist owned by a Spotify user.
#'
#' @param playlist_id Required. The \href{https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids}{Spotify ID} for the playlist.
#' @param fields Optional. Filters for the query: a comma-separated list of the fields to return. If omitted, all fields are returned. For example, to get just the playlist’s creation date and album information: \code{fields = c("added_at", "track.album")}. A dot separator can be used to specify non-reoccurring fields, while parentheses can be used to specify reoccurring fields within objects. For example, to get just the added date and user ID of the adder: \cr
#' \code{fields = "tracks.items(added_at,added_by.id)"}. Use multiple parentheses to drill down into nested objects, for example: \cr
#' \code{fields = "tracks.items(track(name,href,album(name,href)))"}. Fields can be excluded by prefixing them with an exclamation mark, for example: \cr
#' \code{fields = "tracks.items(track(name,href,album(!name,href)))"}.
#' @param limit Optional. \cr
#' Maximum number of tracks to return. \cr
#' Default: 100 \cr
#' Minimum: 1 \cr
#' Maximum: 100 \cr
#' @param offset Optional. \cr
#' The index of the first track to return. \cr
#' Default: 0 (the first object). \cr
#' @param market Optional. \cr
#' An ISO 3166-1 alpha-2 country code or the string \code{"from_token"}. Provide this parameter if you want to apply \href{https://developer.spotify.com/documentation/general/guides/track-relinking-guide/}{Track Relinking}
#' @param authorization Required. A valid access token from the Spotify Accounts service. See the \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/}{Web API authorization guide} for more details. Both Public and Private playlists belonging to any user are retrievable on provision of a valid access token. Defaults to \code{spotifyr::get_spotify_access_token()}
#' @param include_meta_info Optional. Boolean indicating whether to include full result, with meta information such as \code{"total"}, and \code{"limit"}. Defaults to \code{FALSE}.
#' @return
#' Returns a data frame of results containing user profile information. See \url{https://developer.spotify.com/documentation/web-api} for more information.
#' @export

get_playlist_tracks <- function(playlist_id, fields = NULL, limit = 100,
                                offset = 0, market = NULL,
                                authorization = get_spotify_access_token(),
                                include_meta_info = FALSE) {
    base_url <- 'https://api.spotify.com/v1/playlists'
    url <- paste0(base_url, "/", playlist_id, "/tracks")
    params <- list(
                   fields = ifelse(!is.null(fields),
                                   paste0('items(', paste0(fields, collapse = ','), ')'),
                                   ''),
                   limit = limit,
                   offset = offset,
                   market = market
    )
    res <- tinyoauth::oauth_request(authorization, url, "GET", query = params, flatten = TRUE)

    if (!include_meta_info) {
        res <- res$items
    }
    return(res)
}

#' Remove one or more tracks from a user’s playlist.
#'
#' @param playlist_id Required. The \href{https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids}{Spotify ID} for the playlist.
#' @param uris Optional. A character vector of \href{https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids}{Spotify track URIs} to add. For example \cr
#' \code{uris = "spotify:track:4iV5W9uYEdYUVa79Axb7Rh", "spotify:track:1301WleyT98MSxVHPZCA6M"} \cr
#' A maximum of 100 tracks can be removed in one request.
#' @param authorization Required. A valid access token from the Spotify Accounts service. See the \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/}{Web API authorization Guide} for more details. Defaults to \code{spotifyr::get_spotify_authorization_code()}. The access token must have been issued on behalf of the current user. \cr
#' Removing tracks to the current user’s public playlists requires authorization of the \code{playlist-modify-public} scope; removing tracks from the current user’s private playlist (including collaborative playlists) requires the \code{playlist-modify-private} scope. See \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/#list-of-scopes}{Using Scopes}.
#' @return
#' No return value. Tracks are removed from playlist.
#' @export
remove_tracks_from_playlist <- function(playlist_id, uris,
                                        authorization = get_spotify_authorization_code()) {
    base_url <- 'https://api.spotify.com/v1/playlists'
    url <- paste0(base_url, "/", playlist_id, "/tracks/")

    # For DELETE the URIs go in the body, as an R list; oauth_request JSON-encodes it.
    uris_list <- lapply(uris, function(x) list(uri = x))
    params <- list(tracks = uris_list)

    tinyoauth::oauth_request(authorization, url, "DELETE", body = params,
                             flatten = TRUE)
}

#' Add the latest episode of a podcast to a user’s playlist.
#'
#' @param playlist_id Required. The \href{https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids}{Spotify ID} for the playlist.
#' @param uri The show \href{https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids}{Spotify URIs} to add. For example \cr
#' \code{uris = "spotify:show:6BRSvIBNQnB68GuoXJRCnQ", "spotify:show:1x1n9iWJLYNXYdDgLk5yQu"} \cr
#' @param position Optional. Integer indicating the position to insert the tracks, a zero-based index. For example, to insert the tracks in the first position: \code{position = 0}; to insert the tracks in the third position: \code{position = 2} . If omitted, the tracks will be appended to the playlist. Tracks are added in the order they are listed in the query string or request body.
#' @param market Required. \cr
#' An ISO 3166-1 alpha-2 country code or the string \code{"from_token"}. Provide this parameter if you want to apply \href{https://developer.spotify.com/documentation/general/guides/track-relinking-guide/}{Track Relinking}
#' @param authorization Required. A valid access token from the Spotify Accounts service. See the \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/}{Web API authorization Guide} for more details. Defaults to \code{spotifyr::get_spotify_authorization_code()}. The access token must have been issued on behalf of the current user. \cr
#' Adding tracks to the current user’s public playlists requires authorization of the \code{playlist-modify-public} scope; adding tracks to the current user’s private playlist (including collaborative playlists) requires the \code{playlist-modify-private} scope. See \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/#list-of-scopes}{Using Scopes}.
#' @return
#' No return value. Tracks are added to playlist.
#' @export
add_latest_to_playlist <- function(playlist_id, uri, position = NULL,
                                   market = "US",
                                   authorization = get_spotify_authorization_code()) {
    id <- strsplit(uri, ":")[[1]][3]
    episodes <- get_shows_episodes(id = id)
    # Spotify sometimes returns a phantom NA row (e.g. a region-restricted or
    # unavailable episode) ahead of the real latest one. Skip NA URIs and take
    # the first valid episode instead of blindly grabbing row 1 (which would
    # POST a "uris=NA" and 400).
    valid_uris <- episodes$items$uri[!is.na(episodes$items$uri)]
    if (length(valid_uris) > 0) {
        add_items_to_playlist(playlist_id = playlist_id, uris = valid_uris[1],
                              position = position, market = market,
                              authorization = authorization)
    }
}

#' Reorder or replace one or more items from a user’s playlist.
#'
#' @param playlist_id Required. The \href{https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids}{Spotify ID} for the playlist.
#' @param uris Optional. A character vector of \href{https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids}{Spotify track URIs} to add. For example \cr
#' \code{uris = "spotify:track:4iV5W9uYEdYUVa79Axb7Rh", "spotify:track:1301WleyT98MSxVHPZCA6M"} \cr
#' A maximum of 100 tracks can be removed in one request.
#' @param authorization Required. A valid access token from the Spotify Accounts service. See the \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/}{Web API authorization Guide} for more details. Defaults to \code{spotifyr::get_spotify_authorization_code()}. The access token must have been issued on behalf of the current user. \cr
#' Removing tracks to the current user’s public playlists requires authorization of the \code{playlist-modify-public} scope; removing tracks from the current user’s private playlist (including collaborative playlists) requires the \code{playlist-modify-private} scope. See \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/#list-of-scopes}{Using Scopes}.
#' @return
#' No return value. Tracks are added or reordered on playlist.
#' @export

reorder_replace_playlist_items <- function(playlist_id, uris,
    authorization = get_spotify_authorization_code()) {
    base_url <- 'https://api.spotify.com/v1/playlists'
    url <- paste0(base_url, "/", playlist_id, "/tracks?uris=",
                  paste0(uris, collapse = ","))
    tinyoauth::oauth_request(authorization, url, "PUT", flatten = TRUE)
}
