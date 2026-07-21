#' Get a detailed audio analysis for a single track identified by its unique Spotify ID.
#'
#' @param id The \href{https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids}{Spotify ID} for the track.
#' @param authorization Required. A valid access token from the Spotify Accounts service. See the \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/}{Web API authorization guide} for more details. Defaults to \code{spotifyr::get_spotify_access_token()}
#' @return
#' Returns a data frame of results containing track audio analysis data. See \url{https://developer.spotify.com/documentation/web-api} for more information.
#' @export

get_track_audio_analysis <- function(id,
                                     authorization = get_spotify_access_token()) {
    base_url <- 'https://api.spotify.com/v1/audio-analysis'
    url <- paste0(base_url, "/", id)
    tinyoauth::oauth_request(authorization, url, "GET", flatten = TRUE)
}

#' Get audio feature information for a single track identified by its unique Spotify ID.
#'
#' @details Deprecated. Spotify restricted \code{GET /v1/audio-features}
#'   (HTTP 403) in November 2024; it no longer returns data for apps without
#'   prior access.
#' @param ids Required. A comma-separated list of the \href{https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids}{Spotify IDs} of the tracks. Maximum: 100 IDs.
#' @param authorization Required. A valid access token from the Spotify Accounts service. See the \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/}{Web API authorization guide} for more details. Defaults to \code{spotifyr::get_spotify_access_token()}
#' @return
#' Returns a data frame of results containing track audio features data. See \url{https://developer.spotify.com/documentation/web-api} for more information.
#' @export

get_track_audio_features <- function(ids,
                                     authorization = get_spotify_access_token()) {
    .Deprecated(msg = paste("get_track_audio_features(): Spotify restricted",
                            "GET /v1/audio-features (HTTP 403) in November 2024; it no longer",
                            "returns data for apps without prior access."))
    stopifnot(length(ids) <= 100)
    base_url <- 'https://api.spotify.com/v1/audio-features'
    params <- list(ids = paste0(ids, collapse = ','))
    res <- tinyoauth::oauth_request(authorization, base_url, "GET",
                                    query = params, flatten = TRUE)
    res$audio_features
}

#' Get Spotify catalog information for a single track identified by its unique Spotify ID.
#'
#' @param id The \href{https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids}{Spotify ID} for the track.
#' @param market Optional. \cr
#' An ISO 3166-1 alpha-2 country code or the string \code{"from_token"}. Provide this parameter if you want to apply \href{https://developer.spotify.com/documentation/general/guides/track-relinking-guide/}{Track Relinking}
#' @param authorization Required. A valid access token from the Spotify Accounts service. See the \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/}{Web API authorization guide} for more details. Defaults to \code{spotifyr::get_spotify_access_token()}
#' @return
#' Returns a data frame of results containing track data. See \url{https://developer.spotify.com/documentation/web-api} for more information.
#' @export

get_track <- function(id, market = NULL,
                      authorization = get_spotify_access_token()) {
    base_url <- 'https://api.spotify.com/v1/tracks'
    if (!is.null(market)) {
        if (!grepl('^[[:alpha:]]{2}$', market)) {
            stop('"market" must be an ISO 3166-1 alpha-2 country code')
        }
    }
    url <- paste0(base_url, "/", id)
    tinyoauth::oauth_request(authorization, url, "GET",
                             query = list(market = market), flatten = TRUE)
}

#' Get Spotify catalog information for a single track identified by its unique Spotify ID.
#'
#' @param ids The \href{https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids}{Spotify ID} for the track.
#' @param market Optional. \cr
#' An ISO 3166-1 alpha-2 country code or the string \code{"from_token"}. Provide this parameter if you want to apply \href{https://developer.spotify.com/documentation/general/guides/track-relinking-guide/}{Track Relinking}
#' @param authorization Required. A valid access token from the Spotify Accounts service. See the \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/}{Web API authorization guide} for more details. Defaults to \code{spotifyr::get_spotify_access_token()}
#' @param include_meta_info Optional. Boolean indicating whether to include full result, with meta information such as \code{"total"}, and \code{"limit"}. Defaults to \code{FALSE}.
#' @return
#' Returns a data frame of results containing track data. See \url{https://developer.spotify.com/documentation/web-api} for more information.
#' @export

get_tracks <- function(ids, market = NULL,
                       authorization = get_spotify_access_token(),
                       include_meta_info = FALSE) {
    base_url <- 'https://api.spotify.com/v1/tracks'
    if (!is.null(market)) {
        if (!grepl('^[[:alpha:]]{2}$', market)) {
            stop('"market" must be an ISO 3166-1 alpha-2 country code')
        }
    }
    params <- list(ids = paste(ids, collapse = ','), market = market)
    res <- tinyoauth::oauth_request(authorization, base_url, "GET",
                                    query = params, flatten = TRUE)
    if (!include_meta_info) {
        res <- res$tracks
    }
    res
}
