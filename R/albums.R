#' Get Spotify catalog information for a single album.
#'
#' @param id The \href{https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids}{Spotify ID} for the album.
#' @param authorization Required. A valid access token from the Spotify Accounts service. See the \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/}{Web API authorization Guide} for more details. Defaults to \code{spotifyr::get_spotify_access_token()}
#' @param market Optional. \cr
#' An \href{https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2}{ISO 3166-1 alpha-2 country code} or the string \code{"from_token"}. Provide this parameter if you want to apply \href{https://developer.spotify.com/documentation/general/guides/track-relinking-guide/}{Track Relinking}
#' @return
#' Returns a data frame of results containing album data. See the \href{https://developer.spotify.com/documentation/web-api}{official documentation} for more information.
#' @export

get_album <- function(id, market = NULL,
                      authorization = get_spotify_access_token()) {
    base_url <- 'https://api.spotify.com/v1/albums'

    if (!is.null(market)) {
        if (!grepl('^[[:alpha:]]{2}$', market)) {
            stop('"market" must be an ISO 3166-1 alpha-2 country code')
        }
    }

    params <- list(market = market)

    url <- paste0(base_url, "/", id)
    res <- tinyoauth::oauth_request(authorization, url, "GET",
                                    query = params, flatten = TRUE)

    return(res)
}

#' Get Spotify catalog information for multiple albums identified by their Spotify IDs.
#'
#' @param ids Required. A character vector of the \href{https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids}{Spotify IDs} for the albums. Maximum: 20 IDs.
#' @param authorization Required. A valid access token from the Spotify Accounts service. See the \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/}{Web API authorization Guide} for more details. Defaults to \code{spotifyr::get_spotify_access_token()}
#' @param market Optional. \cr
#' An \href{https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2}{ISO 3166-1 alpha-2 country code} or the string \code{"from_token"}. Provide this parameter if you want to apply \href{https://developer.spotify.com/documentation/general/guides/track-relinking-guide/}{Track Relinking}
#' @param include_meta_info Optional. Boolean indicating whether to include full result, with meta information such as \code{"total"}, and \code{"limit"}. Defaults to \code{FALSE}.
#' @return
#' Returns a data frame of results containing album data. See \url{https://developer.spotify.com/documentation/web-api} for more information.
#' @export

get_albums <- function(ids, market = NULL,
                       authorization = get_spotify_access_token(),
                       include_meta_info = FALSE) {
    base_url <- 'https://api.spotify.com/v1/albums'

    if (!is.null(market)) {
        if (!grepl('^[[:alpha:]]{2}$', market)) {
            stop('"market" must be an ISO 3166-1 alpha-2 country code')
        }
    }

    params <- list(ids = paste(ids, collapse = ','), market = market)
    res <- tinyoauth::oauth_request(authorization, base_url, "GET",
                                    query = params, flatten = TRUE)

    if (!include_meta_info) {
        res <- res$albums
    }

    return(res)
}

#' Get Spotify catalog information about an album’s tracks. Optional parameters can be used to limit the number of tracks returned.
#'
#' @param id Required. The \href{https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids}{Spotify ID} for the album.
#' @param authorization Required. A valid access token from the Spotify Accounts service. See the \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/}{Web API authorization Guide} for more details. Defaults to \code{spotifyr::get_spotify_access_token()}
#' @param limit Optional. \cr
#' Maximum number of results to return. \cr
#' Default: 20 \cr
#' Minimum: 1 \cr
#' Maximum: 50 \cr
#' Note: The limit is applied within each type, not on the total response. \cr
#' For example, if the limit value is 3 and the type is \code{c("artist", "album")}, the response contains 3 artists and 3 albums.
#' @param offset Optional. \cr
#' The index of the first album to return. \cr
#' Default: 0 (the first album). \cr
#' Maximum offset (including limit): 10,000. \cr
#' Use with limit to get the next page of albums.
#' @param market Optional. \cr
#' An \href{https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2}{ISO 3166-1 alpha-2 country code} or the string \code{"from_token"}. Provide this parameter if you want to apply \href{https://developer.spotify.com/documentation/general/guides/track-relinking-guide/}{Track Relinking}
#' @param include_meta_info Optional. Boolean indicating whether to include full result, with meta information such as \code{"total"}, and \code{"limit"}. Defaults to \code{FALSE}.
#' @return
#' Returns a data frame of results containing album data. See the official API \href{https://developer.spotify.com/documentation/web-api}{documentation} for more information.
#' @export

get_album_tracks <- function(id, limit = 20, offset = 0, market = NULL,
                             authorization = get_spotify_access_token(),
                             include_meta_info = FALSE) {
    base_url <- 'https://api.spotify.com/v1/albums'

    if (!is.null(market)) {
        if (!grepl('^[[:alpha:]]{2}$', market)) {
            stop('"market" must be an ISO 3166-1 alpha-2 country code')
        }
    }

    params <- list(market = market, offset = offset, limit = limit)
    url <- paste0(base_url, "/", id, "/tracks")
    res <- tinyoauth::oauth_request(authorization, url, "GET",
                                    query = params, flatten = TRUE)

    if (!include_meta_info) {
        res <- res$items
    }

    return(res)
}
