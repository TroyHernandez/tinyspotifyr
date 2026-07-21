#' Get Spotify catalog information for a single show identified by their unique Spotify ID.
#'
#' @param id The \href{https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids}{Spotify ID} for the show.
#' @param market Required. \cr
#' An ISO 3166-1 alpha-2 country code or the string \code{"from_token"}. \cr
#' Supply this parameter to limit the response to one particular geographical market. For example, for albums available in Sweden: \code{market = "SE"}. \cr
#' If not given, results will be returned for all markets and you are likely to get duplicate results per album, one for each market in which the album is available!
#' @param authorization Required. A valid access token from the Spotify Accounts service. See the \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/}{Web API authorization Guide} for more details. Defaults to \code{spotifyr::get_spotify_access_token()}
#' @return
#' Returns a data frame of results containing show data. See \url{https://developer.spotify.com/documentation/web-api} for more information.
#' @export

get_show <- function(id, market = "US",
                     authorization = get_spotify_access_token()) {
    base_url <- 'https://api.spotify.com/v1/shows'

    params <- list(market = market)
    url <- paste0(base_url, "/", id)
    res <- tinyoauth::oauth_request(authorization, url, "GET",
                                    query = params, flatten = TRUE)

    return(res)
}

#' Get Spotify catalog information for a single show identified by their unique Spotify ID.
#'
#' @param ids Required. A comma-separated list of the Spotify IDs for the shows. Maximum: 50 IDs.
#' @param market Required. \cr
#' An ISO 3166-1 alpha-2 country code or the string \code{"from_token"}. \cr
#' Supply this parameter to limit the response to one particular geographical market. For example, for albums available in Sweden: \code{market = "SE"}. \cr
#' If not given, results will be returned for all markets and you are likely to get duplicate results per album, one for each market in which the album is available!
#' @param authorization Required. A valid access token from the Spotify Accounts service. See the \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/}{Web API authorization Guide} for more details. Defaults to \code{spotifyr::get_spotify_access_token()}
#' @return
#' Returns a data frame of results containing show data. See \url{https://developer.spotify.com/documentation/web-api} for more information.
#' @export

get_shows <- function(ids, market = "US",
                      authorization = get_spotify_access_token()) {
    base_url <- 'https://api.spotify.com/v1/shows'

    params <- list(ids = paste(ids, collapse = ','), market = market)
    url <- paste0(base_url)
    res <- tinyoauth::oauth_request(authorization, url, "GET",
                                    query = params, flatten = TRUE)

    return(res)
}

#' Get Spotify catalog information for a show's episodes identified by their unique Spotify ID.
#'
#' @param id The \href{https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids}{Spotify ID} for the show.
#' @param market Required. \cr
#' An ISO 3166-1 alpha-2 country code or the string \code{"from_token"}. \cr
#' Supply this parameter to limit the response to one particular geographical market. For example, for albums available in Sweden: \code{market = "SE"}. \cr
#' If not given, results will be returned for all markets and you are likely to get duplicate results per album, one for each market in which the album is available!
#' @param authorization Required. A valid access token from the Spotify Accounts service. See the \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/}{Web API authorization Guide} for more details. Defaults to \code{spotifyr::get_spotify_access_token()}
#' @return
#' Returns a data frame of results containing the episode data for a show. See \url{https://developer.spotify.com/documentation/web-api} for more information.
#' @export

get_shows_episodes <- function(id, market = "US",
                               authorization = get_spotify_access_token()) {
    base_url <- 'https://api.spotify.com/v1/shows'

    params <- list(market = market)
    url <- paste0(base_url, "/", id, "/episodes")
    res <- tinyoauth::oauth_request(authorization, url, "GET",
                                    query = params, flatten = TRUE)

    return(res)
}

#' Get Spotify uri information for a show's latest episodes identified by their unique Spotify ID.
#'
#' @param id The \href{https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids}{Spotify ID} for the show.
#' @param market Required. \cr
#' An ISO 3166-1 alpha-2 country code or the string \code{"from_token"}. \cr
#' Supply this parameter to limit the response to one particular geographical market. For example, for albums available in Sweden: \code{market = "SE"}. \cr
#' If not given, results will be returned for all markets and you are likely to get duplicate results per album, one for each market in which the album is available!
#' @param authorization Required. A valid access token from the Spotify Accounts service. See the \href{https://developer.spotify.com/documentation/general/guides/authorization-guide/}{Web API authorization Guide} for more details. Defaults to \code{spotifyr::get_spotify_access_token()}
#' @return
#' Returns a string containing the latest episode data for a show. See \url{https://developer.spotify.com/documentation/web-api} for more information.
#' @export

get_latest_episode <- function(id, market = "US",
                               authorization = get_spotify_authorization_code()) {
    episodes <- get_shows_episodes(id = id)
    uri <- episodes$items$uri[1]
    uri
}
