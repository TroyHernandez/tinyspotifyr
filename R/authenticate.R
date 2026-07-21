# authenticate.R
# Spotify OAuth via tinyoauth. Client-credentials and user (authorization-code)
# tokens, with a back-compat path that carries over an existing httr .httr-oauth
# cache so prior authorizations keep working without a fresh login.

#' Spotify OAuth client
#' @param client_id Spotify client id (default env SPOTIFY_CLIENT_ID).
#' @param client_secret Spotify client secret (default env SPOTIFY_CLIENT_SECRET).
#' @return A tinyoauth client.
#' @keywords internal
.spotify_client <- function(client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
                            client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET")) {
    tinyoauth::oauth_client(
                            id = client_id, secret = client_secret,
                            token_url = "https://accounts.spotify.com/api/token",
                            auth_url = "https://accounts.spotify.com/authorize")
}

#' Get Spotify Access Token
#'
#' Creates a Spotify access token via the client-credentials grant (app-only).
#' @param client_id Defaults to system environment variable "SPOTIFY_CLIENT_ID"
#' @param client_secret Defaults to system environment variable "SPOTIFY_CLIENT_SECRET"
#' @keywords auth
#' @return An access token string.
#' @export
#' @examples
#' \dontrun{
#' get_spotify_access_token()
#' }
get_spotify_access_token <- function(client_id = Sys.getenv('SPOTIFY_CLIENT_ID'),
                                     client_secret = Sys.getenv('SPOTIFY_CLIENT_SECRET')) {
    tok <- tinyoauth::oauth_token_client(.spotify_client(client_id,
            client_secret))
    tok$access_token
}

#' Get Spotify authorization Code
#'
#' Obtains a user-authorized token via the authorization-code grant, with
#' caching and refresh. If a legacy httr \code{.httr-oauth} file is present, its
#' authorization is carried over (refreshed) so no new browser login is needed.
#' @param client_id Defaults to system environment variable "SPOTIFY_CLIENT_ID"
#' @param client_secret Defaults to system environment variable "SPOTIFY_CLIENT_SECRET"
#' @param scope Spotify scopes; all are requested by default.
#' @keywords auth
#' @return A tinyoauth token (carries the access and refresh tokens).
#' @export
#' @examples
#' \dontrun{
#' get_spotify_authorization_code()
#' }
get_spotify_authorization_code <- function(client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
    client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"),
    scope = tinyspotifyr::scopes) {
    client <- .spotify_client(client_id, client_secret)
    cache <- tinyoauth::oauth_cache_path(client)

    # One-time migration: seed the tinyoauth cache from a legacy httr
    # .httr-oauth file so prior authorizations keep working without a fresh
    # login. Best-effort only: import or refresh failures (e.g. a revoked or
    # stale legacy refresh token) fall through to oauth_token() below rather
    # than hard-erroring the caller. Skipped once a tinyoauth cache exists.
    legacy <- ".httr-oauth"
    if (!file.exists(cache) && file.exists(legacy)) {
        seeded <- tryCatch({
            imp <- tinyoauth::oauth_import_httr(legacy)
            tok <- tinyoauth::oauth_refresh(imp$client, imp$token)
            saveRDS(tok, cache)
            tok
        }, error = function(e) NULL)
        if (!is.null(seeded)) {
            return(seeded)
        }
    }

    # oauth_token() returns a cached token if valid, refreshes it if expired,
    # and otherwise runs the authorization-code (browser) flow, writing the
    # result back to the cache.
    tinyoauth::oauth_token(client, scope = paste(scope, collapse = " "),
        cache = cache)
}

