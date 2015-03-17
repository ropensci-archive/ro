#' Authorize with GitHub.
#'
#' This function is run automatically to allow gistr to access your GitHub account.
#'
#' There are two ways to authorise gistr to work with your GitHub account:
#' \itemize{
#'  \item Generate a personal access token at
#'    \url{https://help.github.com/articles/creating-an-access-token-for-command-line-use} and
#'    record in the \code{GITHUB_PAT} envar.
#'  \item Interactively login into your GitHub account and authorise with OAuth.
#' }
#'
#' Using \code{GITHUB_PAT} is recommended.
#'
#' @import httr jsonlite
#' @export
#' @param app An \code{\link[httr]{oauth_app}} for GitHub.
#' @param reauth (logical) Force re-authorization?
#' @examples \dontrun{
#' auth()
#' auth(reauth = TRUE)
#' }
auth <- function(app = ro_app, reauth = FALSE) {

  if (exists("auth_config", envir = cache) && !reauth) {
    return(cache$auth_config)
  }
  pat <- Sys.getenv("RO_PAT", "")
  if (!identical(pat, "")) {
    auth_config <- httr::add_headers(Authorization = paste0("token ", pat))
  } else if (!interactive()) {
    stop("In non-interactive environments, please set GITHUB_PAT env to a GitHub",
         " access token (https://help.github.com/articles/creating-an-access-token-for-command-line-use)",
         call. = FALSE)
  } else  {
    endpt <- httr::oauth_endpoints("github")
    token <- httr::oauth2.0_token(endpt, app, scope = "public_repo", cache = !reauth)
    auth_config <- httr::config(token = token)
  }
  cache$auth_config <- auth_config
  auth_config
}

cache <- new.env(parent = emptyenv())

ro_app <- httr::oauth_app(
  "rotesting",
  "3101bc53a2b32bec3e54",
  "95e9ea7cad512ff9b8d8068703a94a305d46fe15"
)
