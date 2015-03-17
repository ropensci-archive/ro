#' Open an issue
#'
#' @param title (character) Required. Title of the issue
#' @param body (character) Optional. Body of the issue. Use newlines to make line breaks.
#' @param ... Curl options passed to \code{\link[httr]{POST}}
#' @examples \dontrun{
#' issue(title = "hello, world!", body = "adflaj asfljas flasjf adfs")
#' issue(title = "hello there!")
#' issue(title = "hello there!", body = "adsfasdfdsf \n asdfadfasdfasdf")
#' }
issue <- function(title, body = NULL, ...) {
  body <- ct(list(title = title,
                  body = body,
                  labels = list("duplicate")))
  roPOST(make_url("sckott", "hello"), body, ...)
}

roPOST <- function(url, body, ...) {
  ghjson <- accept("application/vnd.github.v3+json")
  ua <- user_agent("ropenci_ro")
  ah <- add_headers(Authorization = sprintf("token %s", auth()$token$credentials$access_token))
  x <- POST(url, body = jsonlite::toJSON(body, auto_unbox = TRUE), config = c(ah, ghjson, ua), encode = "json", ...)
  process_result(x)
}

make_url <- function(x, y) {
  sprintf("https://api.github.com/repos/%s/%s/issues", x, y)
}

process_result <- function(x) {
  if(x$status_code > 201) {
    stop(sprintf("%s %s", x$status_code, gsub("\n", " ", content(x)$message)), call. = FALSE)
  }
  tmp <- content(x, as = "text")
  jsonlite::fromJSON(tmp, FALSE)
}

ct <- function(l) Filter(Negate(is.null), l)

browse_issue <- function(x, browse = FALSE) {
  url <- x$html_url
  if(browse) {
    browseURL(url)
  } else {
    url
  }
}
