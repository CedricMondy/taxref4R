#' List all types of taxa status
#'
#' @return a data frame
#' @export
#'
#' @importFrom httr GET http_status content
#' @importFrom jsonlite fromJSON
get_status_types <- function() {
  response <- file.path(base_url, "status/types") %>%
    httr::GET()

  if (httr::http_status(response)$category != "Success")
    stop("Request failed with the message : ", httr::http_status(response)$message)

  content <- response %>%
    httr::content("text") %>%
    jsonlite::fromJSON()

  content$`_embedded`$statusTypes

}
