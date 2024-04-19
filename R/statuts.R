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

#' Search available status
#'
#' @param ... named arguments used to build the query. Names correspond
#'   to parameters described in the API documentation
#'   (https://taxref.mnhn.fr/taxref-web/api/doc, section /taxa/search). The
#'   values correspond to the searched terms. Here are some examples of
#'   usefull parameters:
#' * taxrefId
#' * statusTypeId
#' * operationalGroupId
#' * locationId
#'
#' @return a data frame
#' @export
#'
#' @importFrom dplyr select bind_cols
#' @importFrom httr parse_url build_url GET http_status content
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
search_status <- function(...) {
  url <- file.path(base_url, "status/search/lines") %>%
    httr::parse_url()

  url$query <- c(list(...), size=5000, page=1)

  url <- httr::build_url(url)

  response <- url %>%
    httr::GET()

  if (httr::http_status(response)$category != "Success")
    stop("Request failed with the message : ", httr::http_status(response)$message)

  content <- response %>%
    httr::content("text") %>%
    jsonlite::fromJSON()

  content$`_embedded`$status %>%
    dplyr::select(-`_links`) %>%
    (function(x) {
      dplyr::bind_cols(x$taxon, dplyr::select(x, -taxon))
    }) %>%
    tibble::as_tibble()
}
