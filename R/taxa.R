#' Search taxa in TAXREF
#'
#' @param search_strings a named list used to build the query. Names correspond
#'   to parameters described in the API documentation
#'   (https://taxref.mnhn.fr/taxref-web/api/doc, section /taxa/search). The
#'   values correspond to the searched terms. Here are some examples of
#'   usefull parameters:
#' * scientificNames
#' * frenchVernacularNames
#' * taxonomicRanks : e.g. ES for species or GN for genus
#' * territories : e.g. fr for mainland France
#'
#' @return a data frame with the responses (including synonyms) matching the
#'   search parameters
#' @export
#'
#' @importFrom dplyr select everything
#' @importFrom httr parse_url build_url GET http_status content
#' @importFrom jsonlite fromJSON
search_taxa <- function(search_strings) {


  url <- file.path(base_url, "taxa/search") %>%
    httr::parse_url()

  url$query <- c(search_strings, size=5000, page=1)

  url <- httr::build_url(url)

  response <- url %>%
    httr::GET()

  if (httr::http_status(response)$category != "Success")
    stop("Request failed with the message : ", httr::http_status(response)$message)

  content <- response %>%
    httr::content("text") %>%
    jsonlite::fromJSON()

  content$`_embedded`$taxa %>%
    dplyr::select(-`_links`) %>%
    dplyr::select(referenceId, referenceName, scientificName, id, frenchVernacularName, dplyr::everything())
}
