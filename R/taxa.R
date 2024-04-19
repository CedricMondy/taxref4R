#' Search taxa in TAXREF
#'
#' @param ... named arguments used to build the query. Names correspond
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
#' @importFrom dplyr select everything
#' @importFrom httr parse_url build_url GET http_status content
#' @importFrom jsonlite fromJSON
search_taxa <- function(...) {


  url <- file.path(base_url, "taxa/search") %>%
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

  content$`_embedded`$taxa %>%
    dplyr::select(-`_links`) %>%
    dplyr::select(referenceId, referenceName, scientificName, id, frenchVernacularName, dplyr::everything())
}


#' Get taxa information
#'
#' @param cd_nom TAXREF taxon identifier
#'
#' @return a data frame
#' @export
#'
#' @importFrom dplyr select everything
#' @importFrom httr GET http_status content
#' @importFrom jsonlite fromJSON
get_taxa <- function(cd_nom) {
  response <- file.path(base_url, "taxa", cd_nom) %>%
    httr::GET()

  if (httr::http_status(response)$category != "Success")
    stop("Request failed with the message : ", httr::http_status(response)$message)

  response %>%
    httr::content("text") %>%
    jsonlite::fromJSON() %>%
    (function(x) {
      x[names(x) != "_links"] %>%
        lapply(function(y) if(is.null(y)) NA else y)
    }) %>%
    as.data.frame() %>%
    dplyr::select(referenceId, referenceName, scientificName, id, frenchVernacularName, dplyr::everything())

}

#' Get taxa classification
#'
#' @inheritParams get_taxa
#'
#' @return a data frame with one row per taxon from the highest parent (first
#'   row) to the taxon corresponding to searched cd_nom (last row)
#' @export
#'
#' @importFrom dplyr select everything
#' @importFrom httr GET http_status content
#' @importFrom jsonlite fromJSON
get_taxa_classification <- function(cd_nom) {
  response <- file.path(base_url, "taxa", cd_nom, "classification") %>%
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

#' Get taxa synonyms
#'
#' @inheritParams get_taxa
#'
#' @return a data frame with one row per synonym of the taxon corresponding to
#'   searched cd_nom
#' @export
#'
#' @importFrom dplyr select bind_rows everything
#' @importFrom httr GET http_status content
#' @importFrom jsonlite fromJSON
get_taxa_synonyms <- function(cd_nom) {
  cd_ref <- get_taxa(cd_nom)$referenceId

  response <- file.path(base_url, "taxa", cd_ref, "synonyms") %>%
    httr::GET()

  if (httr::http_status(response)$category != "Success")
    stop("Request failed with the message : ", httr::http_status(response)$message)

  content <- response %>%
    httr::content("text") %>%
    jsonlite::fromJSON()

  if (is.null(content$`_embedded`$taxa)) {
    cat("No synonym found")
  } else {
    content$`_embedded`$taxa %>%
      dplyr::select(-`_links`) %>%
      dplyr::bind_rows(get_taxa(cd_ref), .) %>%
      dplyr::select(fullName, id, referenceName, referenceId, frenchVernacularName, dplyr::everything())
  }
}

#' Get taxa children
#'
#' @inheritParams get_taxa
#'
#' @return a data frame
#' @export
#'
#' @importFrom dplyr select everything
#' @importFrom httr GET http_status content
#' @importFrom jsonlite fromJSON
get_taxa_children <- function(cd_nom) {
  cd_ref <- get_taxa(cd_nom)$referenceId

  response <- file.path(base_url, "taxa", cd_ref, "children") %>%
    httr::GET()

  if (httr::http_status(response)$category != "Success")
    stop("Request failed with the message : ", httr::http_status(response)$message)

  content <- response %>%
    httr::content("text") %>%
    jsonlite::fromJSON()

  if (is.null(content$`_embedded`$taxa)) {
    cat("No children found")
  } else {
    content$`_embedded`$taxa %>%
      dplyr::select(-`_links`) %>%
      dplyr::select(referenceId, referenceName, scientificName, id, frenchVernacularName, dplyr::everything())
  }

}


#' Get taxa status
#'
#' @inheritParams get_taxa
#'
#' @return a data frame
#' @export
#'
#' @importFrom dplyr select starts_with
#' @importFrom httr GET http_status content
#' @importFrom jsonlite fromJSON
get_taxa_status <- function(cd_nom) {
  cd_ref <- get_taxa(cd_nom)$referenceId

  response <- file.path(base_url, "taxa", cd_ref, "status/lines") %>%
    httr::GET()

  if (httr::http_status(response)$category != "Success")
    stop("Request failed with the message : ", httr::http_status(response)$message)

  content <- response %>%
    httr::content("text") %>%
    jsonlite::fromJSON()

  if (is.null(content$`_embedded`$status)) {
    cat("No status found")
  } else {
    content$`_embedded`$status %>%
      dplyr::mutate(referenceId = cd_ref) %>%
      dplyr::select(referenceId, dplyr::starts_with("status"), dplyr::starts_with("location"), dplyr::starts_with("source"))
  }

}
