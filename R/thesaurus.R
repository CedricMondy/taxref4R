
#' Get TAXREF versions information
#'
#' @param all logical. Should all the versions be returned
#' @param id numeric identifier of the searched version
#' @param current logical. Should only the current version be returned
#'
#' @return a data frame with information about:
#' * `id` and `name` : the version number
#' * `date` : the release date
#' * `responsable` : name of the person in charge of the version
#' * `current` : is this version the current one
#'
#' @export
#'
#' @importFrom dplyr case_when select
#' @importFrom httr GET http_status content
#' @importFrom jsonlite fromJSON
get_taxref_versions <- function(all = FALSE, id = NULL, current = TRUE) {

  if (!current && !all && is.null(id))
    stop("id must be provided if current and all are both FALSE")

  url_path <- dplyr::case_when(
    all ~ "taxrefVersions",
    !is.null(id) ~ paste0("taxrefVersions/", id),
    current ~ "taxrefVersions/current"
  )

  response <- file.path(base_url, path = url_path) %>%
    httr::GET()

  if (httr::http_status(response)$category != "Success")
    stop("La requête a échoué avec le message : ", httr::http_status(response)$message)

  content <- response %>%
    httr::content("text") %>%
    jsonlite::fromJSON()

  if ("_embedded" %in% names(content)) {
    content$`_embedded`$taxrefVersions %>%
      dplyr::select(-href)
  } else {
    content %>%
      as.data.frame() %>%
      dplyr::select(-href)
  }
}

#' Get TAXREF taxonomic ranks information
#'
#' @return a data frame
#' @export
#'
#' @importFrom dplyr select
#' @importFrom httr GET http_status content
#' @importFrom jsonlite fromJSON
get_taxonomicRanks <- function() {
  response <- file.path(base_url, "taxonomicRanks") %>%
    httr::GET()

  if (httr::http_status(response)$category != "Success")
    stop("La requête a échoué avec le message : ", httr::http_status(response)$message)

  content <- response %>%
    httr::content("text") %>%
    jsonlite::fromJSON()

  content$`_embedded`$taxonomicRanks %>%
    dplyr::select(-`_links`)
}
