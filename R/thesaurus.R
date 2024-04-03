
#' Get TAXREF versions information
#'
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
#' @importFrom dplyr case_when select `%>%`
#' @importFrom httr GET http_status content
#' @importFrom jsonlite fromJSON
get_taxref_versions <- function(current = TRUE) {

  url_path <- dplyr::case_when(
    current ~ "taxrefVersions/current",
    TRUE ~ "taxrefVersions"
  )

  response <- file.path(base_url, path = url_path) %>%
    httr::GET()

  if (httr::http_status(response)$category != "Success")
    stop("Request failed with the message : ", httr::http_status(response)$message)

  content <- response %>%
    httr::content("text") %>%
    jsonlite::fromJSON()

  if ("_embedded" %in% names(content)) {
    content$`_embedded`$taxrefVersions %>%
      dplyr::select(-`_links`)
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
    stop("Request failed with the message : ", httr::http_status(response)$message)

  content <- response %>%
    httr::content("text") %>%
    jsonlite::fromJSON()

  content$`_embedded`$taxonomicRanks %>%
    dplyr::select(-`_links`)
}

#' Get INPN group names
#'
#' @return a data frame with the level and name of the INPN group
#' @export
#'
#' @importFrom dplyr arrange
#' @importFrom httr GET http_status content
#' @importFrom jsonlite fromJSON
get_vernacularGroups <- function() {
  response <- file.path(base_url, "vernacularGroups") %>%
    httr::GET()

  if (httr::http_status(response)$category != "Success")
    stop("Request failed with the message : ", httr::http_status(response)$message)

  content <- response %>%
    httr::content("text") %>%
    jsonlite::fromJSON()

  content$`_embedded`$vernacularGroupList %>%
    dplyr::arrange(level, name)

}

#' Get information about external reference systems connected to TAXREF
#'
#' @return a data frame
#' @export
#'
get_externalDB <- function() {
  response <- file.path(base_url, "externalDb") %>%
    httr::GET()

  if (httr::http_status(response)$category != "Success")
    stop("Request failed with the message : ", httr::http_status(response)$message)

  content <- response %>%
    httr::content("text") %>%
    jsonlite::fromJSON()

  content$`_embedded`$externalDb %>%
    dplyr::select(-`_links`)

}

#' Get biogeographic status reference system used in TAXREF
#'
#' @return a data frame
#' @export
#'
get_biogeographicStatus <- function() {
  response <- file.path(base_url, "biogeographicStatus") %>%
    httr::GET()

  if (httr::http_status(response)$category != "Success")
    stop("Request failed with the message : ", httr::http_status(response)$message)

  content <- response %>%
    httr::content("text") %>%
    jsonlite::fromJSON()

  content$`_embedded`$biogeographicStatus %>%
    dplyr::select(-`_links`)

}
