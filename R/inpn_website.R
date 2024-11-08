#' Get information about taxa habitat from the INPN website
#'
#' @param cd_nom TAXREF taxon identifier
#'
#' @return a data frame
#' @export
#'
#' @importFrom dplyr select mutate
#' @importFrom httr GET
#' @importFrom rvest read_html html_node html_table
get_taxa_habitats <- function(cd_nom) {
  cd_ref <- get_taxa(cd_nom)$referenceId

  habitat_url <- paste0("https://inpn.mnhn.fr/espece/cd_nom/", cd_ref, "/tab/habitats")

  table_habitat <- habitat_url %>%
    httr::GET() %>%
    rvest::read_html() %>%
    rvest::html_node("table")

  if (class(table_habitat) == "xml_node") {
    table_habitat %>%
      rvest::html_table(fill = TRUE) %>%
      dplyr::select(-1) %>%
      dplyr::mutate(cd_ref = cd_ref)
  }
}

#' Title
#'
#' @param cd_nom
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom httr GET
#' @importFrom rvest html_attr html_node read_html
get_taxa_photo <- function(cd_nom) {
  cd_ref <- get_taxa(cd_nom)$referenceId

  portrait_url <- paste0("https://inpn.mnhn.fr/espece/cd_nom/", cd_ref, "/tab/fiche")

  photo_url <- portrait_url |>
    httr::GET() |>
    rvest::read_html() |>
    rvest::html_node("div > a > figure > img") |>
    rvest::html_attr("src")
}
