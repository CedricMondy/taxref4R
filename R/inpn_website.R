#' Get information about taxa habitat from the INPN website
#'
#' @param cd_nom TAXREF taxon identifier
#'
#' @return a data frame
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom httr GET
#' @importFrom rvest read_html html_node html_table
get_habitats <- function(cd_nom) {
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
