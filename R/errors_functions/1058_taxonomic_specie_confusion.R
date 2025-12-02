#' Detect species which can have problems with taxonomic confusion
#' in both, catches and catches_in_lengths datasets.
#' The species which can have taxonomic confusion are stored
#' in the data set ESP_TAXONOMIC_CONFUSION.
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @param catches_in_lengths Data frame with catches and lengths data from sapmuebase::importRIMCatchesInLengths()
#' @return A data frame with warnings if found, NULL otherwise
#' @note Check code: 1058
taxonomic_specie_confusion <- function(catches, catches_in_lengths) {
  err_catches <- catches %>%
    select(any_of(c(BASE_FIELDS, "COD_ORIGEN", "COD_ESP_MUE", "ESP_MUE"))) %>%
    unique %>%
    merge(,
      y = ESP_TAXONOMIC_CONFUSION,
      by.x = (c("COD_ESP_MUE", "COD_ORIGEN")),
      by.y = (c("COD_ESP_WARNING", "COD_ORIGEN"))
    ) %>%
    add_type_of_error(
      "WARNING: ¿seguro que la especie del muestreo no es la propuesta en ESP_PROPUESTA?"
    )

  err_catches_in_lengths <- catches_in_lengths %>%
    select(any_of(c(BASE_FIELDS, "COD_ORIGEN", "COD_ESP_CAT", "ESP_CAT"))) %>%
    unique %>%
    merge(,
      y = ESP_TAXONOMIC_CONFUSION,
      by.x = (c("COD_ESP_CAT", "COD_ORIGEN")),
      by.y = (c("COD_ESP_WARNING", "COD_ORIGEN"))
    ) %>%
    add_type_of_error(
      "WARNING: ¿seguro que la especie de la categoría no es la propuesta en ESP_PROPUESTA?"
    )

  err <- merge(err_catches, err_catches_in_lengths, all = TRUE) %>%
    select(-c(ESP_WARNING))

  colnames(err)[colnames(err) == "COMENTARIOS"] <- "COMENTARIO ESPECIE"

  if (nrow(err) > 0) {
    return(err)
  }
  
  return(NULL)
}
