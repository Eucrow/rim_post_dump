#' Detect grouped species in Species of the Category
#' @param catches_in_lengths Data frame with catches and lengths data from sapmuebase::importRIMCatchesInLengths()
#' @return A data frame with errors
#' @note Check code: 1027
mixed_species_in_category <- function(catches_in_lengths) {
  selected_fields <- c(
    BASE_FIELDS,
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_CATEGORIA",
    "CATEGORIA",
    "COD_ESP_CAT",
    "ESP_CAT"
  )

  not_allowed_in_category <- especies_mezcla %>%
    select(COD_ESP_MUE) %>%
    unique()

  clean_catches_in_lenghts <- catches_in_lengths %>%
    select(any_of(selected_fields))

  errors <- merge(
    x = clean_catches_in_lenghts,
    y = not_allowed_in_category,
    by.x = "COD_ESP_CAT",
    by.y = "COD_ESP_MUE"
  )

  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "ERROR: muestreo MT2 con especie de mezcla que está agrupada en Especies para la Categoría"
    )
    return(errors)
  }
  
  return(NULL)
}
