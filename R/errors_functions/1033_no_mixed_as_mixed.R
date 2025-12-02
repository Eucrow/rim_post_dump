#' Detect no mixed species keyed as mixed species
#' @details In COD_ESP_MUE there are codes from mixed species
#' @param lengths Data frame with lengths data from sapmuebase::importRIMLengths()
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1033
no_mixed_as_mixed <- function(lengths) {
  selected_fields <- c(
    BASE_FIELDS,
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_CATEGORIA",
    "CATEGORIA",
    "COD_ESP_CAT",
    "ESP_CAT"
  )
  non_mixed <- merge(
    x = lengths,
    y = especies_no_mezcla["COD_ESP"],
    by.x = "COD_ESP_MUE",
    by.y = "COD_ESP"
  )

  
  if (nrow(non_mixed) > 0) {
    non_mixed <- non_mixed[, c(selected_fields)]
    non_mixed <- unique(non_mixed)

    non_mixed <- add_type_of_error(
      non_mixed,
      "ERROR: especie no de mezcla agrupada en Especies del Muestreo"
    )
    return(non_mixed)
  }
  
  return(NULL)
}
