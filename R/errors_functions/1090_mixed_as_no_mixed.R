#' Detect mixed species keyed as non mixed species
#' @details In COD_ESP_MUE there are codes from non mixed species
#' @param catches Data frame with catches data
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1090
mixed_as_no_mixed <- function(catches) {
  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE")
  non_mixed <- merge(
    x = catches,
    y = especies_mezcla["COD_ESP_CAT"],
    by.x = "COD_ESP_MUE",
    by.y = "COD_ESP_CAT"
  )
  non_mixed <- non_mixed[, c(selected_fields)]
  
  if (nrow(non_mixed) > 0) {
    non_mixed <- add_type_of_error(
      non_mixed,
      "ERROR: especie de mezcla tecleada sin agrupar en Especies del Muestreo"
    )
    return(non_mixed)
  }
  
  return(NULL)
}
