#' Detect categories with various equal sexes (both of them male or female)
#' @param catches_in_lengths Data frame with catches and lengths data from sapmuebase::importRIMCatchesInLengths()
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1035
categories_with_repeated_sexes <- function(catches_in_lengths) {
  selected_fields <- c(
    BASE_FIELDS,
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_CATEGORIA",
    "CATEGORIA",
    "COD_ESP_CAT",
    "ESP_CAT",
    "SEXO"
  )
  errors <- catches_in_lengths %>%
    filter(SEXO != "U") %>%
    group_by_at(selected_fields) %>%
    summarise(NUM_MISMOS_SEXOS = n()) %>%
    filter(NUM_MISMOS_SEXOS != 1)
  
  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "categorías con varios sexos iguales (la misma especie con varias distribuciones de machos o hembras"
    )
    return(errors)
  }
  
  return(NULL)
}
