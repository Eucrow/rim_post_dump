#' Detect samples without lengths but with weight sampled
#' @param catches_in_lengths Data frame with catches and lengths data from sapmuebase::importRIMCatchesInLengths()
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1039
weight_sampled_without_lengths_sampled <- function(catches_in_lengths) {
  selected_fields <- c(
    BASE_FIELDS,
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_CATEGORIA",
    "CATEGORIA",
    "P_DESEM",
    "P_VIVO",
    "COD_ESP_CAT",
    "ESP_CAT",
    "P_MUE_DESEM",
    "EJEM_MEDIDOS"
  )
  err <- catches_in_lengths %>%
    filter(P_MUE_DESEM != 0 & (EJEM_MEDIDOS == 0 | is.na(EJEM_MEDIDOS)))
  
  if (nrow(err) > 0) {
    err %>%
    select(any_of(selected_fields)) %>%
    add_type_of_error(
      "ERROR: especie sin tallas muestreadas pero con peso muestra"
    )
    return(err)
  }
  
  return(NULL)
}
