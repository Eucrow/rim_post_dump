#' Detect samples with weight sampled = 0 with lengths
#' @param catches_in_lengths Data frame with catches and lengths data from sapmuebase::importRIMCatchesInLengths()
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1037
weight_sampled_zero_with_lengths_sampled <- function(catches_in_lengths) {
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
    "P_MUE_VIVO",
    "SOP"
  )
  err <- catches_in_lengths %>%
    filter(P_MUE_DESEM == 0) %>%
    select(any_of(selected_fields))
  
  if (nrow(err) > 0) {
    err <- add_type_of_error(err, "ERROR: peso muestra 0 con tallas muestreadas")
    return(err)
  }
  
  return(NULL)
}
