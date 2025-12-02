#' Detect samples with SOP = 0
#' @param catches_in_lengths Data frame with catches and lengths data from sapmuebase::importRIMCatchesInLengths()
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1042
sop_zero <- function(catches_in_lengths) {
  fields_to_select <- c(
    BASE_FIELDS,
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_CATEGORIA",
    "CATEGORIA",
    "COD_ESP_CAT",
    "ESP_CAT",
    "P_MUE_DESEM",
    "SOP"
  )
  errors <- catches_in_lengths %>%
    select(any_of(fields_to_select)) %>%
    filter(SOP == 0)
  
  if (nrow(errors) > 0) {
    errors <- add_type_of_error(errors, "ERROR: SOP igual a 0")
    return(errors)
  }
  
  return(NULL)
}
