#' Detect samples with P_MUE_DESEM = 0 or NA
#' @param catches_in_lengths Data frame with catches and lengths data from sapmuebase::importRIMCatchesInLengths()
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1040
pes_mue_desem_zero <- function(catches_in_lengths) {
  fields_to_select <- c(
    BASE_FIELDS,
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_CATEGORIA",
    "CATEGORIA",
    "COD_ESP_CAT",
    "ESP_CAT",
    "P_MUE_DESEM"
  )
  errors <- catches_in_lengths %>%
    filter(P_MUE_DESEM == 0 | is.na(P_MUE_DESEM)) %>%
    select(any_of(fields_to_select))

  if (nrow(errors) > 0) {
    errors <- add_type_of_error(errors, "ERROR: Peso muestreado es 0")
    return(errors)
  }
  
  return(NULL)
}
