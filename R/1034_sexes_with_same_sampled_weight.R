#' Detect sexes with exactly the same sampled weight
#' @param catches_in_lengths Data frame with catches and lengths data from sapmuebase::importRIMCatchesInLengths()
#' @return A data frame with warnings if found, NULL otherwise
#' @note Check code: 1034
sexes_with_same_sampled_weight <- function(catches_in_lengths) {
  selected_fields <- c(
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
    group_by_at(selected_fields) %>%
    summarise(NUM_SEXOS_MISMO_P_MUE_DESEM = n()) %>%
    filter(NUM_SEXOS_MISMO_P_MUE_DESEM > 1)
  
  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "WARNING: Sexos de una misma especie tienen exactamente el mismo peso muestra"
    )
    return(errors)
  }
  
  return(NULL)
}
