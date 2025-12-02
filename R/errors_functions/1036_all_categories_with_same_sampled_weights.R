#' Detect samples with categories which all of this species has the same sampled weight
#' @param catches_in_lengths Data frame with catches and lengths data from sapmuebase::importRIMCatchesInLengths()
#' @return A data frame with warnings if found, NULL otherwise
#' @note Check code: 1036
all_categories_with_same_sampled_weights <- function(catches_in_lengths) {
  selected_fields <- c(
    BASE_FIELDS,
    "N_CATEGORIAS",
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_CATEGORIA",
    "CATEGORIA",
    "P_MUE_DESEM",
    "SEXO"
  )

  errors <- catches_in_lengths %>%
    group_by_at(selected_fields) %>%
    summarise(NUM_ESP_CAT_MISMO_PESO_MUE = n()) %>%
    filter(NUM_ESP_CAT_MISMO_PESO_MUE > 1)
  
  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "WARNING: varias especies de la categorías con igual peso muestreado"
    )
    return(errors)
  }
  
  return(NULL)
}
