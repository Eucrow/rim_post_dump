#' Detect categories with equal species weight landings
#' @param catches Data frame with catches data
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1041
species_with_categories_with_same_weight_landing <- function(catches) {
  catches <- catches[, c(
    BASE_FIELDS,
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_CATEGORIA",
    "P_DESEM"
  )]
  fields_to_count <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "P_DESEM")
  errors <- catches %>%
    unique() %>%
    group_by_at(c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "P_DESEM")) %>%
    summarise(n = n()) %>%
    filter(n > 1)
  
  if (nrow(errors) > 0) {
    colnames(errors)[names(errors) == "n"] <- "NUM_OCU_CAT_MISMO_PESO_DESEM"
    errors <- add_type_of_error(
      errors,
      "WARNING: varias categorías con igual peso desembarcado"
    )
    return(errors)
  }
  
  return(NULL)
}
