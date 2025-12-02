#' Detect samples with not allowed species of the category
#' @details This function use the not allowed species dataframe of SAPMUEBASE
#' @param catches_in_lengths Data frame with catches and lengths data from sapmuebase::importRIMCatchesInLengths()
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1030
not_allowed_category_species <- function(catches_in_lengths) {
  selected_fields <- c(
    BASE_FIELDS,
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_CATEGORIA",
    "CATEGORIA",
    "COD_ESP_CAT",
    "ESP_CAT"
  )

  # create a dataframe with species not allowed
  not_allowed <- merge(
    x = catches_in_lengths,
    y = NOT_ALLOWED_SPECIES,
    by.x = "COD_ESP_CAT",
    by.y = "COD_ESP"
  ) %>%
    select(any_of(selected_fields))

  # remove duplicates
  errors <- unique(not_allowed)

  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "ERROR: muestreos con especies no permitidos en Especies de la Categor?a"
    )
    return(errors)
  }
  
  return(NULL)
}
