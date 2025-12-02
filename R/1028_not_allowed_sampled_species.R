#' Detect if there are not allowed species in Sampling Species
#' @param catches Data frame with catches data
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1028
not_allowed_sampled_species <- function(catches) {
  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE")

  # create a dataframe with species not allowed
  sampling_species_not_allowed <- merge(
    x = catches,
    y = NOT_ALLOWED_SPECIES,
    by.x = "COD_ESP_MUE",
    by.y = "COD_ESP"
  ) %>%
    select(any_of(selected_fields))

  # remove duplicates
  errors <- unique(sampling_species_not_allowed)

  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "ERROR: muestreo con especie no permitida en Especies del Muestreo"
    )
    return(errors)
  }
  
  return(NULL)
}
