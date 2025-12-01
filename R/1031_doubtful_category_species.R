#' Detect samples with doubtful species of the category where
#' the genus finished in -formes, -dae, - spp and - sp
#' @param catches_in_lengths Data frame with catches and lengths data from sapmuebase::importRIMCatchesInLengths()
#' @return A data frame with warnings if found, NULL otherwise
#' @note Check code: 1031
doubtful_category_species <- function(catches_in_lengths) {
  selected_fields <- c(
    BASE_FIELDS,
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_CATEGORIA",
    "CATEGORIA",
    "COD_ESP_CAT",
    "ESP_CAT"
  )

  #create a dataframe with other species not allowed
  # by sufixes
  to_check_genus <- grep(
    "(.+(formes$))|(.+(spp$))|(.+(sp$))|(.+(dae$))",
    catches_in_lengths$ESP_CAT
  )

  genus_not_allowed <- catches_in_lengths[to_check_genus, ] %>%
    select(any_of(selected_fields))
  errors <- unique(genus_not_allowed)

  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "WARNING: ¿seguro que es esa especie en Especies de la Categoría?"
    )
    return(errors)
  }
  
  return(NULL)
}
