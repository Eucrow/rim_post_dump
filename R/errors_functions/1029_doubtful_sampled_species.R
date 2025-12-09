#' Detect doubtful species in Sampling Species
#' @param catches Data frame with catches data
#' @return A data frame with warnings if found, NULL otherwise
#' @note Check code: 1029
doubtful_sampled_species <- function(catches) {
  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE")

  # create a dataframe with other species not allowed
  # by sufixex
  to_check_genus <- grep(
    "(.+(formes$))|(.+(spp$))|(.+(sp$))|(.+(dae$))",
    catches$ESP_MUE
  )
  # . = any single character
  # + = one of more of previous
  # | = or

  genus_not_allowed <- catches[to_check_genus, ]

  # remove the mixed species (allowed)
  errors <- genus_not_allowed[
    !(genus_not_allowed[["COD_ESP_MUE"]] %in%
      unique(mixed_species[["COD_ESP_MUE"]])),
  ] %>%
    select(any_of(selected_fields))

  # this is obsolete: when was allowed Loligo spp an Allotheuthis spp saved in
  # 'especies de la categoría':
  # remove other allowed species
  # genus_not_allowed <- genus_not_allowed[!(genus_not_allowed[["COD_ESP_MUE"]] %in% ALLOWED_GENUS[["COD_ESP"]]),] %>%
  #  select(any_of(selected_fields))


  if (nrow(errors) > 0) {
    errors <- unique(errors) %>%
      add_type_of_error(
        "WARNING: ¿seguro que es esa especie en Especies del Muestreo?"
      )
    return(errors)
  }

  return(NULL)
}
