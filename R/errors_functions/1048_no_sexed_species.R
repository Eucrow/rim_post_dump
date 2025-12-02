#' Detect samples with sexed species that must not be sexed
#' @details No sexed species must have the variable SEXO as U
#' @param lengths Data frame with lengths data from sapmuebase::importRIMLengths()
#' @return A data frame with erroneous samples if found, NULL otherwise
#' @note Check code: 1048
no_sexed_species <- function(lengths) {
  # Subset especies_sexadas dataframe only with required?? variables:
  sexed_species <- especies_sexadas[, c("COD_ESP", "COD_PUERTO")]

  # Subset sexed especies sampled
  sexed_species_sampled <- lengths %>%
    select(any_of(c(
      BASE_FIELDS,
      "COD_ESP_MUE",
      "ESP_MUE",
      "COD_CATEGORIA",
      "CATEGORIA",
      "COD_ESP_CAT",
      "ESP_CAT",
      "SEXO"
    ))) %>%
    filter(SEXO == "M" | SEXO == "H")

  # Check if all the species sampled must be sexed:
  errors_species_must_not_be_sexed <- sexed_species_sampled %>%
    filter(!(COD_ESP_CAT %in% sexed_species$COD_ESP)) %>%
    unique()

  # Check if all the species sampled must be sexed in some ports:
  sexed_species_by_port <- sexed_species[
    sexed_species[["COD_PUERTO"]] != "ALL",
  ]

  errors_species_must_be_sexed_only_in_some_ports <- sexed_species_sampled %>%
    filter(COD_ESP_CAT %in% sexed_species_by_port[["COD_ESP"]]) %>% # filter only species wich must be sampled in some ports
    merge(,
      y = sexed_species_by_port,
      by.x = c("COD_ESP_CAT", "COD_PUERTO"),
      by.y = c("COD_ESP", "COD_PUERTO"),
      all.x = T
    ) %>%
    unique()

  # merge errors
  errors <- rbind(
    errors_species_must_not_be_sexed,
    errors_species_must_be_sexed_only_in_some_ports
  )
  
  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "WARNING: especie que NO debería ser sexada. Es posible que el SOP de estos muestreos sea 0, por lo que se ha de comprobar."
    )
    return(errors)
  }
  
  return(NULL)
}
