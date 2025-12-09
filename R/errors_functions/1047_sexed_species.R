#' Detect samples with no sexed species that must be sexed
#' @details Sexed species must have the variable SEXO as M (male) or H (female)
#' @param lengths Data frame with lengths data from sapmuebase::importRIMLengths()
#' @return A data frame with erroneous samples if found, NULL otherwise
#' @note Check code: 1047
sexed_species <- function(lengths) {
  errors <- lengths %>%
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
    filter((COD_ESP_MUE %in% especies_sexadas[["COD_ESP"]])) %>% # only the sexed species
    merge(
      y = especies_sexadas,
      by.x = c("COD_ESP_CAT"),
      by.y = c("COD_ESP"),
      all.x = T
    ) %>% # merge with sexed species
    mutate(COD_PUERTO.y = as.character(COD_PUERTO.y)) %>% # <--- THIS IS IMPERATIVE BECAUSE
    # THE sexed_species DATAFRAME HAS DIFFERENT LEVELS THAN THE lengths DATAFRAME.
    filter(COD_PUERTO.y == "ALL" | COD_PUERTO.x == COD_PUERTO.y) %>% # sexed species
    # must be only with port field "ALL" or a port similiar between lengths and especies_sexadas dataframe
    filter((SEXO != "M" & SEXO != "H")) %>%
    rename(
      "COD_PUERTO" = COD_PUERTO.x,
      "PUERTO" = PUERTO.x,
      "LOCODE" = LOCODE.x,
      "ESP_MUE" = ESP_MUE.x
    )

  if (nrow(errors) > 0) {
    errors <- errors %>%
      select(any_of(c(
        BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA",
        "CATEGORIA", "COD_ESP_CAT", "ESP_CAT", "SEXO"
      ))) %>%
      unique() %>%
      add_type_of_error("ERROR: especie que debería ser sexada")
    return(errors)
  }

  return(NULL)
}
