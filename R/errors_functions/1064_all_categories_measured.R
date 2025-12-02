#' Detect all the categories are measured. If one or more are not, return a
#' warning
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @param lengths Data frame with lengths data from sapmuebase::importRIMLengths()
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1064
all_categories_measured <- function(catches, lengths) {
  # Some categories never are measured, so must be ignored
  regex_to_ignore <- c(
    "^.*(?i)colas.*",
    "^.*(?i)melad(a|o)$",
    "^.*(?i)melad(a|o).*",
    "^.*(?i)trozos$",
    "^.*(?i)picad(a|o)",
    "^(?i)alas\ .*$",
    "^Huevas$"
  )

  regex_to_ignore <- paste(regex_to_ignore, collapse = "|")

  # Clean the categories master
  maestro_categorias_clean <- categorias[, c(
    "COD_ESP",
    "COD_PUERTO",
    "COD_CATEGORIA",
    "PROCESO"
  )]

  # Clean catches dataframe
  clean_catches <- catches[which(catches$COD_TIPO_MUE == "2"), ]

  clean_catches <- merge(
    clean_catches,
    maestro_categorias_clean,
    by.x = c("COD_ESP_MUE", "COD_PUERTO", "COD_CATEGORIA"),
    by.y = c("COD_ESP", "COD_PUERTO", "COD_CATEGORIA"),
    all.x = T
  )

  clean_catches <- clean_catches[
    -grep(regex_to_ignore, clean_catches$CATEGORIA),
  ]

  # Get the number of categories in catches dataframe
  cat_cat <- clean_catches %>%
    group_by(COD_ID, COD_ESP_MUE, ESP_MUE) %>%
    mutate(n_cat_catches = n_distinct(COD_CATEGORIA)) %>%
    select(all_of(BASE_FIELDS), COD_ESP_MUE, ESP_MUE, n_cat_catches) %>%
    unique()

  # Get the number of categories with sampled lengths
  sam_cat_len <- lengths %>%
    group_by(COD_ID, COD_ESP_MUE, ESP_MUE) %>%
    mutate(n_cat_lengths = n_distinct(COD_CATEGORIA)) %>%
    select(COD_ID, COD_ESP_MUE, ESP_MUE, n_cat_lengths) %>%
    unique()

  # Get errors

  errors <- merge(
    cat_cat,
    sam_cat_len,
    all.x = T,
    by = c("COD_ID", "COD_ESP_MUE", "ESP_MUE")
  )

  errors <- errors[
    which(
      errors[["n_cat_catches"]] > 1 &
        errors[["n_cat_lengths"]] < errors[["n_cat_catches"]]
    ),
  ]

  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "WARNING: algunas categorías de la especie están muestreadas pero otras no"
    )

    errors <- errors[, c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "TIPO_ERROR")]

    return(errors)
  }
  
  return(NULL)
}
