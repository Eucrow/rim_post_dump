#' Function to process RIM length file for use in the functions checkMiddleMeasures()
#' and checkMeasures()
#' @param lengths Lengths data frame returned by the importRIMLengths() or
#' importRIMFiles() functions.
#' @return a data frame ready to be used in the functions described below.

prepare_lengths_for_check_measures <- function(lengths) {
  lengths <- lengths[, c(
    BASE_FIELDS,
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_CATEGORIA",
    "CATEGORIA",
    "COD_ESP_CAT",
    "ESP_CAT",
    "TALLA"
  )]

  # Count the distinct size of every species sample
  lengths_register <- lengths %>%
    group_by(COD_ID, FECHA_MUE, COD_BARCO, COD_ESP_CAT) %>%
    mutate(REGISTROS = n_distinct(TALLA))

  # Count the number of size entries ending in .5 (0.5 cm measurements)
  lengths_middle <- lengths %>%
    group_by(COD_ID, FECHA_MUE, COD_BARCO, COD_ESP_CAT) %>%
    filter(grepl("\\.5$", TALLA)) %>%
    mutate(TALLAS_MED = n_distinct(TALLA)) %>%
    select(-TALLA) %>%
    unique()

  # Merge both dataframes
  lengths <- merge(lengths_register, lengths_middle, all.x = TRUE)

  lengths[is.na(lengths)] <- 0

  return(lengths)
}
