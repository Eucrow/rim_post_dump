#' Detect lengths out of the size range of species in the rango_tallas_historico_caladero dataset
#' @param lengths Data frame with lengths data from sapmuebase::importRIMLengths()
#' @return A data frame with warnings lengths if found, NULL otherwise
#' @note Check code: 1052
lengths_outside_size_range <- function(lengths) {
  lengths <- merge(
    x = lengths,
    y = sapmuebase::caladero_origen[, c("CALADERO", "COD_ORIGEN")],
    all.x = TRUE,
    by = "COD_ORIGEN"
  )

  lengths <- merge(
    x = lengths,
    y = rango_tallas_historico_caladero,
    by.x = c("COD_ESP_CAT", "CALADERO"),
    by.y = c("COD_ESP", "CALADERO"),
    all.x = T
  )

  warningsOutOfRange <- lengths %>%
    select(
      any_of(BASE_FIELDS),
      COD_ESP_CAT,
      COD_CATEGORIA,
      TALLA,
      TALLA_MIN,
      TALLA_MAX
    ) %>%
    filter(TALLA < TALLA_MIN | TALLA > TALLA_MAX)
  
  if (nrow(warningsOutOfRange) > 0) {
    # it's not possible use add_type_of_error here, I don't know why
    warningsOutOfRange <- warningsOutOfRange %>%
      mutate(
        TIPO_ERROR = paste(
          "WARNING: Talla fuera del rango histórico de tallas por caladero:",
          TALLA_MIN,
          "-",
          TALLA_MAX
        )
      ) %>%
      humanizeVariable("COD_ESP_CAT")
    return(warningsOutOfRange)
  }
  
  return(NULL)
}
