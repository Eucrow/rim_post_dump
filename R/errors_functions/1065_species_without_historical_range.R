#' Detect species without historical range by fishing ground
#' @param lengths Data frame with lengths data from sapmuebase::importRIMLengths()
#' @return A data frame with warnings if found, NULL otherwise
#' @note Check code: 1065
species_without_historical_range <- function(lengths) {
  warningsIsRanged <- lengths %>%
    select(any_of(BASE_FIELDS), COD_ESP_CAT, COD_CATEGORIA) %>%
    merge(
      y = rango_tallas_historico_caladero,
      by.x = c("COD_ESP_CAT"),
      by.y = c("COD_ESP"),
      all.x = T
    ) %>%
    filter(is.na(TALLA_MIN) | is.na((TALLA_MAX))) %>%
    unique()
  
  if (nrow(warningsIsRanged) > 0) {
    warningsIsRanged <- warningsIsRanged %>%
      add_type_of_error(
        "WARNING: esta especie no se encuentra en el maestro histórico por caladero de tallas mínimas y máximas."
      ) %>%
      humanizeVariable("COD_ESP_CAT") %>%
      select(-c(TALLA_MIN, TALLA_MAX))
    return(warningsIsRanged)
  }
  
  return(NULL)
}
