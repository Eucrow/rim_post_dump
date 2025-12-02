#' Detect samples with SOP > P_VIVO
#' @param catches_in_lengths Data frame with catches and lengths data from sapmuebase::importRIMCatchesInLengths()
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1044
sop_greater_pes_vivo <- function(catches_in_lengths) {
  fields_to_select <- c(
    BASE_FIELDS,
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_CATEGORIA",
    "CATEGORIA",
    "COD_ESP_CAT",
    "ESP_CAT",
    "SOP",
    "P_VIVO",
    "DIF_SOP_P_VIVO"
  )

  errors <- catches_in_lengths %>%
    mutate(DIF_SOP_P_VIVO = SOP - P_VIVO) %>%
    mutate(DIF_SOP_P_VIVO = round(DIF_SOP_P_VIVO, 2)) %>%
    filter(DIF_SOP_P_VIVO > 0.01) %>%
    select(any_of(fields_to_select))

  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "ERROR: SOP mayor que peso vivo desembarcado"
    )
    return(errors)
  }
  
  return(NULL)
}
