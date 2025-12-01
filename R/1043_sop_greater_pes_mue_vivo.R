#' Detect samples with SOP > P_MUE_VIVO
#' @param catches_in_lengths Data frame with catches and lengths data from sapmuebase::importRIMCatchesInLengths()
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1043
sop_greater_pes_mue_vivo <- function(catches_in_lengths) {
  selected_fields <- c(
    BASE_FIELDS,
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_CATEGORIA",
    "CATEGORIA",
    "COD_ESP_CAT",
    "ESP_CAT",
    "P_MUE_VIVO",
    "SOP"
  )
  errors <- catches_in_lengths %>%
    select(any_of(selected_fields)) %>%
    mutate(DIF_SOP_P_MUE_VIVO = SOP - P_MUE_VIVO) %>%
    mutate(DIF_SOP_P_MUE_VIVO = round(DIF_SOP_P_MUE_VIVO, 2)) %>%
    filter(DIF_SOP_P_MUE_VIVO > 0.01)
  
  if (nrow(errors) > 0) {
    errors <- add_type_of_error(errors, "ERROR: SOP mayor que peso muestreado vivo")
    return(errors)
  }
  
  return(NULL)
}
