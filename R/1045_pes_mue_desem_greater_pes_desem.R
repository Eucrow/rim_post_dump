#' Detect samples with p_desem <= p_mue_desem
#' @param catches_in_lengths Data frame with catches and lengths data from sapmuebase::importRIMCatchesInLengths()
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1045
pes_mue_desem_greater_pes_desem <- function(catches_in_lengths) {
  fields_to_select <- c(
    BASE_FIELDS,
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_CATEGORIA",
    "CATEGORIA",
    "COD_ESP_CAT",
    "ESP_CAT",
    "P_DESEM",
    "P_MUE_DESEM",
    "DIF_P_MUE_DESEM_P_DESEM"
  )

  errors <- catches_in_lengths %>%
    mutate(DIF_P_MUE_DESEM_P_DESEM = P_MUE_DESEM - P_DESEM) %>%
    filter(DIF_P_MUE_DESEM_P_DESEM > 0) %>%
    select(any_of(fields_to_select)) %>%
    unique()

  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "ERROR: Peso muestreado mayor al peso desembarcado"
    )
    return(errors)
  }
  
  return(NULL)
}
