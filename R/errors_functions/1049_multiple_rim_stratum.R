#' Detect samples with same date, vessel, gear, and port but with different
#' ESTRATO_RIM variable
#' @param catches Data frame with catches data
#' @return A data frame with erroneous samples if found, NULL otherwise
#' @note Check code: 1049
multiple_rim_stratum <- function(catches) {
  errors <- catches %>%
    select(any_of(BASE_FIELDS)) %>%
    unique() %>%
    group_by(
      COD_PUERTO,
      PUERTO,
      LOCODE,
      FECHA_MUE,
      COD_BARCO,
      BARCO,
      COD_TIPO_MUE,
      TIPO_MUE
    ) %>%
    mutate(num_estrato_rim = n_distinct(ESTRATO_RIM)) %>%
    ungroup() %>%
    # summarise(num_estrato_rim = n_distinct(ESTRATO_RIM)) %>%
    filter(num_estrato_rim != 1)

  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "ERROR: mismo puerto/fecha/barco/tipo_muestreo con distinto ESTRATO_RIM"
    )
    return(errors)
  }
  
  return(NULL)
}
