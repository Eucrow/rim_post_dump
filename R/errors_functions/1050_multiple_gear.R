#' Detect multiple gear in the same trip: samples with same date, vessel,
#' ESTRATO_RIM, TIPO_MUE, and port but with different gear variable
#' @param catches Data frame with catches data
#' @return A data frame with erroneous samples if found, NULL otherwise
#' @note Check code: 1050
multiple_gear <- function(catches) {
  errors <- catches %>%
    select(any_of(c(BASE_FIELDS, "COD_ARTE", "ARTE"))) %>%
    unique() %>%
    group_by(
      COD_PUERTO,
      PUERTO,
      LOCODE,
      FECHA_MUE,
      COD_BARCO,
      BARCO,
      ESTRATO_RIM,
      COD_TIPO_MUE,
      TIPO_MUE
    ) %>%
    mutate(num_arte = n_distinct(COD_ARTE)) %>%
    ungroup() %>%
    # summarise(num_Estrato_RIM = n_distinct(ESTRATO_RIM)) %>%
    filter(num_arte != 1)

  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "ERROR: mismo puerto/fecha/barco/estrato_rim con distinto ARTE"
    )
    return(errors)
  }
  
  return(NULL)
}
