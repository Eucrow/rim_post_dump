#' Detecth multiple COD_PUERTO in the same trip: samples with same date,
#' vessel, ESTRATO_RIM, TIPO_MUE, and gear but with different port variable
#' @param catches Data frame with catches data
#' @return A data frame with erroneous samples if found, NULL otherwise
#' @note Check code: 1032
multiple_port <- function(catches) {
  # errors <- catches %>%
  #   select(any_of(c(BASE_FIELDS, "COD_ARTE", "ARTE"))) %>%
  #   unique() %>%
  #   group_by(COD_ID, COD_PUERTO, PUERTO, LOCODE, FECHA_MUE, COD_BARCO, BARCO, ESTRATO_RIM, COD_TIPO_MUE, TIPO_MUE) %>%
  #   mutate(num_puerto = n_distinct(COD_PUERTO))%>%
  #   ungroup()%>%
  #   filter(num_puerto != 1) %>%
  #   add_type_of_error("ERROR: mismo fecha/barco/estrato_rim/tipo_muestre con distinto PUERTO")
  errors <- catches %>%
    select(any_of(c(BASE_FIELDS, "COD_ARTE", "ARTE"))) %>%
    unique() %>%
    group_by(FECHA_MUE, COD_BARCO, ESTRATO_RIM, COD_TIPO_MUE) %>%
    mutate(num_puerto = n()) %>%
    filter(num_puerto != 1)

  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "ERROR: mismo fecha/barco/estrato_rim/tipo_muestreo con distinto PUERTO"
    )
    return(errors)
  }
  
  return(NULL)
}
