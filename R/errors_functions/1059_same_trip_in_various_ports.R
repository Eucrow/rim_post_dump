#' Search ships which landed in various ports at the same date.
#' There are a function 'checkMultiplePort' which find trips with FECHA_MUE,
#' COD_BARCO, ESTRATO_RIM, COD_TIPO_MUE. But this checkSameTripInVariousPorts
#' ignore ESTRATO_RIM and COD_TIPO_MUE variables.
#' @param catches Data frame with catches data
#' @return A data frame with warnings if found, NULL otherwise
#' @note Check code: 1059
same_trip_in_various_ports <- function(catches) {
  err <- catches %>%
    select(COD_PUERTO, FECHA_MUE, COD_BARCO) %>%
    unique() %>%
    group_by(FECHA_MUE, COD_BARCO) %>%
    mutate(n_ports_per_trip = n_distinct(COD_PUERTO)) %>%
    filter(n_ports_per_trip > 1) %>%
    sapmuebase::humanize()

  if (nrow(err) > 0) {
    err <- add_type_of_error(
      err,
      "ERROR: Un mismo barco ha descargado en varios puertos en la misma fecha. Es posible que el puerto pertenezca a un área de influencia distinta."
    )
    return(err)
  }
  
  return(NULL)
}
