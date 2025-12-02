#' Detect if there are samples with the same name vessel but with different
#' SIRENO codification or SGPM codification
#' @param catches Data frame with catches data
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1061
multiple_ship_code <- function(catches) {
  # find the ship names with more than one COD_BARCO and COD_SGPM
  dsn <- catches %>%
    select(BARCO, COD_BARCO, COD_SGPM) %>%
    unique() %>%
    count(BARCO) %>%
    filter(n > 1)

  dsn <- as.character(dsn$BARCO)

  # get the samples with the multiple ships names
  err <- catches[catches$BARCO %in% dsn, ]

  err <- err[, BASE_FIELDS]
  err <- unique(err)
  
  if (nrow(err) > 0) {
    err <- add_type_of_error(
      err,
      "WARNING: Hay varios muestreos que tienen este mismo nombre de barco, pero con distinto código SIRENO o distinto Código Secretaría. ¿Seguro que es el barco correcto?"
    )
    return(err)
  }
  
  return(NULL)
}
