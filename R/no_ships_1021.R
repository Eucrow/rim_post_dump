#' Detect hauls where the number of ships is zero or NA
#' @param catches catches data frame returned by the importRIMCatches()
#' or importRIMFiles() functions
#' @return Data frame with errors
#' @note Check code: 1021

no_ships <- function(catches) {
  
  catches <- catches[, c(BASE_FIELDS, "N_BARCOS")]
  
  catches <- unique(catches)
  
  errors <- catches[catches$N_BARCOS == 0 |
                      is.na(catches$N_BARCOS), ]
  
  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "ERROR 1021: Número de barcos 0 o NA."
    )
  }
  
  return(errors)
  
}
