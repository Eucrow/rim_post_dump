#' Detect hauls where the number of ships is greater than two
#' @param catches catches data frame returned by the importRIMCatchesInLengths() 
#' or importRIMFiles() functions
#' @return Data frame with errors
#' @note Check code: 1087

more_than_two_ships <- function(catches) {
  
  catches <- catches[, c(BASE_FIELDS, "N_BARCOS")]
  
  catches <- unique(catches)
  
  errors <- catches[!is.na(catches$N_BARCOS) & 
                      catches$N_BARCOS > 2, ]
  
  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "WARNING 1087: number of vessels is greater than 2."
    )
  }
  
  return(errors)
  
}
