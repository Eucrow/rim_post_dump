#' Check code: 1087
#' Detect hauls where the number of ships is incorrect. It will reflect a 
#' warning in the case where this number is greater than two
#' @param catches: catches data frame returned by the importRIMCatchesInLengths() 
#' or importRIMFiles() functions.
#' @return Data frame whit errors.

checkSeveralShips <- function(catches) {
  
  # Filter work columns
  
  catches <- catches[, c(BASE_FIELDS, "N_BARCOS")]
  
  catches <- unique(catches)
  
  # Subset warings
  
  errors <- catches[!is.na(catches$N_BARCOS) & 
                      catches$N_BARCOS > 2, ]
  
  if (nrow(errors) > 0) {
    errors <- addTypeOfError(
      errors,
      "WARNING 1087: number of vessels is greater than 2."
    )
  }
  
  return(errors)
  
}