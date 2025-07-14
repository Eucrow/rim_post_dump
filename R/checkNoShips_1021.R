#' Check code: 1021
#' Detect hauls where the number of ships is incorrect. It will reflect a 
#' error when the number of ships is zero or NA
#' @param catches: catches data frame returned by the importRIMCatchesInLengths() 
#' or importRIMFiles() functions.
#' @return Data frame whit errors.

checkNoShips <- function(catches) {
  
  # Filter work columns
  
  catches <- catches[, c(BASE_FIELDS, "N_BARCOS")]
  
  catches <- unique(catches)
  
  # Subset errors
  
  errors <- catches[catches$N_BARCOS == 0 |
                      is.na(catches$N_BARCOS), ]
  
  if (nrow(errors) > 0) {
    errors <- addTypeOfError(
      errors,
      "ERROR 1021: number of vessels equal to 0 or NA."
    )
  }
  
  return(errors)
  
}