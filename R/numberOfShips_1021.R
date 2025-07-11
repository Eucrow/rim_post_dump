#' Check code: 1021
#' Detect hauls where the number of ships is incorrect. It will reflect a 
#' error when the number of ships is zero or NA and a warning in the case
#' where is greater than two
#' @param catches: catches data frame returned by the importRIMCatchesInLengths() 
#' or importRIMFiles() functions.
#' @return Data frame whit errors.

numberOfShips <- function(catches) {
  
  # Filter work columns
  
  catches <- catches[, c(BASE_FIELDS, "N_BARCOS")]
  
  catches <- unique(catches)
  
  # Subset possible errors
  
  errors <- catches[catches$N_BARCOS == 0 |
                      is.na(catches$N_BARCOS), ]
  
  errors[, "TIPO_ERROR"] <- "ERROR: number of vessels equal to 0 or NA"
  
  # Subset possible warings
  
  warns <- catches[!is.na(catches$N_BARCOS) & 
                     catches$N_BARCOS > 2, ]
  
  warns[, "TIPO_ERROR"] <- "WARNING: number of vessels greater than 2"
  
  # Export final error dataframe
  
  errors <- rbind(errors, warns)
  
}