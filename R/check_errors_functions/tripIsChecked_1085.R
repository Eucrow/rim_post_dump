#' Check code: 1085
#' Detect unvalidated samples (column "CHEQUEADO" from catches dataframe
#' with FALSE or NA values).
#' @param catches: catches data frame returned by the importRIMCatchesInLengths()
#' or importRIMFiles() functions.
#' @return Data frame whit errors.

tripIsChecked <- function(catches) {
  BASE_FIELDS <- c(BASE_FIELDS, "CHEQUEADO")
  
  catches <- catches[
    is.na(catches$CHEQUEADO) | catches$CHEQUEADO == FALSE,
    BASE_FIELDS
  ]
  
  if (nrow(catches) > 0) {
    catches <- addTypeOfError(
      catches,
      "WARNING 1085: trip is not checked"
    )
    return(catches)
  }
}
