#' Detect unvalidated samples
#' @details Column "CHEQUEADO" from catches dataframe with FALSE or NA values
#' @param catches catches data frame returned by the importRIMCatchesInLengths()
#' or importRIMFiles() functions
#' @return Data frame with errors
#' @note Check code: 1085

trip_is_checked <- function(catches) {
  
  catches <- catches[
    is.na(catches$CHEQUEADO) | catches$CHEQUEADO == FALSE,
    c(BASE_FIELDS, "CHEQUEADO")
  ]
  
  if (nrow(catches) > 0) {
    catches <- add_type_of_error(
      catches,
      "WARNING 1085: trip is not checked"
    )
    return(catches)
  }
}
