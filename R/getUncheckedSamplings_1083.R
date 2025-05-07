#' Check code: 1083
#' Detect unvalidated samples (column "VALIDADO" from lengths dataframe
#' with FALSE or NA values).
#' @param lengths: catches data frame returned by the importRIMCatchesInLengths()
#' or importRIMFiles() functions.
#' @return Data frame whit errors.

getUncheckedSamplings <- function(lengths) {
  BASE_FIELDS <- c(BASE_FIELDS, "VALIDADO")

  lengths <- lengths[
    is.na(lengths$VALIDADO) | lengths$VALIDADO == FALSE,
    BASE_FIELDS
  ]

  if (nrow(lengths) > 0) {
    lengths <- addTypeOfError(
      lengths,
      "WARNING: sample is not checked"
    )
    return(lengths)
  }
}
