#' Detect coherence between rim stratum and gear
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @param specification Specification parameter
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1019
coherence_rim_stratum_gear <- function(catches, specification) {
  estrato_rim_arte$VALID <- TRUE

  catches <- merge(
    x = catches,
    y = estrato_rim_arte,
    by.x = c("ESTRATO_RIM", "COD_ARTE", "ARTE"),
    by.y = c("ESTRATO_RIM", "COD_ARTE", "ARTE"),
    all.x = TRUE
  )

  errors <- catches[is.na(catches$VALID), ]

  if (nrow(errors) > 0) {
    errors <- errors[, c(BASE_FIELDS, "COD_ARTE", "ARTE")]
    errors <- unique(errors)
    errors <- add_type_of_error(
      errors,
      "ERROR: el arte no es coherente con el estrato rim."
    )

    return(errors)
  }
  
  return(NULL)
}
