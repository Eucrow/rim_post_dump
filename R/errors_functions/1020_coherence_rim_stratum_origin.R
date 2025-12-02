#' Detect coherence between ESTRATO_RIM and origin
#' @param catches Data frame with catches data
#' @param specification Specification parameter
#' @return A data frame with wrong coherence
#' @note Check code: 1020
coherence_rim_stratum_origin <- function(catches, specification) {
  estrato_rim_origen$VALID <- TRUE

  df <- merge(
    x = catches,
    y = estrato_rim_origen,
    by.x = c("ESTRATO_RIM", "COD_ORIGEN"),
    by.y = c("ESTRATO_RIM", "COD_ORIGEN"),
    all.x = TRUE
  )

  errors <- df[is.na(df$VALID), ]

  if (nrow(errors) > 0) {
    errors <- errors[, c(BASE_FIELDS, "COD_ORIGEN")]
    errors <- unique(errors)
    errors <- add_type_of_error(
      errors,
      "ERROR: no concuerda el estrato_rim con el origen."
    )
    return(errors)
  }
  
  return(NULL)
}
