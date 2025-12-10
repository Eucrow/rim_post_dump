#' Ships not in CFPO
#' @details Require a valid CFPO file.
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @param cfpo: valid CFPO file.
#' @return A dataframe with errors.
#' @note Check code: 1025
ships_not_in_cfpo <- function(catches, cfpo = CFPO) {
  catches <- unique(catches[, c(BASE_FIELDS, "COD_SGPM")])

  errors <- merge(catches,
                  cfpo,
                  by = "COD_SGPM",
                  all.x = TRUE)

  errors <- errors[is.na(errors[["ESTADO"]]), ]

  if (nrow(errors) > 0) {
    text_type_of_error <- paste0("ERROR: barco no incluido en el CFPO.")
    errors <- cbind(errors, "TIPO_ERROR" = text_type_of_error)
    return(errors)
  }

  return(NULL)
}
