#' Ships registered in CFPO in removal state ("Baja Definitiva" and "Baja
#' Provisional")
#' @details Require a valid CFPO file.
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @param cfpo: valid CFPO file.
#' @return A dataframe with errors.
#' @note Check code: 1026
ships_not_registered <- function(catches, cfpo = CFPO) {
  catches <- unique(catches[, c(BASE_FIELDS, "COD_SGPM")])
  catches[, "CFR"] <- sprintf("ESP%09d", as.numeric(as.character(catches[["COD_SGPM"]])))

  errors <- merge(x = catches, y = cfpo, by.x = "CFR", by.y = "CFR", all.x = TRUE)

  errors <- errors[
    errors[["ESTADO"]] %in% c("Baja Definitiva", "Baja Provisional"),
  ]

  if (nrow(errors) > 0) {
    text_type_of_error <- paste0(
      "ERROR: barco en ",
      errors[["ESTADO"]],
      " en el CFPO."
    )
    errors <- cbind(errors, "TIPO_ERROR" = text_type_of_error)
    return(errors)
  }
  
  return(NULL)
}
