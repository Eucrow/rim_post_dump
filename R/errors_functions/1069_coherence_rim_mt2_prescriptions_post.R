#' Detect if the variables "COD_PUERTO", "COD_ARTE", "COD_ORIGEN", "ESTRATO_RIM",
#' "METIER_DCF" and "CALADERO_DCF" are coherent with MT2 rim prescriptions
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1069
coherence_rim_mt2_prescriptions_post <- function(catches) {
  catches <- catches[catches[["COD_TIPO_MUE"]] == 2, ] #THIS IS DIFFERENT IN rim_pre_dump, COD_TIPO_MUE is "MT2A"

  errors <- unique(catches[, c(
    "COD_ID",
    "FECHA_MUE",
    "COD_BARCO",
    "BARCO",
    "COD_PUERTO",
    "COD_ARTE",
    "COD_ORIGEN",
    "ESTRATO_RIM",
    "METIER_DCF",
    "CALADERO_DCF"
  )])
  errors <- merge(
    errors,
    prescripciones_rim_mt2_coherencia,
    by = c(
      "COD_PUERTO",
      "COD_ARTE",
      "COD_ORIGEN",
      "ESTRATO_RIM",
      "METIER_DCF",
      "CALADERO_DCF"
    ),
    all.x = TRUE
  )
  if (nrow(errors) > 0) {
    errors <- errors[
      is.na(errors[["PESQUERIA"]]),
      c(
        "COD_ID",
        "FECHA_MUE",
        "COD_BARCO",
        "BARCO",
        "COD_PUERTO",
        "COD_ARTE",
        "COD_ORIGEN",
        "METIER_DCF",
        "CALADERO_DCF"
      )
    ]
    
    if (nrow(errors) > 0) {
      errors <- humanize(errors)
      errors <- add_type_of_error(
        errors,
        "Esta combinación de puerto, arte, origen, estrato rim, metier DCF y caladero DCF no está en las prescripciones MT2 RIM de 2021."
      )
      return(errors)
    }
  }
  
  return(NULL)
}
