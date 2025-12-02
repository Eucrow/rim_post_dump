#' Detect ships with "COD_SGPM" value empty, "0" or NA
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @return Data frame with errors if found, NULL otherwise
#' @note Check code: 1081
ship_without_cod_sgpm <- function(catches) {
  catches <- catches[
    catches$COD_SGPM == "0" | catches$COD_SGPM == "" | is.na(catches$COD_SGPM),
    c(BASE_FIELDS, "COD_SGPM")
  ]

  if (nrow(catches) != 0) {
    catches <- unique(catches)
    catches <- add_type_of_error(
      catches,
      "ERROR: El código de la SGPM para este barco está vacio."
    )
    return(catches)
  }
  
  return(NULL)
}
