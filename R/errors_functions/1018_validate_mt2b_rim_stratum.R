#' Detect if all mt2b has CERCO_GC or BACA_GC strata
#' @details Only useful in COD_TIPO_MUE = 4
#' @param catches Data frame with catches data
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1018
validate_mt2b_rim_stratum <- function(catches) {
  # select all the mt2b samples

  mt2b <- catches[catches[["COD_TIP_MUE"]] == 4, c(BASE_FIELDS)]

  err_stratum <- mt2b[!(mt2b[["ESTRATO_RIM"]] %in% c("CERCO_GC", "BACA_GC")), ]

  err_stratum <- unique(err_stratum)

  if (nrow(err_stratum) > 0) {
    err_stratum <- add_type_of_error(
      err_stratum,
      "ERROR: MT2B con estrato rim distinto a CERCO_GC y BACA_GC"
    )
    return(err_stratum)
  }
  
  return(NULL)
}
