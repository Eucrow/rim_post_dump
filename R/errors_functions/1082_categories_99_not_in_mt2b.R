#' Detect categories with code ended in '99' and its sample type is different of
#' "MT2B(Muestreo a bordo)" or "MT3 (Muestreo dirigido)"
#' @details Categories ended in '99' can only be assigned to one of those sample types
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @return Data frame with errors if found, NULL otherwise
#' @note Check code: 1082
categories_99_not_in_mt2b <- function(catches) {
  catches <- catches[
    !(catches$COD_TIPO_MUE %in% c("4", "6")) &
      grepl("99$", catches$COD_CATEGORIA),
    BASE_FIELDS
  ]

  if (nrow(catches) != 0) {
    catches <- add_type_of_error(
      catches,
      "ERROR: el código COD_CATEGORIA acaba en 99 pero el tipo de muestreo NO es MT2B(Muestreo a bordo). Las categorías que acaban en 99 están reservadas sólo para los tipo de muestreo MT2B(Muestreo a bordo) o MT3 (Muestreo dirigido)"
    )

    return(catches)
  }
  
  return(NULL)
}
