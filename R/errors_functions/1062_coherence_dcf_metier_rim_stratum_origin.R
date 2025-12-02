#' Detect coherence between DCF Metier, Rim Stratum and Origin
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1062
coherence_dcf_metier_rim_stratum_origin <- function(catches) {
  metier_caladero_dcf_clean <- metier_caladero_dcf[, c(
    "ESTRATO_RIM",
    "METIER_DCF",
    "COD_ORIGEN"
  )]
  metier_caladero_dcf_clean$VALID <- TRUE

  catches_clean <- catches[, c(BASE_FIELDS, "COD_ORIGEN", "METIER_DCF")]
  catches_clean <- unique(catches_clean)
  catches_clean <- catches_clean[
    !(is.na(catches_clean$COD_ORIGEN) | catches_clean$COD_ORIGEN == ""),
  ]
  catches_clean <- catches_clean[
    !(is.na(catches_clean$METIER_DCF) | catches_clean$METIER_DCF == ""),
  ]

  err <- merge(
    catches_clean,
    metier_caladero_dcf_clean,
    by = c("ESTRATO_RIM", "METIER_DCF", "COD_ORIGEN"),
    all.x = T
  )

  err <- err[which(err$VALID != TRUE | is.na(err$VALID)), ]

  if (nrow(err) > 0) {
    err <- add_type_of_error(
      err,
      "El Metier DCF no concuerda con el estrato RIM y el origen"
    )
    return(err)
  }
  
  return(NULL)
}
