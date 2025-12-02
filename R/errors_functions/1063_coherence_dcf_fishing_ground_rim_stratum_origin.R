#' Detect coherence between DCF Fishing Ground, Rim Stratum and Origin
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1063
coherence_dcf_fishing_ground_rim_stratum_origin <- function(catches) {
  metier_caladero_dcf_clean <- metier_caladero_dcf[, c(
    "ESTRATO_RIM",
    "CALADERO_DCF",
    "COD_ORIGEN"
  )]
  metier_caladero_dcf_clean$VALID <- TRUE

  catches_clean <- catches[, c(BASE_FIELDS, "COD_ORIGEN", "CALADERO_DCF")]
  catches_clean <- unique(catches_clean)
  # catches_clean$CALADERO_DCF <- as.character(catches_clean$CALADERO_DCF)
  catches_clean <- catches_clean[
    !(is.na(catches_clean$COD_ORIGEN) | catches_clean$COD_ORIGEN == ""),
  ]
  catches_clean <- catches_clean[
    !(is.na(catches_clean$CALADERO_DCF) | catches_clean$CALADERO_DCF == ""),
  ]

  err <- merge(
    catches_clean,
    metier_caladero_dcf_clean,
    by = c("ESTRATO_RIM", "CALADERO_DCF", "COD_ORIGEN"),
    all.x = T
  )

  err <- err[which(err$VALID != TRUE | is.na(err$VALID)), ]

  if (nrow(err) > 0) {
    err <- add_type_of_error(
      err,
      "El Caladero DCF no concuerda con el estrato RIM y el origen"
    )
    return(err)
  }
  
  return(NULL)
}
