#' Check code: 1076
#' Function to check Sardina Pilchardus (10152) and Engraulis encrasicolus (10156)
#' are measured at the middle centimeter (1/2 cm).
#' @param lengths Lengths data frame returned by the importRIMLengths() or
#' importRIMFiles() functions.
#' @return A data frame with warnings of Sardina pilchardus and Engraulis
#' encrasicolus measured at the centimeter.

checkHalfCmMeasures <- function(lengths) {
  lengths <- prepareLengthsForCheckMeasures(lengths)

  middle_species <- c("10152", "10156")

  errors <- lengths[lengths$COD_ESP_MUE %in% middle_species &
      lengths$TALLAS_MED == 0 &
      lengths$REGISTROS > 1,
  ]

  if (nrow(errors) > 0) {
    # Only return a row by species instead of a row by length.
    errors <- errors[, colnames(errors) != "TALLA"]
    errors <- unique(errors)
    
    errors <- addTypeOfError(
      errors,
      "WARNING 1076: Comprobar que se hayan medido las tallas al cm en vez del 1/2 cm"
    )
  }

  return(errors)
}
