#' Function to check whether species were measured at the mid-centimeter instead
#' of the centimeter
#' @details Used in all species except Sardina pilchardus, Engraulis
#' encrasicolus and crustaceans
#' @param lengths Lengths data frame returned by the importRIMLengths() or
#' importRIMFiles() functions
#' @return A data frame with species measured at the middle centimeter instead of
#' the centimeter
#' @note Check code: 1084

incorrect_cm_measures <- function(lengths) {
  lengths <- prepare_lengths_for_check_measures(lengths)

  #Extract all the crustacean codes present
  crustacean <- unique(lengths$COD_ESP_MUE[grepl("^2", lengths$COD_ESP_MUE)])
  crustacean <- as.character(crustacean)

  #Species to check:
  middle_species <- c("10152", "10156", crustacean)

  #Check if the species where measured in the wrong way (middle centimeter)
  errors <- lengths[
    !(lengths$COD_ESP_MUE %in% middle_species) &
      lengths$TALLAS_MED != 0,
  ]

  if (nrow(errors) > 0) {
  # Only return a row by species instead of a row by length.
    errors <- errors[, colnames(errors) != "TALLA"]
    errors <- unique(errors)

    errors <- add_type_of_error(
      errors,
      "ERROR 1084: Especie medida erróneamente al 1/2 cm"
    )
  }

  return(errors)
}
