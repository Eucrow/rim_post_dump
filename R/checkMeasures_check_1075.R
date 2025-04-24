#' Check code:1075
#' Function to check if species where measured in the correct way, at centimeter.
#' @param lengths: lengths data frame returned by the importRIMLengths() or
#' importRIMFiles() functions.
#' @param midSpecies: default parameter that is the vector with the specie code 
#' for species which are measured at the middle centimenter and measured at the
#' milimeter level: Sardina Pilchardus (10152), Engraulis encrasicolus (10156),
#' Nephrops norvegicus (20914), Maja squinado (20190) and Cancer pagurus(20152)
#' @return A data frame where the species were measured wrong.

checkMeasures <- function(lengths, midSpecies = c("10152", "10156", "20194",
                                                  "20190", "20152")){
  
  lengths <- processLengthFileForCheckMeasures(lengths)
  
  #' Check if not both Sardina Pilchardus or Engraulis encrasicolus 
  #' species where measured in the wrong way (middle centimeter)
  
  error <- lengths[!(lengths$COD_ESP_MUE %in% midSpecies) & 
                     lengths$TALLAS_MED != 0, ]
  
  # Add Error message to the respective dataframe
  
  if (nrow(error) > 0) {
    
    error <- addTypeOfError(error, "ERROR: Especie medida erróneamente al 1/2 cm")
  } 
  
  return(error)
  
}