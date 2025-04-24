
#' Check code:1076
#' Function to check if species where measured in the correct way, at middle centimeter
#' (1/2 cm) in the case of Sardina Pilchardus (10152) and Engraulis
#' encrasicolus (10156).
#' @param lengths: lengths data frame returned by the importRIMLengths() or
#' importRIMFiles() functions.
#' @param midSpecies: default parameter that is the vector with the specie code 
#' for species which are measured at the middle centimenter: 
#' Sardina Pilchardus (10152), Engraulis encrasicolus (10156)
#' @return A data frame where the species were measured wrong.

checkMiddleMeasures <- function(lengths, midSpecies = c("10152", "10156")){
  
  lengths <- processLengthFileForCheckMeasures(lengths)
  
  #' Check if both Sardina Pilchardus or Engraulis encrasicolus where measured
  #' wrong (not middle centimeter)
  
  error <- lengths[lengths$COD_ESP_MUE %in% midSpecies & 
                     lengths$TALLAS_MED == 0 & 
                     lengths$REGISTROS > 1, ]
  
  # Add Warning/Error message to the respective dataframe
  
  if(nrow(error) > 0){
    
    error <- addTypeOfError(error, 
                            "WARNING: Comprobar que se hayan medido las tallas al cm en vez del 1/2 cm")
  } 
  
  return(error)
  
}
