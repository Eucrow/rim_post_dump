
#' Check code:1076
#' Function to check if species where measured in the correct way, at middle centimeter
#' (1/2 cm) in the case of Sardina Pilchardus (10152) and Engraulis
#' encrasicolus (10156), and at the centimeter in the case of the rest of species.
#' @param lengths: lengths data frame returned by the importRIMLengths() or
#' importRIMFiles() functions.
#' @param midSpecies: default parameter that is the vector with the specie code 
#' for species which are measured at the middle centimenter: 
#' Sardina Pilchardus (10152), Engraulis encrasicolus (10156)
#' @return A data frame where the species were measured wrong.

correctMeasurement <- function(lenghts, midSpecies = c("10152", "10156")){
  
  # Work columns 
  
  columns <- c(BASE_FIELDS, "COD_ESP_MUE", "COD_ESP_CAT", "COD_CATEGORIA", "TALLA")
  
  lengths <- lengths[, columns]
  
  # Count the number of measurement registers 
  
  lengths_register <-  lengths %>% 
    group_by(COD_ID, FECHA_MUE, COD_BARCO, COD_ESP_CAT) %>%
    mutate(REGISTROS = n_distinct(TALLA))
  
  # Count the number of half centimeter measurements done
  
  lengths_middle <- lengths %>% 
    group_by(COD_ID, FECHA_MUE, COD_BARCO, COD_ESP_CAT) %>%
    filter(grepl("\\.5$", TALLA)) %>% 
    mutate(TALLAS_MED = n_distinct(TALLA)) %>%
    select(-TALLA) %>%
    unique()
  
  # Merge both dataframes 
  
  lengths <- merge(lengths_register,
                   lengths_middle,
                   all.x = TRUE)
  
  # Clean up NA values, make them zero
  
  lengths[is.na(lengths)] <- 0
  
  #' Check if Sardina Pilchardus or Engraulis encrasicolus where measured
  #' wrong (not middle centimeter)
  
  error_mid_species <- lengths[lengths$COD_ESP_MUE %in% midSpecies & 
                                 lengths$TALLAS_MED == 0 & 
                                 lengths$REGISTROS > 1, ]
  
  #' Check if the rest of the species where measured in the wrong way
  #' (middle centimeter)
  
  error_rest <- lengths[!(lengths$COD_ESP_MUE %in% midSpecies) & 
                          lengths$TALLAS_MED != 0, ]
  
  # Add Warning/Error message to the respective dataframe
  
  if(nrow(error_mid_species) > 0){
    
    error_mid_species <- addTypeOfError(error_mid_species, 
                                        "WARNING: Comprobar que se hayan medido las tallas al cm en vez del 1/2 cm")
  } else if (nrow(error_rest) > 0) {
    
    error_rest <- addTypeOfError(error_rest, 
                                 "ERROR: Especie medida erróneamente al 1/2 cm")
  } 
  
  # Merge both dataframe (in the case they exist) 
  
  error <- rbind(error_mid_species, error_rest)
  
  return(error)
  
}