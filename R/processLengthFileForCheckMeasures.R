#' Function to process length's RIM file for the functions checkMiddleMeasures() and
#' checkMeasures()
#' @param lengths: lengths data frame returned by the importRIMLengths() or
#' importRIMFiles() functions.
#' @return a processed dataframe to work with it in the mentioned functions 
#' at the description.

processLengthFileForCheckMeasures <- function(lengths){
  
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
  
  return(lenghts)
  
}