#' Function to check the presence of the ships from our ships' masterdata on the
#' working data
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @param shipFishingGearMaster master dataframe with the code ships and its
#' ESTRATO_RIM from 2020 to 2022
#' @return Data frame with those ships no presents in our working data if found, NULL otherwise
#' @note Check code: 1079 PENDIENTE DE IMPLEMENTAR --> FALTA CREAR MAESTRO
ship_not_in_master_fishing_gear <- function(catches, shipFishingGearMaster) {
  #Step 1: filter the necessary columns from the catches df and pass from level to normal data

  catches <- catches[, BASE_FIELDS]

  catches$COD_BARCO <- as.character(catches$COD_BARCO)

  catches$ESTRATO_RIM <- as.character(catches$ESTRATO_RIM)

  #Step 2: filter the ships in the catches' dataframe that are not present in boatFishingArtMaster

  masterShip <- unique(shipFishingGearMaster$COD_BARCO)

  catchesNoMasterShip <- catches[!(catches$COD_BARCO %in% masterShip), ]

  #Step 3: check that the number of road is not equal to zero and add the type or error for obtained dataframes

  if (nrow(catchesNoMasterShip) != 0) {
    catchesNoMasterShip <- add_type_of_error(
      catchesNoMasterShip,
      "WARNING: Barco no presente en el maestro"
    )
    
    catchesNoMasterShip <- unique(catchesNoMasterShip)
    
    return(catchesNoMasterShip)
  }
  
  return(NULL)
}
