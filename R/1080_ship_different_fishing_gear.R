#' Function to check the coherence of the ESTRATO_RIM between our ships' masterdata
#' and the working data
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @param shipFishingGearMaster master dataframe with the code ships and its
#' ESTRATO_RIM from 2020 to 2022
#' @return Data frame that mark those ships where there is not coincidence if found, NULL otherwise
#' @note Check code: 1080 NOT IMPLEMENTED --> FALTA CREAR EL MAESTRO
ship_different_fishing_gear <- function(catches, shipFishingGearMaster) {
  #Step 1: add the "Testigo" column to shipFishingGearMaster and convert "COD_BARCO" and "ESTRATO_RIM" as parameter

  shipFishingGearMaster$TESTIGO <- "T"

  shipFishingGearMaster$COD_BARCO <- as.character(
    shipFishingGearMaster$COD_BARCO
  )

  shipFishingGearMaster$ESTRATO_RIM <- as.character(
    shipFishingGearMaster$ESTRATO_RIM
  )

  #Step 2: filter the necessary columns from the catches df and pass from level to normal data

  catches <- catches[, BASE_FIELDS]

  catches$COD_BARCO <- as.character(catches$COD_BARCO)

  catches$ESTRATO_RIM <- as.character(catches$ESTRATO_RIM)

  #Step 3: filter the ships in the catches' dataframe that are present in shipFishingGearMaster

  masterShip <- unique(shipFishingGearMaster$COD_BARCO)

  catchesMasterShip <- catches[catches$COD_BARCO %in% masterShip, ]

  #Step 4: merging the shipFishingGearMaster frame with the catches' one

  catchesMasterShip <- merge(
    catchesMasterShip,
    shipFishingGearMaster,
    all.x = TRUE
  )

  #Step 5: develop new dataframes, one to detect where the TESTIGO values are "Na"

  catchesNaValues <- unique(catchesMasterShip[
    is.na(catchesMasterShip$TESTIGO),
  ])

  #Step 6: now, we make a subset of the masterShip with the ship's codes of the catchesNaValues.
  #Then, we create a special dataframe where we have unite all ESTRATOS that has one ship
  #using the function «concatenaThor».

  shipFishingGearMasterNaValues <- shipFishingGearMaster[
    shipFishingGearMaster$COD_BARCO %in% catchesNaValues$COD_BARCO,
    c("COD_BARCO", "ESTRATO_RIM")
  ]

  fusionEstratos <- tapply(
    shipFishingGearMasterNaValues$ESTRATO_RIM,
    shipFishingGearMasterNaValues$COD_BARCO,
    paste,
    collapse = ", "
  )

  shipCodes <- unique(shipFishingGearMasterNaValues$COD_BARCO)

  fusionEstratosDataFrame <- data.frame(
    COD_BARCO = shipCodes,
    ESTRATOS = fusionEstratos
  )

  #Step 7: here we merge the fusionEstratosDataFrame with the catchesNaValues

  catchesNaValues <- merge(
    catchesNaValues,
    fusionEstratosDataFrame,
    all.x = TRUE
  )

  #Step 8: check that the number of road is not equal to zero and add the type or error for obtained dataframes

  if (nrow(catchesNaValues) != 0) {
    message <- paste0(
      "WARNING: El barco trabajó en 2020-2022 en otros estratos diferentes: ",
      catchesNaValues$ESTRATOS
    )
    catchesNaValues$TIPO_ERROR <- message
    
    #Note: making a little fix in the columns of the catchesNaValues

    catchesNaValues <- catchesNaValues[, c(BASE_FIELDS, "TIPO_ERROR")]

    catchesNaValues <- unique(catchesNaValues)

    return(catchesNaValues)
  }
  
  return(NULL)
}
