
# unique(catches$COD_TIPO_MUE)
# 
# catches %>%
#   filter(COD_TIPO_MUE == 4) %>%
#   select(ESTRATO_RIM, ORIGEN, COD_ORIGEN) %>%
#   unique()

coherenceEstratoRimGear <- function(df, specification){
  
  estratorim_arte <- get_dataset_by_specification("estratorim_arte", specification)
  
  estratorim_arte$VALID<-TRUE
  merge_estrato_rim_gear<-merge(x=df, y=estratorim_arte, by.x = c("ESTRATO_RIM", "ARTE"), by.y = c("ESTRATO_RIM", "ARTE"), all.x = TRUE)
  incoherent_data <- -which(merge_estrato_rim_gear[["VALID"]])
  incoherent_data <- merge_estrato_rim_gear[incoherent_data,c(BASE_FIELDS, "COD_ARTE.x", "ARTE")]
  incoherent_data <- unique(incoherent_data)
  incoherent_data <- addTypeOfError(incoherent_data, "ERROR: no concuerda el estrato_rim con el arte")
  return(incoherent_data)
}

coherenceEstratoRimGear(catches, "OAB")