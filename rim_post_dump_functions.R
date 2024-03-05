# Function to check the coherence between 'ESTRATO_RIM' and 'gear'
coherenceEstratoRimGear <- function(df, specification){

  estrato_rim_arte$VALID<-TRUE

  df <- merge(x=df, y=estrato_rim_arte, by.x = c("ESTRATO_RIM", "COD_ARTE", "ARTE"), by.y = c("ESTRATO_RIM", "COD_ARTE", "ARTE"), all.x = TRUE)

  errors <- df[is.na(df$VALID),]

  if(nrow(errors)>0){
  errors <- errors[ ,c(BASE_FIELDS, "COD_ARTE", "ARTE")]
  errors <- unique(errors)
  errors <- addTypeOfError(errors, "ERROR: el arte no es coherente con el estrato rim.")

    return(errors)
  }
}


# Function to search errors in number of ships (empty field, =0 or >2)
numberOfShips <- function (catches){
  errors <- catches[catches["N_BARCOS"] == 0 | catches["N_BARCOS"] > 2 | is.null(catches["N_BARCOS"]), c(BASE_FIELDS, "N_BARCOS")]
  errors <- addTypeOfError(errors, "WARNING: número de barcos igual a 0 o mayor de 2")
  return (errors)
}


# Function to search errors in number of rejects (only empty, nas or null)
numberOfRejections <- function(catches){
  errors <- catches %>%
    filter(N_RECHAZOS == ""|is.na(N_RECHAZOS)|is.null(N_RECHAZOS)) %>%
    select(one_of(BASE_FIELDS)) %>%
    unique()
  errors <- addTypeOfError(errors, "ERROR: número de rechazos sin rellenar")
  return(errors)
}


# Function to search samples with SOP > P_MUE_VIVO
sopGreaterPesMueVivo <- function(catches_in_lengths){
  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "COD_ESP_CAT", "ESP_CAT", "P_MUE_VIVO", "SOP")
  errors <- catches_in_lengths %>%
    select(one_of(selected_fields))%>%
    mutate(DIF_SOP_P_MUE_VIVO = SOP - P_MUE_VIVO )%>%
    mutate(DIF_SOP_P_MUE_VIVO = round(DIF_SOP_P_MUE_VIVO,2))%>%
    filter(DIF_SOP_P_MUE_VIVO > 0.01 )
  errors <- addTypeOfError(errors, "ERROR: SOP mayor que peso muestreado vivo")
  return (errors)
}


# Function to search samples with SOP = 0
sopZero <- function(catches_in_lengths){
  fields_to_select <- c(BASE_FIELDS,"COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA",
                        "CATEGORIA", "COD_ESP_CAT", "ESP_CAT", "P_MUE_DESEM", "SOP")
  errors <- catches_in_lengths %>%
    select(one_of(fields_to_select)) %>%
    filter(SOP == 0)
  errors <- addTypeOfError(errors, "ERROR: SOP igual a 0")
  return (errors)
}

# function to search samples with P_MUE_DESEM = 0 or NA
pesMueDesemZero <- function(catches_in_lengths){
  fields_to_select <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA",
                        "CATEGORIA", "COD_ESP_CAT", "ESP_CAT", "P_MUE_DESEM")
  errors <- catches_in_lengths %>%
    filter(P_MUE_DESEM == 0 | is.na(P_MUE_DESEM)) %>%
    select(one_of(fields_to_select))

  errors <- addTypeOfError(errors, "ERROR: Peso muestreado es 0")

  return(errors)
}


# function to search samples with p_desem <= p_mue_desem
pesMueDesemGreaterPesDesem <- function (catches_in_lengths){
  fields_to_select <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA",
                        "CATEGORIA", "COD_ESP_CAT", "ESP_CAT", "P_DESEM",
                        "P_MUE_DESEM", "DIF_P_MUE_DESEM_P_DESEM")

  errors <- catches_in_lengths %>%
    mutate(DIF_P_MUE_DESEM_P_DESEM = P_MUE_DESEM - P_DESEM) %>%
    filter(DIF_P_MUE_DESEM_P_DESEM > 0) %>%
    select(one_of(fields_to_select)) %>%
    unique()

  errors <- addTypeOfError(errors, "ERROR: Peso muestreado mayor al peso desembarcado")

  return(errors)
}


# function to check samples with SOP > P_VIVO
sopGreaterPesVivo <- function (catches_in_lengths){
  fields_to_select <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA",
                        "CATEGORIA", "COD_ESP_CAT", "ESP_CAT", "SOP", "P_VIVO",
                        "DIF_SOP_P_VIVO")

  errors <- catches_in_lengths %>%
    mutate(DIF_SOP_P_VIVO = SOP - P_VIVO) %>%
    mutate(DIF_SOP_P_VIVO = round(DIF_SOP_P_VIVO,2)) %>%
    filter(DIF_SOP_P_VIVO > 0.01) %>%
    select(one_of(fields_to_select))

  errors <- addTypeOfError(errors, "ERROR: SOP mayor que peso vivo desembarcado")

  return(errors)
}


# function to search categories with equal species weight landings
speciesWithCategoriesWithSameWeightLanding <- function(catches){
  catches <- catches[,c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "P_DESEM")]
  fields_to_count <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "P_DESEM")
  errors <- catches %>%
    unique() %>%
    group_by_at(c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "P_DESEM"))%>%
    summarise(n=n())%>%
    filter(n>1)
  colnames(errors)[names(errors) == "n"] <- "NUM_OCU_CAT_MISMO_PESO_DESEM"
  errors <- addTypeOfError(errors, "WARNING: varias categorías con igual peso desembarcado")
  return(errors)
}

# function to check the samples with categories which all of this species has the same sampled weigh
allCategoriesWithSameSampledWeights <- function (catches_in_lengths){

  selected_fields<-c(BASE_FIELDS,"N_CATEGORIAS","COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA","CATEGORIA", "P_MUE_DESEM", "SEXO")

  by_category <- catches_in_lengths %>%
    group_by_at(selected_fields) %>%
    summarise(NUM_ESP_CAT_MISMO_PESO_MUE=n())%>%
    filter(NUM_ESP_CAT_MISMO_PESO_MUE > 1) %>%
    addTypeOfError("WARNING: varias especies de la categorías con igual peso muestreado")
}

# function to search samples with doubtfull species of the category
# Search, too, the genus finished in -formes, -dae, - spp and - sp.
doubtfulCategorySpecies <- function(catches_in_lengths){

  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "COD_ESP_CAT", "ESP_CAT")

  #create a dataframe with other species not allowed
  # by sufixes
  to_check_genus <- grep("(.+(formes$))|(.+(spp$))|(.+(sp$))|(.+(dae$))",catches_in_lengths$ESP_CAT)
  # . = any single character
  # + = one of more of previous
  # | = or

  genus_not_allowed <- catches_in_lengths[to_check_genus,] %>%
    select(one_of(selected_fields))

  # this is obsolete: when was allowed Loligo spp an Allotheuthis spp saved in
  # 'especies de la categoría':
  # remove other allowed species from the dataframe with not allowed species
  # genus_not_allowed <- genus_not_allowed[!(genus_not_allowed[["COD_ESP_CAT"]] %in% ALLOWED_GENUS[["COD_ESP"]]),] %>%
  #   select(one_of(selected_fields))

  # remove duplicates
  errors <- unique(genus_not_allowed)

  errors <- addTypeOfError(errors, "WARNING: ¿seguro que es esa especie en Especies de la Categoría?")

  return(errors)
}

# function to search samples with not allowed species of the category
# This function use the not allowed species dataframe of SAPMUEBASE.
notAllowedCategorySpecies <- function(catches_in_lengths){

  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "COD_ESP_CAT", "ESP_CAT")

  # create a dataframe with species not allowed
  not_allowed <- merge(x = catches_in_lengths, y = NOT_ALLOWED_SPECIES, by.x = "COD_ESP_CAT", by.y = "COD_ESP") %>%
    select(one_of(selected_fields))

  # remove duplicates
  errors <- unique(not_allowed)

  errors <- addTypeOfError(errors, "ERROR: muestreos con especies no permitidos en Especies de la Categor?a")

  return(errors)
}


# function to check foreings ships in MT1 samples
# df: dataframe
check_foreing_ships_MT1 <- function(df){
  ships <- df %>%
    select(one_of(BASE_FIELDS))%>%
    filter(grepl("^8\\d{5}",COD_BARCO), COD_TIPO_MUE == "1") %>%
    unique()

  ships <- addTypeOfError(ships, "ERROR: MT1 con barco extranjero")

  return(ships)
}

# function to check foreings ships in MT2 samples
# df: dataframe
check_foreing_ships_MT2 <- function(df){
  ships <- df %>%
    select(one_of(BASE_FIELDS))%>%
    filter(grepl("^8\\d{5}",COD_BARCO), COD_TIPO_MUE == "2") %>%
    unique()

  ships <- addTypeOfError(ships, "WARNING: MT2 con barco extranjero")

  return(ships)
}


#' Check code: 1026
#' Ships registered in CFPO in removal state ("Baja definitiva" and "Baja
#' Provisional")
#' @details Require a valid CFPO file.
#' @param df: dataframe returned by one of the importRIM functions.
#' @param cfpo: valid CFPO file.
#' @return A dataframe with errors.
shipsNotRegistered <- function(df, cfpo = CFPO){

  df <- unique(df[,c(BASE_FIELDS, "CODSGPM")])
  df[,"CFR"] <- sprintf("ESP%09d", as.numeric(as.character(df[["CODSGPM"]])))

  errors <- merge(x=df, y=cfpo, by.x = "CFR", by.y = "CFR", all.x = TRUE)

  errors <- errors[errors[["ESTADO"]] %in% c("Baja Definitiva", "Baja Provisional"), ]

  if(nrow(errors) > 0) {
    text_type_of_error <- paste0("ERROR: barco en ", errors[["ESTADO"]], " en el CFPO.")
    errors <- cbind(errors, "TIPO_ERROR" = text_type_of_error)
    return (errors)
  }

}


#' Check code: 1025
#' Ships not in CFPO
#' @details Require a valid CFPO file.
#' @param df: dataframe returned by one of the importRIM functions.
#' @param cfpo: valid CFPO file.
#' @return A dataframe with errors.
shipsNotInCFPO <- function(df, cfpo = CFPO){

  df <- unique(df[,c(BASE_FIELDS, "CODSGPM")])

  df[,"CFR"] <- sprintf("ESP%09d", as.numeric(as.character(df[["CODSGPM"]])))

  errors <- merge(x=df, y=cfpo, by.x = "CFR", by.y = "CFR", all.x = TRUE)

  errors <- errors[is.na(errors[["ESTADO"]]),]

  # if(nrow(errors)>0) {
  #   errors <- addTypeOfError(errors, "ERROR: barco no incluido en el CFPO")
  #   return (errors)
  # }

  if(nrow(errors)>0) {
    text_type_of_error <- paste0("ERROR: barco no incluido en el CFPO.")
    errors <- cbind(errors, "TIPO_ERROR" = text_type_of_error)
    return (errors)
  }

}

#function to check if all mt2b has CERCO_GC or BACA_GC stratA
#only usefull in COD_TIPO_MUE = 4
checkMt2bRimStratum <- function (catches) {

  # select all the mt2b samples

  mt2b <- catches[catches[["COD_TIP_MUE"]] == 4, c(BASE_FIELDS)]

  err_stratum <- mt2b[!(mt2b[["ESTRATO_RIM"]] %in% c("CERCO_GC", "BACA_GC")),]

  err_stratum <- unique(err_stratum)

  err_stratum <- addTypeOfError(err_stratum, "ERROR: MT2B con estrato rim distinto a CERCO_GC y BACA_GC")

}

# function to check samples with weight sampled = 0 with lenghts
weightSampledZeroWithLengthsSampled <- function (catches_in_lengths) {
  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "P_DESEM", "P_VIVO", "COD_ESP_CAT", "ESP_CAT", "P_MUE_DESEM", "P_MUE_VIVO", "SOP")
  err <- catches_in_lengths %>%
    filter(P_MUE_DESEM == 0) %>%
    select(one_of(selected_fields)) %>%
    addTypeOfError("ERROR: peso muestra 0 con tallas muestreadas")
}

# function to check samples with weight landed = 0 or NA
weightLandedZero <- function (catches) {
  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "P_DESEM", "P_VIVO")
  err <- catches %>%
    filter(P_DESEM == 0 | is.na(P_DESEM)) %>%
    select(one_of(selected_fields)) %>%
    addTypeOfError("ERROR: peso desembarcado = 0")
  return(err)
}

# function to check samples without lengths but with weight sampled
weightSampledWithoutLengthsSampled <- function (catches_in_lengths) {
  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "P_DESEM", "P_VIVO", "COD_ESP_CAT", "ESP_CAT", "P_MUE_DESEM", "EJEM_MEDIDOS")
  err <- catches_in_lengths %>%
    filter(P_MUE_DESEM != 0 & (EJEM_MEDIDOS == 0 | is.na(EJEM_MEDIDOS))) %>%
    select(one_of(selected_fields)) %>%
    addTypeOfError("ERROR: especie sin tallas muestreadas pero con peso muestra")
  return(err)
}


# function to search false mt2 samples
# samples with COD_TIPO_MUE as MT2 and without any length
# df: dataframe
# return: dataframe with erroneous samples
check_false_mt2 <- function(catches, lengths_sampled){

  #Select all the samples with COD_TIPO_MUE = MT2
  mt2 <- catches[catches[["COD_TIPO_MUE"]] == 2 | catches[["COD_TIPO_MUE"]] == 4, c(BASE_FIELDS)]
  mt2 <- unique(mt2)

  # select all the samples with lengths
  mt2_with_lenghts <- lengths_sampled %>%
    filter(COD_TIPO_MUE == 2 | COD_TIPO_MUE == 4)  %>%
    group_by_at(BASE_FIELDS) %>%
    summarise(summatory = sum(EJEM_MEDIDOS, na.rm = TRUE))

  # check if all the samples keyed as MT2 has lengths
  false_mt2 <- anti_join(x = mt2, y = mt2_with_lenghts, by = c("FECHA_MUE","COD_BARCO")) %>% unique()
  false_mt2 <- addTypeOfError(false_mt2, "ERROR: MT2 sin tallas")

  return(false_mt2)

}

# search false mt1 samples
# samples with COD_TIPO_MUE as MT1A and lenghts
# df: dataframe
# return: dataframe with erroneus samples
check_false_mt1 <- function(catches, lengths_sampled){

  #Select all the samples with COD_TIPO_MUE = MT1

  mt1 <- catches[catches[["COD_TIPO_MUE"]] == 1, c(BASE_FIELDS)]
  mt1 <- unique(mt1)

  # select all the samples with lengths
  mt1_with_lenghts <- lengths_sampled %>%
    filter(COD_TIPO_MUE == 1)  %>%
    group_by_at(BASE_FIELDS) %>%
    summarise(summatory = sum(EJEM_MEDIDOS, na.rm = TRUE))

  # check if all the samples keyed as MT1 hasn't lenghts
  false_mt1 <- merge(x = mt1, y = mt1_with_lenghts, by = BASE_FIELDS) %>% unique()
  false_mt1 <- addTypeOfError(false_mt1, "ERROR: MT1 con tallas")

  return(false_mt1)
}

# check mixed species keyed as non mixed species
# in COD_ESP_MUE there are codes from non mixed species
# df: dataframe
# return a dataframe with the samples with species keyed as non mixed species
check_mixed_as_no_mixed <- function(catches){
  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE")
  non_mixed <- merge(x=catches, y=especies_mezcla["COD_ESP_CAT"], by.x = "COD_ESP_MUE", by.y = "COD_ESP_CAT")
  non_mixed <- non_mixed[, c(selected_fields)]
  non_mixed <- addTypeOfError(non_mixed, "ERROR: especie de mezcla tecleada sin agrupar en Especies del Muestreo")
  return(non_mixed)
}

# function to check no mixed species keyed as mixed species
# in COD_ESP_MUEthere are codes from mixed species
# df: dataframe
# return a dataframe with the samples with species keyed as non mixed species
check_no_mixed_as_mixed <- function(lengths_sampled){
  selected_fields <- c(BASE_FIELDS,"COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "COD_ESP_CAT", "ESP_CAT")
  non_mixed <- merge(x=lengths_sampled, y=especies_no_mezcla["COD_ESP"], by.x = "COD_ESP_MUE", by.y = "COD_ESP")
  non_mixed <- non_mixed[, c(selected_fields)]
  non_mixed <- unique(non_mixed)
  non_mixed <- addTypeOfError(non_mixed, "ERROR: especie no de mezcla agrupada en Especies del Muestreo")
  return(non_mixed)
}

# function to check grouped species in Species of the Category
# return a dataframe with the samples with grouped species in Species of the Category
mixedSpeciesInCategory <- function(catches_in_lengths){

  selected_fields<-c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "COD_ESP_CAT", "ESP_CAT")

  not_allowed_in_category <- especies_mezcla %>%
    select(COD_ESP_MUE) %>%
    unique()

  clean_catches_in_lenghts <- catches_in_lengths %>%
    select(one_of(selected_fields))

  errors <- merge(x=clean_catches_in_lenghts, y=not_allowed_in_category, by.x = "COD_ESP_CAT", by.y = "COD_ESP_MUE")

  errors <- addTypeOfError(errors, "ERROR: muestreo MT2 con especie de mezcla que está agrupada en Especies para la Categoría")

}

# function to check doubtful species in Sampling Species
doubtfulSampledSpecies <- function(catches){

  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE")

  #create a dataframe with other species not allowed
  # by sufixex
  to_check_genus <- grep("(.+(formes$))|(.+(spp$))|(.+(sp$))|(.+(dae$))",catches$ESP_MUE)
  # . = any single character
  # + = one of more of previous
  # | = or

  genus_not_allowed <- catches[to_check_genus,]

  # remove the mixed species (allowed)
  genus_not_allowed <- genus_not_allowed[!(genus_not_allowed[["COD_ESP_MUE"]] %in% unique(mixed_species[["COD_ESP_MUE"]])),] %>%
    select(one_of(selected_fields))

  # this is obsolete: when was allowed Loligo spp an Allotheuthis spp saved in
  # 'especies de la categoría':
  # remove other allowed species
  # genus_not_allowed <- genus_not_allowed[!(genus_not_allowed[["COD_ESP_MUE"]] %in% ALLOWED_GENUS[["COD_ESP"]]),] %>%
  #  select(one_of(selected_fields))

  # remove duplicates
  errors <- unique(genus_not_allowed)

  errors <- addTypeOfError(errors, "WARNING: ¿seguro que es esa especie en Especies del Muestreo?")

  return(errors)
}


# function to check if there are not allowed species in Sampling Species
notAllowedSampledSpecies <- function(catches){

  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE")

  # create a dataframe with species not allowed
  sampling_species_not_allowed <- merge(x = catches, y = NOT_ALLOWED_SPECIES, by.x = "COD_ESP_MUE", by.y = "COD_ESP") %>%
    select(one_of(selected_fields))

  # remove duplicates
  errors <- unique(sampling_species_not_allowed)

  errors <- addTypeOfError(errors, "ERROR: muestreo con especie no permitida en Especies del Muestreo")

  return(errors)
}

# function to check sexes with exactly the same sampled weight
sexesWithSameSampledWeight <- function (catches_in_lengths){

  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA",
                       "CATEGORIA", "COD_ESP_CAT", "ESP_CAT", "P_MUE_DESEM")

  errors <- catches_in_lengths %>%
    group_by_at(selected_fields)%>%
    summarise(NUM_SEXOS_MISMO_P_MUE_DESEM = n())%>%
    filter(NUM_SEXOS_MISMO_P_MUE_DESEM > 1) %>%
    addTypeOfError("WARNING: Sexos de una misma especie tienen exactamente el mismo peso muestra")

}


# function to check categories with varios equal sexes (both of them male or female)
categoriesWithRepeatedSexes <- function(catches_in_lengths) {

  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA",
                       "CATEGORIA", "COD_ESP_CAT", "ESP_CAT", "SEXO")
  errors <- catches_in_lengths %>%
    filter(SEXO != "U") %>%
    group_by_at(selected_fields) %>%
    summarise(NUM_MISMOS_SEXOS = n()) %>%
    filter(NUM_MISMOS_SEXOS != 1) %>%
    addTypeOfError("categorías con varios sexos iguales (la misma especie con varias distribuciones de machos o hembras")

}

# check if the COD_ID variable is filled
#' The COD_ID variable is filled when the data is dumped in SIRENO. But, if
#' a new sample is hand typed, the COD_ID it is not generated and is saved as
#' an empty field.
#' This function check that all the registers of COD_ID are filled.
#' @return Return a dataframe with the erroneus codes.

checkCodId <- function(df){

  if (variable_exists_in_df("COD_ID", df)){

    errors <- df[is.na(df[["COD_ID"]]),]

    errors <- errors[,BASE_FIELDS]

    errors <- addTypeOfError(errors, "ERROR: COD_ID variable is empty. This error only
                   can be resolved by IT services in Madrid.")

    return(errors)

  } else {

    return(FALSE)

  }

}


#' Change the content of variable TALL.PESO to TRUE.
#' This variable is T for lenghts samples and P for weight samples.
#' We allways work with lengths samples so all of them must be T.
#
#' @param df: dataframe to modify
#' @return Return a dataframe with the TALL.PESO variable fixed
fixTALL.PESOVariable <- function (df) {

  if ("TALL.PESO" %in% colnames(df)){
    df[["TALL.PESO"]] <- "T"
    return(df)
  } else {
    stop(paste0("TALL.PESO doesn't exists in ", substitute(df)))
  }

}


#' Check if the content of variable TALL.PESO is allways TRUE.
#' This logical variable is TRUE for lenghts samples and false to weight samples.
#' We allways work with lengths samples so all of them must be TRUE.
#' @return dataframe with erroneus samples
checkTALL.PESO <- function(catches) {

  if ("TALL.PESO" %in% colnames(catches)){
    errors <- catches %>%
      select(one_of(c(BASE_FIELDS, "TALL.PESO"))) %>%
      filter(TALL.PESO != "T"|is.na(TALL.PESO)|is.null(TALL.PESO)) %>%
      unique()%>%
      addTypeOfError("ERROR: Muestreo no metido como muestreo de talla")
    return(errors)
  } else {
    stop("TALL.PESO doesn't exists in catches")
  }
}


#' Check samples with no sexed species that must be sexed
#' Sexed species must have the variable SEXO as M (male) or H (female).
#' @return dataframe with erroneus samples
checkSexedSpecies <- function(lengths_sampled) {

  errors <- lengths_sampled %>%
    select(one_of(c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "COD_ESP_CAT", "ESP_CAT", "SEXO"))) %>%
    filter( (COD_ESP_MUE %in% especies_sexadas[["COD_ESP"]]))  %>% #only the sexed species
    merge(y=especies_sexadas, by.x=c("COD_ESP_CAT"), by.y=c("COD_ESP"), all.x = T)%>% #merge with sexed species
    mutate(COD_PUERTO.y=as.character(COD_PUERTO.y))%>% # <--- THIS IS IMPERATIVE BECAUSE
    #THE sexed_species DATAFRAME HAS DIFFERENT LEVELS THAN THE lengths DATAFRAME.
    filter(COD_PUERTO.y == "ALL" | COD_PUERTO.x == COD_PUERTO.y)%>% # sexed species
    #must be only with port field "ALL" or a port similiar between lengths and especies_sexadas dataframe
    filter( (SEXO != "M" & SEXO != "H") ) %>%
    rename("COD_PUERTO" = COD_PUERTO.x, "PUERTO" = PUERTO.x, "LOCODE" = LOCODE.x, "ESP_MUE" = ESP_MUE.x) %>%
    select(one_of(c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "COD_ESP_CAT", "ESP_CAT", "SEXO"))) %>%
    unique()%>%
    addTypeOfError("ERROR: especie que debería ser sexada")

  return(errors)
}

#' Check samples with sexed species that must not be sexed
#' No sexed species must have the variable SEXO as U.
#' @return dataframe with erroneus samples
checkNoSexedSpecies <- function(lengths_sampled) {

  # Subset especies_sexadas dataframe only with required?? variables:
  sexed_species <- especies_sexadas[,c("COD_ESP", "COD_PUERTO")]

  # Subset sexed especies sampled
  sexed_species_sampled <- lengths_sampled %>%
    select(one_of(c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "COD_ESP_CAT", "ESP_CAT", "SEXO"))) %>%
    filter( SEXO == "M" | SEXO == "H" )

  # Check if all the species sampled must be sexed:
  errors_species_must_not_be_sexed <- sexed_species_sampled %>%
    filter(!(COD_ESP_CAT %in% sexed_species$COD_ESP)) %>%
    unique()

  # Check if all the species sampled must be sexed in some ports:
  sexed_species_by_port <- sexed_species[sexed_species[["COD_PUERTO"]]!="ALL",]

  errors_species_must_be_sexed_only_in_some_ports <- sexed_species_sampled %>%
    filter(COD_ESP_CAT %in% sexed_species_by_port[["COD_ESP"]]) %>% # filter only species wich must be sampled in some ports
    merge(, y=sexed_species_by_port, by.x = c("COD_ESP_CAT", "COD_PUERTO"), by.y = c("COD_ESP", "COD_PUERTO"), all.x = T)%>%
    unique()

  # merge errors
  errors <- rbind(errors_species_must_not_be_sexed, errors_species_must_be_sexed_only_in_some_ports) %>%
    addTypeOfError("WARNING: especie que NO debería ser sexada. Es posible que el SOP de estos muestreos sea 0, por lo que se ha de comprobar.")

  return(errors)
}


#' Check samples with same date, vessel, gear, and port but with different
#' ESTRATO_RIM variable.
#'
#' @return dataframe with erroneus samples
#'
checkMultipleEstratoRIM <- function(catches){

  errors <- catches %>%
    select(one_of(BASE_FIELDS)) %>%
    unique() %>%
    group_by(COD_PUERTO, PUERTO, LOCODE, FECHA_MUE, COD_BARCO, BARCO, COD_TIPO_MUE, TIPO_MUE) %>%
    mutate(num_estrato_rim = n_distinct(ESTRATO_RIM))%>%
    ungroup()%>%
    # summarise(num_estrato_rim = n_distinct(ESTRATO_RIM)) %>%
    filter(num_estrato_rim != 1) %>%
    addTypeOfError("ERROR: mismo puerto/fecha/barco/tipo_muestreo con distinto ESTRATO_RIM")

  return(errors)

}

# check multiple gear in the same trip
#' Check samples with same date, vessel, ESTRATO_RIM, TIPO_MUE, and port but with different
#' gear variable.
#'
#' @return dataframe with erroneous samples
#'
checkMultipleGear <- function(catches){

  errors <- catches %>%
    select(one_of(c(BASE_FIELDS, "COD_ARTE", "ARTE"))) %>%
    unique() %>%
    group_by(COD_PUERTO, PUERTO, LOCODE, FECHA_MUE, COD_BARCO, BARCO, ESTRATO_RIM, COD_TIPO_MUE, TIPO_MUE) %>%
    mutate(num_arte = n_distinct(COD_ARTE))%>%
    ungroup()%>%
    # summarise(num_Estrato_RIM = n_distinct(ESTRATO_RIM)) %>%
    filter(num_arte != 1) %>%
    addTypeOfError("ERROR: mismo puerto/fecha/barco/estrato_rim con distinto ARTE")

  return(errors)

}

# check multiple COD_PUERTO in the same trip
#' Check samples with same date and vessel, ESTRATO_RIM, TIPO_MUE, and gear but with different
#' port variable.
#' @return dataframe with erroneous samples
checkMultiplePort <- function(catches){

  # errors <- catches %>%
  #   select(one_of(c(BASE_FIELDS, "COD_ARTE", "ARTE"))) %>%
  #   unique() %>%
  #   group_by(COD_ID, COD_PUERTO, PUERTO, LOCODE, FECHA_MUE, COD_BARCO, BARCO, ESTRATO_RIM, COD_TIPO_MUE, TIPO_MUE) %>%
  #   mutate(num_puerto = n_distinct(COD_PUERTO))%>%
  #   ungroup()%>%
  #   filter(num_puerto != 1) %>%
  #   addTypeOfError("ERROR: mismo fecha/barco/estrato_rim/tipo_muestre con distinto PUERTO")
  errors <- catches %>%
    select(one_of(c(BASE_FIELDS, "COD_ARTE", "ARTE"))) %>%
    unique() %>%
    group_by(FECHA_MUE, COD_BARCO, ESTRATO_RIM, COD_TIPO_MUE) %>%
    mutate(num_puerto = n()) %>%
    filter(num_puerto != 1) %>%
    addTypeOfError("ERROR: mismo fecha/barco/estrato_rim/tipo_muestreo con distinto PUERTO")

  return(errors)

}

# check coherence between ESTRATO_RIM and origin
#
#' Check coherence between ESTRATO_RIM and origin.
#'
#'  @return data frame with wrong coherence.
#'
checkCoherenceEstratoRimOrigin <- function(catches, specification){

  estrato_rim_origen$VALID<-TRUE

  df <- merge(x=catches, y=estrato_rim_origen, by.x = c("ESTRATO_RIM", "COD_ORIGEN"), by.y = c("ESTRATO_RIM", "COD_ORIGEN"), all.x = TRUE)

  errors <- df[is.na(df$VALID),]

  if(nrow(errors)>0){
    errors <- errors[ ,c(BASE_FIELDS, "COD_ORIGEN")]
    errors <- unique(errors)
    errors <- addTypeOfError(errors, "ERROR: no concuerda el estrato_rim con el origen.")
    return(errors)
  }

}

# check ESTRATO_RIM and gear of trips with 2 or more ships
# Exception with Santa Eugenia de Ribeira port, that usually only one ship is
# sampled
#
#' Check ESTRATO_RIM and gear of trips with 2 ships
#'
#' @return dataframe with erroneous trips
#'
checkShipsPairBottomTrawl <- function(catches){
  errors <- catches %>%
    select(one_of(BASE_FIELDS), N_BARCOS) %>%
    unique()%>%
    filter(COD_PUERTO != "0917")%>% #This is an execption: only one ship is sampled in Ribeira pair bottom trails
    filter(
      ((N_BARCOS == 2 | N_BARCOS == 3 | N_BARCOS == 4) & ESTRATO_RIM != "PAREJA_CN") |
        (N_BARCOS != 2 & N_BARCOS != 3 & N_BARCOS != 4 & ESTRATO_RIM == "PAREJA_CN")
    ) %>%
    addTypeOfError("ERROR: número de barcos no coherente con el estrato rim")
  return (errors)
  #errors <- addInfluenceAreaVariable(errors, "COD_PUERTO")
  #write.csv(errors, file = "parejas_num_barcos_1.csv")
}

#' Check code: 1052
# warning outliers in lengths by fishing ground
#
#' Check the size range of species in the rango_tallas_historico_caladero dataset.
#'
#' @return dataframe with warnings lengths
#'
checkSizeRangeByFishingGround <- function (lengths_data){

  lengths_data <- merge(x = lengths_data,
                        y = sapmuebase::caladero_origen[,c("CALADERO", "COD_ORIGEN")],
                        all.x = T,
                        by="COD_ORIGEN")

  lengths_data <- merge(x=lengths_data,
                        y = rango_tallas_historico_caladero,
                        by.x = c("COD_ESP_CAT", "CALADERO"),
                        by.y = c("COD_ESP", "CALADERO"),
                        all.x = T)

  warningsOutOfRange <- lengths_data %>%
    select(one_of(BASE_FIELDS), COD_ESP_CAT, COD_CATEGORIA, TALLA, TALLA_MIN, TALLA_MAX)%>%
    filter(TALLA < TALLA_MIN | TALLA > TALLA_MAX)%>%
    # it's not possible use addTypeOfError here, I don't know why
    mutate(TIPO_ERROR = paste("WARNING: Talla fuera del rango histórico de tallas por caladero:", TALLA_MIN, "-", TALLA_MAX))%>%
    humanizeVariable("COD_ESP_CAT")

  return(warningsOutOfRange)

}
#' Check code: 1065
# warning species without historical range by fishing ground
#
#' Check species without historical range by fishing ground.
#'
#' @return dataframe with warnings.
#'
checkRangeInHistorical <- function(lengths_data){

  warningsIsRanged <- lengths_data%>%
    select(one_of(BASE_FIELDS), COD_ESP_CAT, COD_CATEGORIA)%>%
    merge(y = rango_tallas_historico_caladero,
          by.x = c("COD_ESP_CAT"),
          by.y = c("COD_ESP"),
          all.x = T)%>%
    filter(is.na(TALLA_MIN) | is.na((TALLA_MAX)))%>%
    unique()%>%
    addTypeOfError("WARNING: esta especie no se encuentra en el maestro histórico por caladero de tallas mínimas y máximas.")%>%
    humanizeVariable("COD_ESP_CAT")%>%
    select(-c(TALLA_MIN, TALLA_MAX))

  return(warningsIsRanged)
}



# warning outliers in species catches
checkCatchesP99 <- function(catches){

  warnings <- catches %>%
    select(one_of(BASE_FIELDS), COD_ESP_MUE, ESP_MUE, P_DESEM) %>%
    group_by_at(c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE")) %>%
    summarise(P_DESEM_TOT = sum(P_DESEM)) %>%
    merge(., p99_capturas_historico,
          by.x = c("ESTRATO_RIM", "COD_ESP_MUE"),
          by.y = c("ESTRATO_RIM", "COD_ESP"), all.x = T) %>%
    filter(P_DESEM_TOT > P99) %>%
    mutate('% dif respecto al histórico de capturas' = round(((P_DESEM_TOT-P99) * 100 / P_DESEM_TOT)))%>%
    addTypeOfError("WARNING: Captura de la especie (de todas las categorías de la especie) superior al percentil 99 del histórico de capturas 2014 al 2018 por estrato rim.")

  warnings[['P99']] <- round(warnings[['P99']], 1)


  return(warnings)

}

# check estrategia
# TODO: rellenar esto para document()

checkStrategy <- function(catches){

  error_strategy <- catches %>%
    select(one_of(c(BASE_FIELDS, "ESTRATEGIA")))%>%
    anti_join(y=tipo_muestreo, by = c("COD_TIPO_MUE", "ESTRATEGIA"))%>%
    addTypeOfError("ERROR: No concuerda el campo ESTRATEGIA con el campo TIPO DE MUESTREO")

  # MT2 samples of VORACERA_GC must be "En base a especie", so remove it of
  # the errors dataframe
  error_strategy <- error_strategy[
    -which(error_strategy$ESTRATO_RIM == "VORACERA_GC" &
             error_strategy$ESTRATEGIA == "En base a especie"), ]

  return(error_strategy)

}

# check same COD_BARCO and FECHA_MUE but different COD_TIPO_MUE
multipleTypeSample <- function(catches){

  # error <- catches %>%
  #   select(COD_BARCO, COD_PUERTO, LOCODE, FECHA_MUE, COD_TIPO_MUE) %>%
  #   unique()%>%
  #   group_by(COD_BARCO, FECHA_MUE)%>%
  #   mutate(number_type_sample = n()) %>%
  #   filter(number_type_sample >1) %>%
  #   addTypeOfError("ERROR: Para un mismo barco y fecha, hay muestreos de varios tipos")

  # error <- catches %>%
  #   select(COD_BARCO, FECHA_MUE, COD_TIPO_MUE) %>%
  #   unique()%>%
  #   group_by(COD_BARCO, FECHA_MUE)%>%
  #   mutate(number_type_sample = n()) %>%
  #   filter(number_type_sample >1) %>%
  #   addTypeOfError("ERROR: Para un mismo barco y fecha, hay muestreos de varios tipos")

  error <- catches %>%
    select(COD_ID, COD_PUERTO, COD_BARCO, FECHA_MUE, COD_TIPO_MUE) %>%
    unique()%>%
    group_by(COD_BARCO, FECHA_MUE)%>%
    mutate(number_type_sample = n_distinct(COD_TIPO_MUE, COD_BARCO)) %>%
    filter(number_type_sample >1) %>%
    addTypeOfError("ERROR: Para un mismo barco y fecha, hay muestreos de varios tipos")

  error <- humanize(error)

  return(error)

}

#' Check elapsed days between landing date and sampling date -------------------
#' A warning is generated if the elepsaed days are minor than 0 or greater than 3
#' @return dataframe with erroneous trips
check_elapsed_days <- function(catches){

  catches$FECHA_MUE <- as.POSIXlt(catches$FECHA_MUE, format = "%d-%m-%y")

  # change the column "FECHA_DESEM" to a date format
  # to avoid some problems with Spanish_Spain.1252 (or if you are using another
  # locale), change locale to Spanish_United States.1252:
  lct <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME","Spanish_United States.1252")

  catches[["FECHA_DESEM"]] <- as.POSIXlt(catches[["FECHA_DESEM"]], format = "%d-%b-%y")

  # and now the come back to the initial configuration of locale:
  Sys.setlocale("LC_TIME", lct)

  catches[["elapsed_days"]] <- difftime(catches[["FECHA_MUE"]], catches[["FECHA_DESEM"]], units = "days")

  catches[["FECHA_MUE"]] <- as.POSIXct(catches[["FECHA_MUE"]])
  catches[["FECHA_DESEM"]] <- as.POSIXct(catches[["FECHA_DESEM"]])

  errors <- catches %>%
    select(one_of(c(BASE_FIELDS), "FECHA_DESEM", "elapsed_days")) %>%
    filter(elapsed_days>3 | elapsed_days<(-1)) %>%
    unique() %>%
    addTypeOfError("WARNING: tiempo transcurrido entre la fecha de desembarco y la de muestreo mayor que 3 días o menor que 0 días")

  return(errors)

}

# warning species with taxonomic confusion
#' Search in catches and catches_in_lengths data set the species which can have
#' problems with taxonomic confusion.
#' This species are listed in the data set
#' ESP_TAXONOMIC_CONFUSION.
#' @return data frame with warnings
taxonomicSpecieConfusion <- function (catches, catches_in_lengths) {

  err_catches <- catches%>%
    select(one_of(c(BASE_FIELDS, "COD_ORIGEN", "COD_ESP_MUE", "ESP_MUE"))) %>%
    unique%>%
    merge(, y = ESP_TAXONOMIC_CONFUSION, by.x=(c("COD_ESP_MUE", "COD_ORIGEN")), by.y = (c("COD_ESP_WARNING", "COD_ORIGEN"))) %>%
    addTypeOfError("WARNING: ¿seguro que la especie del muestreo no es la propuesta en ESP_PROPUESTA?")

  err_catches_in_lengths <- catches_in_lengths%>%
    select(one_of(c(BASE_FIELDS, "COD_ORIGEN", "COD_ESP_CAT", "ESP_CAT"))) %>%
    unique%>%
    merge(, y = ESP_TAXONOMIC_CONFUSION, by.x=(c("COD_ESP_CAT", "COD_ORIGEN")), by.y = (c("COD_ESP_WARNING", "COD_ORIGEN"))) %>%
    addTypeOfError("WARNING: ¿seguro que la especie de la categoría no es la propuesta en ESP_PROPUESTA?")

  err <- merge(err_catches, err_catches_in_lengths, all = TRUE) %>%
    select(-c(ESP_WARNING))

  colnames(err)[colnames(err) == "COMENTARIOS"] <- "COMENTARIO ESPECIE"

  return(err)

}

# error same trip in variuous ports at the same date
#' Search ships which landed in various ports at the same date.
#'
#' There are a function 'checkMultiplePort' which find trips with FECHA_MUE,
#' COD_BARCO, ESTRATO_RIM, COD_TIPO_MUE. But this checkSameTripInVariousPorts
#' ignore ESTRATO_RIM and COD_TIPO_MUE variables.
#'
#' @return dataframe with warnings
#'
checkSameTripInVariousPorts <- function (catches){
  err <- catches %>%
    select(COD_PUERTO, FECHA_MUE, COD_BARCO) %>%
    unique()%>%
    group_by(FECHA_MUE, COD_BARCO) %>%
    mutate(n_ports_per_trip=n_distinct(COD_PUERTO)) %>%
    filter(n_ports_per_trip > 1)%>%
    humanize()%>%
    addTypeOfError("ERROR: Un mismo barco ha descargado en varios puertos en la misma fecha. Es posible que el puerto pertenezca a un área de influencia distinta.")

  return(err)
}





# check if the a variable is filled in all the rows
#' TODO: put in sapmuebase???
checkVariableFilled <- function(df, var) {

  tryCatch({

    variable_exists_in_df(var, df)

    fields <- c(BASE_FIELDS, var)

    err <- df[df[[var]] == ""
              | is.na(df[[var]])
              | is.null(df[[var]]), fields]

    err <- unique(err)

    err <- addTypeOfError(err, "ERROR: campo ", substitute(var), " vacío.")

    return(err)

  }, error = function(e) {

    print(e)

  })

}

# function to check variables
#' Check variables
#
#' Check if the value of variables are consistent to the value in its SIRENO master.
#' It's only available for variables with a data source (master): ESTRATO_RIM, COD_PUERTO,
#' COD_ORIGEN, COD_ARTE, COD_PROCEDENCIA and TIPO_MUESTREO
#' @param variable: one of this values: ESTRATO_RIM, COD_PUERTO, COD_ORIGEN,
#' COD_ARTE, COD_PROCEDENCIA or TIPO_MUESTREO
#' @return Return a data frame with samples containing erroneous variables
check_variable_with_master <- function (df, variable){

  if(variable != "ESTRATO_RIM" &&
     variable != "COD_PUERTO" &&
     variable != "COD_ORIGEN" &&
     variable != "COD_ARTE" &&
     variable != "PROCEDENCIA" &&
     variable != "COD_TIPO_MUE"){
    stop(paste("This function is not available for ", variable))
  }

  # If the variable begin with "COD_", the name of the data source
  # is the name of the variable without "COD_"
  variable_formatted <- variable
  if (grepl("^COD_", variable)){
    variable_formatted <- strsplit(variable, "COD_")
    variable_formatted <- variable_formatted[[1]][2]
  }

  # In case COD_TIPO_MUE, the data set is "tipo_muestreo" instead of "tipo_mue":
  if ( variable_formatted == "TIPO_MUE") {
    name_dataset <- "tipo_muestreo"
  }  else {
    name_dataset <- tolower(variable_formatted)
  }


  #search the errors in variable
  errors <- anti_join(df, get(name_dataset), by = variable)

  #prepare to return
  fields_to_filter <- c(BASE_FIELDS, variable, variable_formatted)

  errors <- errors %>%
    select(one_of(fields_to_filter))%>%
    unique()


  text_type_of_error <- paste0("ERROR: el campo ", variable_formatted, " no concuerda con los maestros de este script.")
  errors <- addTypeOfError(errors, text_type_of_error)

  #return
  return(errors)
}

#' Check code: 1061
#' Check if there are samples with the same name vessel but with different
#' SIRENO codification or SGPM codification.
#'
#' @return dataframe with errors
checkMultipleShipCode <- function(catches){

  # find the ship names with more than one COD_BARCO and CODSGPM
  dsn <- catches %>%
    select(BARCO,COD_BARCO, CODSGPM) %>%
    unique()%>%
    count(BARCO) %>%
    filter(n>1)

  dsn <- as.character(dsn$BARCO)

  # get the samples with the multiple ships names
  err <- catches[catches$BARCO %in% dsn,]

  err <- err[, BASE_FIELDS]
  err <- unique(err)
  err <- addTypeOfError(err, "WARNING: Hay varios muestreos que tienen este mismo nombre de barco, pero con distinto código SIRENO o distinto Código Secretaría. ¿Seguro que es el barco correcto?")

  return(err)
}

#' Check code: 1062
#' Check coherence between DCF Metier, Rim Stratum and Origin.
#'
#' @return dataframe with errors
coherenceDCFMetierRimStratumOrigin <- function(df){

  metier_caladero_dcf_clean <- metier_caladero_dcf[,c("ESTRATO_RIM",
                                                      "METIER_DCF", "COD_ORIGEN")]
  metier_caladero_dcf_clean$VALID <- TRUE

  df_clean <- df[, c(BASE_FIELDS, "COD_ORIGEN", "METIER_DCF")]
  df_clean <- unique(df_clean)

  err <- merge(df_clean, metier_caladero_dcf_clean,
               by = c("ESTRATO_RIM", "METIER_DCF", "COD_ORIGEN"),
               all.x = T
  )

  err <- err[which(err$VALID!=TRUE | is.na(err$VALID)),]

  err <- addTypeOfError(err, "Metier DCF does not match with Rim Stratum and Origin")

  return(err)

}

#' Check code: 1063
#' Check coherence between DCF Fishing Ground, Rim Stratum and Origin.
#'
#' @return dataframe with errors
coherenceDCFFishingGroundRimStratumOrigin <- function(df){

  metier_caladero_dcf_clean <- metier_caladero_dcf[,c("ESTRATO_RIM",
                                                      "CALADERO_DCF", "COD_ORIGEN")]
  metier_caladero_dcf_clean$VALID <- TRUE

  df_clean <- df[, c(BASE_FIELDS, "COD_ORIGEN", "CALADERO_DCF")]
  df_clean <- unique(df_clean)

  err <- merge(df_clean, metier_caladero_dcf_clean,
               by = c("ESTRATO_RIM", "CALADERO_DCF", "COD_ORIGEN"),
               all.x = T
  )

  err <- err[which(err$VALID!=TRUE | is.na(err$VALID)),]

  err <- addTypeOfError(err, "Fishing Ground DCF does not match with Rim Stratum and Origin")

  return(err)

}

#' Check code: 1064
#' Check all the categories are measured. If one or more are not, return a
#' warning.
#'
#' @return data frame with errors
allCategoriesMeasured <- function(df_catches, df_lengths_sampled){

  # Some categories never are measured, so must be ignored
  regex_to_ignore <- c("^.*(?i)colas.*",
                       "^.*(?i)melad(a|o)$",
                       "^.*(?i)melad(a|o).*",
                       "^.*(?i)trozos$",
                       "^.*(?i)picad(a|o)",
                       "^(?i)alas\ .*$",
                       "^Huevas$"
  )

  regex_to_ignore <- paste(regex_to_ignore, collapse = "|")

  # Clean the categories master
  maestro_categorias_clean <- categorias[, c("COD_ESP", "COD_PUERTO", "COD_CATEGORIA", "PROCESO")]

  # Clean catches dataframe
  clean_catches <- df_catches[ which(df_catches$COD_TIPO_MUE == "2"), ]

  clean_catches <- merge(clean_catches, maestro_categorias_clean,
                         by.x = c("COD_ESP_MUE", "COD_PUERTO", "COD_CATEGORIA"),
                         by.y = c("COD_ESP", "COD_PUERTO", "COD_CATEGORIA"),
                         all.x = T)


  clean_catches <- clean_catches[ -grep(regex_to_ignore, clean_catches$CATEGORIA),]

  # Get the number of categories in catches dataframe
  cat_cat <- clean_catches %>%
    group_by(COD_ID, COD_ESP_MUE, ESP_MUE) %>%
    mutate( n_cat_catches = n_distinct(COD_CATEGORIA)) %>%
    select(all_of(BASE_FIELDS), COD_ESP_MUE, ESP_MUE, n_cat_catches) %>%
    unique()

  # Get the number of categories with sampled lengths
  sam_cat_len <- df_lengths_sampled %>%
    group_by(COD_ID, COD_ESP_MUE, ESP_MUE) %>%
    mutate( n_cat_lengths = n_distinct(COD_CATEGORIA)) %>%
    select(COD_ID, COD_ESP_MUE, ESP_MUE, n_cat_lengths) %>%
    unique()

  # Get errors

  errors <- merge(cat_cat,
                  sam_cat_len,
                  all.x = T,
                  by = c("COD_ID", "COD_ESP_MUE", "ESP_MUE"))

  errors <- errors[which(errors[["n_cat_catches"]] >1 &
                             errors[["n_cat_lengths"]] < errors[["n_cat_catches"]]),]

  if(nrow(errors)>0){

    errors <- addTypeOfError(errors, "WARNING: some categories of the species are measured but others are not")

    errors <- errors[, c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "TIPO_ERROR")]

    return(errors)
  }

}


#' Check code: 1070 ----
#' Check if the value of variable is in prescriptions dataset. Use the
#' prescriptions_rim_2021_variables dataset.
#' @param df Dataframe where the variable to check is.
#' @param variable Variable to check as character. Allowed variables:
#' ESTRATO_RIM, COD_PUERTO, COD_ORIGEN, COD_ARTE, METIER_DCF and CALADERO_DCF.
#' @return dataframe with errors
checkVariableWithRimMt2PrescriptionsPost <- function(df, variable) {

  valid_variables = c("ESTRATO_RIM","COD_PUERTO","COD_ORIGEN","COD_ARTE",
                      "METIER_DCF", "CALADERO_DCF")
  if (!(variable %in% valid_variables)) {
    stop(paste("This function is not available for variable", variable))
  }

  allowed <- prescripciones_rim_mt2_coherencia[,variable]

  # only MT2 samples:
  df <- df[df[["COD_TIPO_MUE"]]==2, ]

  df <- df[!(df[[variable]] %in% allowed), ]

  if (nrow(df)>0){
    fields <- BASE_FIELDS

    if (!(variable %in% BASE_FIELDS)) {
      fields <- c(BASE_FIELDS, variable)
    }

    df <- df[, fields]

    df <- unique(df)

    df[["TIPO_ERROR"]] <- apply(df, 1, function(x){
      paste0( "The ", variable , " ", x[[variable]], " is not in actual MT2 prescriptions.")
    })

    return(df)
  }


}


#' Check code: 1069
#' Check if the variables "COD_PUERTO", "COD_ARTE", "COD_ORIGEN", "ESTRATO_RIM",
#' "METIER_DCF" and "CALADERO_DCF" are coherent with MT2 rim prescriptions.
#' @return dataframe with errors, if there are any.
coherenceRimMt2PrescriptionsPost <- function(df){

  df <- df[df[["COD_TIPO_MUE"]]==2, ] #THIS IS DIFFERENT IN rim_pre_dump, COD_TIPO_MUE is "MT2A"

  errors <- unique(df[, c("COD_ID","FECHA_MUE", "COD_BARCO", "BARCO", "COD_PUERTO", "COD_ARTE", "COD_ORIGEN", "ESTRATO_RIM", "METIER_DCF", "CALADERO_DCF")])
  errors <- merge(errors,
                  prescripciones_rim_mt2_coherencia,
                  by=c("COD_PUERTO", "COD_ARTE", "COD_ORIGEN", "ESTRATO_RIM", "METIER_DCF", "CALADERO_DCF"),
                  all.x = TRUE)
  if(nrow(errors)>0){
    errors <- errors[is.na(errors[["PESQUERIA"]]), c("COD_ID","FECHA_MUE", "COD_BARCO", "BARCO", "COD_PUERTO", "COD_ARTE", "COD_ORIGEN", "METIER_DCF", "CALADERO_DCF")]
    errors <- humanize(errors)
    errors <- addTypeOfError(errors, "This combination of port, gear, origin, rim stratum, dcf metier and dcf fishing ground are not in 2021 MT2 RIM prescriptions.")
    return (errors)
  }

}


#' Check if a variable exists in a dataframe -----------------------------------
#' TODO: put in sapmuebase
#' @return TRUE if the variable exists. Otherwise return an error.
#' @param variable: variable to check.
#' @param df: dataframe to check
#' @export
variable_exists_in_df <- function (variable, df){

  # get all the variables of df with the variable name = variable
  var_in_df <- colnames(df)[colnames(df) %in% variable]

  if (length(var_in_df) > 1) {
    stop(paste("Hey hard worker! check the ", variable,
               "variable. Looks like there are multiple columns with the same variable name.
               Using consistents dataframes we will get a better world. Really :) ",
               variable, "."))
  } else if (length(var_in_df) == 0) {
    stop(paste(variable, " does not exists in this dataframe."))
  } else return (TRUE)

}



#' Check if a variable or variables of a dataframe contain empty values. ----
#' @param variables: vector with variables to check.
#' @param df: dataframe to check.
#' @param df_name: name of the dataframe where the error is found.
#' @return A list with a dataframe of every variable with empty values. Every
#' dataframe contains erroneus rows.
#' @export
checkEmptyValuesInVariables <- function (df, variables, df_name){

  # check if all the variables are in the dataframe
  if(!all(variables %in% colnames(df))){
    stop("Not all the variables are in the dataframe.")
  }

  variables <- as.list(variables)

  if(df_name!=""){
    df_name <- paste(" in", df_name, " screen")
  }

  errors <- lapply(variables, function(x){

    error <- (df[df[[x]]=="" | is.na(df[[x]]),])

    if (nrow(error)>0){
      error <- addTypeOfError(error, "ERROR: Variable ", x, " empty", df_name )
      if(x %in% BASE_FIELDS){
        error <- error[, c(BASE_FIELDS, "TIPO_ERROR")]
      } else {

        # The variables "EJEM_MEDIDOS", "EJEM_PONDERADOS", "SOP"
        # and "P_MUE_DESEM", must have more
        # variables to properly identify the error:
        if(x%in%c("EJEM_MEDIDOS", "EJEM_PONDERADOS", "SOP")){
          error <- error[, c(BASE_FIELDS, "COD_ESP_MUE","ESP_MUE",
                             "COD_CATEGORIA", "CATEGORIA", "COD_ESP_CAT",
                             "ESP_CAT", "TALLA", x, "TIPO_ERROR")]
        } else if(x%in%c("P_MUE_VIVO","P_MUE_DESEM")){
        # The variables"P_MUE_DESEM", must have more
        # variables to properly identify the error:
          error <- error[, c(BASE_FIELDS, "COD_ESP_MUE","ESP_MUE",
                             "COD_CATEGORIA", "CATEGORIA", "COD_ESP_CAT",
                             "ESP_CAT", x, "TIPO_ERROR")]
        } else {
          error <- error[, c(BASE_FIELDS, x, "TIPO_ERROR")]
        }

      }


    }
    return(error)

  })


  errors <- lapply(errors, unique)

  errors <- Filter(function(x) nrow(x) > 0, errors)

  if(length(errors)>0){

    errors <- Reduce(function(x,y){
      merge(x, y, all=TRUE)
    },errors)

    return(errors)

  }

}

#' Check code: 1071
#' Empty fields in variables
#' Return empty variables of a RIM dataframes imported by importOAB functions. Only
#' variables saved in formato_variables dataset as mandatory are checked.
#' @details Require one of the dataframes returned by importOABFiles functions:
#' importRIMCatches() and importRIMLengths().
#' @param df: dataframe returned by one of the importRIM functions.
#' @param type_file: type of the imported file according to this values:
#' RIM_CATCHES or RIM_LENGTHS.
#' @return A dataframe with the COD_MAREA and variables with values missing.
emptyFieldsInVariables <- function(df, type_file = c("RIM_CATCHES", "RIM_LENGTHS")){

  # Detect if the variable type_file is correct:
  match.arg(type_file)

  # Create helper_text
  helper_text <- substr(type_file, 5, nchar(type_file))
  helper_text <- tolower(helper_text)


  mandatory_field <- paste0(type_file, "_MANDATORY")

  mandatory <- formato_variables[which(!is.na(formato_variables[type_file])
                                       & formato_variables[mandatory_field] == TRUE), c("name_variable")]
  df_mandatory <- df[,mandatory]

  err <- checkEmptyValuesInVariables(df_mandatory, mandatory, helper_text)

  # in case there aren't any errors, check_empty_values returns NULL, so:
  if (!is.null(err)){
    return(err)
  }

}

#' Check code:1076
#' Function to check if species Sardina Pilchardus (10152) and Engraulis
#' encrasicolus (10156) where measured in the correct way, at middle centimeter
#' (1/2 cm).
#' @param lengths: lengths data frame returned by the importRIMLengths() or
#' importRIMFiles() functions.
#' @return A data frame where the species were measured centimeter to centimeter.
halfCentimeter <- function(lengths){

  #Fields: species, Sardina Pilchardus (10152) and Engraulis encrasicolus (10156)
  especies <- c("10152", "10156")
  #Fields: columns
  col_filter <- c("COD_ID", "FECHA_MUE", "COD_ESP_MUE", "COD_BARCO", "COD_ESP_CAT", "COD_CATEGORIA", "TALLA")

  #Filter where we have more than one register
  lengths <- lengths[lengths$COD_ESP_MUE %in% especies, col_filter] %>%
    group_by(COD_ID, FECHA_MUE, COD_ESP_MUE, COD_BARCO, COD_ESP_CAT, COD_CATEGORIA) %>%
    mutate(Registros = n()) %>%
    filter(Registros>1)

  #Count the middle centimeter measures
  midCentTrue <- grepl("^.*.5$", lengths$TALLA)

  lengths <- lengths[midCentTrue, ] %>%
    group_by(COD_ID, FECHA_MUE, COD_ESP_MUE, COD_BARCO, COD_ESP_CAT, COD_CATEGORIA) %>%
    mutate(TALLAS_MED = n_distinct(TALLA))

  err <- lengths[lengths$TALLAS_MED==0,]

  #Parameters: count 0 elements
  if(nrow(err)!=0){
    errors <- addTypeOfError(err, "WARNING: Comprobar que se hayan medido las tallas al cm en vez del 1/2 cm")
    return(errors)
  }

}



#' Priority species with catches but not measured.
#' Use MT2A and MT2B samples.
#' @param catches: data frame returned by the importRIMCatches() or
#' importRIMFiles() functions.
#' @param lengths: lengths data frame returned by the importRIMLengths() or
#' importRIMFiles() functions.
#' @param group: character vector with priority group ("G1" to "G6").
#' @return Data frame with species of the priority group without samples measured.
#' @note
#' This function does not return the errors column. Used in functions
#' g1SpeciesNotMeasured and g2SpeciesNotMeasured.
prioritySpeciesNotMeasured <- function(catches, lengths, group){

  sps <- unique(especies_prioritarias[especies_prioritarias$PRIORIDAD %in% group,
                                           "COD_ESP_MUE"])

  catches <- catches[catches$COD_TIPO_MUE %in% c(2, 4),
                     c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE")]
  catches <- unique(catches)

  lengths <- lengths[lengths$COD_TIPO_MUE %in% c(2, 4),
                     c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_ESP_CAT", "ESP_CAT", "EJEM_MEDIDOS")]
  lengths <- lengths[lengths$COD_ESP_MUE %in% sps, ]

  lengths <- lengths[!is.na(lengths$EJEM_MEDIDOS), ]
  lengths <- lengths[lengths$EJEM_MEDIDOS != 0, ]
  lengths$EJEM_MEDIDOS <- "T"
  lengths <- unique(lengths)
  lengths <- lengths[, c("COD_ID", "COD_ESP_MUE", "ESP_MUE", "COD_ESP_CAT", "ESP_CAT", "EJEM_MEDIDOS")]

  catches <- catches[catches$COD_ESP_MUE %in% sps, ]

  errors <- merge(catches,
                   lengths,
                   by = c("COD_ID", "COD_ESP_MUE", "ESP_MUE"),
                   all.x= TRUE)

  errors <- errors[is.na(errors$EJEM_MEDIDOS) | errors$EJEM_MEDIDOS == 0, ]

  errors <- errors[, c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_ESP_CAT", "ESP_CAT" )]

}

#' Check code: 1077
#' G1 priority species with catches but not measured.
#' Use MT2A and MT2B samples.
#' @param catches: data frame returned by the importRIMCatches() or
#' importRIMFiles() functions.
#' @param lengths: lengths data frame returned by the importRIMLengths() or
#' importRIMFiles() functions.
#' @return Data frame with species of the priority group without samples measured.
g1SpeciesNotMeasured <- function(catches, lengths){

  errors <- prioritySpeciesNotMeasured(catches, lengths, "G1")

  if(nrow(errors)!=0){
    errors <- addTypeOfError(errors, "ERROR: priority G1 species not measured.")
    return(errors)
  }

}

#' Check code: 1078
#' G2 priority species with catches but not measured.
#' Use MT2A and MT2B samples.
#' @param catches: data frame returned by the importRIMCatches() or
#' importRIMFiles() functions.
#' @param lengths: lengths data frame returned by the importRIMLengths() or
#' importRIMFiles() functions.
#' @return Data frame with species of the priority group without samples measured.
g2SpeciesNotMeasured <- function(catches, lengths){

  errors <- prioritySpeciesNotMeasured(catches, lengths, "G2")

  if(nrow(errors)!=0){
    errors <- addTypeOfError(errors, "WARNING: priority G2 species not measured.")
    return(errors)
  }

}


#' Check code: 1079 PENDIENTE DE IMPLEMENTAR --> FALTA CREAR MAESTRO
#' Function to check the presence of the ships from our ships' masterdata on the
#' working data
#' @param catches: data frame returned by the importRIMCatches() or
#' importRIMFiles() functions.
#' @param shipFishingGearMaster: master dataframe with the code ships and its
#' ESTRATO_RIM from 2020 to 2022.
#' @return Data frame with those ships no presents in our working data, labeled
#' with a WARNING message
checkShipNoMasterFishingGear <- function(catches, shipFishingGearMaster){

  #Step 1: filter the necessary columns from the catches df and pass from level to normal data

  catches <- catches[, BASE_FIELDS]

  catches$COD_BARCO <- as.character(catches$COD_BARCO)

  catches$ESTRATO_RIM <- as.character(catches$ESTRATO_RIM)

  #Step 2: filter the ships in the catches' dataframe that are not present in boatFishingArtMaster

  masterShip <- unique(shipFishingGearMaster$COD_BARCO)

  catchesNoMasterShip <- catches[!(catches$COD_BARCO %in% masterShip), ]

  #Step 3: check that the number of road is not equal to zero and add the type or error for obtained dataframes

  if (nrow(catchesNoMasterShip)!=0){

    catchesNoMasterShip <- addTypeOfError(catchesNoMasterShip, "WARNING: Barco no presente en el maestro")

  }

  catchesNoMasterShip <- unique(catchesNoMasterShip)

  return(catchesNoMasterShip)

}

#' Check code: 1080 NOT IMPLEMENTED --> FALTA CREAR EL MAESTRO
#' Function to check the coherence of the ESTRATO_RIM between our ships' masterdata
#' and the working data
#' @param catches: data frame returned by the importRIMCatches() or
#' importRIMFiles() functions.
#' @param shipFishingGearMaster: master dataframe with the code ships and its
#' ESTRATO_RIM from 2020 to 2022.
#' @return Data frame that mark those ships where there is not coincidence between the
#' ESTRATO_RIM from our ships' masterdata and the working data, specifying those what does not
#' match with a Warning message
checkShipDifferentFishingGear <- function(catches, shipFishingGearMaster){

  #Step 1: add the "Testigo" column to shipFishingGearMaster and convert "COD_BARCO" and "ESTRATO_RIM" as parameter

  shipFishingGearMaster$TESTIGO <- "T"

  shipFishingGearMaster$COD_BARCO <- as.character(shipFishingGearMaster$COD_BARCO)

  shipFishingGearMaster$ESTRATO_RIM <- as.character(shipFishingGearMaster$ESTRATO_RIM)

  #Step 2: filter the necessary columns from the catches df and pass from level to normal data

  catches <- catches[, BASE_FIELDS]

  catches$COD_BARCO <- as.character(catches$COD_BARCO)

  catches$ESTRATO_RIM <- as.character(catches$ESTRATO_RIM)

  #Step 3: filter the ships in the catches' dataframe that are present in shipFishingGearMaster

  masterShip <- unique(shipFishingGearMaster$COD_BARCO)

  catchesMasterShip <- catches[catches$COD_BARCO %in% masterShip, ]

  #Step 4: merging the shipFishingGearMaster frame with the catches' one

  catchesMasterShip <- merge(catchesMasterShip, shipFishingGearMaster, all.x=TRUE)

  #Step 5: develop new dataframes, one to detect where the TESTIGO values are "Na"

  catchesNaValues <- unique(catchesMasterShip[is.na(catchesMasterShip$TESTIGO), ])

  #Step 6: now, we make a subset of the masterShip with the ship's codes of the catchesNaValues.
  #Then, we create a special dataframe where we have unite all ESTRATOS that has one ship
  #using the function «concatenaThor».

  shipFishingGearMasterNaValues <- shipFishingGearMaster[shipFishingGearMaster$COD_BARCO %in% catchesNaValues$COD_BARCO,
                                                         c("COD_BARCO","ESTRATO_RIM")]

  fusionEstratos <- tapply(shipFishingGearMasterNaValues$ESTRATO_RIM,
                           shipFishingGearMasterNaValues$COD_BARCO,
                           paste, collapse=", ")

  shipCodes <- unique(shipFishingGearMasterNaValues$COD_BARCO)

  fusionEstratosDataFrame <- data.frame(COD_BARCO=shipCodes, ESTRATOS=fusionEstratos)

  #Step 7: here we merge the fusionEstratosDataFrame with the catchesNaValues

  catchesNaValues <- merge(catchesNaValues, fusionEstratosDataFrame, all.x=TRUE)

  #Step 8: check that the number of road is not equal to zero and add the type or error for obtained dataframes

  if (nrow(catchesNaValues)!=0){
    message <- paste0("WARNING: El barco trabajó en 2020-2022 en otros estratos diferentes: ", catchesNaValues$ESTRATOS)
    catchesNaValues$TIPO_ERROR <- message
  }

  #Note: making a little fix in the columns of the catchesNaValues

  catchesNaValues <- catchesNaValues[, c(BASE_FIELDS, "TIPO_ERROR")]

  catchesNaValues <- unique(catchesNaValues)

  return(catchesNaValues)

}


#' Check code: 1080
#' Check ships with "CODSGPM" value empty, "0" or NA.
#' @param catches: catches data frame returned by the importRIMCatches() or
#' importRIMFiles() functions.
#' @return Data frame whit errors.
shipWhithoutCODSGPM <- function(catches){

  catches <- catches[catches$CODSGPM=="0" | catches$CODSGPM=="" | is.na(catches$CODSGPM), c(BASE_FIELDS, "CODSGPM")]

  if (nrow(catches)!=0){
    catches <- unique(catches)
    catches <- addTypeOfError(catches, "ERROR: El código de la SGPM para este barco está vacio.")
    return(catches)
  }

}

