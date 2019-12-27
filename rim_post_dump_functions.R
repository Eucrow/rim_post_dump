# FUNCTIONS ####################################################################


# function to make a backup of the errors files --------------------------------
backup_files <- function(){
  date <- Sys.time();
  date <- as.POSIXlt(date);
  
  directory_backup<-paste(PATH_BACKUP, "/", YEAR, "_", MONTH, "_backup_", date$mday,
                          '_', date$mon+1, '_', date$year+1900, '_', date$hour, '_',
                          date$min, '_', round(date$sec, 0), "/", sep="")
  dir.create(directory_backup, recursive=TRUE); #create the directory backup
  
  files <- list.files(PATH_ERRORS, pattern = "*.csv", full.names = TRUE)
  
  lapply(as.list(files), function(x){ file.copy(x, directory_backup)})
}

# function to add variable with type of error to a dataframe -------------------
addTypeOfError <- function(df, ...){
  
  arguments <- list(...)
  
  type <- paste(arguments, collapse = '', sep = " ")
  
  if(nrow(df)!=0){
    # df[["TIPO_ERROR"]] <- type
    df <- df %>% mutate(TIPO_ERROR = type)
  }
  return(df)
}

# function to format the errors produced ---------------------------------------
# This function combine all the dataframes of the errors_list (a list of dataframes)
# and format it:
# - combine all dataframes in one
# - order columns
# - separate the dataframe by influence area
# - order every area dataframe
# - remove empty columns in every area dataframe
# the separate_by_ia = FALSE generate one dataframe without separate by influence
# area, but with the AREA_INF variable
formatErrorsList <- function(errors_list = ERRORS, separate_by_ia = TRUE){
  
  # Combine all the dataframes of ERRORS list:
  # Reduce uses a binary function to successively combine the elements of a
  # given vector. In this case, merge the dataframes in the ERRORS list
  #errors <- Reduce(function(x, y) merge(x, y, all=TRUE), errors_list)
  
  #better with join_all form plyr package because doesn't change the order of columns:
  errors <- join_all(errors_list, type = "full")
  
  if(separate_by_ia == FALSE){
    areas_influencia <- areas_influencia[, c("COD_PUERTO", "AREA_INF")]
    errors <- merge(errors, areas_influencia, by = "COD_PUERTO", all.x = TRUE)
    errors <- list(total = errors)
  } else if (separate_by_ia == TRUE) {
    # Separate dataframe by influece area
    errors <- separateDataframeByInfluenceArea(errors, "COD_PUERTO")
  } else {
    stop("separete_by_ia must be TRUE or FALSE")
  }
  
  # Order the errors and remove columns with only NA values
  errors <- lapply(errors, function(x){
    
    # Order columns
    x <- x %>%
      select(AREA_INF, COD_ID, COD_PUERTO, PUERTO, FECHA_MUE, COD_BARCO, BARCO, ESTRATO_RIM,
             TIPO_MUE, COD_ESP_MUE, ESP_MUE, COD_CATEGORIA, CATEGORIA, P_DESEM, 
             P_VIVO, COD_ESP_CAT, ESP_CAT, SEXO, everything()) %>%
      select(-one_of("TIPO_ERROR"), one_of("TIPO_ERROR")) %>% #remove TIPO_ERROR, and add it to the end
      mutate(FECHA_MUE = as.Date(FECHA_MUE, "%d-%m-%y")) %>%
      arrange_( "COD_PUERTO", "COD_ID", "FECHA_MUE", "COD_ESP_MUE", "COD_CATEGORIA", "COD_ESP_CAT")
    
    #Remove columns with only NA values
    #Filter extracts the elements of a vector for which a predicate (logical) function gives true
    x <- Filter(function(x){!all(is.na(x))}, x)
    
    # Add column Comprobado
    x[["comprobado"]] <- ""
    
    
    return(x)
  })  
  
  return(errors)
}


# Function to check the coherence between 'ESTRATO_RIM' and 'gear' -------------
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


# Function to search errors in number of ships (empty field, =0 or >2) ---------
numberOfShips <- function (catches){
  errors <- catches[catches["N_BARCOS"] == 0 | catches["N_BARCOS"] > 2 | is.null(catches["N_BARCOS"]), c(BASE_FIELDS, "N_BARCOS")]
  errors <- addTypeOfError(errors, "WARNING: número de barcos igual a 0 o mayor de 2")
  return (errors)
}


# Function to search errors in number of rejects (only empty, nas or null) -----
numberOfRejections <- function(catches){
  errors <- catches %>%
    filter(N_RECHAZOS == ""|is.na(N_RECHAZOS)|is.null(N_RECHAZOS)) %>%
    select(one_of(BASE_FIELDS)) %>%
    unique()
  errors <- addTypeOfError(errors, "ERROR: número de rechazos sin rellenar")
  return(errors)
}


# Function to search samples with SOP > P_MUE_VIVO -----------------------------
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


# Function to search samples with SOP = 0 --------------------------------------
sopZero <- function(catches_in_lengths){
  fields_to_select <- c(BASE_FIELDS,"COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA",
                        "CATEGORIA", "COD_ESP_CAT", "ESP_CAT", "P_MUE_DESEM", "SOP")
  errors <- catches_in_lengths %>%
    select(one_of(fields_to_select)) %>%
    filter(SOP == 0)
  errors <- addTypeOfError(errors, "ERROR: SOP igual a 0")
  return (errors)
}

# function to search samples with P_MUE_DESEM = 0 or NA ------------------------
pesMueDesemZero <- function(catches_in_lengths){
  fields_to_select <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA",
                        "CATEGORIA", "COD_ESP_CAT", "ESP_CAT", "P_MUE_DESEM")
  errors <- catches_in_lengths %>%
    filter(P_MUE_DESEM == 0 | is.na(P_MUE_DESEM)) %>%
    select(one_of(fields_to_select))
  
  errors <- addTypeOfError(errors, "ERROR: Peso muestreado es 0")
  
  return(errors)
}


# function to search samples with p_desem <= p_mue_desem -----------------------
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


# function to check samples with SOP > P_VIVO ----------------------------------
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


# function to search categories with equal species weight landings -------------
speciesWithCategoriesWithSameWeightLanding <- function(catches){
  catches <- catches[,c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "P_DESEM")]
  fields_to_count <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "P_DESEM")
  errors <- catches %>% 
    distinct() %>%
    count_(fields_to_count) %>%
    filter(n>1)
  colnames(errors)[names(errors) == "n"] <- "NUM_OCU_CAT_MISMO_PESO_DESEM"
  errors <- addTypeOfError(errors, "WARNING: varias categorías con igual peso desembarcado")
  return(errors)
}

# function to check the samples with categories which all of this species has the same sampled weight --------
allCategoriesWithSameSampledWeights <- function (catches_in_lengths){
  
  selected_fields<-c(BASE_FIELDS,"N_CATEGORIAS","COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA","CATEGORIA", "P_MUE_DESEM", "SEXO")
  
  by_category <- catches_in_lengths %>%
    group_by_(.dots = selected_fields) %>%
    #tally() %>% # tally() is the same that summarise(num = n())
    summarise(NUM_ESP_CAT_MISMO_PESO_MUE=n())%>%
    filter(NUM_ESP_CAT_MISMO_PESO_MUE > 1) %>%
    addTypeOfError("WARNING: varias especies de la categorías con igual peso muestreado")
}

# function to search samples with doubtfull species of the category ------------
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
  errors <- unique(genus_not_allowed)  %>%
    arrange_(BASE_FIELDS)
  
  errors <- addTypeOfError(errors, "WARNING: ¿seguro que es esa especie en Especies de la Categoría?")
  
  return(errors)
}

# function to search samples with not allowed species of the category ----------
# This function use the not allowed species dataframe of SAPMUEBASE.
notAllowedCategorySpecies <- function(catches_in_lengths){
  
  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "COD_ESP_CAT", "ESP_CAT")
  
  # create a dataframe with species not allowed
  not_allowed <- merge(x = catches_in_lengths, y = NOT_ALLOWED_SPECIES, by.x = "COD_ESP_CAT", by.y = "COD_ESP") %>%
    select(one_of(selected_fields))
  
  # remove duplicates
  errors <- unique(not_allowed)  %>%
    arrange_(BASE_FIELDS)
  
  errors <- addTypeOfError(errors, "ERROR: muestreos con especies no permitidos en Especies de la Categor?a")
  
  return(errors)
}


# function to check foreings ships in MT1 samples ------------------------------
# df: dataframe
check_foreing_ships_MT1 <- function(df){
  ships <- df %>%
    select(one_of(BASE_FIELDS))%>%
    filter(grepl("^8\\d{5}",COD_BARCO), COD_TIPO_MUE == "1") %>%
    unique()
  
  ships <- addTypeOfError(ships, "ERROR: MT1 con barco extranjero")
  
  return(ships)
}

# function to check foreings ships in MT2 samples ------------------------------
# df: dataframe
check_foreing_ships_MT2 <- function(df){
  ships <- df %>%
    select(one_of(BASE_FIELDS))%>%
    filter(grepl("^8\\d{5}",COD_BARCO), COD_TIPO_MUE == "2") %>%
    unique()
  
  ships <- addTypeOfError(ships, "WARNING: MT2 con barco extranjero")
  
  return(ships)
}


# function to check ships ------------------------------------------------------
# not in "ALTA DEFINITIVA", "ALTA PROVISIONAL POR NUEVA CONSTRUCCIÓN or ALTA PROVISIONAL
# POR REACTIVACIÓN in CFPO
shipsNotRegistered <- function(df, cfpo = CFPO){
  
  to_ships <- unique(df[,c(BASE_FIELDS, "CODSGPM")])
  errors_ships <- merge(x=to_ships, y=cfpo, by.x = "CODSGPM", by.y = "CODIGO_BUQUE", all.x = TRUE)
  errors_ships <- errors_ships %>%
    filter( ESTADO != "ALTA DEFINITIVA" &
              ESTADO != "Alta Definitiva" &
              ESTADO != "H - A.P. POR REACTIVACION" &
              ESTADO != "G - A.P. POR NUEVA CONSTRUCCION" )
  text_type_of_error <- paste0("ERROR: barco incluido en el CFPO pero con un estado distinto a Alta Definitiva, A. P. Por Reactivaci?n, o A.P Por Nueva Construcci?n")
  errors_ships <- addTypeOfError(errors_ships, text_type_of_error)
  return (errors_ships)
}


# function to check ships not in CFPO ------------------------------------------
shipsNotInCFPO <- function(df, cfpo = CFPO){
  
  to_ships <- unique(df[,c(BASE_FIELDS, "CODSGPM")])
  
  errors_ships <- merge(x=to_ships, y=cfpo, by.x = "CODSGPM", by.y = "CODIGO_BUQUE", all.x = TRUE)
  errors_ships <- errors_ships %>%
    filter(is.na(ESTADO))
  errors_ships <- addTypeOfError(errors_ships, "ERROR: barco no incluido en el CFPO")
  return (errors_ships)
}

#function to check if all mt2b has CERCO_GC or BACA_GC stratums ----------------
#only usefull in COD_TIPO_MUE = 4
checkMt2bRimStratum <- function (catches) {
  
  # select all the mt2b samples
  mt2b <- catches %>%
    filter(COD_TIPO_MUE == 4) %>%
    select_(.dots = BASE_FIELDS)
  
  err_stratum <- mt2b[!(mt2b[["ESTRATO_RIM"]] %in% c("CERCO_GC", "BACA_GC")),]
  
  err_stratum <- unique(err_stratum)
  
  err_stratum <- addTypeOfError(err_stratum, "ERROR: MT2B con estrato rim distinto a CERCO_GC y BACA_GC")
  
}

# function to check samples with weight sampled = 0 with lenghts ---------------
weightSampledZeroWithLengthsSampled <- function (catches_in_lengths) {
  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "P_DESEM", "P_VIVO", "COD_ESP_CAT", "ESP_CAT", "P_MUE_DESEM", "P_MUE_VIVO", "SOP")
  err <- catches_in_lengths %>%
    filter(P_MUE_DESEM == 0) %>%
    select(one_of(selected_fields)) %>%
    addTypeOfError("ERROR: peso muestra 0 con tallas muestreadas")
}

# function to check samples with weight landed = 0 or NA -----------------------
weightLandedZero <- function (catches) {
  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "P_DESEM", "P_VIVO")
  err <- catches %>%
    filter(P_DESEM == 0 | is.na(P_DESEM)) %>%
    select(one_of(selected_fields)) %>%
    addTypeOfError("ERROR: peso desembarcado = 0")
  return(err)
}

# function to check samples without lengths but with weight sampled ------------
weightSampledWithoutLengthsSampled <- function (catches_in_lengths) {
  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "P_DESEM", "P_VIVO", "COD_ESP_CAT", "ESP_CAT", "P_MUE_DESEM", "EJEM_MEDIDOS")
  err <- catches_in_lengths %>%
    filter(P_MUE_DESEM != 0 & (EJEM_MEDIDOS == 0 | is.na(EJEM_MEDIDOS))) %>%
    select(one_of(selected_fields)) %>%
    addTypeOfError("ERROR: especie sin tallas muestreadas pero con peso muestra")
  return(err)
}


# function to search false mt2 samples -----------------------------------------
# samples with COD_TIPO_MUE as MT2 and without any lenght
# df: dataframe
# return: dataframe with erroneus samples
check_false_mt2 <- function(catches, lengths_sampled){
  
  #Select all the samples with COD_TIPO_MUE = MT2
  mt2 <- catches %>%
    filter(COD_TIPO_MUE == 2 | COD_TIPO_MUE == 4)  %>%
    select_(.dots = BASE_FIELDS) %>%
    unique()
  
  # select all the samples with lengths
  mt2_with_lenghts <- lengths_sampled %>%
    filter(COD_TIPO_MUE == 2 | COD_TIPO_MUE == 4)  %>%
    group_by_(.dots = BASE_FIELDS) %>%
    summarise(summatory = sum(EJEM_MEDIDOS, na.rm = TRUE))
  
  # check if all the samples keyed as MT2 has lenghts
  false_mt2 <- anti_join(x = mt2, y = mt2_with_lenghts, by = c("FECHA_MUE","COD_BARCO")) %>% unique()
  false_mt2 <- addTypeOfError(false_mt2, "ERROR: MT2 sin tallas")
  
  return(false_mt2)  
  
}

# search false mt1 samples -----------------------------------------------------
# samples with COD_TIPO_MUE as MT1A and lenghts
# df: dataframe
# return: dataframe with erroneus samples
check_false_mt1 <- function(catches, lengths_sampled){
  
  #Select all the samples with COD_TIPO_MUE = MT1
  mt1 <- catches %>%
    filter(COD_TIPO_MUE == 1)  %>%
    select_(.dots = BASE_FIELDS) %>%
    unique()
  
  # select all the samples with lengths
  mt1_with_lenghts <- lengths_sampled %>%
    filter(COD_TIPO_MUE == 1)  %>%
    group_by_(.dots = BASE_FIELDS) %>%
    summarise(summatory = sum(EJEM_MEDIDOS, na.rm = TRUE))
  
  # check if all the samples keyed as MT1 hasn't lenghts
  false_mt1 <- merge(x = mt1, y = mt1_with_lenghts, by = BASE_FIELDS) %>% unique()
  false_mt1 <- addTypeOfError(false_mt1, "ERROR: MT1 con tallas")
  
  return(false_mt1)  
}

# check mixed species keyed as non mixed species -------------------------------
# in COD_ESP_MUE there are codes from non mixed species
# df: dataframe
# return a dataframe with the samples with species keyed as non mixed species
check_mixed_as_no_mixed <- function(catches){
  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE")
  non_mixed <- merge(x=catches, y=especies_mezcla["COD_ESP_CAT"], by.x = "COD_ESP_MUE", by.y = "COD_ESP_CAT") %>%
    select_(.dots = selected_fields)
  non_mixed <- addTypeOfError(non_mixed, "ERROR: especie de mezcla tecleada sin agrupar en Especies del Muestreo")
  return(non_mixed)
}

# function to check no mixed species keyed as mixed species --------------------
# in COD_ESP_MUEthere are codes from mixed species
# df: dataframe
# return a dataframe with the samples with species keyed as non mixed species
check_no_mixed_as_mixed <- function(lengths_sampled){
  selected_fields <- c(BASE_FIELDS,"COD_ESP_MUE", "ESP_MUE", "COD_ESP_CAT", "ESP_CAT")
  non_mixed <- merge(x=lengths_sampled, y=especies_no_mezcla["COD_ESP"], by.x = "COD_ESP_MUE", by.y = "COD_ESP") %>%
    select_(.dots = selected_fields) %>%
    unique()
  non_mixed <- addTypeOfError(non_mixed, "ERROR: especie no de mezcla agrupada en Especies del Muestreo")
  return(non_mixed)
}

# function to check grouped species in Species of the Category -----------------
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

# function to check doubtful species in Sampling Species -----------------------
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


# function to check if there are not allowed species in Sampling Species -------
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

# function to check sexes with exactly the same sampled weight -----------------
sexesWithSameSampledWeight <- function (catches_in_lengths){
  
  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", 
                       "CATEGORIA", "COD_ESP_CAT", "ESP_CAT", "P_MUE_DESEM")
  
  errors <- catches_in_lengths %>%
    group_by_(.dots=selected_fields)%>%
    summarise(NUM_SEXOS_MISMO_P_MUE_DESEM = n())%>%
    filter(NUM_SEXOS_MISMO_P_MUE_DESEM > 1) %>%
    addTypeOfError("WARNING: Sexos de una misma especie tienen exactamente el mismo peso muestra")
  
}


# function to check categories with varios equal sexes (both of them male or female) ---------
categoriesWithRepeatedSexes <- function(catches_in_lengths) {
  
  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", 
                       "CATEGORIA", "COD_ESP_CAT", "ESP_CAT", "SEXO")
  errors <- catches_in_lengths %>%
    filter(SEXO != "U") %>%
    group_by_(.dots = selected_fields) %>%
    summarise(NUM_MISMOS_SEXOS = n()) %>%
    filter(NUM_MISMOS_SEXOS != 1) %>%
    addTypeOfError("categorías con varios sexos iguales (la misma especie con varias distribuciones de machos o hembras")
  
}

# check if the COD_ID variable is filled ---------------------------------------
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

# fix TALL.PESO variable -------------------------------------------------------
#
#' Change the content of variable TALL.PESO to TRUE.
#' 
#' This variable is T for lenghts samples and P for weight samples.
#' We allways work with lengths samples so all of them must be T. 
#
#' @param df: dataframe to modify
#' @return Return a dataframe with the TALL.PESO variable fixed
#'
fixTALL.PESOVariable <- function (df) {
  
  if ("TALL.PESO" %in% colnames(df)){
    df[["TALL.PESO"]] <- "T"
    return(df)
  } else {
    stop(paste0("TALL.PESO doesn't exists in ", substitute(df)))
  }
  
}

# check TALL.PESO variable -----------------------------------------------------
#
#' Check if the content of variable TALL.PESO is allways TRUE.
#' 
#' This logical variable is TRUE for lenghts samples and false to weight samples.
#' We allways work with lengths samples so all of them must be TRUE. 
#'
#' @return dataframe with erroneus samples
#'
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

# check sexed species ----------------------------------------------------------
#
#' Check samples with no sexed species that must be sexed
#' 
#' Sexed species must have the variable SEXO as M (male) or H (female).
#'
#' @return dataframe with erroneus samples
#'
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

# check no sexed species -----------------------------------------
#
#' Check samples with sexed species that must not be sexed
#' 
#' No sexed species must have the variable SEXO as U.
#'
#' @return dataframe with erroneus samples
#'
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
    addTypeOfError("ERROR: especie que NO debería ser sexada. Es posible que el SOP de estos muestreos sea 0, por lo que se ha de corregir.")
  
  return(errors)
}

# check multiple ESTRATO_RIM in the same trip -----------------
#
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

# check multiple gear in the same trip -----------------
#
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

# check multiple COD_PUERTO in the same trip ------------------
#
#' Check samples with same date and vessel, ESTRATO_RIM, TIPO_MUE, and gear but with different 
#' port variable.
#'
#' @return dataframe with erroneous samples
#'
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

# check coherence between ESTRATO_RIM and origin --------------
#
#' Check coherence between ESTRATO_RIM and origin.
#' 
#'  @return dataframe with wrong coherence.
#'  
checkCoherenceEstratoRimOrigin <- function(catches, specification){
  
  estratorim_origen <- get_dataset_by_specification("estratorim_origen", specification)
  
  errors <- catches %>%
    select(one_of(BASE_FIELDS), COD_ORIGEN) %>%
    unique() %>%
    anti_join(y=estratorim_origen, by=c("ESTRATO_RIM", "COD_ORIGEN")) %>%
    humanize()%>%
    addTypeOfError("ERROR: no concuerda el estrato_rim con el origen")
  
  return(errors)
  
}

# check ESTRATO_RIM and gear of trips with 2 or more ships --------------------- 
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

# warning outliers in species lengths ------------------------------------------
#
#' Check the size range of species of the species inthe rango_tallas_historico dataset
#' 
#' @return dataframe with warnings lengths
#' 
checkSizeRange<- function (lengths_sampled){
  
  warningsIsRanged <- lengths_sampled%>%
    select(one_of(BASE_FIELDS), COD_ESP_CAT, SEXO)%>%
    merge(y = rango_tallas_historico, by.x = c("COD_ESP_CAT", "SEXO"), by.y = c("COD_ESP", "SEXO"), all.x = T)%>%
    filter(is.na(TALLA_MIN) | is.na((TALLA_MAX)))%>%
    unique()%>%
    addTypeOfError("WARNING: esta especie con ese sexo no se encuentra en el maestro histórico de tallas mínimas y máximas por estrato rim.")%>%
    humanizeVariable("COD_ESP_CAT")%>%
    select(-c(TALLA_MIN, TALLA_MAX))
  
  warningsOutOfRange <- lengths_sampled %>%
    select(one_of(BASE_FIELDS), COD_ESP_CAT, SEXO, TALLA)%>%
    merge(y = rango_tallas_historico, by.x = c("COD_ESP_CAT", "SEXO"), by.y = c("COD_ESP", "SEXO"), all.x = T)%>%
    filter(TALLA < TALLA_MIN | TALLA > TALLA_MAX)%>%
    # it's not possible use addTypeOfError here, I don't know why
    mutate(TIPO_ERROR = paste("WARNING: Talla fuera del rango histórico de tallas (para ese sexo):", TALLA_MIN, "-", TALLA_MAX))%>%
    humanizeVariable("COD_ESP_CAT")%>%
    select(-c(TALLA_MIN, TALLA_MAX))
  
  # warnings <- rbind(warningsIsRanged, warningsOutOfRange)
  warnings <- merge(warningsIsRanged, warningsOutOfRange, all = T)
  
  return(warnings)
  
}

# warning outliers in species catches ------------------------------------------
checkCatchesP97 <- function(catches){
  
  warnings <- catches %>%
    select(one_of(BASE_FIELDS), COD_ESP_MUE, ESP_MUE, P_DESEM) %>%
    group_by_(.dots = c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE")) %>%
    summarise(P_DESEM_TOT = sum(P_DESEM)) %>%
    merge(., p97_capturas_historico,
          by.x = c("ESTRATO_RIM", "COD_ESP_MUE"),
          by.y = c("ESTRATO_RIM", "COD_ESP"), all.x = T) %>%
    filter(P_DESEM_TOT > P97) %>%
    mutate('%dif respecto al histórico de capturas' = format(((P_DESEM_TOT-P97) * 100 / P_DESEM_TOT), digits=0))%>%
    addTypeOfError("WARNING: Captura de la especie (de todas las categorías de la especie) superior al percentil 97 del histórico de capturas 2014 al 2017 por estrato rim.")
  
  warnings[['P97']] <- format(round(warnings[['P97']], 1), round=1)
  
  
  return(warnings)
  
}

# check estrategia -------------------------------------------------------------
# TODO: rellenar esto para document()

checkStrategy <- function(catches){
  
  error_strategy <- catches %>% 
    select(one_of(c(BASE_FIELDS, "ESTRATEGIA")))%>%
    anti_join(y=tipo_mue, by = c("COD_TIPO_MUE", "ESTRATEGIA"))%>%
    addTypeOfError("ERROR: No concuerda el campo ESTRATEGIA con el campo TIPO DE MUESTREO")
  
  # MT2 samples of VORACERA_GC must be "En base a especie", so remove it of
  # the errors dataframe
  error_strategy <- error_strategy[
    -which(error_strategy$ESTRATO_RIM == "VORACERA_GC" &
             error_strategy$ESTRATEGIA == "En base a especie"), ]
  
  return(error_strategy)
  
}

# check same COD_BARCO and FECHA_MUE but different COD_TIPO_MUE ----------------
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

# check elapsed days between landing date and sampling date --------------------
#' Check elapsed days between landing date and sampling date -------------------
#' 
#' A warning is generated if the elepsaed days are minor than 0 or greater than
#' 3
#' 
#' @return dataframe with erroneous trips
#' 
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

# warning species with taxonomic confusion -------------------------------------
#' Search in catches and catches_in_lengths dataset the species which can have
#' problems with taxonomic confusion. 
#' 
#' This species are listed in the dataset
#' ESP_TAXONOMIC_CONFUSION.
#' 
#' @return dataframe with warnings
#' 
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

# error same trip in variuous ports at the same date -------------------------
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


# check if the a variable is filled in all the rows ----------------------------
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


#' Get dataset filter by specification. -----------------------------------------
#' Some datasets from SAPMUEBASE contains information which is useful only for
#' RIM data or OAB data. This datasets contain logical variables to differentiate
#' this: RIM variable and OAB variable.
#' This function return just the records of a specification
#' @param dataset: dataset to filter, as character. For example: "origen"
#' @param specification: RIM or OAB
#' @return Return the records of a specification of a dataset
get_dataset_by_specification <- function (dataset, specification){
  
  # check if is a valid dataset
  valid_datasets <- c("arte", "origen", "estrato_rim", "puerto", "procedencia",
                      "tipo_mue", "estratorim_origen", "estratorim_arte")
  
  if (!(dataset %in% valid_datasets)){
    stop(paste0("This dataset is not available to work with this function.
                Only ", paste(valid_datasets, collapse=", "), " are allowed."))
  }
  
  # check if specification is RIM or OAB  
  if (specification != "RIM" & specification != "OAB"){
    stop("\'specification\' variable must be \'RIM\' or \'OAB\'")
  }
  
  # filter dataset
  dataset <- get(dataset)
  out <- dataset[dataset[[specification]]==T,]
  
  # remove useless specification variable
  if (specification == "RIM") {
    out <- out[, !(names(out) %in% c("OAB"))]
  }
  if (specification == "OAB") {
    out <- out[, !(names(out) %in% c("RIM"))]
  }
  
  #return
  return(out)
  }


# function to check variables --------------------------------------------------
#' Check variables
#
#' Check if the value of variables are consistents to the value in its SIRENO master.
#' It's only available for variables with a data source (master): ESTRATO_RIM, COD_PUERTO,
#' COD_ORIGEN, COD_ARTE, COD_PROCEDENCIA and TIPO_MUESTREO
#' @param variable: one of this values: ESTRATO_RIM, COD_PUERTO, COD_ORIGEN,
#' COD_ARTE, COD_PROCEDENCIA or TIPO_MUESTREO
#' @param specification: RIM or OAB
#' @return Return a dataframe with samples containing erroneus variables
check_variable_with_master <- function (df, variable, specification){
  
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
  name_dataset <- tolower(variable_formatted)
  
  # filter records by specification
  
  dataset <- get_dataset_by_specification(name_dataset, specification)
  
  #search the errors in variable
  errors <- anti_join(df, dataset, by = variable)
  
  #prepare to return
  fields_to_filter <- c(BASE_FIELDS, variable, variable_formatted)
  
  errors <- errors %>%
    select(one_of(fields_to_filter))%>%
    unique()
  
  
  text_type_of_error <- paste0("ERROR: ", name_dataset, " no concuerda con los maestros de SIRENO")
  errors <- addTypeOfError(errors, text_type_of_error)
  
  #return
  return(errors)
}

# Export error list ------------------------------------------------------------
# This is an improvement of exportListToXlsx, with colorization of rows with the
# same COD_ID variable.
# This does not work: Allways in row 23 the color is allways the same.
# Instead of color the rows, I put a line between different cod_id rows
exportErrorsList <- function (list, prefix = "", suffix = "", separation = "") {
  
  # Create errors subdirectory in case it doesn't exists:
  if (!file.exists(file.path(PATH_FILES, ERRORS_SUBDIRECTORY))){
    dir.create(file.path(PATH_FILES, ERRORS_SUBDIRECTORY))
  }
  
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Openxlsx package needed for this function to work. Please install it.", 
         call = FALSE)
  }
  lapply(seq_along(list), function(i) {
    if (is.data.frame(list[[i]])) {
      list_name <- names(list)[[i]]
      if (prefix != "") 
        prefix <- paste0(prefix, separation)
      if (suffix != "") 
        suffix <- paste0(separation, suffix)
      filename <- paste0(PATH_ERRORS, "/", prefix, list_name, 
                         suffix, ".xlsx")
      wb <- openxlsx::createWorkbook()
      name_worksheet <- paste("0", MONTH, sep = "")
      openxlsx::addWorksheet(wb, name_worksheet)
      openxlsx::writeData(wb, name_worksheet, list[[i]])
      num_cols_df <- length(list[[i]])
      num_rows_df <- nrow(list[[1]])
      head_style <- openxlsx::createStyle(fgFill = "#EEEEEE",
                                          fontName = "Calibri", fontSize = "11", halign = "center",
                                          valign = "center")
      openxlsx::addStyle(wb, sheet = name_worksheet, head_style,
                         rows = 1, cols = 1:num_cols_df)
      openxlsx::setColWidths(wb, name_worksheet, cols = c(1:num_cols_df), 
                             widths = "auto")
      
      id_changes <- which(!duplicated(list[[i]]$COD_ID))
      
      number_cod_id <- length(id_changes)
      
      # colors_to_use <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
      # colors_to_use <- sample(colors_to_use, number_cod_id)
      
      # colors_to_use <- grDevices::gray.colors(number_cod_id, start = 0.5)
      
      
      counter <- 1
      
      for (r in id_changes){
        
        # style_cells <- openxlsx::createStyle(fgFill = colors_to_use[counter])
        
        stile_first_row <- openxlsx::createStyle(border = "top",
                                                 borderColour = "black",
                                                 borderStyle = "medium")
        
        if (!is.na(id_changes[r+1])) {
          rows <- (r+1):id_changes[counter+1]
        } else if (is.na(id_changes[r+1])){
          rows <- (r+1):num_rows_df
        } else {
          stop("Error: see function exportErrorsList.")
        }
        # openxlsx::addStyle(wb, sheet = name_worksheet, style_cells,
        #                    rows = rows, cols = 1:num_cols_df, gridExpand=T, stack = T)
        
        openxlsx::addStyle(wb, sheet = name_worksheet, stile_first_row,
                           rows = r+1, cols = 1:num_cols_df, gridExpand=T, stack = T)
        
        counter <- counter+1
      }
      
      Sys.setenv(R_ZIPCMD = "C:/Rtools/bin/zip.exe")
      openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
    }
    else {
      return(paste("This isn't a dataframe"))
    }
  })
}


# Export to google drive -------------------------------------------------------
# Export the dataframes contained in a list to google drive
exportListToGoogleSheet <- function(list, prefix = "", suffix = "", separation = ""){
  
  #check if package openxlsx is instaled:
  if (!requireNamespace("googlesheets", quietly = TRUE)) {
    stop("Googlesheets package needed for this function to work. Please install it.",
         call = FALSE)
  }
  
  # sep_along(list): generate regular sequences. With a list, generates
  # the sequence 1, 2, ..., length(from). Return a integer vector.
  lapply(seq_along(list), function(i){
    
    
    if(is.data.frame(list[[i]])){
      
      list_name <- names(list)[[i]]
      
      if (prefix != "") prefix <- paste0(prefix, separation)
      
      if (suffix != "") suffix <- paste0(separation, suffix)
      
      # Before export to google drive, is mandatory export file to csv in local:
      # When the googlesheet4 packages have the oauth implemented, we can
      # use it instead of googledrive package. Googlesheet4 already have the
      # oauth implemented --> CHANGE!!!
      filename <- paste0(PATH_ERRORS, "/", prefix, list_name, suffix, '.csv')
      
      write.table(
        list[[i]], 
        file = filename, 
        quote = FALSE, 
        sep = ",", 
        dec = ".", 
        row.names = FALSE,
        na = "")
      
      # export to google drive
      google_drive_path <- paste0(GOOGLE_DRIVE_PATH, list_name, "/")
      
      
      drive_upload(
        media = filename,
        path = google_drive_path,
        type = "spreadsheet"
      )
      
    } else {
      return(paste("This isn't a dataframe"))
    }
    
  })
}

#' Check code: 1061
#' Check if there are samples with the same name vessel but with differente
#' SIRENO codification or SGPM codification.
#' 
#' @return dataframe with errors
checkMultipleShipCode <- function(catches){
  
  # find the ship names with more than one COD_BARDO and CODSGPM
  dsn <- catches %>%
    select(BARCO,COD_BARCO, CODSGPM) %>%
    unique()%>%
    count(BARCO) %>%
    filter(n>1)
  
  dsn <- as.character(dsn$BARCO)
  
  # get the samples with the multiple ships names
  err <- catches[catches$BARCO %in% dsn,]
  
  err <- err[, BASE_FIELDS]
  err <- addTypeOfError(err, "ERROR: Hay varios muestreos que tienen este mismo nombre de barco, pero con distinto código SIRENO o distinto Código Secretaría. ¿Seguro que es el barco correcto?")
  
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
#' @return dataframe with errors
allCategoriesMeasured <- function(df_catches, df_lengths_sampled){
  
  # Some categories never are measured, so must be ignored
  regex_to_ignore <- c("^.*(?i)colas.*",
                       "^.*(?i)melada$",
                       "^.*trozos$",
                       "^(?i)alas\ .*$"
  )
  
  regex_to_ignore <- paste(regex_to_ignore, collapse = "|")
  
  # Clean the categories master
  maestro_categorias_clean <- maestro_categorias[, c("ESPCOD", "PUECOD", "ESPCAT", "TIPPROCOD")]
  
  # Clean catches dataframe
  clean_catches <- df_catches[ which(df_catches$COD_TIPO_MUE == "2"), ]
  
  clean_catches <- merge(clean_catches, maestro_categorias_clean,
                         by.x = c("COD_ESP_MUE", "COD_PUERTO", "COD_CATEGORIA"),
                         by.y = c("ESPCOD", "PUECOD", "ESPCAT"),
                         all.x = T)
  
  
  clean_catches <- clean_catches[ -grep(regex_to_ignore, clean_catches$CATEGORIA),]
  
  # Get the number of categories in catches dataframe
  cat_cat <- clean_catches %>%
    group_by(COD_ID, COD_ESP_MUE) %>%
    mutate( n_cat_catches = n_distinct(COD_CATEGORIA)) %>%
    select(COD_ID, FECHA_MUE, COD_BARCO, COD_ESP_MUE, n_cat_catches) %>%
    unique()
  
  # Get the number of categories with sampled lengths
  sam_cat_len <- df_lengths_sampled %>%
    group_by(COD_ID, COD_ESP_MUE) %>%
    mutate( n_cat_lengths = n_distinct(COD_CATEGORIA)) %>%
    select(COD_ID, COD_ESP_MUE, n_cat_lengths) %>%
    unique()
  
  # Get errors
  
  errors <- merge(cat_cat, sam_cat_len, all.x = T, by.x = c("COD_ID", "COD_ESP_MUE"))
  
  errors <- errors[which(errors[["n_cat_catches"]] >1 &
                           errors[["n_cat_lengths"]] < errors[["n_cat_catches"]]),]
  
  errors <- addTypeOfError(errors, "WARNING: some species in the category are measured but others are not")
  
  errors <- errors[, c("COD_ID", "COD_ESP_MUE", "FECHA_MUE", "COD_BARCO", "TIPO_ERROR")]
  
  return(errors)
  
}