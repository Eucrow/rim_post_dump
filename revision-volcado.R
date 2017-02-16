#### ---------------------------------------------------------------------------
#### Check monthly data recorded in SIRENO
#### 
#### Return xls or upload to google docs files with errors detected by influence
#### area
####
#### author: Marco A. Amez Fernandez
#### email: ieo.marco.a.amez@gmail.com
####
#### files required: especies_no_permitidas.csv,
#### generos_permitidos.csv, report 'tallas por UP' from SIRENO
# ------------------------------------------------------------------------------



# ------------------------------------------------------------------------------
# #### PACKAGES ################################################################
# ------------------------------------------------------------------------------

library(plyr)
library(dplyr) #arrange_()
library(tools) #file_path_sans_ext()
library(devtools)

# ---- install googlesheets from github
#install_github("jennybc/googlesheets")
#library(googlesheets)
#suppressMessages(library(dplyr)) #What is suppressMessages????


# ---- install sapmuebase from local
#install("F:/misdoc/sap/sapmuebase")
library(sapmuebase)



# ------------------------------------------------------------------------------
# #### SET WORKING DIRECTORY ###################################################
# ------------------------------------------------------------------------------

setwd("F:/misdoc/sap/revision volcado/revision_volcado_R/")


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# YOU HAVE ONLY TO CHANGE THIS VARIABLES 

PATH_FILES <- "F:/misdoc/sap/revision volcado/datos/anual2016"
FILENAME_DES_TOT <- "IEOUPMUEDESTOTSIRENO_con_errores.TXT"
FILENAME_DES_TAL <- "IEOUPMUEDESTALSIRENO.TXT"
FILENAME_TAL <- "IEOUPMUETALSIRENO.TXT"

MONTH <- 12 #only if a filter by month is necesary. It's imperative use the atributte 'by_month' in import_muestreos_up() function
YEAR <- "2016"

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------



# ------------------------------------------------------------------------------
# #### GLOBAL VARIABLES ########################################################
# ------------------------------------------------------------------------------

# list with all errors found in dataframes:
ERRORS <- list()

# list with the errors:
MESSAGE_ERRORS<- list()

# list with the common fields used in all tables
BASE_FIELDS <- c("COD_ID", "COD_PUERTO", "PUERTO", "LOCODE", "FECHA", "COD_BARCO", "BARCO", "ESTRATO_RIM", "COD_TIPO_MUE", "TIPO_MUE")

# useful paths
PATH_ERRORS <- paste(PATH_FILES,"/errors",sep="")
PATH_BACKUP <- paste(PATH_ERRORS, "/backup", sep="")

# month as character with a 0 at the left if it is less than 10
MONTH_AS_CHARACTER <- sprintf("%02d", MONTH)


# ------------------------------------------------------------------------------
# #### FUNCTIONS ###############################################################
# ------------------------------------------------------------------------------

# ---- function to make a backkup of the errors files --------------------------
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



#function to add variable with type of error to a dataframe --------------------
addTypeOfError <- function(df, type){
  
  if(nrow(df)!=0){
    df[["TIPO_ERROR"]] <- type
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
formatErrorsList <- function(errors_list = ERRORS){
  
  # Combine all the dataframes of ERRORS list:
  # Reduce uses a binary function to successively combine the elements of a
  # given vector. In this case, merge the dataframes in the ERRORS list
  #errors <- Reduce(function(x, y) merge(x, y, all=TRUE), errors_list)
  
  #better with join_all form plyr package because doesn't change the order of columns:
  errors <- join_all(errors_list, type = "full")
  
  # Separate dataframe by influece area
  errors <- separateDataframeByInfluenceArea(errors, "COD_PUERTO")
  
  # Order the errors and remove columns with only NA values
  errors <- lapply(errors, function(x){
    
    # Order columns
    x <- x %>%
      select(AREA_INF, COD_ID, COD_PUERTO, PUERTO, FECHA, COD_BARCO, BARCO, ESTRATO_RIM,
             TIPO_MUE, COD_ESP_MUE, ESP_MUE, COD_CATEGORIA, CATEGORIA, P_DESEM, 
             P_VIVO, COD_ESP_CAT, ESP_CAT, SEXO, everything()) %>%
             select(-one_of("TIPO_ERROR"), one_of("TIPO_ERROR")) %>% #remove TIPO_ERROR, and add it to the end
             mutate(FECHA = as.Date(FECHA, "%d-%m-%y")) %>%
             arrange_( "COD_PUERTO", "FECHA", "COD_ESP_MUE", "COD_CATEGORIA", "COD_ESP_CAT")
    
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
coherenceEstratoRimGear <- function(df){
  merge_estrato_rim_gear<-merge(x=df, y=CORRECT_ESTRATORIM_ARTE, by.x = c("ESTRATO_RIM","ARTE"), by.y = c("ESTRATO_RIM", "ARTE"), all.x = TRUE)
  incoherent_data <- -which(merge_estrato_rim_gear[["VALID"]])
  incoherent_data <- merge_estrato_rim_gear[incoherent_data,c(BASE_FIELDS, "ARTE")]
  incoherent_data <- unique(incoherent_data)
  incoherent_data <- addTypeOfError(incoherent_data, "ERROR: no concuerda el estrato_rim con el arte")
  return(incoherent_data)
}


# Function to search errors in number of ships (empty field, =0 or >2) ---------
numberOfShips <- function (){
  errors <- catches[catches["N_BARCOS"] == 0 | catches["N_BARCOS"] > 2 | is.null(catches["N_BARCOS"]), c(BASE_FIELDS, "N_BARCOS")]
  errors <- addTypeOfError(errors, "WARNING: número de barcos igual a 0 o mayor de 2")
  return (errors)
}


# Function to search errors in number of rejects (only empty fields) -----------
numberOfRejections <- function(){
  errors <- catches[is.null(catches["N_RECHAZOS"]), c(BASE_FIELDS, "N_RECHAZOS")]
  errors <- addTypeOfError(errors, "ERROR: número de rechazos sin rellenar")
  return(errors)
}


# Function to search samples with SOP > P_MUE_VIVO -----------------------------
sopGreaterPesMueVivo <- function(df){
  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "COD_ESP_CAT", "ESP_CAT", "P_MUE_VIVO", "SOP")
  errors <- df %>%
              select(one_of(selected_fields))%>%
              mutate(DIF_SOP_P_MUE_VIVO = SOP - P_MUE_VIVO )%>%
              mutate(DIF_SOP_P_MUE_VIVO = round(DIF_SOP_P_MUE_VIVO,2))%>%
              filter(DIF_SOP_P_MUE_VIVO,2 > 0.01 )
  errors <- addTypeOfError(errors, "ERROR: SOP mayor que peso muestreado vivo")
  return (errors)
}


# Function to search samples with SOP = 0 --------------------------------------
sopZero <- function(df){
  fields_to_select <- c(BASE_FIELDS, "SOP")
  errors <- df %>%
    select(one_of(fields_to_select)) %>%
    filter(SOP == 0)
  errors <- addTypeOfError(errors, "ERROR: SOP igual a 0")
  return (errors)
}

#function to search samples with P_MUE_DESEM = 0 or NA -------------------------
pesMueDesemZero <- function(df){
  #errors <- df[df["P_MUE_DESEM"] == 0 | is.na(df["P_MUE_DESEM"]),]
  fields_to_select <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA",
                        "CATEGORIA", "COD_ESP_CAT", "ESP_CAT", "P_MUE_DESEM")
  errors <- df %>%
    filter(P_MUE_DESEM == 0 | is.na(P_MUE_DESEM)) %>%
    select(one_of(fields_to_select))
  
  errors <- addTypeOfError(errors, "ERROR: Peso muestreado desembarcado es 0")
  
  return(errors)
}


#function to search samples with p_desem <= p_mue_desem ------------------------
pesMueDesemGreaterPesDesem <- function (df){
  fields_to_select <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA",
                        "CATEGORIA", "COD_ESP_CAT", "ESP_CAT", "SOP", "P_DESEM",
                        "P_MUE_DESEM", "DIF_P_MUE_DESEM_P_DESEM")
  
  errors <- df %>%
    mutate(DIF_P_MUE_DESEM_P_DESEM = P_MUE_DESEM - P_DESEM) %>%
    filter(DIF_P_MUE_DESEM_P_DESEM > 0) %>%
    select(one_of(fields_to_select))
  
  errors <- addTypeOfError(errors, "ERROR: Peso muestreado mayor al peso desembarcado")
  
  return(errors)
}


# function to check samples with SOP > P_VIVO ----------------------------------
SopGreaterPesVivo <- function (df){
  fields_to_select <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA",
                        "CATEGORIA", "COD_ESP_CAT", "ESP_CAT", "SOP", "P_VIVO",
                        "DIF_SOP_P_VIVO")
  
  errors <- df %>%
    mutate(DIF_SOP_P_VIVO = SOP - P_VIVO) %>%
    mutate(DIF_SOP_P_VIVO = round(DIF_SOP_P_VIVO,2)) %>%
    filter(DIF_SOP_P_VIVO > 0.01) %>%
    select(one_of(fields_to_select))
  
  errors <- addTypeOfError(errors, "ERROR: SOP mayor que peso vivo desembarcado")
  
  return(errors)
}


#function to search categories with equal species weight landings --------------
speciesWithCategoriesWithSameWeightLanding <- function(df){
  df <- df[,c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "P_DESEM")]
  fields_to_count <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "P_DESEM")
  errors <- df %>% 
    distinct() %>%
    count_(fields_to_count) %>%
    filter(n>1)
  colnames(errors)[names(errors) == "n"] <- "NUM_OCU_CAT_MISMO_PESO_DESEM"
  errors <- addTypeOfError(errors, "WARNING: varias categorías con igual peso desembarcado")
  return(errors)
}

# function to search samples with not allowed species of the category. This function
# use the not allowed species and the allowed genus dataframes of SAPMUEBASE.
# Search, too, the genus finished in -formes, -dae, - spp and - sp.
categorySpeciesNotAllowed <- function(){
  
  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "COD_ESP_CAT", "ESP_CAT")
  
  # create a dataframe with species not allowed
  not_allowed <- merge(x = catches_in_lengths, y = NOT_ALLOWED_SPECIES, by.x = "COD_ESP_CAT", by.y = "COD_ESP") %>%
    select(one_of(selected_fields))
  
  #create a dataframe with other species not allowed
  # by sufixes
  to_check_genus <- grep("(.+(formes$))|(.+(spp$))|(.+(sp$))|(.+(dae$))",catches_in_lengths$ESP_CAT)
  # . = any single character
  # + = one of more of previous
  # | = or
  
  genus_not_allowed <- catches_in_lengths[to_check_genus,]
  
  # remove other allowed species from the dataframe with not allowed species
  genus_not_allowed <- genus_not_allowed[!(genus_not_allowed[["COD_ESP_CAT"]] %in% ALLOWED_GENUS[["COD_ESP"]]),] %>%
    select(one_of(selected_fields))
  
  errors <- rbind(not_allowed, genus_not_allowed)
  
  # remove duplicates
  errors <- unique(errors)  %>%
    arrange_(BASE_FIELDS)
  
  errors <- addTypeOfError(errors, "ERROR: muestreos con especies no permitidos en Especies de la Categoría")
  
  return(errors)
}


# function to check foreings ships in MT1 samples ------------------------------
# df: dataframe
check_foreing_ships_MT1 <- function(df){
  ships <- df %>%
    select(one_of(BASE_FIELDS))%>%
    filter(grepl("^8\\d{5}",COD_BARCO)) %>%
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


# function to check ships not in "ALTA DEFINITIVA", "ALTA PROVISIONAL POR NUEVA
# CONSTRUCCI?N or ALTA PROVISIONAL POR REACTIVACI?N in CFPO --------------------
shipsNotRegistered <- function(df, cfpo = CFPO){
  
  to_ships <- unique(df[,c(BASE_FIELDS, "CODSGPM")])
  errors_ships <- merge(x=to_ships, y=cfpo, by.x = "CODSGPM", by.y = "CODIGO_BUQUE", all.x = TRUE)
  errors_ships <- errors_ships %>%
    filter( ESTADO != "ALTA DEFINITIVA" &
              ESTADO != "H - A.P. POR REACTIVACION" &
              ESTADO != "G - A.P. POR NUEVA CONSTRUCCION" )
  text_type_of_error <- paste0("WARNING: barco incluido en el CFPO pero con un estado distinto a Alta Definitiva, A. P. Por Reactivación, o A.P Por Nueva Construcción")
  errors_ships <- addTypeOfError(errors_ships, text_type_of_error)
  return (errors_ships)
}


# function to check ships not in CFPO
shipsNotInCFPO <- function(df){
  
  to_ships <- unique(df[,c(BASE_FIELDS, "CODSGPM")])
  
  errors_ships <- merge(x=to_ships, y=CFPO, by.x = "CODSGPM", by.y = "CODIGO_BUQUE", all.x = TRUE)
  errors_ships <- errors_ships %>%
    filter(is.na(ESTADO))
  errors_ships <- addTypeOfError(errors_ships, "WARNING: barco no incluido en el CFPO")
  return (errors_ships)
}

check_mt2b <- function(){
  
  # select all the mt2b samples
  mt2b <- catches %>%
            filter(COD_TIPO_MUE == 4) %>%
            select_(.dots = BASE_FIELDS)
  
  # select all the samples with lengths
  mt2b_with_lenghts <- lengths %>%
    filter(COD_TIPO_MUE == 4)  %>%
    group_by_(.dots = BASE_FIELDS) %>%
    summarise(summatory = sum(EJEM_MEDIDOS, na.rm = TRUE))
  
  false_mt2b <- anti_join(x = mt2b, y = mt2b_with_lenghts, by = c("FECHA","COD_BARCO")) %>% unique()
  false_mt2b <- addTypeOfError(false_mt2b, "ERROR: MT2b sin tallas")
  
  return(false_mt2b)
}


# function to check the samples with categories which all of this species has the same sampled weight
allCategoriesWithSameSampledWeight <- function (){
  
  selected_fields<-c(BASE_FIELDS,"N_CATEGORIAS","COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA","CATEGORIA", "P_MUE_DESEM", "SEXO")
  
  by_category <- catches_in_lengths %>%
    group_by_(.dots = selected_fields) %>%
    #tally() %>% # tally() is the same that summarise(num = n())
    summarise(num_cat_igual_peso=n())%>%
    filter(num_cat_igual_peso > 1)
}

# function to check samples with weight sampled = 0 with lenghts
weightSampledZeroWithLengthsSampled <- function () {
  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "P_DESEM", "P_VIVO", "COD_ESP_CAT", "ESP_CAT", "P_MUE_DESEM", "P_MUE_VIVO", "SOP")
  err <- catches_in_lengths %>%
    filter(P_MUE_DESEM == 0) %>%
    select(one_of(selected_fields)) %>%
    addTypeOfError("ERROR: peso muestra 0 con tallas muestreadas")
}

# function to check samples with weight landed = 0 or NA
weightLandedZero <- function () {
  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "P_DESEM", "P_VIVO")
  err <- catches %>%
    filter(P_DESEM == 0 | is.na(P_DESEM)) %>%
    select(one_of(selected_fields)) %>%
    addTypeOfError("ERROR: peso desembarcado = 0")
  return(err)
}

# function to check samples without lengths but with weight sampled
weightSampledWithoutLengthsSampled <- function () {
  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "P_DESEM", "P_VIVO", "COD_ESP_CAT", "ESP_CAT", "P_MUE_DESEM", "EJEM_MEDIDOS")
  err <- catches_in_lengths %>%
    filter(P_MUE_DESEM != 0 & (EJEM_MEDIDOS == 0 | is.na(EJEM_MEDIDOS))) %>%
    select(one_of(selected_fields)) %>%
    addTypeOfError("ERROR: especie sin tallas muestreadas pero con peso muestra")
  return(err)
}





# ------------------------------------------------------------------------------
# #### REPEATED FUNCTIONS FROM SAVED PREVIOUS CHECK ############################
# ------------------------------------------------------------------------------

# ---- function to ckeck variables ---------------------------------------------
#' Check variables
#
#' Check if the value of variables are consistents to the value in its SIRENO master.
#' It's only available for variables with a data source (master): ESTRATO_RIM, COD_PUERTO,
#' COD_ORIGEN, COD_ARTE, COD_PROCEDENCIA and TIPO_MUESTREO
#' @param variable: one of this values: ESTRATO_RIM, COD_PUERTO, COD_ORIGEN,
#' COD_ARTE, COD_PROCEDENCIA or TIPO_MUESTREO
#' @return Return a dataframe with samples containing erroneus variables
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
  name_data_set <- tolower(variable_formatted)
  #search the errors in variable
  errors <- anti_join(df, get(name_data_set), by = variable)

  #prepare to return
  fields_to_filter <- c("COD_ID", "COD_PUERTO", "PUERTO", "FECHA", "COD_BARCO", variable)

  errors <- errors %>%
              select(one_of(fields_to_filter))%>%
              unique()
  
  
  text_type_of_error <- paste0("ERROR: ", name_data_set, " no concuerda con los maestros de SIRENO")
  errors <- addTypeOfError(errors, text_type_of_error)
  
  #return
  return(errors)
}

# function to search false mt2 samples: samples with COD_TIPO_MUE as MT2A and
# without any lenght
# df: dataframe
# return: dataframe with erroneus samples
check_false_mt2 <- function(){

  #Select all the samples with COD_TIPO_MUE = MT2
  mt2 <- catches %>%
          filter(COD_TIPO_MUE == 2)  %>%
          select_(.dots = BASE_FIELDS) %>%
          unique()
  
  # select all the samples with lengths
  mt2_with_lenghts <- lengths %>%
          filter(COD_TIPO_MUE == 2)  %>%
          group_by_(.dots = BASE_FIELDS) %>%
          summarise(summatory = sum(EJEM_MEDIDOS, na.rm = TRUE))
  
  # check if all the samples keyed as MT2 has lenghts
  false_mt2 <- anti_join(x = mt2, y = mt2_with_lenghts, by = c("FECHA","COD_BARCO")) %>% unique()
  false_mt2 <- addTypeOfError(false_mt2, "ERROR: MT2 sin tallas")

  return(false_mt2)  

}

# function to search false mt1 samples: samples with COD_TIPO_MUE as MT1A and
# lenghts
# df: dataframe
# return: dataframe with erroneus samples
check_false_mt1 <- function(){
  
  #Select all the samples with COD_TIPO_MUE = MT1
  mt1 <- catches %>%
    filter(COD_TIPO_MUE == 1)  %>%
    select_(.dots = BASE_FIELDS) %>%
    unique()
  
  # select all the samples with lengths
  mt1_with_lenghts <- lengths %>%
    filter(COD_TIPO_MUE == 1)  %>%
    group_by_(.dots = BASE_FIELDS) %>%
    summarise(summatory = sum(EJEM_MEDIDOS, na.rm = TRUE))
  
  # check if all the samples keyed as MT1 hasn't lenghts
  false_mt1 <- merge(x = mt1, y = mt1_with_lenghts, by = BASE_FIELDS) %>% unique()
  false_mt1 <- addTypeOfError(false_mt1, "ERROR: MT1 con tallas")
  
  return(false_mt1)  
}

# function to check mixed species keyed as non mixed species: in COD_ESP_MUE
# there are codes from non mixed species
# df: dataframe
# return a dataframe with the samples with species keyed as non mixed species
check_mixed_as_no_mixed <- function(){
  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE")
  non_mixed <- merge(x=catches, y=especies_mezcla["COD_ESP_CAT"], by.x = "COD_ESP_MUE", by.y = "COD_ESP_CAT") %>%
                select_(.dots = selected_fields)
  non_mixed <- addTypeOfError(non_mixed, "ERROR: especie de mezcla tecleada sin agrupar en Especies del Muestreo")
  return(non_mixed)
}

# function to check no mixed species keyed as mixed species: in COD_ESP_MUE
# there are codes from mixed species
# df: dataframe
# return a dataframe with the samples with species keyed as non mixed species
check_no_mixed_as_mixed <- function(){
  selected_fields <- c(BASE_FIELDS,"COD_ESP_MUE", "ESP_MUE", "COD_ESP_CAT", "ESP_CAT")
  non_mixed <- merge(x=lengths, y=especies_no_mezcla["COD_ESP"], by.x = "COD_ESP_MUE", by.y = "COD_ESP") %>%
    select_(.dots = selected_fields) %>%
    unique()
  non_mixed <- addTypeOfError(non_mixed, "ERROR: especie no de mezcla agrupada en Especies del Muestreo")
  return(non_mixed)
}

# function to check grouped species in Species of the Category
# return a dataframe with the samples with grouped species in Species of the Category
mixedSpeciesInCategory <- function(){
  
  selected_fields<-c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "COD_ESP_CAT", "ESP_CAT")
  
  not_allowed_in_category <- especies_mezcla %>%
    select(COD_ESP_MUE) %>%
    unique()
  
  clean_catches_in_lenghts <- catches_in_lengths %>%
    select(one_of(selected_fields))
  
  errors <- merge(x=clean_catches_in_lenghts, y=not_allowed_in_category, by.x = "COD_ESP_CAT", by.y = "COD_ESP_MUE")
  
  errors <- addTypeOfError(errors, "ERROR: muestreo MT2 con especie de mezcla que está agrupada en Especies para la Categoría")
  
} 

# function to check if there are not allowed species in Sampling Species
sampledSpeciesNotAllowed <- function(){
  
  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE")
  
  # create a dataframe with species not allowed
  sampling_species_not_allowed <- merge(x = catches, y = NOT_ALLOWED_SPECIES, by.x = "COD_ESP_MUE", by.y = "COD_ESP") %>%
    select(one_of(selected_fields))
  
  #create a dataframe with other species not allowed
  # by sufixex
  to_check_genus <- grep("(.+(formes$))|(.+(spp$))|(.+(sp$))|(.+(dae$))",catches$ESP_MUE)
  # . = any single character
  # + = one of more of previous
  # | = or
  
  genus_not_allowed <- catches[to_check_genus,]
  
  # remove the mixed species (allowed)
  genus_not_allowed <- genus_not_allowed[!(genus_not_allowed[["COD_ESP_MUE"]] %in% unique(mixed_species[["COD_ESP_MUE"]])),]
  
  # remove other allowed species
  genus_not_allowed <- genus_not_allowed[!(genus_not_allowed[["COD_ESP_MUE"]] %in% ALLOWED_GENUS[["COD_ESP"]]),] %>%
    select(one_of(selected_fields))
  
  errors <- rbind(sampling_species_not_allowed, genus_not_allowed)
  
  # remove duplicates
  errors <- unique(errors)
  
  errors <- addTypeOfError(errors, "ERROR: Muestreo con especie no permitida en Especies del Muestreo")
  
  return(errors)
}

# ------------------------------------------------------------------------------
# #### IMPORT DATA #############################################################
# ------------------------------------------------------------------------------

#read the mixed species dataset
mixed_species <- especies_mezcla

#read the no mixed species dataset
sampled_spe_no_mixed <- especies_no_mezcla

#read the estrato-rim - arte dataset to obtain the correct estratorim, gear and its relation
CORRECT_ESTRATORIM_ARTE <- estratorim_arte
CORRECT_ESTRATORIM_ARTE$VALID<-TRUE

###obtain the not allowed species
NOT_ALLOWED_SPECIES <- read.csv("especies_no_permitidas.csv", fileEncoding = "UTF-8")

### obtain the allowed genus
ALLOWED_GENUS <- read.csv("generos_permitidos.csv")

### obtain the cfpo
CFPO <- cfpo2015
  # ignore useless columns
  CFPO <- CFPO[,c("CODIGO_BUQUE", "ESTADO")]
  

  
# ------------------------------------------------------------------------------ 
# #### IMPORT muestreos_UP files ###############################################
# ------------------------------------------------------------------------------
  
muestreos_up <- importMuestreosUP(FILENAME_DES_TOT, FILENAME_DES_TAL, FILENAME_TAL, path = PATH_FILES) #by_month = MONTH, 


#isolate dataframes
catches <- muestreos_up$catches
catches_in_lengths <- muestreos_up$catches_in_lengths
lengths <- muestreos_up$lengths


# ------------------------------------------------------------------------------
# #### SEARCHING ERRORS ########################################################
# ------------------------------------------------------------------------------

# ---- REPEATED IN IPDTOSIRENO ----

ERRORS$estrato_rim <- check_variable_with_master(catches, "ESTRATO_RIM")

ERRORS$puerto <- check_variable_with_master(catches, "COD_PUERTO")

ERRORS$arte <- check_variable_with_master(catches, "COD_ARTE")

ERRORS$origen <- check_variable_with_master(catches, "COD_ORIGEN")

ERRORS$procedencia <- check_variable_with_master(catches, "PROCEDENCIA")

ERRORS$tipo_muestreo <- check_variable_with_master(catches, "COD_TIPO_MUE")

ERRORS$false_MT1 <- check_false_mt1()

ERRORS$false_MT2 <- check_false_mt2()

ERRORS$no_mixed_as_mixed <- check_no_mixed_as_mixed()

ERRORS$mixed_as_no_mixed <- check_mixed_as_no_mixed()


# ---- IN HEADER ----

ERRORS$false_mt2b <- check_mt2b()

ERRORS$coherence_estrato_rim_gear <- coherenceEstratoRimGear(catches)

ERRORS$number_of_ships <- numberOfShips()

ERRORS$number_of_rejections <- numberOfRejections()

ERRORS$errors_countries_mt1 <- check_foreing_ships_MT1(catches)

ERRORS$errors_countries_mt2 <- check_foreing_ships_MT2(catches)

##### TO DO: ADD CHECKING SHIPS WITH SIRENO FILES

ERRORS$errors_ships_not_in_cfpo <-shipsNotInCFPO(catches)
        
ERRORS$errors_ships_not_registered <- shipsNotRegistered(catches)

##### TO DO: estrato_rim, gear and division coherence


# ---- IN SPECIES ----

ERRORS$mixed_species_category <- mixedSpeciesInCategory()
      
ERRORS$not_allowed_sampled_species <- sampledSpeciesNotAllowed()

ERRORS$not_allowed_category_species <- categorySpeciesNotAllowed()

# ---- IN WEIGHTS ----

ERRORS$same_sampled_weight <- allCategoriesWithSameSampledWeight()
  
ERRORS$sampled_weight_zero <- weightSampledZeroWithLengthsSampled()
    
ERRORS$weight_landed_zero <- weightLandedZero()

ERRORS$weight_sampled_without_length_sampled <- weightSampledWithoutLengthsSampled()


REMOVE USELESS ARGUMENTS!!!!
    
ERRORS$pes_mue_desem_zero <- pesMueDesemZero(catches_in_lengths)

ERRORS$especies_con_categorias_igual_peso_desembarcado <- speciesWithCategoriesWithSameWeightLanding(catches)
        
ERRORS$sop_zero <- sopZero(catches_in_lengths) 
     
ERRORS$sop_greater_pes_mue_vivo <- sopGreaterPesMueVivo(catches_in_lengths)

ERRORS$sop_mayor_peso_vivo <- SopGreaterPesVivo(catches_in_lengths)

ERRORS$pes_mue_desem_mayor_pes_desem <- pesMueDesemGreaterPesDesem (lengths)


# ------------------------------------------------------------------------------    
# #### COMBINE ERRORS ##########################################################
# ------------------------------------------------------------------------------

    combined_errors <- formatErrorsList()

# ------------------------------------------------------------------------------
# #### EXPORT ERRORS ###########################################################
# ------------------------------------------------------------------------------
    
    #exportListToCsv(combined_errors, suffix = paste0(YEAR,"_",MONTH_AS_CHARACTER), separation = "_")

    #exportListToXlsx(combined_errors, suffix = paste0("errors", "_", YEAR,"_",MONTH_AS_CHARACTER), separation = "_")
    
    exportListToXlsx(combined_errors, suffix = paste0("errors", "_", YEAR), separation = "_")
       
    #exportListToGoogleSheet( combined_errors, suffix = paste0("errors", "_", YEAR,"_",MONTH_AS_CHARACTER), separation = "_" ) 
    
    #lapply(names(ERRORS), export_errors_lapply, ERRORS) #The 'ERRORS' argument is an argument to the export_errors_lapply function

# ------------------------------------------------------------------------------    
# #### MAKE A BACKUP
# ------------------------------------------------------------------------------
    # backup_files()
    