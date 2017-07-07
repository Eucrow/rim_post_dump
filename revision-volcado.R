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
####
#### To install (with devtools library loaded):
#### install_github("Eucrow/revision-volcado.R")
#### ---------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# #### INSTRUCTIONS ############################################################
# ------------------------------------------------------------------------------

# To use this scritp:

# - Change variables in "YOU HAVE ONLY TO CHANGE THIS VARIABLES" section of this
# script.
# - Make sure report files tallas_x_up from SIRENO are in PATH_FILES.
# - Choose the way to export in the "EXPORT ERRORS" section of this script.
# Uncomment the interested way. It's available in xlsx file or upload directly
# to google drive. In this case an account and password is required, and a token
# is automatically generated.
# - If xlsx option is choosen to export files, make sure a directory "errors" is
# in PATH_FILENAME path
# - Run all the script
# - A file by influence area is generated in "errors" directory.

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# YOU HAVE ONLY TO CHANGE THIS VARIABLES 

PATH_FILES <- "F:/misdoc/sap/revision volcado/datos/2017/2017-03"
FILENAME_DES_TOT <- "IEOUPMUEDESTOTMARCO.TXT"
FILENAME_DES_TAL <- "IEOUPMUEDESTALMARCO.TXT"
FILENAME_TAL <- "IEOUPMUETALMARCO.TXT"

MONTH <- 3 # month in numeric or FALSE for a complete year 
YEAR <- "2017"

# ------------------------------------------------------------------------------
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
#install_github("Eucrow/sapmuebase")
#install("F:/misdoc/sap/sapmuebase")
library(sapmuebase)



# ------------------------------------------------------------------------------
# #### SET WORKING DIRECTORY ###################################################
# ------------------------------------------------------------------------------

setwd("F:/misdoc/sap/revision volcado/revision_volcado_R/")

# ------------------------------------------------------------------------------
# #### GLOBAL VARIABLES ########################################################
# ------------------------------------------------------------------------------

# list with all errors found in dataframes:
ERRORS <- list()


# list with the common fields used in all tables
BASE_FIELDS <- c("COD_ID", "COD_PUERTO", "PUERTO", "LOCODE", "FECHA", "COD_BARCO", "BARCO", "ESTRATO_RIM", "COD_TIPO_MUE", "TIPO_MUE")

# useful paths
PATH_ERRORS <- paste(PATH_FILES,"/errors", sep="")
PATH_BACKUP <- paste(PATH_ERRORS, "/backup", sep="")

# month as character
MONTH_AS_CHARACTER <- sprintf("%02d", MONTH)

# check the month is correct
# can be 1 to 12, or "annual" to check all the gear
# tryCatch(
#   {
#     if (grepl("^\\d{1}$", MONTH)) {
#       MONTH <- paste0("0",MONTH)
#     } else if (grepl("^\\d{2}$", MONTH)) {
#       if (as.integer(MONTH) < 0  | as.numeric(MONTH) > 13){
#         error_text <- paste0("MONTH = ", MONTH, "??? How many months has your planet?")
#         stop(error_text)
#       }
#     } else if (MONTH != "FALSE") {
#       error_text <- paste("You have to select a month in digits or FALSE for a complete year")
#       stop(error_text)
#     }
#   },
#   error = function(err) {
#     error_text <- paste0("C'mon boy... ", err)
#     message(error_text)
#   }
# )

# ------------------------------------------------------------------------------
# #### FUNCTIONS ###############################################################
# ------------------------------------------------------------------------------

# ---- function to make a backup of the errors files ---------------------------
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
addTypeOfError <- function(df, type){
  
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
  merge_estrato_rim_gear<-merge(x=df, y=CORRECT_ESTRATORIM_ARTE, by.x = c("ESTRATO_RIM", "ARTE"), by.y = c("ESTRATO_RIM", "ARTE"), all.x = TRUE)
  incoherent_data <- -which(merge_estrato_rim_gear[["VALID"]])
  incoherent_data <- merge_estrato_rim_gear[incoherent_data,c(BASE_FIELDS, "COD_ARTE.x", "ARTE")]
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


# Function to search errors in number of rejects (only empty, nas or null) -----
numberOfRejections <- function(){
  errors <- catches %>%
    filter(N_RECHAZOS == ""|is.na(N_RECHAZOS)|is.null(N_RECHAZOS))
  errors <- addTypeOfError(errors, "ERROR: número de rechazos sin rellenar")
  return(errors)
}


# Function to search samples with SOP > P_MUE_VIVO -----------------------------
sopGreaterPesMueVivo <- function(){
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
sopZero <- function(){
  fields_to_select <- c(BASE_FIELDS,"COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA",
                        "CATEGORIA", "COD_ESP_CAT", "ESP_CAT", "P_MUE_DESEM", "SOP")
  errors <- catches_in_lengths %>%
    select(one_of(fields_to_select)) %>%
    filter(SOP == 0)
  errors <- addTypeOfError(errors, "ERROR: SOP igual a 0")
  return (errors)
}

# function to search samples with P_MUE_DESEM = 0 or NA -------------------------
pesMueDesemZero <- function(){
  fields_to_select <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA",
                        "CATEGORIA", "COD_ESP_CAT", "ESP_CAT", "P_MUE_DESEM")
  errors <- catches_in_lengths %>%
    filter(P_MUE_DESEM == 0 | is.na(P_MUE_DESEM)) %>%
    select(one_of(fields_to_select))
  
  errors <- addTypeOfError(errors, "ERROR: Peso muestreado desembarcado es 0")
  
  return(errors)
}


# function to search samples with p_desem <= p_mue_desem ------------------------
pesMueDesemGreaterPesDesem <- function (){
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
sopGreaterPesVivo <- function (){
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
speciesWithCategoriesWithSameWeightLanding <- function(){
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

# function to check the samples with categories which all of this species has the same sampled weight
allCategoriesWithSameSampledWeights <- function (){
  
  selected_fields<-c(BASE_FIELDS,"N_CATEGORIAS","COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA","CATEGORIA", "P_MUE_DESEM", "SEXO")
  
  by_category <- catches_in_lengths %>%
    group_by_(.dots = selected_fields) %>%
    #tally() %>% # tally() is the same that summarise(num = n())
    summarise(NUM_ESP_CAT_MISMO_PESO_MUE=n())%>%
    filter(NUM_ESP_CAT_MISMO_PESO_MUE > 1) %>%
    addTypeOfError("WARNING: varias especies de la categoría con igual peso muestreado")
}

# function to search samples with doubtfull species of the category. This function
# use the allowed genus dataframe of SAPMUEBASE.
# Search, too, the genus finished in -formes, -dae, - spp and - sp.
doubtfulCategorySpecies <- function(){
  
  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "COD_ESP_CAT", "ESP_CAT")

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
  
  # remove duplicates
  errors <- unique(genus_not_allowed)  %>%
    arrange_(BASE_FIELDS)
  
  errors <- addTypeOfError(errors, "WARNING: ¿seguro que es esa especie en Espcies de la Categoría?")
  
  return(errors)
}

# function to search samples with not allowed species of the category. This function
# use the not allowed species dataframe of SAPMUEBASE.
notAllowedCategorySpecies <- function(){
  
  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "COD_ESP_CAT", "ESP_CAT")
  
  # create a dataframe with species not allowed
  not_allowed <- merge(x = catches_in_lengths, y = NOT_ALLOWED_SPECIES, by.x = "COD_ESP_CAT", by.y = "COD_ESP") %>%
    select(one_of(selected_fields))
  
  # remove duplicates
  errors <- unique(not_allowed)  %>%
    arrange_(BASE_FIELDS)
  
  errors <- addTypeOfError(errors, "ERROR: muestreos con especies no permitidos en Especies de la Categoría")
  
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


# function to check ships not in "ALTA DEFINITIVA", "ALTA PROVISIONAL POR NUEVA
# CONSTRUCCIÓN or ALTA PROVISIONAL POR REACTIVACIÓN in CFPO --------------------
shipsNotRegistered <- function(df, cfpo = CFPO){
  
  to_ships <- unique(df[,c(BASE_FIELDS, "CODSGPM")])
  errors_ships <- merge(x=to_ships, y=cfpo, by.x = "CODSGPM", by.y = "CODIGO_BUQUE", all.x = TRUE)
  errors_ships <- errors_ships %>%
    filter( ESTADO != "ALTA DEFINITIVA" &
              ESTADO != "H - A.P. POR REACTIVACION" &
              ESTADO != "G - A.P. POR NUEVA CONSTRUCCION" )
  text_type_of_error <- paste0("ERROR: barco incluido en el CFPO pero con un estado distinto a Alta Definitiva, A. P. Por Reactivación, o A.P Por Nueva Construcción")
  errors_ships <- addTypeOfError(errors_ships, text_type_of_error)
  return (errors_ships)
}


# function to check ships not in CFPO
shipsNotInCFPO <- function(df, cfpo = CFPO){
  
  to_ships <- unique(df[,c(BASE_FIELDS, "CODSGPM")])
  
  errors_ships <- merge(x=to_ships, y=cfpo, by.x = "CODSGPM", by.y = "CODIGO_BUQUE", all.x = TRUE)
  errors_ships <- errors_ships %>%
    filter(is.na(ESTADO))
  errors_ships <- addTypeOfError(errors_ships, "ERROR: barco no incluido en el CFPO")
  return (errors_ships)
}

# function to check if all the mt2b has lengths
checkMt2b <- function(){
  
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
  false_mt2b <- addTypeOfError(false_mt2b, "ERROR: MT2B sin tallas")
  
  return(false_mt2b)
}

#function to check if all mt2b has CERCO_GC or BACA_GC stratums
checkMt2bRimStratum <- function () {
  
  # select all the mt2b samples
  mt2b <- catches %>%
    filter(COD_TIPO_MUE == 4) %>%
    select_(.dots = BASE_FIELDS)
  
  err_stratum <- mt2b[!(mt2b[["ESTRATO_RIM"]] %in% c("CERCO_GC", "BACA_GC")),]
  
  err_stratum <- addTypeOfError(err_stratum, "ERROR: MT2B con estrato rim distinto a CERCO_GC y BACA_GC")

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


# function to check variables
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
  fields_to_filter <- c(BASE_FIELDS, variable, variable_formatted)

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

# function to check doubtful species in Sampling Species
doubtfulSampledSpecies <- function(){
  
  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE")
  
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

  # remove duplicates
  errors <- unique(genus_not_allowed)
  
  errors <- addTypeOfError(errors, "WARNING: ¿seguro que es esa especie en Especies del Muestreo?")
  
  return(errors)
}


# function to check if there are not allowed species in Sampling Species
notAllowedSampledSpecies <- function(){
  
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
sexesWithSameSampledWeight <- function (){
  
  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", 
                        "CATEGORIA", "COD_ESP_CAT", "ESP_CAT", "P_MUE_DESEM")
                        
  errors <- catches_in_lengths %>%
              group_by_(.dots=selected_fields)%>%
              summarise(NUM_SEXOS_MISMO_P_MUE_DESEM = n())%>%
              filter(NUM_SEXOS_MISMO_P_MUE_DESEM > 1) %>%
              addTypeOfError("WARNING: Sexos de una misma especie tienen exactamente el mismo peso muestra")
              
}


# function to check categories with varios equal sexes (both of them male or female)
categoriesWithRepeatedSexes <- function() {
  
  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", 
                       "CATEGORIA", "COD_ESP_CAT", "ESP_CAT", "SEXO")
  errors <- catches_in_lengths %>%
              filter(SEXO != "U") %>%
              group_by_(.dots = selected_fields) %>%
              summarise(NUM_MISMOS_SEXOS = n()) %>%
              filter(NUM_MISMOS_SEXOS != 1) %>%
              addTypeOfError("Categorías con varios sexos iguales (la misma especie con varias distribuciones de machos o hembras")

}

# function to check if the cod_id are correctly created
#' Check cod_id
#
#' Check if the cod_id are correctly variables.
#' Check the length of the code, the type of sample, and the port.
#' @return Return a dataframe with the erroneus codes.
checkCodId <- function() {
  
  # prepare dataframe
  cod_id <- catches %>%
    select(COD_ID, COD_PUERTO, TIPO_MUE, AÑO)%>%
    unique()%>%
    addInfluenceAreaVariable("COD_PUERTO")
  
  errors_cod_id <- list()
  
  # check year
  errors_cod_id[["year_cod_id"]] <- cod_id %>%
    mutate(year_id = substr(COD_ID, 1, 4)) %>%
    filter(year_id != AÑO)%>%
    addTypeOfError("incorrect year")
    
  # check number of characters of cod_id
  errors_cod_id[["lenght_cod_id"]] <- cod_id %>% 
    mutate(length_cod_id = nchar(as.character(COD_ID))) %>%
    filter(length_cod_id != 13)%>%
    addTypeOfError("code with less than 13 characteres")
  
  # check sample type
  errors_cod_id[["sample_type_cod_id"]] <- cod_id %>%
    mutate(
      type_id = substr(COD_ID, 8, 9),
      type_mue_cod = substr(TIPO_MUE, 3, 4),
      check_tipo_mue = ifelse (type_id == type_mue_cod, "correct", "incorrect")
    ) %>%
    filter(check_tipo_mue == "incorrect") %>%
    addTypeOfError("incoherent sample type")
  
  # check the field port
  errors_cod_id[["field_port_cod_id"]] <- cod_id %>%
    mutate(puerto_id = substr(COD_ID, 5, 7 )) %>%
    mutate(
      check_puerto = ifelse ( puerto_id == "CAD" & AREA_INF == "GC", "correcto",
                              ifelse ( puerto_id == "LCG" & AREA_INF == "GN", "correcto",
                                       ifelse (puerto_id == "SDR" & AREA_INF == "AC", "correcto",
                                               ifelse(puerto_id == "VGO" & AREA_INF == "GS", "correcto", "incorrect")
                                       )
                              )
      )
    ) %>%
    filter(check_puerto == "incorrect") %>%
    addTypeOfError("incoherent port")
  
  # combine list errors_cod_id
  errors <- Reduce(function(x, y) { merge(x, y, all = TRUE)} , errors_cod_id) %>%
    # NOSE QUE ES ESE CAMPO TIPO_ERROR... 
    select(COD_PUERTO, COD_ID, TIPO_MUE, AÑO, TIPO_ERROR, AREA_INF)%>%
    arrange(COD_ID, TIPO_ERROR)
    # arrange(COD_ID)
  return(errors)
  
}

# ---- function to fix TALL.PESO variable --------------------------------------
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

# ---- function to check TALL.PESO variable ------------------------------------
#
#' Check if the content of variable TALL.PESO is allways TRUE.
#' 
#' This logical variable is TRUE for lenghts samples and false to weight samples.
#' We allways work with lengths samples so all of them must be TRUE. 
#'
#' @return dataframe with erroneus samples
#'
checkTALL.PESO <- function() {
  
  if ("TALL.PESO" %in% colnames(catches)){
    errors <- catches %>%
                select(one_of(c(BASE_FIELDS, "TALL.PESO"))) %>%
                filter(TALL.PESO != "T"|is.na(TALL.PESO)|is.null(TALL.PESO)) %>%
                addTypeOfError("ERROR: Muestreo no metido como muestreo de talla")
    return(errors)
  } else {
    stop("TALL.PESO doesn't exists in catches")
  }
}

# ---- function to check sexed species -----------------------------------------
#
#' Check samples with no sexed species that must be sexed
#' 
#' Sexed species must have the variable SEXO as M (male) or H (female).
#'
#' @return dataframe with erroneus samples
#'
checkSexedSpecies <- function() {
  
  errors <- lengths %>%
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

# ---- function to check no sexed species -----------------------------------------
#
#' Check samples with sexed species that must not be sexed
#' 
#' No sexed species must have the variable SEXO as U.
#'
#' @return dataframe with erroneus samples
#'
checkNoSexedSpecies <- function() {
  
  # Subset especies_sexadas dataframe only with required?? variables:
  sexed_species <- especies_sexadas[,c("COD_ESP", "COD_PUERTO")]
  
  # Subset sexed especies sampled 
  sexed_species_sampled <- lengths %>%
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
    addTypeOfError("ERROR: especie que NO debería ser sexada")
  
  return(errors)
}

# ---- function to check multiple ESTRATO_RIM in the same trip -----------------
#
#' Check samples with same date, vessel, gear, and port but with different 
#' ESTRATO_RIM variable.
#'
#' @return dataframe with erroneus samples
#'
checkMultipleEstratoRIM <- function(){
  
  errors <- catches %>%
    select(one_of(BASE_FIELDS)) %>%
    unique() %>%
    group_by(COD_ID, COD_PUERTO, PUERTO, LOCODE, FECHA, COD_BARCO, BARCO, COD_TIPO_MUE, TIPO_MUE) %>%
    mutate(num_estrato_rim = n_distinct(ESTRATO_RIM))%>%
    ungroup()%>%
    # summarise(num_estrato_rim = n_distinct(ESTRATO_RIM)) %>%
    filter(num_estrato_rim != 1) %>%
    addTypeOfError("ERROR: misma marea con distinto ESTRATO_RIM")
    
  return(errors)
 
}

# ---- function to check multiple gear in the same trip -----------------
#
#' Check samples with same date, vessel, ESTRATO_RIM, TIPO_MUE, and port but with different 
#' gear variable.
#'
#' @return dataframe with erroneous samples
#'
checkMultipleGear <- function(){
  
  errors <- catches %>%
    select(one_of(c(BASE_FIELDS, "COD_ARTE", "ARTE"))) %>%
    unique() %>%
    group_by(COD_ID, COD_PUERTO, PUERTO, LOCODE, FECHA, COD_BARCO, BARCO, ESTRATO_RIM, COD_TIPO_MUE, TIPO_MUE) %>%
    mutate(num_arte = n_distinct(COD_ARTE))%>%
    ungroup()%>%
    # summarise(num_Estrato_RIM = n_distinct(ESTRATO_RIM)) %>%
    filter(num_arte != 1) %>%
    addTypeOfError("ERROR: misma marea con distinto ARTE")
  
  return(errors)
  
}

# ---- function to check multiple COD_PUERTO in the same trip ------------------
#
#' Check samples with same date and vessel, ESTRATO_RIM, TIPO_MUE, and gear but with different 
#' gear variable.
#'
#' @return dataframe with erroneous samples
#'
checkMultiplePort <- function(){
  
  errors <- catches %>%
    select(one_of(c(BASE_FIELDS, "COD_ARTE", "ARTE"))) %>%
    unique() %>%
    group_by(COD_ID, COD_PUERTO, PUERTO, LOCODE, FECHA, COD_BARCO, BARCO, ESTRATO_RIM, COD_TIPO_MUE, TIPO_MUE) %>%
    mutate(num_puerto = n_distinct(COD_PUERTO))%>%
    ungroup()%>%
    filter(num_puerto != 1) %>%
    addTypeOfError("ERROR: misma marea con distinto PUERTO")
  
  return(errors)
  
}

# ---- function to check coherence between ESTRATO_RIM and origin --------------
#
#' Check coherence between ESTRATO_RIM and origin.
#' 
#'  @return dataframe with wrong coherence.
#'  
checkCoherenceEstratoRimOrigin <- function(){
  
  errors <- catches %>%
    select(one_of(BASE_FIELDS), COD_ORIGEN) %>%
    unique() %>%
    anti_join(y=estratorim_origen, by=c("ESTRATO_RIM", "COD_ORIGEN")) %>%
    addTypeOfError("ERROR: no concuerda el estrato_rim con el origen")
  
  return(errors)
  
}

# ---- function to check ESTRATO_RIM and gear of trips with 2 or more ships. 
# Exception with Santa Eugenia de Ribeira port, that usually only one ship is
# sampled
#
#' Check ESTRATO_RIM and gear of trips with 2 ships
#' 
#' @return dataframe with erroneous trips
#' 
checkShipsPairBottomTrawl <- function(){
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

# ---- function to warning the out of size of the species lengths
#
#' Check the size range of species. And if the species exists in 
#' the rango_tallas_historico dataset
#' 
#' @return dataframe with warnings lengths
#' 
checkSizeRange <- function (){
  
  warningsIsRanged <- lengths%>%
    select(one_of(BASE_FIELDS), COD_ESP_CAT, SEXO, TALLA)%>%
    merge(y = rango_tallas_historico, by.x = c("COD_ESP_CAT", "SEXO"), by.y = c("COD_ESP", "SEXO"), all.x = T)%>%
    filter(is.na(TALLA_MIN) | is.na((TALLA_MAX)))%>%
    addTypeOfError("WARNING: esta especie no se encuentra en el maestro histórico de tallas mínimas y máximas")%>%
    select(-c(TALLA_MIN, TALLA_MAX))
  
  warningsOutOfRange <- lengths %>%
    select(one_of(BASE_FIELDS), COD_ESP_CAT, SEXO, TALLA)%>%
    merge(y = rango_tallas_historico, by.x = c("COD_ESP_CAT", "SEXO"), by.y = c("COD_ESP", "SEXO"), all.x = T)%>%
    filter(TALLA < TALLA_MIN | TALLA > TALLA_MAX)%>%
    # it's not possible use addTypeOfError here, I don't know why
    mutate(TIPO_ERROR = paste("WARNING: Talla fuera del rango histórico de tallas:", TALLA_MIN, "-", TALLA_MAX))%>%
    select(-c(TALLA_MIN, TALLA_MAX))
  
  warnings <- rbind(warningsIsRanged, warningsOutOfRange)
  
  return(warnings)

}

# function to check estrategia
# TODO: rellenar esto para document()

checkStrategy <- function(){
  
  error_strategy <- catches %>% 
    select(one_of(c(BASE_FIELDS, "ESTRATEGIA")))%>%
    anti_join(y=tipo_mue, by = c("COD_TIPO_MUE", "ESTRATEGIA"))%>%
    addTypeOfError("ERROR: No concuerda el campo ESTRATEGIA con el campo TIPO DE MUESTREO")
  
  return(error_strategy)
  
}

# ------------------------------------------------------------------------------
# #### IMPORT DATA #############################################################
# ------------------------------------------------------------------------------

#PONER EN CASTELLANO
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
CFPO <- cfpo2016
  # ignore useless columns
  CFPO <- CFPO[,c("CODIGO_BUQUE", "ESTADO")]
  
  
# ------------------------------------------------------------------------------ 
# #### IMPORT muestreos_UP files ###############################################
# ------------------------------------------------------------------------------

muestreos_up <- importMuestreosUP(FILENAME_DES_TOT, FILENAME_DES_TAL, FILENAME_TAL, path = PATH_FILES, by_month = MONTH)


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

ERRORS$false_mt2b <- checkMt2b()

ERRORS$errors_mt2b_rim_stratum <- checkMt2bRimStratum()

ERRORS$coherence_estrato_rim_gear <- coherenceEstratoRimGear(catches)

ERRORS$coherence_estrato_rim_origin <- checkCoherenceEstratoRimOrigin()

ERRORS$number_of_ships <- numberOfShips()

ERRORS$number_of_rejections <- numberOfRejections()

ERRORS$errors_countries_mt1 <- check_foreing_ships_MT1(catches)

ERRORS$errors_countries_mt2 <- check_foreing_ships_MT2(catches)

##### TO DO: ADD CHECKING SHIPS WITH SIRENO FILES

ERRORS$errors_ships_not_in_cfpo <-shipsNotInCFPO(catches)

# no_en_cfpo <- ERRORS$errors_ships_not_in_cfpo %>%
#                 filter(!grepl("^8\\d{5}",COD_BARCO) & COD_BARCO != 0) %>%
#                 select(COD_BARCO, BARCO, COD_PUERTO, PUERTO)%>%
#                 unique()
        
ERRORS$errors_ships_not_registered <- shipsNotRegistered(catches)

ERRORS$errors_multiple_estrato_rim <- checkMultipleEstratoRIM()

ERRORS$errors_multiple_arte <- checkMultipleGear()

ERRORS$errors_multiple_puerto <- checkMultiplePort()

ERRORS$errors_num_barcos_pareja <- checkShipsPairBottomTrawl()

ERRORS$estrategia <- checkStrategy()

# ---- IN SPECIES ----

ERRORS$mixed_species_category <- mixedSpeciesInCategory()
      
ERRORS$not_allowed_sampled_species <- notAllowedSampledSpecies()

ERRORS$sampled_species_doubtful <- doubtfulSampledSpecies()

ERRORS$not_allowed_category_species <- notAllowedCategorySpecies()

ERRORS$doubtful_category_species <- doubtfulCategorySpecies()

ERRORS$sexes_with_same_sampled_weight <- sexesWithSameSampledWeight()

ERRORS$categories_with_repeated_sexes <- categoriesWithRepeatedSexes()

ERRORS$lenghts_weights_sample <- checkTALL.PESO()

ERRORS$no_sexed_species <- checkNoSexedSpecies()

ERRORS$sexed_species <- checkSexedSpecies()


# ---- IN WEIGHTS ----

ERRORS$same_sampled_weight <- allCategoriesWithSameSampledWeights()
  
ERRORS$sampled_weight_zero <- weightSampledZeroWithLengthsSampled()
    
ERRORS$weight_landed_zero <- weightLandedZero()

ERRORS$weight_sampled_without_length_sampled <- weightSampledWithoutLengthsSampled()
    
ERRORS$pes_mue_desem_zero <- pesMueDesemZero()

ERRORS$especies_con_categorias_igual_peso_desembarcado <- speciesWithCategoriesWithSameWeightLanding()
        
ERRORS$sop_zero <- sopZero() 
     
ERRORS$sop_greater_pes_mue_vivo <- sopGreaterPesMueVivo()

ERRORS$sop_mayor_peso_vivo <- sopGreaterPesVivo()

ERRORS$pes_mue_desem_mayor_pes_desem <- pesMueDesemGreaterPesDesem()

# ---- IN LENGTHS ----

ERRORS$rango_tallas <- checkSizeRange()

# ------------------------------------------------------------------------------    
# #### COMBINE ERRORS ##########################################################
# ------------------------------------------------------------------------------

    combined_errors <- formatErrorsList()

# ------------------------------------------------------------------------------
# #### EXPORT ERRORS ###########################################################
# ------------------------------------------------------------------------------

# Uncomment the way to export errors:

    # one month

    #exportListToCsv(combined_errors, suffix = paste0(YEAR,"_",MONTH_AS_CHARACTER), separation = "_")

    #exportListToXlsx(combined_errors, suffix = paste0("errors", "_", YEAR,"_",MONTH_AS_CHARACTER), separation = "_")

    exportListToGoogleSheet(combined_errors, suffix = paste0("errors", "_", YEAR,"_",MONTH_AS_CHARACTER), separation = "_" ) 

    # a complete year 

    #exportListToXlsx(combined_errors, suffix = paste0("errors", "_", YEAR), separation = "_")

    #exportListToGoogleSheet(combined_errors, suffix = paste0("errors", "_", YEAR), separation = "_")

# ------------------------------------------------------------------------------    
# #### CHECK CODE_ID ###########################################################
# This check is not for send to the sups, so it's out the ERRORS dataframe
# ------------------------------------------------------------------------------

# errors_cod_id <- checkCodId()


# ------------------------------------------------------------------------------    
# #### MAKE A BACKUP
# ------------------------------------------------------------------------------
    # backup_files()
    
