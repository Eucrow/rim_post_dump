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

library(dplyr) #arrange_()
library(tools) #file_path_sans_ext()
library(plyr)
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


PATH_FILES <- "F:/misdoc/sap/revision volcado/datos/noviembre"
FILENAME_DES_TOT <- "IEOUPMUEDESTOTMARCO.TXT"
FILENAME_DES_TAL <- "IEOUPMUEDESTALMARCO.TXT"
FILENAME_TAL <- "IEOUPMUETALMARCO.TXT"

MONTH <- 11 #only if a filter by month is necesary. It's imperative use the atributte 'by_month' in import_muestreos_up() function
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
      select(-one_of("TIPO_ERROR"), one_of("TIPO_ERROR")) #remove TIPO_ERROR, and add it to the end
    
    # Order the errors
    x <- x %>%
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
numberOfShips <- function (df){
  errors <- df[df["N_BARCOS"] == 0 | df["N_BARCOS"] > 2 | is.null(df["N_BARCOS"]), c(BASE_FIELDS, "N_BARCOS")]
  errors <- addTypeOfError(errors, "WARNING: número de barcos igual a 0 o mayor de 2")
  return (errors)
}


# Function to search errors in number of rejects (only empty fields) -----------
numberOfRejections <- function(df){
  errors <- df[is.null(df["N_RECHAZOS"]), c(BASE_FIELDS, "N_RECHAZOS")]
  errors <- addTypeOfError(errors, "ERROR: número de rechazos sin rellenar")
  return(errors)
}


# Function to search samples with SOP > P_MUE_VIVO, when P_MUE_VIVO != 0 -------
sopGreaterPesMueVivo <- function(df){
  errors <- df[, c(BASE_FIELDS,"P_MUE_VIVO", "SOP")]
  errors <- errors[errors["SOP"]>errors["P_MUE_VIVO"] & errors["P_MUE_VIVO"]!=0 & !is.na(errors["P_MUE_VIVO"]),]
  errors["P_MUE_VIVO-SOP"] <- round(errors["P_MUE_VIVO"] - errors["SOP"],1)
  errors["POR_DIF_P_MUE_VIVO-SOP"] <- round((errors["P_MUE_VIVO-SOP"] * 100) / errors["P_MUE_VIVO"])
  errors <- addTypeOfError(errors, "ERROR: SOP mayor que peso muestreado vivo, cuando peso muestreado vivo es distinto que 0")
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
    filter(DIF_SOP_P_VIVO > 0) %>%
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


# TODO: remove (this is checked in IPDtoSIRENO.R)
# function to check foreings ships in MT1 samples
# df: dataframe
check_foreing_ships_MT1 <- function(df){
  ships <- df %>%
    select(one_of(BASE_FIELDS))%>%
    filter(grepl("^8\\d{5}",COD_BARCO)) %>%
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


# function to check ships not in "ALTA DEFINITIVA", "ALTA PROVISIONAL POR NUEVA
# CONSTRUCCI?N or ALTA PROVISIONAL POR REACTIVACI?N in CFPO --------------------
shipsNotRegistered <- function(df, cfpo = CFPO){
  
  to_ships <- unique(df[,c(BASE_FIELDS, "CODSGPM")])
  errors_ships <- merge(x=to_ships, y=cfpo, by.x = "CODSGPM", by.y = "CODIGO_BUQUE", all.x = TRUE)
  errors_ships <- errors_ships %>%
    filter( ESTADO != "ALTA DEFINITIVA" &
              ESTADO != "H - A.P. POR REACTIVACION" &
              ESTADO != "G - A.P. POR NUEVA CONSTRUCCION" )
  text_type_of_error <- paste0("WARNING: este Barco está incluido en el ", cfpo, " pero con un estado distinto a Alta Definitiva, A. P. Por Reactivación, o A.P Por Nueva Construcción")
  errors_ships <- addTypeOfError(errors_ships, text_type_of_error)
  return (errors_ships)
}


# ------------------------------------------------------------------------------
# #### IMPORT DATA #############################################################
# ------------------------------------------------------------------------------

#read the mixed species dataset
#TODO: change the name of cat_spe_mixed
cat_spe_mixed <- especies_mezcla

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
  
muestreos_up <- importMuestreosUP(FILENAME_DES_TOT, FILENAME_DES_TAL, FILENAME_TAL, by_month = MONTH, path = PATH_FILES)


#isolate dataframes
catches <- muestreos_up$catches
catches_in_lengths <- muestreos_up$catches_in_lengths
lengths <- muestreos_up$lengths


# ------------------------------------------------------------------------------
# #### SEARCHING ERRORS ########################################################
# ------------------------------------------------------------------------------

# ---- IN HEADER ----

ERRORS$coherence_estrato_rim_gear <- coherenceEstratoRimGear(catches)

ERRORS$number_of_ships <- numberOfShips(catches)

ERRORS$number_of_rejections <- numberOfRejections(catches)

ERRORS$errors_countries_mt1 <- check_foreing_ships_MT1(catches)

ERRORS$errors_countries_mt2 <- check_foreing_ships_MT2(catches)

  # ---- search errors in ships
  ##### TO DO: ADD CHECKING WITH SIRENO FILES
    to_ships <- unique(catches[,c(BASE_FIELDS, "CODSGPM")])
    errors_ships <- merge(x=to_ships, y=CFPO, by.x = "CODSGPM", by.y = "CODIGO_BUQUE", all.x = TRUE)

      #ships withouth coincidences in cfpo
      errors_ships_not_in_cfpo <- subset(errors_ships, is.na(errors_ships$ESTADO))
      errors_ships_not_in_cfpo <- errors_ships_not_in_cfpo[, c(BASE_FIELDS, "CODSGPM", "ESTADO")]
      errors_ships_not_in_cfpo <- arrange_(errors_ships_not_in_cfpo, BASE_FIELDS)
      ERRORS$errors_ships_not_in_cfpo <- errors_ships_not_in_cfpo
      ERRORS$errors_ships_not_in_cfpo <- addTypeOfError(ERRORS$errors_ships_not_in_cfpo, "WARNING: este Barco no está en el CFPO" )

      #ships with state different to "alta definitiva"
      ERRORS$errors_ships_not_registered <- shipsNotRegistered(catches) #AddTypeOfError included in function


  # ---- estrato_rim, gear and division coherence ----
  # TO DO


# ---- IN SPECIES ----

  # ---- errors in mixed species of the sample ----
    selected_fields<-catches[,c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE")]
    #search the errors: species fo the sample fill like mixed species:
    mixed_species_sample<-merge(x=selected_fields, y=cat_spe_mixed[,c("ESP_CAT","COD_ESP_CAT")], by.x="COD_ESP_MUE", by.y="COD_ESP_CAT")
    #change the name of a column in dataframe. ???OMG!!!:
    names(mixed_species_sample)[names(mixed_species_sample) == 'ESP_MUE'] <- 'ESP_MUE_INCORRECTA'
    #order columns dataframe:
    mixed_species_sample <- mixed_species_sample[, c(BASE_FIELDS, "ESP_MUE_INCORRECTA")]
    #order dataframe:
    mixed_species_sample<-arrange_(mixed_species_sample, BASE_FIELDS)
    ERRORS$mixed_species_sample <- mixed_species_sample
    ERRORS$mixed_species_sample <- addTypeOfError(ERRORS$mixed_species_sample, "ERROR: especie de mezcla que no está agrupada en Especies del Muestreo")
    rm(selected_fields, mixed_species_sample)


  # ---- errors in mixed species of the category ----
    selected_fields<-catches_in_lengths[,c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "COD_ESP_CAT", "ESP_CAT")]
    #species not allowed in category because are mixed especies:
    not_allowed_in_category <- as.data.frame(cat_spe_mixed[,c("COD_ESP_MUE")])
    not_allowed_in_category <- unique(not_allowed_in_category)
    colnames(not_allowed_in_category)<- c("COD_ESP_MUE")
    #search the errors:
    #mixed_species_category<-merge(x=selected_fields, y=cat_spe_mixed["ESP_MUESTREO"], by.x="ESPECIE", by.y = "ESP_MUESTREO")
    mixed_species_category<-merge(x=selected_fields, y=not_allowed_in_category, by.x="COD_ESP_CAT", by.y = "COD_ESP_MUE")
    #change the name of a column in dataframe. ???OMG!!!:
    names(mixed_species_category)[names(mixed_species_category) == 'ESP_CAT'] <- 'ESP_CAT_INCORRECTA'
    # ---- MT2
    mixed_species_category_mt2 <- subset(mixed_species_category, TIPO_MUE == "MT2A (Biometrico puerto)")
    mixed_species_category_mt2 <- mixed_species_category_mt2[, c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_ESP_CAT", "CATEGORIA", "ESP_CAT_INCORRECTA")]
    mixed_species_category_mt2 <- arrange_(mixed_species_category_mt2, BASE_FIELDS)
    ERRORS$mixed_species_category_mt2 <- mixed_species_category_mt2
    ERRORS$mixed_species_category_mt2 <- addTypeOfError(ERRORS$mixed_species_category_mt2, "WARNING: muestreo MT2 con especie de mezcla que está agrupada en Especies para la Categoría")
    
    # ---- MT1
    mixed_species_category_mt1 <- subset(mixed_species_category, TIPO_MUE == "MT1A (Encuestas IEO)")
    mixed_species_category_mt1 <- mixed_species_category_mt1[, c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_ESP_CAT", "CATEGORIA", "ESP_CAT_INCORRECTA")]
    mixed_species_category_mt1 <- arrange_(mixed_species_category_mt1, BASE_FIELDS)
    ERRORS$mixed_species_category_mt1 <- mixed_species_category_mt1
    ERRORS$mixed_species_category_mt1 <- addTypeOfError(ERRORS$mixed_species_category_mt1, "WARNING: muestreo MT1 con especie de mezcla que está agrupada en Especies para la Categoría")
    rm(selected_fields, mixed_species_category, mixed_species_category_mt1, mixed_species_category_mt2)

  # ---- not allowed species
    # ---- in sampled species
      not_allowed_sampling_species <- merge(x = catches, y = NOT_ALLOWED_SPECIES, by.x = "COD_ESP_MUE", by.y = "COD_ESP")
      not_allowed_sampling_species <- not_allowed_sampling_species[c(BASE_FIELDS,"COD_ESP_MUE","ESP_MUE")]
      #change the name of a column in dataframe. ???OMG!!!:
      names(not_allowed_sampling_species)[names(not_allowed_sampling_species) == 'ESP_MUE'] <- 'ESP_MUE_INCORRECTA'
      names(not_allowed_sampling_species)[names(not_allowed_sampling_species) == 'COD_ESP_MUE'] <- 'COD_ESP_MUE_INCORRECTA'
      not_allowed_sampling_species <- arrange_(not_allowed_sampling_species, BASE_FIELDS)
      ERRORS$not_allowed_sampling_species <- not_allowed_sampling_species
      ERRORS$not_allowed_sampling_species <- addTypeOfError(ERRORS$not_allowed_sampling_species, "WARNING: Muestreo con especie no permitida en Especies del Muestreo")

        # select all the genus to check
        to_check_genus <- grep("(.+(formes$))|(.+(spp$))|(.+(sp$))|(.+(dae$))",catches$ESP_MUE)
          # . = any single character
          # + = one of more of previous
          # | = or
        to_check_genus<-catches[to_check_genus, c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE")]

        # allowed genus
          # allowed genus from mixed species
          allowed_genus_mixed_species <- as.data.frame(unique(cat_spe_mixed$COD_ESP_MUE))
          colnames(allowed_genus_mixed_species)<-"COD_ESP"

          # other allowed genus
          allowed_genus_other <- as.data.frame(ALLOWED_GENUS$COD_ESP)
          colnames(allowed_genus_other)<-"COD_ESP"

          #allowed genus complete
          allowed_genus <- rbind(allowed_genus_mixed_species, allowed_genus_other)
          allowed_genus$ALLOWED <- "ok" #??CHAPUCILLA!!

        #check the genus
        checked_allowed_genus <- merge(x = to_check_genus, y =allowed_genus, by.x = "COD_ESP_MUE", by.y = "COD_ESP", all.x = TRUE)
        not_allowed_genus <- checked_allowed_genus[is.na(checked_allowed_genus$ALLOWED),]

        #send to the ERRORS
        ERRORS$not_allowed_genus_sampled_species <- subset(not_allowed_genus, select = -c(ALLOWED))
        ERRORS$not_allowed_genus_sampled_species <- addTypeOfError(ERRORS$not_allowed_genus_sampled_species, "WARNING: muestreo con género no permitido en Especies del muestreo")

        #remove unnecesary variables
        rm(to_check_genus, allowed_genus_mixed_species, allowed_genus_other, allowed_genus, checked_allowed_genus, not_allowed_genus)


    # ---- in category species
      not_allowed_category_species <- merge(x = catches_in_lengths, y = NOT_ALLOWED_SPECIES, by.x = "COD_ESP_CAT", by.y = "COD_ESP")
      not_allowed_category_species <- not_allowed_category_species[c(BASE_FIELDS,"COD_ESP_MUE", "ESP_MUE","COD_CATEGORIA","CATEGORIA", "COD_ESP_CAT", "ESP_CAT")]
      #change the name of a column in dataframe. ???OMG!!!:
      names(not_allowed_category_species)[names(not_allowed_category_species) == 'ESP_CAT'] <- 'ESP_CAT_INCORRECTA'
      names(not_allowed_category_species)[names(not_allowed_category_species) == 'COD_ESP_CAT'] <- 'COD_ESP_CAT_INCORRECTA'
      not_allowed_category_species <- arrange_(not_allowed_category_species, BASE_FIELDS)
      ERRORS$not_allowed_category_species <- not_allowed_category_species
      ERRORS$not_allowed_category_species <- addTypeOfError(ERRORS$not_allowed_category_species, "WARNING: muestreo con especie no permitida en Especies de la categoría")
      # ---- genus not allowed
        # select all the genus to check
        to_check_genus <- grep("(.+(formes$))|(.+(spp$))|(.+(sp$))|(.+(dae$))",catches_in_lengths$ESP_CAT)
        # . = any single character
        # + = one of more of previous
        # | = or
        to_check_genus<-catches_in_lengths[to_check_genus, c(BASE_FIELDS, "COD_ESP_CAT", "ESP_CAT")]

        # allowed genus
          # other allowed genus
          allowed_genus_other <- as.data.frame(ALLOWED_GENUS$COD_ESP)
          colnames(allowed_genus_other)<-"COD_ESP"

          #allowed genus complete
          # TO DO: arreglar esto, sobran variables
          allowed_genus <- allowed_genus_other
          allowed_genus$ALLOWED <- "ok" #??CHAPUCILLA!!

          #check the genus
          # TO DO: arreglar esto, sobran variables
          checked_allowed_genus <- merge(x = to_check_genus, y =allowed_genus, by.x = "COD_ESP_CAT", by.y = "COD_ESP", all.x = TRUE)
          not_allowed_genus <- checked_allowed_genus[is.na(checked_allowed_genus$ALLOWED),]

        #to the ERRORS
        ERRORS$not_allowed_genus_category_species <- subset(not_allowed_genus, select = -c(ALLOWED))
        ERRORS$not_allowed_genus_category_species <- addTypeOfError(ERRORS$not_allowed_genus_category_species, "WARNING: muestreos con géneros no permitidos en Especies de la Categoría")
        #remove unnecesary variables
        rm(to_check_genus, allowed_genus_other, allowed_genus, checked_allowed_genus, not_allowed_genus)


# ---- IN WEIGHTS ----

  # ---- errors in species from the categories: all of them has exactly the same sampled weight
  # the last column in the ERRORS$same_sampled_weight dataframe show the number of category species with exactly the same sampled weight in every specie of the category
    selected_fields<-c(BASE_FIELDS,"N_CATEGORIAS","COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA","CATEGORIA","P_DESEM","COD_ESP_CAT", "ESP_CAT","SEXO","P_MUE_DESEM")
    same_sampled_weight<-catches_in_lengths[,selected_fields]
        by <- list(same_sampled_weight$COD_ID,
               same_sampled_weight$COD_PUERTO,
               same_sampled_weight$PUERTO,
               same_sampled_weight$LOCODE,
               same_sampled_weight$FECHA,
               same_sampled_weight$COD_BARCO,
               same_sampled_weight$BARCO,
               same_sampled_weight$ESTRATO_RIM,
               same_sampled_weight$COD_TIPO_MUE,
               same_sampled_weight$TIPO_MUE,
               same_sampled_weight$COD_ESP_MUE,
               same_sampled_weight$ESP_MUE,
               same_sampled_weight$COD_CATEGORIA,
               same_sampled_weight$CATEGORIA,
               same_sampled_weight$P_DESEM,
               same_sampled_weight$SEXO,
               same_sampled_weight$P_MUE_DESEM)
    same_sampled_weight<-aggregate(x = same_sampled_weight$P_MUE_DESEM, by = by, FUN= length)
    colnames(same_sampled_weight) <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA","CATEGORIA","P_DESEM","SEXO","P_MUE_DESEM","NUM_OCU_MISMO_PESO_MUESTRA")
    same_sampled_weight<-subset ( same_sampled_weight, NUM_OCU_MISMO_PESO_MUESTRA > 1)
    same_sampled_weight<-arrange_(same_sampled_weight, BASE_FIELDS)
    ERRORS$same_sampled_weight <- same_sampled_weight
    ERRORS$same_sampled_weight <- addTypeOfError(ERRORS$same_sampled_weight, "WARNING: todas las categorías de la especie tienen exactamente el mismo peso muestreado")
    #TO DO: check this dataframe... why return the COD_CATEGORIA and CATEGORIA fields??
    rm (selected_fields, by, same_sampled_weight)
  
    # ---- errors
    ERRORS[["sampled_weight_zero"]] <- subset(catches_in_lengths, P_MUE_DESEM == 0, select = c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "P_DESEM", "P_VIVO", "COD_ESP_CAT", "ESP_CAT", "P_MUE_DESEM", "P_MUE_VIVO", "SOP"))
    ERRORS[["sampled_weight_zero"]] <- addTypeOfError(ERRORS[["sampled_weight_zero"]], "ERROR: peso muestra = 0")
    
  # ---- errors p.desem = 0
    ERRORS[["weight_landed_zero"]] <- subset(catches_in_lengths, P_DESEM == 0 | is.na( P_DESEM),select = c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "P_DESEM", "P_VIVO"))
    ERRORS[["weight_landed_zero"]] <- addTypeOfError(ERRORS[["weight_landed_zero"]], "ERROR: peso desembarcado = 0")
    
  # ---- errors species of the category WITHOUT length sample but WITH weight sample
    ERRORS[["weight_sampled_0_without_length_sampled"]] <- subset(catches_in_lengths, P_MUE_DESEM == 0 & EJEM_MEDIDOS == 0, select = c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "P_DESEM", "P_VIVO", "COD_ESP_CAT", "ESP_CAT", "P_MUE_DESEM", "EJEM_MEDIDOS"))
    ERRORS[["weight_sampled_0_without_length_sampled"]] <- addTypeOfError(ERRORS[["weight_sampled_0_without_length_sampled"]], "ERROR: especie sin tallas muestreadas pero con peso muestra")
    
  # ---- errors species of the category WITH length sample but WITHOUT weight sample
    ERRORS[["lenght_sampled_without_weight_sampled"]] <- subset(catches_in_lengths, P_MUE_DESEM == 0 & EJEM_MEDIDOS != 0, select = c(BASE_FIELDS, "P_DESEM", "P_MUE_DESEM", "EJEM_MEDIDOS"))
    ERRORS[["lenght_sampled_without_weight_sampled"]] <- addTypeOfError(ERRORS[["lenght_sampled_without_weight_sampled"]], "ERROR: especie con tallas muestreadas pero sin peso muestra")
    
  # ---- errors in samples with P_MUE_DESEM is 0 or NA
    ERRORS$pes_mue_desem_zero <- pesMueDesemZero(catches_in_lengths)

  # ---- (warning) errors in species with categories with the same weight landing
    ERRORS$especies_con_categorias_igual_peso_desembarcado <- speciesWithCategoriesWithSameWeightLanding(catches)
        
  # ---- errors in samples with sop = 0
    ERRORS[["sop_zero"]] <- subset(catches_in_lengths, SOP == 0, select = c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "P_DESEM", "P_VIVO", "COD_ESP_CAT", "ESP_CAT", "P_MUE_DESEM", "P_MUE_VIVO", "SOP"))
    ERRORS[["sop_zero"]] <- addTypeOfError(ERRORS[["sop_zero"]], "ERROR: sop = 0")
     
  # ---- errors in samples with SOP greater than P_MUE_VIVO when P_MUE_VIVO != 0
    ERRORS$sop_greater_pes_mue_vivo <- sopGreaterPesMueVivo(catches_in_lengths)

  # ---- errors in samples with SOP greater than P_VIVO
    ERRORS$sop_mayor_peso_vivo <- SopGreaterPesVivo(catches_in_lengths)

  # ---- errors in samples with P_DESEM <= P_MUE_DESEM
    ERRORS$pes_mue_desem_mayor_pes_desem <- pesMueDesemGreaterPesDesem (lengths)


# ------------------------------------------------------------------------------    
# #### COMBINE ERRORS ##########################################################
# ------------------------------------------------------------------------------

    combined_errors <- formatErrorsList()

    
# ------------------------------------------------------------------------------
# #### EXPORT ERRORS ###########################################################
# ------------------------------------------------------------------------------
    
    #exportListToCsv(combined_errors, suffix = paste0(YEAR,"_",MONTH_AS_CHARACTER), separation = "_")

    exportListToXlsx(combined_errors, suffix = paste0("errors", "_", YEAR,"_",MONTH_AS_CHARACTER), separation = "_")
       
    #exportListToGoogleSheet( combined_errors, suffix = paste0("errors", "_", YEAR,"_",MONTH_AS_CHARACTER), separation = "_" ) 
    
    #lapply(names(ERRORS), export_errors_lapply, ERRORS) #The 'ERRORS' argument is an argument to the export_errors_lapply function

    
    
        
# ------------------------------------------------------------------------------    
# #### MAKE A BACKUP
# ------------------------------------------------------------------------------
    # backup_files()
