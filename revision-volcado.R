#### Review data dump
#### Script to check the monthly data dumps in SIRENO
#### 
#### Return csv files with errors detected
####
#### author: Marco A. Amez Fernandez
#### email: ieo.marco.a.amez@gmail.com
#### date of last modification: 17/8/2016
#### version: 2.00
####
#### files required: esp mezcla.csv, especies_no_mezcla.csv,
#### estratorim_arte.csv, divisiones.csv, especies_no_permitidas.csv,
#### generos_permitidos.csv, areas_influencia.csv
#### CFPO2015.csv (this one not available in github)


# #### CONFIG ##################################################################

# ---- PACKAGES ----------------------------------------------------------------

library(dplyr) #arrange_()
library(tools) #file_path_sans_ext()
library(plyr)
library(devtools)

# ---- install sapmuebase from local
install("F:/misdoc/sap/sapmuebase")
library(sapmuebase)


# ---- SET WORKING DIRECTORY ---------------------------------------------------

setwd("F:/misdoc/sap/revision volcado/revision_volcado_R/")


# ---- CONSTANTS ---------------------------------------------------------------

PATH <- getwd()

# ---- GLOBAL VARIABLES --------------------------------------------------------
  # list with all errors found in dataframes:
    ERRORS <- list() 
    
  # list with the errors:
    MESSAGE_ERRORS<- list()
    
  # list with the common fields used in all tables
  BASE_FIELDS <- c("COD_ID", "PUERTO", "LOCODE", "FECHA", "COD_BARCO", "BARCO", "ESTRATO_RIM", "COD_TIPO_MUE", "TIPO_MUE")
  

################################################################################
# YOU HAVE ONLY TO CHANGE THIS VARIABLES:

PATH_FILENAME <- "F:/misdoc/sap/revision volcado/datos/julio"
FILENAME_DES_TOT <- "IEOUPMUEDESTOTSIRENO.TXT"
FILENAME_DES_TAL <- "IEOUPMUEDESTALSIRENO.TXT"
FILENAME_TAL <- "IEOUPMUETALSIRENO_corregido.TXT"


MONTH <- 7 #only if a filter by month is necesary. It's imperative use the atributte 'by_month' in import_muestreos_up() function
YEAR <- "2016"
################################################################################

PATH_ERRORS <- paste(PATH_FILENAME,"/errors",sep="")
PATH_BACKUP <- paste(PATH_ERRORS, "/backup", sep="")

MONTHASCHARACTER <- sprintf("%02d", MONTH)

# #### FUNCTIONS ###############################################################


# ---- function to save the errors in csv files: -------------------------------
export_errors_csv <- function(errors){
  lapply(names(errors), function(x){
    area <- x
    fullpath<-paste(PATH_ERRORS, "/", area, "_", YEAR, "_", MONTH, "_errors", ".csv", sep="")
    write.csv(errors[[x]], file=fullpath, row.names = FALSE, quote = FALSE)
    print(x)
  })
}


# ---- function to make a copy of the files previous to send to the Area Supervisors ---
backup_files_to_send <- function(){
  date <- Sys.time();
  date <- as.POSIXlt(date);
  
  directory_backup<-paste(PATH_BACKUP, "/", YEAR, "_", MONTH, "_backup_", date$mday,
                   '_', date$mon+1, '_', date$year+1900, '_', date$hour, '_',
                   date$min, '_', round(date$sec, 0), "/", sep="")
  dir.create(directory_backup, recursive=TRUE); #create the directory backup
  
  files <- list.files(PATH_ERRORS, pattern = "*.csv", full.names = TRUE)

  lapply(as.list(files), function(x){ file.copy(x, directory_backup)})
}

# Function to check the coherence between 'ESTRATO_RIM' and 'gear'
coherenceEstratoRimGear <- function(df){
  merge_estrato_rim_gear<-merge(x=df, y=CORRECT_ESTRATORIM_ARTE, by.x = c("ESTRATO_RIM","ARTE"), by.y = c("ESTRATO_RIM", "ARTE"), all.x = TRUE)
  errors <- -which(merge_estrato_rim_gear[["VALID"]])
  errors <- merge_estrato_rim_gear[errors,c(BASE_FIELDS, "ARTE")]
  errors <- unique(errors)
  errors <- addTypeOfError(errors, "no hay coherencia entre estrato Rim y arte")
  return(errors)
}

# Function to search errors in number of ships (empty field, =0 or >2)
numberOfShips <- function (df){
  errors <- df[df["N_BARCOS"] == 0 | df["N_BARCOS"] > 2 | is.null(df["N_BARCOS"]), c(BASE_FIELDS, "N_BARCOS")]
  errors <- addTypeOfError(errors, "número de barcos igual a 0 o mayor dos")
  return (errors)
}

# Function to search errors in number of rejects (only empty fields)
numberOfRejections <- function(df){
  errors <- df[is.null(df["N_RECHAZOS"]), c(BASE_FIELDS, "N_RECHAZOS")]
  errors <- addTypeOfError(errors, "número de rechazos sin rellenar")
  return(errors)
}

# Function to search samples with SOP > P_MUE_VIVO, when P_MUE_VIVO != 0
sopGreaterPesMueVivo <- function(df){
  errors <- df[, c(BASE_FIELDS,"P_MUE_VIVO", "SOP")]
  errors <- errors[errors["SOP"]>errors["P_MUE_VIVO"] & errors["P_MUE_VIVO"]!=0 & !is.na(errors["P_MUE_VIVO"]),]
  errors["P_MUE_VIVO-SOP"] <- round(errors["P_MUE_VIVO"] - errors["SOP"],1)
  errors["POR_DIF_P_MUE_VIVO-SOP"] <- round((errors["P_MUE_VIVO-SOP"] * 100) / errors["P_MUE_VIVO"])
  errors <- addTypeOfError(errors, "SOP mayor que peso muestreado vivo, cuando peso muestreado vivo es distinto que 0")
  return (errors)
}

#function to search samples with P_MUE_DESEM = 0 or NA
pesMueDesemZero <- function(df){
  errors <- df[df["P_MUE_DESEM"] == 0 | is.na(df["P_MUE_DESEM"]),]
  errors <- errors[,c(BASE_FIELDS, "P_MUE_DESEM")]
  errors <- addTypeOfError(errors, "Peso muestreado desembarcado es 0")
  return(errors)
}

#function to search categories with multiple weight landings
speciesWithCategoriesWithSameWeightLanding <- function(df){
  df <- df[,c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "P_DESEM")]
  fields_to_count <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "P_DESEM")
  errors <- df %>% 
    distinct() %>%
    count_(fields_to_count) %>%
    filter(n>1)
  colnames(errors)[names(errors) == "n"] <- "NUM_OCU_CAT_MISMO_PESO_DESEM"
  errors <- addTypeOfError(errors, "categorías con varios pesos desembacados")
  return(errors)
}

#function to combine all the erros produced
combineErrors <- function(errors_list = ERRORS){
  # Combine all the dataframes of ERRORS list:
    # Reduce uses a binary function to successively combine the elements of a
    # given vector. In this case, merge the dataframes in the ERRORS list
  errors <- Reduce(function(x, y) merge(x, y, all=TRUE), errors_list)
}

#function to add variable with type of error to a dataframe
addTypeOfError <- function(df, type){
  if(nrow(df)!=0){
    df[["TIPO_ERROR"]] <- type
  }
  return(df)
}


# #### IMPORT DATA #############################################################

#read the mixed species file
cat_spe_mixed<-read.csv("especies_mezcla.csv", header=TRUE)

#read the no mixed species file
sampled_spe_no_mixed<-read.csv("especies_no_mezcla.csv", header=TRUE)

#read the estrato-rim - arte file to obtain the correct estratorim, gear and its relation
# CORRECT_ESTRATORIM_ARTE<-read.csv("estratorim_arte.csv", header=TRUE, sep = ";")
data(estratorim_arte)
CORRECT_ESTRATORIM_ARTE <- estratorim_arte
CORRECT_ESTRATORIM_ARTE$VALID<-TRUE

###obtain the not allowed species
NOT_ALLOWED_SPECIES <- read.csv("especies_no_permitidas.csv", fileEncoding = "UTF-8")

### obtain the allowed genus
ALLOWED_GENUS <- read.csv("generos_permitidos.csv")

### obtain the cfpo
CFPO <- read.table("CFPO2015.csv", sep=";", quote = "", header = TRUE)
  # ignore superfluous columns
  CFPO <- CFPO[,c("CODIGO_BUQUE", "ESTADO")]
  

################################################################################  
# #### IMPORT muestreos_UP files ###############################################  
muestreos_up <- importMuestreosUP(FILENAME_DES_TOT, FILENAME_DES_TAL, FILENAME_TAL, by_month = MONTH, path = PATH_FILENAME)


#isolate dataframes
catches <- muestreos_up$catches
catches_in_lengths <- muestreos_up$catches_in_lengths
lengths <- muestreos_up$lengths
################################################################################  



# #### SEARCHING ERRORS ########################################################
# ---- IN HEADER ----


ERRORS$coherence_estrato_rim_gear <- coherenceEstratoRimGear(catches)

ERRORS$number_of_ships <- numberOfShips(catches)

ERRORS$number_of_rejections <- numberOfRejections(catches)

  # ---- search errors in country
    ERRORS$errors_countries_mt1 <- subset(catches, COD_TIPO_MUE == 1 & (COD_PAIS != 724 | is.na(COD_PAIS)), c(BASE_FIELDS, "COD_PAIS"))
    ERRORS$errors_countries_mt1 <- addTypeOfError(ERRORS$errors_countries_mt1, "MT1 con barco extranjero")
    ERRORS$errors_countries_mt2 <- subset(catches, COD_TIPO_MUE == 2 & (COD_PAIS != 724 | is.na(COD_PAIS)), c(BASE_FIELDS, "COD_PAIS"))
    ERRORS$errors_countries_mt2 <- addTypeOfError(ERRORS$errors_countries_mt2, "MT2 con barco extranjero")

  # ---- search errors in ships
  ##### TO DO: ADD CHECKING WITH SIRENO FILES
    to_ships <- unique(catches[,c(BASE_FIELDS, "CODSGPM")])
    errors_ships <- merge(x=to_ships, y=CFPO, by.x = "CODSGPM", by.y = "CODIGO_BUQUE", all.x = TRUE)

      #ships withouth coincidences in cfpo
      errors_ships_not_in_cfpo <- subset(errors_ships, is.na(errors_ships$ESTADO))
      errors_ships_not_in_cfpo <- errors_ships_not_in_cfpo[, c(BASE_FIELDS, "CODSGPM", "ESTADO")]
      errors_ships_not_in_cfpo <- arrange_(errors_ships_not_in_cfpo, BASE_FIELDS)
      ERRORS$errors_ships_not_in_cfpo <- errors_ships_not_in_cfpo
      ERRORS$errors_ships_not_in_cfpo <- addTypeOfError(ERRORS$errors_ships_not_in_cfpo, "este Barco no está en el cfpo")

      #ships with state different to "alta definitiva"
      errors_ships_not_registered <- subset(errors_ships, ESTADO != "ALTA DEFINITIVA")
      errors_ships_not_registered <- errors_ships_not_registered[, c(BASE_FIELDS, "CODSGPM", "ESTADO")]
      errors_ships_not_registered <- arrange_(errors_ships_not_registered, BASE_FIELDS)
      ERRORS$errors_ships_not_registered <- errors_ships_not_registered
      ERRORS$errors_ships_not_registered <- addTypeOfError(ERRORS$errors_ships_not_registered, "este Barco no está dado de alta en el maestro del SIRENO")
      
      

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
    ERRORS$mixed_species_sample <- addTypeOfError(ERRORS$mixed_species_sample, "especie de mezcla que no está agrupada en Especies del Muestreo")
    rm(selected_fields, mixed_species_sample)

  # LO SIGUIENTE QUIERO COMPROBARLO ANTES DE ELIMINARLO
    # # ---- errors in not mixed species keyed as mixed species
  #   selected_fields<-catches[,c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE")]
  #   #search the errors:
  #   no_mixed_species_sample <- merge(x=selected_fields, y=sampled_spe_no_mixed, by.x="ESP_MUE", by.y="ESP")
  #   #order columns dataframe:
  #   no_mixed_species_sample <- no_mixed_species_sample[c("LOCODE", "PUERTO", "TIPO_MUE", "ESTRATO_RIM", "FECHA", "COD_BARCO", "BARCO", "COD_ESP_MUE", "ESP_MUE")]
  #   #change the name of a column in dataframe. ???OMG!!!:
  #   names(no_mixed_species_sample)[names(no_mixed_species_sample) == 'ESP_MUE'] <- 'ESP_MUESTREO_INCORRECTA'
  #   #order dataframe:
  #   no_mixed_species_sample<-arrange_(no_mixed_species_sample, BASE_FIELDS)
  #   # ---- MT1
  #   no_mixed_species_sample_mt1<-subset(no_mixed_species_sample, TIPO_MUE == "MT1A (Encuestas IEO)")
  #   ERRORS$no_mixed_species_sample_mt1 <- no_mixed_species_sample_mt1
  #   # ---- MT2
  #   no_mixed_species_sample_mt2<-subset(no_mixed_species_sample, TIPO_MUE == "MT2A (Biometrico puerto)")
  #   ERRORS$no_mixed_species_sample_mt2 <- no_mixed_species_sample_mt2
  #     # ---- Special format to send Ricardo: he has to change the sampled species to its category species
  #     # byx = c(BASE_FIELDS, "ESP_MUESTREO_INCORRECTA")
  #     # byy = c(BASE_FIELDS, "ESP_MUE")
  #     # Ricardo_no_mixed_species_sample_mt2 <- merge(x = no_mixed_species_sample_mt2, y = catches_in_lengths, by.x = byx, by.y = byy, all.x = TRUE)
  #     # Ricardo_no_mixed_species_sample_mt2 <- Ricardo_no_mixed_species_sample_mt2[, c(BASE_FIELDS, "BARCOD", "ESP_MUESTREO_INCORRECTA","ESPCOD", "CATEGORIA", "ESPECIE", "ESPCOD2")]
  #     # # ---- change column names
  #     # colnames(Ricardo_no_mixed_species_sample_mt2)[colnames(Ricardo_no_mixed_species_sample_mt2)=="ESPCOD"] <- "COD_ESP_MUESTREO_INCORRECTA"
  #     # colnames(Ricardo_no_mixed_species_sample_mt2)[colnames(Ricardo_no_mixed_species_sample_mt2)=="ESPECIE"] <- "ESP_MUESTREO_CORRECTA"
  #     # colnames(Ricardo_no_mixed_species_sample_mt2)[colnames(Ricardo_no_mixed_species_sample_mt2)=="ESPCOD2"] <- "COD_ESP_MUESTREO_CORRECTA"
  #     #   # delete the species that have non-grouped specie in species category
  #     #   Ricardo_no_mixed_species_sample_mt2 <- Ricardo_no_mixed_species_sample_mt2[ ! Ricardo_no_mixed_species_sample_mt2$ESP_MUESTREO_CORRECTA %in% as.character(sampled_spe_no_mixed$ESPECIE),]
  #     #   # ---- reorder columns
  #     #   Ricardo_no_mixed_species_sample_mt2 <- Ricardo_no_mixed_species_sample_mt2[, c("PUERTO","FECHA","BARCO","BARCOD","ESTRATO_RIM","TIP_MUESTREO","ESP_MUESTREO_INCORRECTA","COD_ESP_MUESTREO_INCORRECTA","CATEGORIA","ESP_MUESTREO_CORRECTA", "COD_ESP_MUESTREO_CORRECTA")]
  #     #   write.csv(Ricardo_no_mixed_species_sample_mt2, file=paste(PATH_ERRORS, "Ricardo_no_mixed_species_sample_mt2.csv", sep="/"), quote = FALSE, row.names = FALSE)
  #
  #   rm(selected_fields, no_mixed_species_sample, no_mixed_species_sample_mt1, no_mixed_species_sample_mt2)

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
    ERRORS$mixed_species_category_mt2 <- addTypeOfError(ERRORS$mixed_species_category_mt2, "muestreo MT2 con especie de mezcla que está agrupada en Especies para la Categoría")
    
    # ---- MT1
    mixed_species_category_mt1 <- subset(mixed_species_category, TIPO_MUE == "MT1A (Encuestas IEO)")
    mixed_species_category_mt1 <- mixed_species_category_mt1[, c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_ESP_CAT", "CATEGORIA", "ESP_CAT_INCORRECTA")]
    mixed_species_category_mt1 <- arrange_(mixed_species_category_mt1, BASE_FIELDS)
    ERRORS$mixed_species_category_mt1 <- mixed_species_category_mt1
    ERRORS$mixed_species_category_mt1 <- addTypeOfError(ERRORS$mixed_species_category_mt1, "muestreo MT1 con especie de mezcla que está agrupada en Especies para la Categoría")
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
      ERRORS$not_allowed_sampling_species <- addTypeOfError(ERRORS$not_allowed_sampling_species, "Muestreo con especie no permitida en Especies del Muestreo")

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
          allowed_genus$ALLOWED <- "ok" #¡¡CHAPUCILLA!!

        #check the genus
        checked_allowed_genus <- merge(x = to_check_genus, y =allowed_genus, by.x = "COD_ESP_MUE", by.y = "COD_ESP", all.x = TRUE)
        not_allowed_genus <- checked_allowed_genus[is.na(checked_allowed_genus$ALLOWED),]

        #to the ERRORS
        ERRORS$not_allowed_genus_sampled_species <- subset(not_allowed_genus, select = -c(ALLOWED))
        ERRORS$not_allowed_genus_sampled_species <- addTypeOfError(ERRORS$not_allowed_genus_sampled_species, "muestreo con género no permitido en Especies del muestreo")

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
      ERRORS$not_allowed_category_species <- addTypeOfError(ERRORS$not_allowed_category_species, "muestreo con especie no permitida en Especies de la categoría")
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
          allowed_genus$ALLOWED <- "ok" #¡¡CHAPUCILLA!!

          #check the genus
          # TO DO: arreglar esto, sobran variables
          checked_allowed_genus <- merge(x = to_check_genus, y =allowed_genus, by.x = "COD_ESP_CAT", by.y = "COD_ESP", all.x = TRUE)
          not_allowed_genus <- checked_allowed_genus[is.na(checked_allowed_genus$ALLOWED),]

        #to the ERRORS
        ERRORS$not_allowed_genus_category_species <- subset(not_allowed_genus, select = -c(ALLOWED))
        ERRORS$not_allowed_genus_category_species <- addTypeOfError(ERRORS$not_allowed_genus_category_species, "muestreos con géneros no permitidos en Especies de la Categoría")
        #remove unnecesary variables
        rm(to_check_genus, allowed_genus_other, allowed_genus, checked_allowed_genus, not_allowed_genus)


# ---- IN WEIGHTS

  # ---- errors sampled weight greater than landing weight ----
    sampled_weight_greater_landing_weight <- subset(catches_in_lengths, P_MUE_DESEM > P_DESEM)
    sampled_weight_greater_landing_weight <- sampled_weight_greater_landing_weight[,c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA","P_DESEM", "COD_ESP_CAT", "ESP_CAT","SEXO","P_MUE_DESEM" )]
    sampled_weight_greater_landing_weight["P_DESEM-P_MUE_DESEM"] <- round(sampled_weight_greater_landing_weight["P_DESEM"] - sampled_weight_greater_landing_weight["P_MUE_DESEM"],1)
    ERRORS$sampled_weight_greater_landing_weight <- sampled_weight_greater_landing_weight
    ERRORS$sampled_weight_greater_landing_weight <- addTypeOfError(ERRORS$sampled_weight_greater_landing_weight, "peso muestreado mayor que peso desembarcado")

  # ---- errors in species from the categories: all of them has exactly the same sampled weight
  # the last column in the ERRORS$same_sampled_weight dataframe show the number of category species with exactly the same sampled weight in every specie of the category
    selected_fields<-c(BASE_FIELDS,"N_CATEGORIAS","COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA","CATEGORIA","P_DESEM","COD_ESP_CAT", "ESP_CAT","SEXO","P_MUE_DESEM")
    same_sampled_weight<-catches_in_lengths[,selected_fields]
        by <- list(same_sampled_weight$COD_ID,
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
    ERRORS$same_sampled_weight <- addTypeOfError(ERRORS$same_sampled_weight, "todas las categorías de la especie tienen exactamente el mismo peso muestreado")
    #TO DO: check this dataframe... why return the COD_CATEGORIA and CATEGORIA fields??
    rm (selected_fields, by, same_sampled_weight)

  # ---- errors in the weight sampled similar to the category weight?
    weight_sampled_similar_weight_landing <- catches_in_lengths[,c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "P_DESEM", "COD_ESP_CAT", "ESP_CAT", "P_MUE_VIVO", "SOP")]
    weight_sampled_similar_weight_landing <- subset(weight_sampled_similar_weight_landing, P_DESEM==P_MUE_VIVO)
    weight_sampled_similar_weight_landing <- arrange_(weight_sampled_similar_weight_landing, c("PUERTO", "TIPO_MUE", "FECHA", "BARCO", "ESP_MUE", "CATEGORIA"))
    weight_sampled_similar_weight_landing["P_MUE_VIVO-SOP"] <- weight_sampled_similar_weight_landing["P_MUE_VIVO"] - weight_sampled_similar_weight_landing["SOP"]
    weight_sampled_similar_weight_landing["P_MUE_VIVO-SOP"] <- round(weight_sampled_similar_weight_landing["P_MUE_VIVO-SOP"], digits = 1)
    weight_sampled_similar_weight_landing["POR_DIF_P_MUE_VIVO-SOP"] <- (weight_sampled_similar_weight_landing["P_MUE_VIVO-SOP"] * 100) / weight_sampled_similar_weight_landing["P_MUE_VIVO"]
    weight_sampled_similar_weight_landing["POR_DIF_P_MUE_VIVO-SOP"] <- round(weight_sampled_similar_weight_landing["POR_DIF_P_MUE_VIVO-SOP"])
    ERRORS$weight_sampled_similar_weight_landing <- weight_sampled_similar_weight_landing
    ERRORS$weight_sampled_similar_weight_landing <- addTypeOfError(ERRORS$weight_sampled_similar_weight_landing, "peso muestreado igual al peso desembarcado")
    rm(weight_sampled_similar_weight_landing)
    unique(ERRORS$weight_sampled_similar_weight_landing$PUERTO)

  # ---- errors sop = 0
    ERRORS[["sop_zero"]] <- subset(catches_in_lengths, SOP == 0, select = c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "P_DESEM", "P_VIVO", "COD_ESP_CAT", "ESP_CAT", "P_MUE_DESEM", "P_MUE_VIVO", "SOP"))
    ERRORS[["sop_zero"]] <- addTypeOfError(ERRORS[["sop_zero"]], "sop = 0")
  
    # ---- errors
    ERRORS[["sampled_weight_zero"]] <- subset(catches_in_lengths, P_MUE_DESEM == 0, select = c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "P_DESEM", "P_VIVO", "COD_ESP_CAT", "ESP_CAT", "P_MUE_DESEM", "P_MUE_VIVO", "SOP"))
    ERRORS[["sampled_weight_zero"]] <- addTypeOfError(ERRORS[["sampled_weight_zero"]], "peso muestra = 0")
    
  # ---- errors p.desem = 0
    ERRORS[["weight_landed_zero"]] <- subset(catches_in_lengths, P_DESEM == 0 | is.na( P_DESEM),select = c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "P_DESEM", "P_VIVO"))
    ERRORS[["weight_landed_zero"]] <- addTypeOfError(ERRORS[["weight_landed_zero"]], "peso descargado = 0")
    
  # ---- errors species of the category WITHOUT length sample but WITH weight sample
    ERRORS[["weight_sampled_0_without_length_sampled"]] <- subset(catches_in_lengths, P_MUE_DESEM == 0 & EJEM_MEDIDOS == 0, select = c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "P_DESEM", "P_VIVO", "COD_ESP_CAT", "ESP_CAT", "P_MUE_DESEM", "EJEM_MEDIDOS"))
    ERRORS[["weight_sampled_0_without_length_sampled"]] <- addTypeOfError(ERRORS[["weight_sampled_0_without_length_sampled"]], "especie sin tallas muestreadas pero con peso muestra")
    
  # ---- errors species of the category WITH length sample but WITHOUT weight sample
    ERRORS[["lenght_sampled_without_weight_sampled"]] <- subset(catches_in_lengths, P_MUE_DESEM == 0 & EJEM_MEDIDOS != 0, select = c(BASE_FIELDS, "P_DESEM", "P_MUE_DESEM", "EJEM_MEDIDOS"))
    ERRORS[["lenght_sampled_without_weight_sampled"]] <- addTypeOfError(ERRORS[["lenght_sampled_without_weight_sampled"]], "especie con tallas muestreadas pero sin peso muestra")
  
    
  # ---- errors in samples with SOP greater than P_MUE_VIVO when P_MUE_VIVO != 0
  # REMOVE THIS BECAUSE I'VE TO CREATE THE 10% CHECK
    # ERRORS$sop_greater_pes_mue_vivo <- sopGreaterPesMueVivo(catches_in_lengths)
    # ERRORS$sop_greater_pes_mue_vivo <- addTypeOfError(ERRORS$sop_greater_pes_mue_vivo, "sop mayor que peso muestreado vivo")
    
  # ---- errors in samples with P_MUE_DESEM is 0 or NA
    ERRORS$pes_mue_desem_zero <- pesMueDesemZero(catches_in_lengths)
    ERRORS$pes_mue_desem_zero <- addTypeOfError(ERRORS$pes_mue_desem_zero, "peso muestreado desembarcado = 0")
    
  # ---- (warning) errors in species with categories with the same weight landing
    ERRORS$especies_con_categorias_igual_peso_desembarcado <- speciesWithCategoriesWithSameWeightLanding(catches)
    ERRORS$especies_con_categorias_igual_peso_desembarcado <- addTypeOfError(ERRORS$especies_con_categorias_igual_peso_desembarcado, "especie con todas sus categorías con igual peso desembarcado")

    
# #### COMBINE ERRORS ##########################################################
    combined_errors <- combineErrors()
    
  # Remove all the columns with only NA values.
  #Filter extracts the elements of a vector for which a predicate (logical) function gives true
  combined_errors <- Filter(function(x){!all(is.na(x))}, combined_errors)

    # ADD COLUMN COD_PUERTO
    # TODO: fix it in the import function?
    combined_errors <- merge(x = combined_errors, y = puerto, all.x = TRUE)

    # ORDER COLUMNS
    # TODO: include this in combineErrors()
    combined_errors <- combined_errors %>%
      select(COD_ID, LOCODE, COD_PUERTO, PUERTO, FECHA, COD_BARCO, BARCO, ESTRATO_RIM,
             COD_TIPO_MUE, TIPO_MUE, COD_ESP_MUE, ESP_MUE, COD_CATEGORIA, CATEGORIA, P_DESEM, P_VIVO, COD_ESP_CAT, ESP_CAT, SEXO, everything()) %>%
      select(-one_of("TIPO_ERROR"), one_of("TIPO_ERROR")) #remove TIPO_ERROR, and add it to the end

  # SEPARATE DATAFRAME IN INFLUENCE AREA
  combined_errors <- separateDataframeByInfluenceArea(combined_errors, "COD_PUERTO")
  
  
  # Order the errors
  combined_errors <- lapply(combined_errors, function(x){
    
    x <- x %>%
      arrange_("FECHA", "COD_ID", "COD_ESP_MUE", "COD_CATEGORIA", "COD_ESP_CAT")
    return(x)
    
  })
  
  
    

# #### EXPORT ERRORS TO CSV ####################################################

    exportListToCsv(combined_errors, suffix = paste0(YEAR,"_",MONTHASCHARACTER), separation = "_")
    
    #lapply(names(ERRORS), export_errors_lapply, ERRORS) #The 'ERRORS' argument is an argument to the export_errors_lapply function

# #### MAKE A BACKUP
# #### usually, when the files will be send to Supervisors Area
    # backup_files_to_send()
    