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
  BASE_FIELDS <- c("PUERTO", "LOCCODE", "FECHA", "COD_BARCO", "BARCO", "ESTRATO_RIM", "COD_TIPO_MUE", "TIPO_MUE")
  

################################################################################
# YOU HAVE ONLY TO CHANGE THIS VARIABLES:
PATH_FILENAME <- "F:/misdoc/sap/revision volcado/datos/"
FILENAME_DES_TOT <- "IEOUPMUEDESTOTMARCO.TXT"
FILENAME_DES_TAL <- "IEOUPMUEDESTALMARCO.TXT"
FILENAME_TAL <- "IEOUPMUETALMARCO.TXT"

MONTH <- "" #only if a filter by month is necesary. It's imperative use the atributte 'by_month' in import_muestreos_up() function
YEAR <- "2016"
################################################################################

PATH_ERRORS <- paste(PATH_FILENAME,"errors",sep="")
PATH_BACKUP <- paste(PATH_ERRORS, "/backup", sep="")

# #### FUNCTIONS ###############################################################


# ---- function to remove coma in the category "Chicharros, jureles" -----------
# return the dataframe corrected
remove_coma_in_category <- function(dataframe){
  dataframe$CATEGORIA<- gsub(",", "", dataframe$CATEGORIA)
  #assign('dataframe',dataframe,.GlobalEnv)#this doesn't work, and I dont know why
  return(dataframe)
}

# ---- function to change date format in all dataframes of muestreos_up list ----
change_date_format <- function (dataframe){
  dataframe$FECHA <- as.Date(dataframe$FECHA, "%d-%b-%y")
  dataframe$FECHA <- as.POSIXlt(dataframe$FECHA)
  dataframe$FECHA <- format(dataframe$FECHA, "%d-%m-%y")
}

# ---- function to filter by month all the dataframes of muestreos_up list ------
filter_by_month <- function (dataframe){
  # format the month:
  MONTH <- sprintf("%02d", MONTH)
  
  # filter:
  dataframe <- dataframe[format.Date(dataframe$FECHA, "%m") == MONTH,]
  
  #return dataframe:
  return(dataframe)
}

# ---- function to import the 'tallas por up' files ----------------------------
# require the 3 files from 'tallas por up'
# return a list with 3 data frames
# by_month --> to select only one month. MONTH must be fill in the constants section. False by default.
# export --> to export muestreos_up dataframe in csv file. False by default.
import_muestreos_up <- function(by_month = FALSE, export = FALSE){
  # full paths for every file
  fullpath_des_tot <- paste(PATH_FILENAME, FILENAME_DES_TOT, sep="")
  fullpath_des_tal <- paste(PATH_FILENAME, FILENAME_DES_TAL, sep="")
  fullpath_tal <- paste(PATH_FILENAME, FILENAME_TAL, sep="/")
  
  # import files to data.frame
  catches <- read.table(fullpath_des_tot, sep=";", header = TRUE) 
  catches_in_lengths <- read.table(fullpath_des_tal, sep=";", header = TRUE) 
  lengths <- read.table(fullpath_tal, sep=";", header = TRUE) 
  
  # group in list
  muestreos_up<-list(catches_in_lengths=catches_in_lengths, lengths=lengths, catches=catches)

  # change the column "FECHA" to a date format
    # to avoid some problems with Spanish_Spain.1252 (or if you are using another locale), change locale to Spanish_United States.1252:
    lct <- Sys.getlocale("LC_TIME")
    Sys.setlocale("LC_TIME","Spanish_United States.1252")
      
      # change the format with lapply
        # it's necesary the last x inside the function, to return de vale of x modified
      muestreos_up <- lapply(muestreos_up, function(x){x$FECHA <- change_date_format(x); x})
   
    # and now the come back to the initial configuration of locale:
    Sys.setlocale("LC_TIME", lct)
    
    # filter by month, only in case by_month == TRUE
    if (by_month == TRUE){
      muestreos_up <- lapply(muestreos_up, function(x){x <- filter_by_month(x); x})    
    }
    
    # remove coma in the name of the categories
    # I don't know why this doesn't work:
    # lapply(muestreos_up, function(x){x <- remove_coma_in_category(x)})
    # so I've to do this:
    muestreos_up$catches <- remove_coma_in_category(muestreos_up$catches)
    muestreos_up$catches_in_lengths <- remove_coma_in_category(muestreos_up$catches_in_lengths)
    muestreos_up$lengths <- remove_coma_in_category(muestreos_up$lengths)
  
  #return list
  return(muestreos_up)
}

# ---- function to save the errors in csv files: -------------------------------
export_errors_lapply <- function(x, errors){
  if(nrow(errors[[x]])!= 0){
    print(x)  
    # separate by influence area
    by_area <- merge(ERRORS[[x]], AREAS_INFLUENCE, by.x = "LOCCODE", by.y = "LOCCODE", all.x = TRUE )
    by_area <- dlply (by_area, "AREA")
    print(names(by_area))
      for (area in names(by_area)){
        by_area[[area]] <- by_area[[area]][, !colnames(by_area[[area]]) %in% c("PUERTO.y")]
        fullpath<-paste(PATH_ERRORS, "/", area, "_", YEAR, "_", MONTH, "_errors_", x, ".csv", sep="")
        write.csv(by_area[[area]], file=fullpath, row.names = FALSE, quote = FALSE)
        MESSAGE_ERRORS$by_area$area<<-"has errors" #this not recomended in R but is the only way I know
        print(x)       
      }
  } else {
    fullpath<-paste(PATH_ERRORS, "/", YEAR, "_", MONTH, "_no_errors_", x, ".csv", sep="")
    write.csv(errors[[x]], file=fullpath, row.names = FALSE, quote = FALSE)
    MESSAGE_ERRORS[[x]]<<-"errors free" #this not recomended in R but is the only way I know
    print(paste('Great,', x, 'is error free!'))
  }
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

# #### IMPORT DATA #############################################################

#read the mixed species file
cat_spe_mixed<-read.csv("especies_mezcla.csv", header=TRUE)

#read the no mixed species file
sampled_spe_no_mixed<-read.csv("especies_no_mezcla.csv", header=TRUE)

#read the estrato-rim - arte file to obtain the correct estratorim, gear and its relation
CORRECT_ESTRATORIM_ARTE<-read.csv("estratorim_arte.csv", header=TRUE, sep = ";")
CORRECT_ESTRATORIM_ARTE$VALID<-TRUE

##obtain the correct division from divisiones.csv
CORRECT_DIVISION <- read.csv("divisiones.csv")
CORRECT_DIVISION <- levels(CORRECT_DIVISION$DIVISION)

###obtain the correct gears
CORRECT_GEARS <- levels(CORRECT_ESTRATORIM_ARTE$ARTE)

###obtain the correct ESTRATO_RIM
CORRECT_ESTRATO_RIM <- levels(CORRECT_ESTRATORIM_ARTE$ESTRATO_RIM)

###obtain the not allowed species
NOT_ALLOWED_SPECIES <- read.csv("especies_no_permitidas.csv", fileEncoding = "UTF-8")

### obtain the allowed genus
ALLOWED_GENUS <- read.csv("generos_permitidos.csv")

### obtain the cfpo
CFPO <- read.table("CFPO2015.csv", sep=";", quote = "", header = TRUE)
  # ignore superfluous columns
  CFPO <- CFPO[,c("CODIGO_BUQUE", "ESTADO")]
  
### obtain areas of invfluence
AREAS_INFLUENCE <- read.csv("areas_influencia.csv")



################################################################################  
# #### IMPORT muestreos_UP files ###############################################  
muestreos_up <- import_muestreos_up(by_month = FALSE)
  
#isolate dataframes
catches <- muestreos_up$catches
catches_in_lengths <- muestreos_up$catches_in_lengths
lengths <- muestreos_up$lengths
################################################################################  


# #### SEARCHING ERRORS ########################################################
# ---- IN HEADER ----

  # ---- 'procedencia': must be allways IEO ----
    levels(catches$PROCEDENCIA)
  
  # ---- search errors in type sample
    errors_type_sample <- catches%>%
                          select_(.dots = BASE_FIELDS) %>%
                          filter(COD_TIPO_MUE != 1 & COD_TIPO_MUE != 2 & COD_TIPO_MUE != 4) %>%
                          group_by_(BASE_FIELDS)  
    ERRORS$errors_type_sample <- errors_type_sample

  # ---- search errors in 'gear'
    #TO DO: USE CODES
    ERRORS[["gears"]] <- catches[!catches$ARTE %in% CORRECT_GEARS, ]
    ERRORS[["gears"]] <- ERRORS$gears[,c(BASE_FIELDS, "ARTE")]
    ERRORS[["gears"]] <- unique(ERRORS$gears)
    ERRORS[["gears"]] <- arrange(ERRORS$gears, PUERTO, FECHA, BARCO, ESTRATO_RIM, ARTE)
    levels(droplevels(ERRORS$gears$PUERTO))
    levels(droplevels(ERRORS$gears$ARTE))
 
  # ---- search errors in 'origin'
    #TO DO: USE CODES
    ERRORS[["division"]] <- catches[!catches$ORIGEN %in% CORRECT_DIVISION, ]
    ERRORS[["division"]] <- ERRORS$division[,c(BASE_FIELDS, "COD_ORIGEN", "ORIGEN")]
    ERRORS[["division"]] <- unique(ERRORS$division)
    ERRORS[["division"]] <- arrange(ERRORS$division, PUERTO, FECHA, BARCO, ESTRATO_RIM, ORIGEN)
    levels(droplevels(ERRORS$division$PUERTO))
    levels(droplevels(ERRORS$division$ORIGEN))
  
  # ---- search errors in 'ESTRATO_RIM'
    estrato_rim <- catches[!catches$ESTRATO_RIM %in% CORRECT_ESTRATO_RIM, ]
    estrato_rim <- estrato_rim[,c(BASE_FIELDS)]
    estrato_rim <- unique(estrato_rim)
    ERRORS[["estrato_rim"]] <- arrange(estrato_rim, PUERTO, FECHA, BARCO, ESTRATO_RIM)
  
  # ---- coherence between 'ESTRATO_RIM' and 'gear'
    coherence_estrato_rim_gear<-merge(x=catches, y=CORRECT_ESTRATORIM_ARTE, by.x = c("ESTRATO_RIM","ARTE"), by.y = c("ESTRATO_RIM", "ARTE"), all.x = TRUE)
    incoherent_data<- -which(coherence_estrato_rim_gear$VALID)
    ERRORS[["coherence_estrato_rim_gear"]]<-coherence_estrato_rim_gear[incoherent_data,c(BASE_FIELDS, "ARTE")]
    ERRORS[["coherence_estrato_rim_gear"]] <-  unique(ERRORS[["coherence_estrato_rim_gear"]])
    levels(droplevels(ERRORS$coherence_estrato_rim_gear$PUERTO))
  
  # ---- search errors in number of ships (empty field, =0 or >2)
    ERRORS[["ships"]] <- subset(catches, N_BARCOS==0 | N_BARCOS>2 | is.null(N_BARCOS))
  
  # ---- search errors in number of rejects (only empty fields)
    ERRORS[["rejections"]] <- subset(catches, is.null(N_RECHAZOS))
  
  # ---- search duplicate samples between MT1 and MT2
    dup <- catches[,c(BASE_FIELDS)]
    dup <- unique(dup)
    dup <- dup[,c("LOCCODE", "PUERTO", "FECHA", "COD_BARCO", "BARCO", "ESTRATO_RIM")]
    dup <- aggregate(x = dup$FECHA, by = list(dup$LOCCODE, dup$PUERTO, dup$FECHA, dup$COD_BARCO, dup$BARCO, dup$ESTRATO_RIM), FUN = length)
    colnames(dup) <-c ("LOCCODE", "PUERTO", "FECHA", "COD_BARCO", "BARCO", "ESTRATO_RIM", "DUPLICADOS")
    dup <- dup[dup$DUPLICADOS>1,]
    dup <- arrange(dup, PUERTO, FECHA, BARCO, ESTRATO_RIM)
    ERRORS[["duplicated_mt1_mt2"]]<-arrange(dup, PUERTO, FECHA, BARCO) 
    rm(dup)
  
  # ---- search errors in country
    ERRORS$errors_countries_mt1 <- subset(catches, COD_TIPO_MUE == 1 & (COD_PAIS != 724 | is.na(COD_PAIS)), c(BASE_FIELDS, "COD_PAIS"))
    ERRORS$errors_countries_mt2 <- subset(catches, COD_TIPO_MUE == 2 & (COD_PAIS != 724 | is.na(COD_PAIS)), c(BASE_FIELDS, "COD_PAIS"))
    
  # ---- search errors in ships
  ##### TO DO: ADD CHECKING WITH SIRENO FILES
    to_ships <- unique(catches[,c(BASE_FIELDS, "CODSGPM")])
    errors_ships <- merge(x=to_ships, y=CFPO, by.x = "CODSGPM", by.y = "CODIGO_BUQUE", all.x = TRUE)

      #ships withouth coincidences in cfpo
      errors_ships_not_in_cfpo <- subset(errors_ships, is.na(errors_ships$ESTADO))
      errors_ships_not_in_cfpo <- errors_ships_not_in_cfpo[, c(BASE_FIELDS, "CODSGPM", "ESTADO")]
      errors_ships_not_in_cfpo <- arrange_(errors_ships_not_in_cfpo, BASE_FIELDS)
      ERRORS$errors_ships_not_in_cfpo <- errors_ships_not_in_cfpo
      
      #ships with state different to "alta definitiva"
      errors_ships_not_registered <- subset(errors_ships, ESTADO != "ALTA DEFINITIVA")
      errors_ships_not_registered <- errors_ships_not_registered[, c(BASE_FIELDS, "CODSGPM", "ESTADO")]
      errors_ships_not_registered <- arrange_(errors_ships_not_registered, BASE_FIELDS)
      ERRORS$errors_ships_not_registered <- errors_ships_not_registered

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
    mixed_species_sample <- mixed_species_sample[, c("LOCCODE", "PUERTO", "TIPO_MUE", "ESTRATO_RIM", "FECHA", "BARCO", "ESP_MUE_INCORRECTA")]
    #order dataframe:
    mixed_species_sample<-arrange_(mixed_species_sample, BASE_FIELDS)
    ERRORS$mixed_species_sample <- mixed_species_sample
    rm(selected_fields, mixed_species_sample)

  # ---- errors in not mixed species keyed as mixed species
    selected_fields<-catches[,c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE")]    
    #search the errors:
    no_mixed_species_sample <- merge(x=selected_fields, y=sampled_spe_no_mixed, by.x="ESP_MUE", by.y="ESP")    
    #order columns dataframe:
    no_mixed_species_sample <- no_mixed_species_sample[c("LOCCODE", "PUERTO", "TIPO_MUE", "ESTRATO_RIM", "FECHA", "COD_BARCO", "BARCO", "COD_ESP_MUE", "ESP_MUE")]
    #change the name of a column in dataframe. ???OMG!!!:
    names(no_mixed_species_sample)[names(no_mixed_species_sample) == 'ESP_MUE'] <- 'ESP_MUESTREO_INCORRECTA'
    #order dataframe:
    no_mixed_species_sample<-arrange_(no_mixed_species_sample, BASE_FIELDS)
    # ---- MT1
    no_mixed_species_sample_mt1<-subset(no_mixed_species_sample, TIPO_MUE == "MT1A (Encuestas IEO)")
    ERRORS$no_mixed_species_sample_mt1 <- no_mixed_species_sample_mt1
    # ---- MT2
    no_mixed_species_sample_mt2<-subset(no_mixed_species_sample, TIPO_MUE == "MT2A (Biometrico puerto)")
    ERRORS$no_mixed_species_sample_mt2 <- no_mixed_species_sample_mt2
      # ---- Special format to send Ricardo: he has to change the sampled species to its category species
      # byx = c(BASE_FIELDS, "ESP_MUESTREO_INCORRECTA")
      # byy = c(BASE_FIELDS, "ESP_MUE")
      # Ricardo_no_mixed_species_sample_mt2 <- merge(x = no_mixed_species_sample_mt2, y = catches_in_lengths, by.x = byx, by.y = byy, all.x = TRUE)
      # Ricardo_no_mixed_species_sample_mt2 <- Ricardo_no_mixed_species_sample_mt2[, c(BASE_FIELDS, "BARCOD", "ESP_MUESTREO_INCORRECTA","ESPCOD", "CATEGORIA", "ESPECIE", "ESPCOD2")]
      # # ---- change column names
      # colnames(Ricardo_no_mixed_species_sample_mt2)[colnames(Ricardo_no_mixed_species_sample_mt2)=="ESPCOD"] <- "COD_ESP_MUESTREO_INCORRECTA"
      # colnames(Ricardo_no_mixed_species_sample_mt2)[colnames(Ricardo_no_mixed_species_sample_mt2)=="ESPECIE"] <- "ESP_MUESTREO_CORRECTA"
      # colnames(Ricardo_no_mixed_species_sample_mt2)[colnames(Ricardo_no_mixed_species_sample_mt2)=="ESPCOD2"] <- "COD_ESP_MUESTREO_CORRECTA"
      #   # delete the species that have non-grouped specie in species category
      #   Ricardo_no_mixed_species_sample_mt2 <- Ricardo_no_mixed_species_sample_mt2[ ! Ricardo_no_mixed_species_sample_mt2$ESP_MUESTREO_CORRECTA %in% as.character(sampled_spe_no_mixed$ESPECIE),]
      #   # ---- reorder columns
      #   Ricardo_no_mixed_species_sample_mt2 <- Ricardo_no_mixed_species_sample_mt2[, c("PUERTO","FECHA","BARCO","BARCOD","ESTRATO_RIM","TIP_MUESTREO","ESP_MUESTREO_INCORRECTA","COD_ESP_MUESTREO_INCORRECTA","CATEGORIA","ESP_MUESTREO_CORRECTA", "COD_ESP_MUESTREO_CORRECTA")]
      #   write.csv(Ricardo_no_mixed_species_sample_mt2, file=paste(PATH_ERRORS, "Ricardo_no_mixed_species_sample_mt2.csv", sep="/"), quote = FALSE, row.names = FALSE)
      
    rm(selected_fields, no_mixed_species_sample, no_mixed_species_sample_mt1, no_mixed_species_sample_mt2) 

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
    # ---- MT1
    mixed_species_category_mt1 <- subset(mixed_species_category, TIPO_MUE == "MT1A (Encuestas IEO)")
    mixed_species_category_mt1 <- mixed_species_category_mt1[, c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_ESP_CAT", "CATEGORIA", "ESP_CAT_INCORRECTA")]
    mixed_species_category_mt1 <- arrange_(mixed_species_category_mt1, BASE_FIELDS)
    ERRORS$mixed_species_category_mt1 <- mixed_species_category_mt1
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
          allowed_genus <- allowed_genus_other
          allowed_genus$ALLOWED <- "ok" #¡¡CHAPUCILLA!!
          
          #check the genus
          checked_allowed_genus <- merge(x = to_check_genus, y =allowed_genus, by.x = "COD_ESP_CAT", by.y = "COD_ESP", all.x = TRUE)
          not_allowed_genus <- checked_allowed_genus[is.na(checked_allowed_genus$ALLOWED),]
        
        #to the ERRORS
        ERRORS$not_allowed_genus_category_species <- subset(not_allowed_genus, select = -c(ALLOWED))
        
        #remove unnecesary variables
        rm(to_check_genus, allowed_genus_other, allowed_genus, checked_allowed_genus, not_allowed_genus)


# ---- IN WEIGHTS
    
  # ---- errors sampled weight greater than landing weight ----
    sampled_weight_greater_landing_weight <- subset(catches_in_lengths, P_MUE_DESEM > P_DESEM)    
    sampled_weight_greater_landing_weight <- sampled_weight_greater_landing_weight[,c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA","P_DESEM", "COD_ESP_CAT", "ESP_CAT","SEXO","P_MUE_DESEM" )]
    sampled_weight_greater_landing_weight["P_DESEM-P_MUE_DESEM"] <- round(sampled_weight_greater_landing_weight["P_DESEM"] - sampled_weight_greater_landing_weight["P_MUE_DESEM"],1)
    ERRORS$sampled_weight_greater_landing_weight<-sampled_weight_greater_landing_weight

  # ---- errors in species from the categories: all of them has exactly the same sampled weight
  # the last column in the ERRORS$same_sampled_weight dataframe show the number of category species with exactly the same sampled weight in every specie of the category
    selected_fields<-c(BASE_FIELDS,"N_CATEGORIAS","COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA","CATEGORIA","P_DESEM","COD_ESP_CAT", "ESP_CAT","SEXO","P_MUE_DESEM")
    same_sampled_weight<-catches_in_lengths[,selected_fields]
        by <- list(same_sampled_weight$PUERTO,
                   same_sampled_weight$LOCCODE,
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
    colnames(same_sampled_weight) <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA","CATEGORIA","P_DESEM","SEXO","P_MUE_DESEM","NUM_OCU")
    same_sampled_weight<-subset ( same_sampled_weight, NUM_OCU >1)
    same_sampled_weight<-arrange_(same_sampled_weight, BASE_FIELDS)
    ERRORS$same_sampled_weight<-same_sampled_weight 
    rm (selected_fields, by, same_sampled_weight)
    
  # ---- errors in the weight sampled similar to the category weight?
    weight_sampled_similar_weight_landing <- catches_in_lengths[,c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "P_DESEM", "COD_ESP_CAT", "ESP_CAT", "P_MUE_VIVO", "SOP")]
    weight_sampled_similar_weight_landing <- subset(weight_sampled_similar_weight_landing, P_DESEM==P_MUE_VIVO)    
    weight_sampled_similar_weight_landing <- arrange_(weight_sampled_similar_weight_landing, c("PUERTO", "TIPO_MUE", "FECHA", "BARCO", "ESP_MUE", "CATEGORIA"))
    weight_sampled_similar_weight_landing["P_MUE_VIVO-SOP"] <- weight_sampled_similar_weight_landing["P_MUE_VIVO"] - weight_sampled_similar_weight_landing["SOP"]
    weight_sampled_similar_weight_landing["P_MUE_VIVO-SOP"] <- round(weight_sampled_similar_weight_landing["P_MUE_VIVO-SOP"], digits = 1)
    weight_sampled_similar_weight_landing["POR_DIF"] <- (weight_sampled_similar_weight_landing["P_MUE_VIVO-SOP"] * 100) / weight_sampled_similar_weight_landing["P_MUE_VIVO"]
    weight_sampled_similar_weight_landing["POR_DIF"] <- round(weight_sampled_similar_weight_landing["POR_DIF"])
    ERRORS$weight_sampled_similar_weight_landing<-weight_sampled_similar_weight_landing
    rm(weight_sampled_similar_weight_landing)
    unique(ERRORS$weight_sampled_similar_weight_landing$PUERTO)

  # ---- errors sop = 0
    ERRORS[["sop_zero"]] <- subset(catches_in_lengths, SOP == 0, select = c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "P_DESEM", "P_VIVO", "COD_ESP_CAT", "ESP_CAT", "P_MUE_DESEM", "P_MUE_VIVO", "SOP"))   
    
  # ---- errors 
    ERRORS[["sampled_weight_zero"]] <- subset(catches_in_lengths, P_MUE_DESEM == 0, select = c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "P_DESEM", "P_VIVO", "COD_ESP_CAT", "ESP_CAT", "P_MUE_DESEM", "P_MUE_VIVO", "SOP"))
    
  # ---- errors p.desem = 0
    ERRORS[["weight_landed_zero"]] <- subset(catches_in_lengths, P_DESEM == 0 | is.na( P_DESEM),select = c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "P_DESEM", "P_VIVO"))   

  # ---- errors species of the category WITHOUT length sample but WITH weight sample
    ERRORS[["weight_sampled_0_without_length_sampled"]] <- subset(catches_in_lengths, P_MUE_DESEM == 0 & EJEM_MEDIDOS == 0, select = c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "COD_CATEGORIA", "CATEGORIA", "P_DESEM", "P_VIVO", "COD_ESP_CAT", "ESP_CAT", "P_MUE_DESEM", "EJEM_MEDIDOS"))
    
  # ---- errors species of the category WITH length sample but WITHOUT weight sample
    ERRORS[["lenght_sampled_without_weight_sampled"]] <- subset(catches_in_lengths, P_MUE_DESEM == 0 & EJEM_MEDIDOS != 0, select = c(BASE_FIELDS, "P_DESEM", "P_MUE_DESEM", "EJEM_MEDIDOS"))
    
  
# #### EXPORT ERRORS TO CSV ####################################################
  lapply(names(ERRORS), export_errors_lapply, ERRORS) #The 'ERRORS' argument is an argument to the export_errors_lapply function
# #### MAKE A BACKUP
# #### usually, when the files will be send to Supervisors Area
    backup_files_to_send()
