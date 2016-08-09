#### Review data dump
#### Script to check the monthly data dumps in SIRENO
#### 
#### Return csv files with errors detected
####
#### author: Marco A. Amez Fernandez
#### email: ieo.marco.a.amez@gmail.com
#### date of last modification: 27/7/2016
#### version: 1.46
####
#### files required: esp mezcla.csv, especies_no_mezcla.csv,
#### estratorim_arte.csv, divisiones.csv, especies_no_permitidas.csv,
#### CFPO2015.csv (this one not available in github)


# #### CONFIG ##################################################################

# ---- PACKAGES ----------------------------------------------------------------

library(dplyr) #arrange_()
library(tools) #file_path_sans_ext()
library(stringr) #str_subset()


# ---- SET WORKING DIRECTORY ---------------------------------------------------

setwd("F:/misdoc/sap/revision volcado/revision_volcado_R/")


# ---- CONSTANTS ---------------------------------------------------------------

PATH <- getwd()

# ---- GLOBAL VARIABLES --------------------------------------------------------
ERRORS <- list() #list with all errors found in dataframes
MESSAGE_ERRORS<- list() #list with the errors
# GLOBAL.TIPO.MUESTREO.ICES create in the import_tallas_x_up
# BASE_FIELDS create in the import_tallas_x_up

################################################################################
# YOU HAVE ONLY TO CHANGE THIS VARIABLES:
PATH_FILENAME <- "F:/misdoc/sap/revision volcado/datos/"
FILENAME <- "muestreos_2016_new0108.TXT"
MONTH <- ""
YEAR <- "2016"
################################################################################

PATH_ERRORS <- paste(PATH_FILENAME,"errors",sep="")
PATH_BACKUP <- paste(PATH_ERRORS, "/backup", sep="")

# #### FUNCTIONS ###############################################################


#function to split the file tallas_x_up
  ##filename: name to import
  ##export = "TRUE" the file is export to csc
split_tallas_x_up <- function(path_filename, filename, export="FALSE", month_selected=""){

  fullpath<-paste(path_filename, filename, sep="/")
  
  #read the file to find where are the "----"
  temp_tallas_x_up<-readLines(c(fullpath))
  cut_rows<-grep("^---", temp_tallas_x_up, value="FALSE")#return two values
  
  #if '---' not exist, this is not a valid file
  if(length(cut_rows)==0){
    stop ("This file doesn't like a valid a tallas_x_up file.")
  } 
  
  #split the file:
  full_lengths<-read.table(file=fullpath, nrows=(cut_rows[1]-2), head=TRUE, sep=";", fill=TRUE)
  
    ##catches_in_lengths: catches of the sampled species
    catches_in_lengths<-subset(full_lengths, full_lengths$TALLA=="T.T. :")
    ##lengths
    lengths<-subset(full_lengths, full_lengths$TALLA!="T.T. :")
    ##catchs
    catches<-read.table(file=fullpath, skip=(cut_rows[2]), sep=";", head=TRUE)
    sapply(catches, class)
    

    #correct category "0901 Chicharros, jureles"
    catches$CATEGORIA <- as.character(catches$CATEGORIA)
    catches$CATEGORIA[catches$CATEGORIA == "0901 Chicharros, jureles"] <- "0901 Chicharros jureles"
    catches$CATEGORIA <- as.factor(catches$CATEGORIA)
    
    catches_in_lengths$CATEGORIA <- as.character(catches_in_lengths$CATEGORIA)
    catches_in_lengths$CATEGORIA[catches_in_lengths$CATEGORIA == "0901 Chicharros, jureles"] <- "0901 Chicharros jureles"
    catches_in_lengths$CATEGORIA <- as.factor(catches_in_lengths$CATEGORIA)
    
    lengths$CATEGORIA <- as.character(lengths$CATEGORIA)
    lengths$CATEGORIA[lengths$CATEGORIA == "0901 Chicharros, jureles"] <- "0901 Chicharros jureles"
    lengths$CATEGORIA <- as.factor(lengths$CATEGORIA)
    
  
  #select only the month
    if (month_selected != ""){
      # to avoid some problems with Spanish_Spain.1252 (or if you are using another locale), change locale to Spanish_United States.1252:
      lct <- Sys.getlocale("LC_TIME")
      Sys.setlocale("LC_TIME","Spanish_United States.1252")
      
      catches$fecha_formateada <- (as.character(catches$FECHA))
      catches$fecha_formateada <- as.Date(catches$fecha_formateada, "%d-%b-%y")
      catches$fecha_formateada <- as.POSIXlt(catches$fecha_formateada)
      catches$month <- catches$fecha_formateada$mon+1 #+1 because POSIXlt$mon is 0 to 11
      catches <- catches[catches$month==month_selected,]
      
      catches_in_lengths$fecha_formateada <- (as.character(catches_in_lengths$FECHA))
      catches_in_lengths$fecha_formateada <- as.Date(catches_in_lengths$fecha_formateada, "%d-%b-%y")
      catches_in_lengths$fecha_formateada <- as.POSIXlt(catches_in_lengths$fecha_formateada)
      catches_in_lengths$month <- catches_in_lengths$fecha_formateada$mon+1 #+1 because POSIXlt$mon is 0 to 11
      catches_in_lengths <- catches_in_lengths[catches_in_lengths$month==month_selected,]
  
      lengths$fecha_formateada <- (as.character(lengths$FECHA))
      lengths$fecha_formateada <- as.Date(lengths$fecha_formateada, "%d-%b-%y")
      lengths$fecha_formateada <- as.POSIXlt(lengths$fecha_formateada)
      lengths$month <- lengths$fecha_formateada$mon+1 #+1 because POSIXlt$mon is 0 to 11
      lengths <- lengths[lengths$month==month_selected,]
      
      # and now the return the initial configuration of locale:
      Sys.setlocale("LC_TIME", lct)
    }
  #group in list
  tallas_x_up<-list(catches_in_lengths=catches_in_lengths, lengths=lengths, catches=catches)
  
  #remove the extension
  filename_without_extension <- file_path_sans_ext(filename)
  
  #export in csv
  if(export=="TRUE"){
    write.csv(tallas_x_up$catches, paste(PATH_FILENAME, filename_without_extension, "_catches.csv", sep=""), quote=FALSE, row.names=FALSE)
    write.csv(tallas_x_up$catches_in_lengths, paste(PATH_FILENAME, filename_without_extension, "_catches_in_lengths.cvs", sep=""), quote=FALSE, row.names=FALSE)
    write.csv(tallas_x_up$lengths, paste(PATH_FILENAME, filename_without_extension, "_lengths.csv", sep=""), quote=FALSE, row.names=FALSE)
  }
  #return data
  return(tallas_x_up)
}

#function to save the errors in csv files:
export_errors_lapply<-function(x, errors){
  if(nrow(errors[[x]])!= 0){
    fullpath<-paste(PATH_ERRORS, "/", YEAR, "_", MONTH, "_errors_", x, ".csv", sep="")
    write.csv(errors[[x]], file=fullpath, row.names = FALSE, quote = FALSE)
    MESSAGE_ERRORS[[x]]<<-"has errors" #this not recomended in R but is the only way I know
    print(x)
  } else {
    fullpath<-paste(PATH_ERRORS, "/", YEAR, "_", MONTH, "_no_errors_", x, ".csv", sep="")
    write.csv(errors[[x]], file=fullpath, row.names = FALSE, quote = FALSE)
    MESSAGE_ERRORS[[x]]<<-"errors free" #this not recomended in R but is the only way I know
    print(paste('Great,', x, 'is error free!'))
  }
}

#function to make a copy of the files previous to send to the Area Supervisor
backup_files_to_send <- function(){
  date <- Sys.time();
  date <- as.POSIXlt(date);
  
  directory_backup<-paste(PATH_BACKUP, "/", YEAR, "_", MONTH, "_backup_", date$mday,
                   '_', date$mon, '_', date$year+1900, '_', date$hour, '_',
                   date$min, '_', round(date$sec, 0), "/", sep="")
  dir.create(directory_backup, recursive=TRUE); #create the directory backup
  
  files <- list.files(PATH_ERRORS, pattern = "*.csv", full.names = TRUE)

  lapply(as.list(files), function(x){ file.copy(x, directory_backup)})
}

# #### IMPORT DATA #############################################################

#import tallas_x_up
tallas_x_up<-split_tallas_x_up(path_filename=PATH_FILENAME, filename=FILENAME, export=FALSE, month_selected = MONTH)

##select type of file: tallas_x_up from SIRENO or tallas_x_up extracted by sireno's team
##in the future, the tallas_x_up file from SIRENO will be similar than the extracted by sirenos's team
##so this won't be necessary
GLOBAL.TIPO.MUESTREO.ICES <- ""

if (is.null(tallas_x_up$catches$TIPO.MUESTREO.ICES)){
  GLOBAL.TIPO.MUESTREO.ICES="TIP_MUESTREO"
}

BASE_FIELDS <- c("PUERTO", "FECHA", "BARCO", "UNIPESCOD", GLOBAL.TIPO.MUESTREO.ICES)  ###list with the common fields used in the tables

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

###obtain the correct unipescod
CORRECT_UNIPESCOD <- levels(CORRECT_ESTRATORIM_ARTE$ESTRATO_RIM)

###obtain the not allowed species
NOT_ALLOWED_SPECIES <- read.csv("especies_no_permitidas.csv", fileEncoding = "UTF-8")

### obtain the cfpo
CFPO <- read.table("CFPO2015.csv", sep=";", quote = "", header = TRUE)
  # ignore superfluous columns
  CFPO <- CFPO[,c("CODIGO_BUQUE", "ESTADO")]

#isolating dataframes
catches<-tallas_x_up[["catches"]]
catches_in_lengths<-tallas_x_up[["catches_in_lengths"]]


# #### SEARCHING ERRORS ########################################################
# ---- IN HEADER ----
  
  # ---- 'procedencia': must be allways IEO ----
    levels(catches$PROCEDENCIA)
  
  # ---- search errors in type sample
    ERRORS[["type_sample"]] <- catches[catches["TIPO.MUESTREO"]!="Concurrente en lonja" & catches["TIPO.MUESTREO"]!="Concurrente a bordo" ,]
    ERRORS[["type_sample"]] <- unique(ERRORS$type_sample[,c(BASE_FIELDS, "TIPO.MUESTREO")])
    ERRORS[["type_sample"]] <- arrange(ERRORS$type_sample, PUERTO, FECHA, BARCO, UNIPESCOD)
    levels(droplevels(ERRORS$type_sample$PUERTO))
    
  # ---- search errors in 'gear'
    ERRORS[["gears"]] <- catches[!catches$ARTE %in% CORRECT_GEARS, ]
    ERRORS[["gears"]] <- ERRORS$gears[,c(BASE_FIELDS, "ARTE")]
    ERRORS[["gears"]] <- unique(ERRORS$gears)
    ERRORS[["gears"]] <- arrange(ERRORS$gears, PUERTO, FECHA, BARCO, UNIPESCOD, ARTE)
    levels(droplevels(ERRORS$gears$PUERTO))
    levels(droplevels(ERRORS$gears$ARTE))
 
  # ---- search errors in 'origin'
    ERRORS[["division"]] <- catches[!catches$ORIGEN %in% CORRECT_DIVISION, ]
    ERRORS[["division"]] <- ERRORS$division[,c(BASE_FIELDS, "ORIGEN")]
    ERRORS[["division"]] <- unique(ERRORS$division)
    ERRORS[["division"]] <- arrange(ERRORS$division, PUERTO, FECHA, BARCO, UNIPESCOD, ORIGEN)
    levels(droplevels(ERRORS$division$PUERTO))
    levels(droplevels(ERRORS$division$ORIGEN))
  
  # ---- search errors in 'unipescod'
    ERRORS[["unipescod"]] <- catches[!catches$UNIPESCOD %in% CORRECT_UNIPESCOD, ]
    ERRORS[["unipescod"]] <- ERRORS$unipescod[,c(BASE_FIELDS)]
    ERRORS[["unipescod"]] <- arrange(ERRORS$unipescod, PUERTO, FECHA, BARCO, UNIPESCOD)
  
  # ---- coherence between 'unipescod' and 'gear'
    coherence_unipescod_gear<-merge(x=catches, y=CORRECT_ESTRATORIM_ARTE, by.x = c("UNIPESCOD","ARTE"), by.y = c("ESTRATO_RIM", "ARTE"), all.x = TRUE)
    incoherent_data<- -which(coherence_unipescod_gear$VALID)
    ERRORS[["coherence_unipescod_gear"]]<-coherence_unipescod_gear[incoherent_data,c(BASE_FIELDS, "ARTE")]
    ERRORS[["coherence_unipescod_gear"]] <-  unique(ERRORS[["coherence_unipescod_gear"]])
    levels(droplevels(ERRORS$coherence_unipescod_gear$PUERTO))
  
  # ---- search errors in number of ships (empty field, =0 or >2)
  # only available in tallas_x_up extracted by sireno's team:
    if (GLOBAL.TIPO.MUESTREO.ICES == "TIP_MUESTREO"){
      ERRORS[["ships"]] <- subset(catches, NUMBARCOS==0 | NUMBARCOS>2 | is.null(NUMBARCOS))
    }
  
  # ---- search errors in number of rejects (only empty fields)
  # only available in tallas_x_up extracted by sireno's team:
    if (GLOBAL.TIPO.MUESTREO.ICES == "TIP_MUESTREO"){
    ERRORS[["rejections"]] <- subset(catches, is.null(NRECHAZOS))
    }
  
  # ---- search duplicate samples between MT1 and MT2
    dup <- catches[,c(BASE_FIELDS)]
    dup <- unique(dup)
    dup <- dup[,c("PUERTO", "FECHA", "BARCO", "UNIPESCOD")]
    dup <- aggregate(x = dup$FECHA, by = list(dup$PUERTO, dup$FECHA, dup$BARCO, dup$UNIPESCOD), FUN = length)
    colnames(dup) <-c ("PUERTO", "FECHA", "BARCO", "UNIPESCOD", "DUPLICADOS")
    dup <- dup[dup$DUPLICADOS>1,]
    dup <- arrange(dup, PUERTO, FECHA, BARCO, UNIPESCOD)
    ERRORS[["duplicated_mt1_mt2"]]<-arrange(dup, PUERTO, FECHA, BARCO) 
    rm(dup)
  
  # ---- search errors in country
    ERRORS$errors_countries_mt1 <- subset(catches, get(GLOBAL.TIPO.MUESTREO.ICES) == 1 & (PAIS != 724 | is.na(PAIS)), c(BASE_FIELDS, "PAIS"))
    ERRORS$errors_countries_mt2 <- subset(catches, get(GLOBAL.TIPO.MUESTREO.ICES) == 2 & (PAIS != 724 | is.na(PAIS)), c(BASE_FIELDS, "PAIS"))
    
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
    selected_fields<-catches[,c(BASE_FIELDS, "ESPECIE.TAX.")]
    #search the errors:
    ERRORS[["mixed_species_sample"]]<-merge(x=selected_fields, y=cat_spe_mixed["ESP_CATEGORIA"], by.x="ESPECIE.TAX.", by.y="ESP_CATEGORIA")
    #change the name of a column in dataframe. ???OMG!!!:
    names(ERRORS$mixed_species_sample)[names(ERRORS$mixed_species_sample) == 'ESPECIE.TAX.'] <- 'ESP_MUESTREO_INCORRECTA'
    #order columns dataframe:
    ERRORS$mixed_species_sample <- ERRORS$mixed_species_sample[, c("PUERTO", GLOBAL.TIPO.MUESTREO.ICES, "UNIPESCOD", "FECHA", "BARCO", "ESP_MUESTREO_INCORRECTA")]
    #order dataframe:
    ERRORS[["mixed_species_sample"]]<-arrange_(ERRORS[["mixed_species_sample"]], BASE_FIELDS)
    rm(selected_fields)
    
  # ---- errors in not mixed species keyed as mixed species
    selected_fields<-catches[,c(BASE_FIELDS, "ESPECIE.TAX.")]    
    #search the errors:
    no_mixed_species_sample <- merge(x=selected_fields, y=sampled_spe_no_mixed["ESPECIE"], by.x="ESPECIE.TAX.", by.y="ESPECIE")    
    #order columns dataframe:
    no_mixed_species_sample <- no_mixed_species_sample[c("PUERTO", GLOBAL.TIPO.MUESTREO.ICES, "UNIPESCOD", "FECHA", "BARCO", "ESPECIE.TAX.")]
    #change the name of a column in dataframe. ???OMG!!!:
    names(no_mixed_species_sample)[names(no_mixed_species_sample) == 'ESPECIE.TAX.'] <- 'ESP_MUESTREO_INCORRECTA'
    #order dataframe:
    no_mixed_species_sample<-arrange_(no_mixed_species_sample, BASE_FIELDS)
    # ---- MT1
    ERRORS$no_mixed_species_sample_mt1<-subset(no_mixed_species_sample, get(GLOBAL.TIPO.MUESTREO.ICES) == "1")
    # ---- MT2
    no_mixed_species_sample_mt2<-subset(no_mixed_species_sample, get(GLOBAL.TIPO.MUESTREO.ICES) == "2")
    ERRORS$no_mixed_species_sample_mt2 <- no_mixed_species_sample_mt2
      # ---- Special format to send Ricardo: he has to change the sampled species to its category species
      # byx = c(BASE_FIELDS, "ESP_MUESTREO_INCORRECTA")
      # byy = c(BASE_FIELDS, "ESPECIE.TAX.")
      # Ricardo_no_mixed_species_sample_mt2 <- merge(x = no_mixed_species_sample_mt2, y = catches_in_lengths, by.x = byx, by.y = byy, all.x = TRUE)
      # Ricardo_no_mixed_species_sample_mt2 <- Ricardo_no_mixed_species_sample_mt2[, c(BASE_FIELDS, "BARCOD", "ESP_MUESTREO_INCORRECTA","ESPCOD", "CATEGORIA", "ESPECIE", "ESPCOD2")]
      # # ---- change column names
      # colnames(Ricardo_no_mixed_species_sample_mt2)[colnames(Ricardo_no_mixed_species_sample_mt2)=="ESPCOD"] <- "COD_ESP_MUESTREO_INCORRECTA"
      # colnames(Ricardo_no_mixed_species_sample_mt2)[colnames(Ricardo_no_mixed_species_sample_mt2)=="ESPECIE"] <- "ESP_MUESTREO_CORRECTA"
      # colnames(Ricardo_no_mixed_species_sample_mt2)[colnames(Ricardo_no_mixed_species_sample_mt2)=="ESPCOD2"] <- "COD_ESP_MUESTREO_CORRECTA"
      #   # delete the species that have non-grouped specie in species category
      #   Ricardo_no_mixed_species_sample_mt2 <- Ricardo_no_mixed_species_sample_mt2[ ! Ricardo_no_mixed_species_sample_mt2$ESP_MUESTREO_CORRECTA %in% as.character(sampled_spe_no_mixed$ESPECIE),]
      #   # ---- reorder columns
      #   Ricardo_no_mixed_species_sample_mt2 <- Ricardo_no_mixed_species_sample_mt2[, c("PUERTO","FECHA","BARCO","BARCOD","UNIPESCOD","TIP_MUESTREO","ESP_MUESTREO_INCORRECTA","COD_ESP_MUESTREO_INCORRECTA","CATEGORIA","ESP_MUESTREO_CORRECTA", "COD_ESP_MUESTREO_CORRECTA")]
      #   write.csv(Ricardo_no_mixed_species_sample_mt2, file=paste(PATH_ERRORS, "Ricardo_no_mixed_species_sample_mt2.csv", sep="/"), quote = FALSE, row.names = FALSE)
      
    rm(selected_fields) 
    
    

  # ---- errors in mixed species of the category ----
    selected_fields<-catches_in_lengths[,c(BASE_FIELDS, "ESPECIE.TAX.", "CATEGORIA", "ESPECIE")]
    #species not allowed in category because are mixed especies:
    not_allowed_in_category <- as.data.frame(cat_spe_mixed$ESP_MUESTREO)
    not_allowed_in_category <- unique(not_allowed_in_category)
    colnames(not_allowed_in_category)<- c("ESP_MUESTREO")
    #search the errors:
    #mixed_species_category<-merge(x=selected_fields, y=cat_spe_mixed["ESP_MUESTREO"], by.x="ESPECIE", by.y = "ESP_MUESTREO")
    mixed_species_category<-merge(x=selected_fields, y=not_allowed_in_category["ESP_MUESTREO"], by.x="ESPECIE", by.y = "ESP_MUESTREO")
    #change the name of a column in dataframe. ???OMG!!!:
    names(mixed_species_category)[names(mixed_species_category) == 'ESPECIE'] <- 'ESP_CATEGORIA_INCORRECTA'
    # ---- MT2
    ERRORS$mixed_species_category_mt2 <- subset(mixed_species_category, get(GLOBAL.TIPO.MUESTREO.ICES) == "2")
    ERRORS$mixed_species_category_mt2 <- ERRORS$mixed_species_category_mt2[, c(BASE_FIELDS, "ESPECIE.TAX.", "CATEGORIA", "ESP_CATEGORIA_INCORRECTA")]
    ERRORS$mixed_species_category_mt2<-arrange_(ERRORS$mixed_species_category_mt2, BASE_FIELDS)
    # ---- MT1
    ERRORS$mixed_species_category_mt1 <- subset(mixed_species_category, get(GLOBAL.TIPO.MUESTREO.ICES) == "1")
    ERRORS$mixed_species_category_mt1 <- ERRORS$mixed_species_category_mt1[, c(BASE_FIELDS, "ESPECIE.TAX.", "CATEGORIA", "ESP_CATEGORIA_INCORRECTA")]
    ERRORS$mixed_species_category_mt1<-arrange_(ERRORS$mixed_species_category_mt1, BASE_FIELDS)
    rm(selected_fields)
    
  # ---- not allowed species
    # ---- in sampled species
      ERRORS$not_allowed_sampling_species <- merge(x = catches, y = NOT_ALLOWED_SPECIES, by.x = "ESPECIE.TAX.", by.y = "ESPECIE")
      ERRORS$not_allowed_sampling_species <- ERRORS$not_allowed_sampling_species[c(BASE_FIELDS,"COD","ESPECIE.TAX.")]
      #change the name of a column in dataframe. ???OMG!!!:
      names(ERRORS$not_allowed_sampling_species)[names(ERRORS$not_allowed_sampling_species) == 'ESPECIE.TAX.'] <- 'ESP_MUESTREO_INCORRECTA'
      ERRORS$not_allowed_sampling_species <- arrange_(ERRORS[["not_allowed_sampling_species"]], BASE_FIELDS)
      # ---- genus not allowed
        # select all the genus to check
        to_check_genus <- grep("(.+(formes$))|(.+(spp$))|(.+(sp$))|(.+(dae$))",catches$ESPECIE.TAX.)
          # . = any single character
          # + = one of more of previous
          # | = or
        to_check_genus<-catches[to_check_genus, c(BASE_FIELDS, "ESPECIE.TAX.")]
        
        # allowed genus
        # allowed genus from mixed species
        allowed_genus_mixed <- as.data.frame(unique(cat_spe_mixed$ESP_CATEGORIA))
        colnames(allowed_genus_mixed)<-"ESPECIE"
        
        # allowed genus from non mixed species
        allowed_genus_no_mixed <- as.data.frame(sampled_spe_no_mixed$ESPECIE)
        colnames(allowed_genus_no_mixed)<-"ESPECIE"
        
        #allowed genus complete
        allowed_genus <- rbind(allowed_genus_mixed, allowed_genus_no_mixed)
        allowed_genus$ALLOWED <- "ok"
        
        #check the genus
        check_genus_allowed <- merge(x = to_check_genus, y =allowed_genus, all.x = TRUE)
        not_allowed_genus_sampled_species <- check_genus_allowed[is.na(check_genus_allowed$ALLOWED),]
        
        #to the ERRORS
        ERRORS$not_allowed_genus_sampled_species <- subset(not_allowed_genus_sampled_species, select = -c(ALLOWED))
        
        #remove unnecesary variables
        rm(to_check_genus, allowed_genus_mixed, allowed_genus_no_mixed, allowed_genus,check_genus_allowed)

      
    # ---- in category species
      ERRORS$not_allowed_category_species <- merge(x = catches_in_lengths, y = NOT_ALLOWED_SPECIES, by.x = "ESPECIE", by.y = "ESPECIE")
      ERRORS$not_allowed_category_species <- ERRORS$not_allowed_category_species[c(BASE_FIELDS,"ESPECIE.TAX.","CATEGORIA", "ESPECIE", "COD")]
      #change the name of a column in dataframe. ???OMG!!!:
      names(ERRORS$not_allowed_category_species)[names(ERRORS$not_allowed_category_species) == 'ESPECIE'] <- 'ESP_CATEGORIA_INCORRECTA'
      ERRORS$not_allowed_category_species <- arrange_(ERRORS[["not_allowed_category_species"]], BASE_FIELDS)
      # ---- genus not allowed
        # select all the genus to check
        to_check_genus <- grep("(.+(formes$))|(.+(spp$))|(.+(sp$))|(.+(dae$))",catches_in_lengths$ESPECIE)
          # . = any single character
          # + = one of more of previous
          # | = or
        to_check_genus<-catches_in_lengths[to_check_genus, c(BASE_FIELDS, "ESPECIE.TAX.","CATEGORIA", "ESPECIE")]
        
        # allowed genus
          # allowed genus from mixed species
          allowed_genus_mixed <- as.data.frame(unique(cat_spe_mixed$ESP_CATEGORIA))
          colnames(allowed_genus_mixed)<-"ESPECIE"
        
          # allowed genus from non mixed species
          allowed_genus_no_mixed <- as.data.frame(sampled_spe_no_mixed$ESPECIE)
          colnames(allowed_genus_no_mixed)<-"ESPECIE"
        
          #allowed genus complete
          allowed_genus <- rbind(allowed_genus_mixed, allowed_genus_no_mixed)
          allowed_genus$ALLOWED <- "ok"
        
        #check the genus
        check_genus_allowed <- merge(x = to_check_genus, y =allowed_genus, all.x = TRUE)
        not_allowed_genus_category_species <- check_genus_allowed[is.na(check_genus_allowed$ALLOWED),]
        
        #to the ERRORS
        ERRORS$not_allowed_genus_category_species <- subset(not_allowed_genus_category_species, select = -c(ALLOWED))
        
        #remove unnecesary variables
        rm(to_check_genus, allowed_genus_mixed, allowed_genus_no_mixed, allowed_genus,check_genus_allowed)


# ---- IN WEIGHTS
    
  # ---- errors sampled weight greater than landing weight ----
    sampled_weight_greater_landing_weight <- subset(catches_in_lengths, P.MUE.DES > P.DESEM.)    
    sampled_weight_greater_landing_weight <- sampled_weight_greater_landing_weight[,c(BASE_FIELDS, "ESPECIE.TAX.", "CATEGORIA","P.DESEM.","ESPECIE","SEXO","P.MUE.DES" )]
    sampled_weight_greater_landing_weight["DIF.DESEM.-MUE."] <- round(sampled_weight_greater_landing_weight["P.DESEM."] - sampled_weight_greater_landing_weight["P.MUE.DES"])
    ERRORS$sampled_weight_greater_landing_weight<-sampled_weight_greater_landing_weight

  # ---- errors in species from the categories: all of them has exactly the same sampled weight
    selected_fields<-c(BASE_FIELDS,"NºCAT.","ESPECIE.TAX.","CATEGORIA","P.DESEM.","ESPECIE","SEXO","P.MUE.DES")
    same_sampled_weight<-catches_in_lengths[,selected_fields]
    by <- list(same_sampled_weight$PUERTO,same_sampled_weight$FECHA,same_sampled_weight$BARCO,same_sampled_weight$TIP_MUESTREO,same_sampled_weight$UNIPESCOD,same_sampled_weight$ESPECIE.TAX.,same_sampled_weight$CATEGORIA,same_sampled_weight$P.DESEM.,same_sampled_weight$SEXO,same_sampled_weight$P.MUE.DES)
    same_sampled_weight<-aggregate(x = same_sampled_weight$P.MUE.DES, by = by, FUN= length)
    colnames(same_sampled_weight) <- c(BASE_FIELDS, "ESPECIE.TAX.","CATEGORIA","P.DESEM.","SEXO","P.MUE.DES","NUM_OCU")
    same_sampled_weight<-subset ( same_sampled_weight, NUM_OCU >1)
    same_sampled_weight<-arrange(same_sampled_weight, PUERTO, FECHA, BARCO, ESPECIE.TAX., CATEGORIA)
    ERRORS$same_sampled_weight<-same_sampled_weight 
    rm (selected_fields, by, same_sampled_weight)
    
  # ---- errors in the weight sampled similar to the category weight?
    weight_sampled_similar_weight_landing <- catches_in_lengths[,c(BASE_FIELDS, "ESPECIE.TAX.", "CATEGORIA", "P.DESEM.", "ESPECIE", "P.MUE.VIVO", "S.O.P.")]
    weight_sampled_similar_weight_landing <- weight_sampled_similar_weight_landing[weight_sampled_similar_weight_landing[, "P.DESEM."]==weight_sampled_similar_weight_landing[,"P.MUE.VIVO"],]    
    weight_sampled_similar_weight_landing <- arrange_(weight_sampled_similar_weight_landing, c("PUERTO", GLOBAL.TIPO.MUESTREO.ICES, "FECHA", "BARCO", "ESPECIE.TAX.", "CATEGORIA"))
    weight_sampled_similar_weight_landing["P.MUE.VIVO-SOP"] <- weight_sampled_similar_weight_landing["P.MUE.VIVO"] - weight_sampled_similar_weight_landing["S.O.P."]
    weight_sampled_similar_weight_landing["P.MUE.VIVO-SOP"] <- round(weight_sampled_similar_weight_landing["P.MUE.VIVO-SOP"], digits = 1)
    weight_sampled_similar_weight_landing["POR.DIF"] <- (weight_sampled_similar_weight_landing["P.MUE.VIVO-SOP"] * 100) / weight_sampled_similar_weight_landing["P.MUE.VIVO"]
    weight_sampled_similar_weight_landing["POR.DIF"] <- round(weight_sampled_similar_weight_landing["POR.DIF"])
    ERRORS$weight_sampled_similar_weight_landing<-weight_sampled_similar_weight_landing
    rm(weight_sampled_similar_weight_landing)
    unique(ERRORS$weight_sampled_similar_weight_landing$PUERTO)

  # ---- errors sop = 0
    ERRORS[["sop_zero"]] <- subset(catches_in_lengths, S.O.P. == 0, select = c(BASE_FIELDS, "ESPECIE.TAX.", "CATEGORIA", "P.DESEM.", "P.VIVO", "ESPECIE", "P.MUE.DES", "P.MUE.VIVO", "S.O.P."))   
    
  # ---- errors 
    ERRORS[["sampled_weight_zero"]] <- subset(catches_in_lengths, P.MUE.DES == 0, select = c(BASE_FIELDS, "ESPECIE.TAX.", "CATEGORIA", "P.DESEM.", "P.VIVO", "ESPECIE", "P.MUE.DES", "P.MUE.VIVO", "S.O.P."))   
    
  # ---- errors p.desem = 0
    ERRORS[["weight_landed_zero"]] <- subset(catches_in_lengths, P.DESEM. == 0 | is.na( P.DESEM.),select = c(BASE_FIELDS, "ESPECIE.TAX.", "CATEGORIA", "P.DESEM.", "P.VIVO"))   

  # ---- errors species of the category WITHOUT length sample but WITH weight sample
    ERRORS[["weight_sampled_0_without_length_sampled"]] <- subset(catches_in_lengths, P.MUE.DES == 0 & EJEMPLARES.MEDIDOS == 0, select = c(BASE_FIELDS, "ESPECIE.TAX.", "CATEGORIA", "P.DESEM.", "ESPECIE", "P.MUE.DES", "EJEMPLARES.MEDIDOS"))
    
  # ---- errors species of the category WITH length sample but WITHOUT weight sample
    ERRORS[["lenght_sampled_without_weight_sampled"]] <- subset(catches_in_lengths, P.MUE.DES == 0 & EJEMPLARES.MEDIDOS != 0, select = c(BASE_FIELDS, "P.DESEM.", "P.MUE.DES", "EJEMPLARES.MEDIDOS"))
    
  
# #### EXPORT ERRORS TO CSV ####################################################
  lapply(names(ERRORS), export_errors_lapply, ERRORS) #The 'ERRORS' argument is an argument to the export_errors_lapply function
# #### MAKE A BACKUP
# #### usually, when the files will be send to Supervisors Area
    backup_files_to_send()
