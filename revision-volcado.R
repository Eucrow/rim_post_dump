#### Review data dump
#### Script to check the data dumps monthly in SIRENO
#### 
#### Return csv files with errors detected
####
#### author: Marco A. Amez Fernandez
#### email: ieo.marco.a.amez@gmail.com
#### date of last modification: 17/6/2016
#### version: 1.1
####
#### files required: esp mezcla.csv, estratorim_arte.csv, divisiones.csv


# #### CONFIG ##################################################################

# ---- PACKAGES ----------------------------------------------------------------

library(plyr)
library(tools) #for file_path_sans_ext()


# ---- SET WORKING DIRECTORY ---------------------------------------------------

setwd("F:/misdoc/sap/revision volcado/revision_volcado_R/")


# ---- CONSTANTS ---------------------------------------------------------------

PATH <- getwd()

# ---- GLOBAL VARIABLES --------------------------------------------------------
ERRORS <- list() #list with all errors found in dataframes
MESSAGE_ERRORS<- list() #list with the errors

################################################################################
# YOU HAVE ONLY TO CHANGE THIS VARIABLES:
PATH_FILENAME <- "F:/misdoc/sap/revision volcado/datos/"
FILENAME <- "tallas_x_up_1er_trim_2016.txt"
MONTH <- "3"
YEAR <- "2016"
################################################################################

PATH_ERRORS <- paste(PATH_FILENAME,"/errors",sep="")

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
  
  #select only the month
    if (month_selected != ""){
      # to avoid some problems with Spanish_Spain.1252 (or if you are use another locale), change locale a Spanish_United States.1252:
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

# #### IMPORT DATA #############################################################

#import tallas_x_up
tallas_x_up<-split_tallas_x_up(path_filename=PATH_FILENAME, filename=FILENAME, export=FALSE, month_selected = MONTH)

#read the mixed species file
cat_spe_mixed<-read.csv("especies_mezcla.csv", header=TRUE)

#read the estrato-rim - arte file to obtain the correct estratorim, gear and its relation
CORRECT_ESTRATORIM_ARTE<-read.csv("estratorim_arte.csv", header=TRUE, sep = ";")
CORRECT_ESTRATORIM_ARTE$VALID<-TRUE

##obtain the correct division from divisiones.csv
CORRECT_DIVISION<-read.csv("divisiones.csv", fileEncoding = "UTF-8")
CORRECT_DIVISION<-levels(CORRECT_DIVISION$DIVISION)

###obtain the correct gears
CORRECT_GEARS<-levels(CORRECT_ESTRATORIM_ARTE$ARTE)

###obtain the correct unipescod
CORRECT_UNIPESCOD<-levels(CORRECT_ESTRATORIM_ARTE$ESTRATO_RIM)

#isolating dataframes
catches<-tallas_x_up[["catches"]]
catches_in_lengths<-tallas_x_up[["catches_in_lengths"]]

#select only information of the month


# #### SEARCHING ERRORS ########################################################
# ---- IN HEADER ----

# ---- 'procedencia': must be allways IEO ----
levels(catches$PROCEDENCIA)

##search errors in gear
ERRORS[["gears"]] <- catches[!catches$ARTE %in% CORRECT_GEARS, ]
ERRORS[["gears"]] <- ERRORS$gears[,c("FECHA", "TIPO.MUESTREO.ICES", "TIPO.MUESTREO", "PROCEDENCIA", "UNIPESCOD", "PUERTO", "BARCO", "ORIGEN", "ARTE")]
ERRORS[["gears"]] <- unique(ERRORS$gears)
ERRORS[["gears"]] <- arrange(ERRORS$gears, UNIPESCOD, ARTE, FECHA, BARCO)

##search errors in origin
ERRORS[["division"]] <- catches[!catches$ORIGEN %in% CORRECT_DIVISION, ]
ERRORS[["division"]] <- ERRORS$division[,c("FECHA", "TIPO.MUESTREO.ICES", "TIPO.MUESTREO", "PROCEDENCIA", "UNIPESCOD", "PUERTO", "BARCO", "ORIGEN", "ARTE")]
ERRORS[["division"]] <- unique(ERRORS$division)

##search errors in 'unipescod'
ERRORS[["unipescod"]] <- catches[!catches$UNIPESCOD %in% CORRECT_UNIPESCOD, ]

##coherence between 'unipescod' and 'gear'
coherence_unipescod_gear<-merge(x=catches, y=CORRECT_ESTRATORIM_ARTE, by.x = c("UNIPESCOD","ARTE"), by.y = c("ESTRATO_RIM", "ARTE"), all.x = TRUE)
incoherent_data<- -which(coherence_unipescod_gear$VALID)
ERRORS[["coherence_unipescod_gear"]]<-coherence_unipescod_gear[incoherent_data,]


# ---- estrato_rim, gear and division coherence ----
# TO DO  


# ---- IN SPECIES ----

    #busca las especies para la categoria que estan en especies del muestreo
    # ---- errors in mixed species of the sample ----
    selected_fields<-catches[,c("PUERTO", "TIPO.MUESTREO.ICES", "UNIPESCOD", "FECHA", "BARCO", "ORIGEN", "ARTE", "ESPECIE.TAX.")]
    #search the errors:
    ERRORS[["mixed_species_sample"]]<-merge(x=selected_fields, y=cat_spe_mixed["ESP_CATEGORIA"], by.x="ESPECIE.TAX.", by.y="ESP_CATEGORIA")
    #change the name of a column in dataframe. ???OMG!!!:
    names(ERRORS$mixed_species_sample)[names(ERRORS$mixed_species_sample) == 'ESPECIE.TAX.'] <- 'ESP_MUESTREO_INCORRECTA'
    #order columns dataframe:
    ERRORS$mixed_species_sample <- ERRORS$mixed_species_sample[, c("PUERTO", "TIPO.MUESTREO.ICES", "UNIPESCOD", "FECHA", "BARCO", "ORIGEN", "ARTE","ESP_MUESTREO_INCORRECTA")]
    #order dataframe:
    ERRORS[["mixed_species_sample"]]<-arrange(ERRORS[["mixed_species_sample"]], PUERTO, TIPO.MUESTREO.ICES, FECHA, BARCO)
    rm(selected_fields)
    
    # ---- errors in mixed species of the category ----
    selected_fields<-catches_in_lengths[,c("PUERTO", "TIPO.MUESTREO.ICES", "UNIPESCOD", "FECHA", "BARCO", "ORIGEN", "ARTE", "ESPECIE.TAX.", "CATEGORIA", "ESPECIE")]
    #search the errors:
    ERRORS[["mixed_species_category"]]<-merge(x=selected_fields, y=cat_spe_mixed["ESP_MUESTREO"], by.x="ESPECIE", by.y = "ESP_MUESTREO")
    #change the name of a column in dataframe. ???OMG!!!:
    names(ERRORS$mixed_species_category)[names(ERRORS$mixed_species_category) == 'ESPECIE'] <- 'ESP_CATEGORIA_INCORRECTA'
    #order columns dataframe:
    ERRORS$mixed_species_category <- ERRORS$mixed_species_category[, c("PUERTO", "TIPO.MUESTREO.ICES", "UNIPESCOD", "FECHA", "BARCO", "ORIGEN", "ARTE", "ESPECIE.TAX.", "CATEGORIA", "ESP_CATEGORIA_INCORRECTA")]
    #order dataframe:
    ERRORS[["mixed_species_category"]]<-arrange(ERRORS[["mixed_species_category"]], PUERTO, TIPO.MUESTREO.ICES, FECHA, BARCO)
    rm(selected_fields)  

# ---- IN WEIGHTS
# ---- errors sampled weight greater than landing weight ----
    ERRORS[["sampled_weight_greater_landing_weight"]]<-catches_in_lengths[catches_in_lengths[,"P.MUE.DES"] > catches_in_lengths[,"P.DESEM."],]

# ---- erros in species from the categories: all of them has exactly the same sampled weight
#    TO DO
    
# ---- errors in the weight sampled similar to the category weight?
    desem_mues_sop <- catches_in_lengths[,c("FECHA", "PUERTO", "TIPO.MUESTREO.ICES", "BARCO", "ESPECIE.TAX.", "CATEGORIA", "P.DESEM.", "ESPECIE", "P.MUE.DES", "S.O.P.")]
    desem_mues_sop <- desem_mues_sop[desem_mues_sop[, "P.DESEM."]==desem_mues_sop[,"P.MUE.DES"],]    
    desem_mues_sop <- arrange(desem_mues_sop, PUERTO, TIPO.MUESTREO.ICES, FECHA, BARCO, ESPECIE.TAX., CATEGORIA)
    desem_mues_sop["P.MUE.DES-SOP"] <- desem_mues_sop["P.MUE.DES"] - desem_mues_sop["S.O.P."]
    desem_mues_sop["P.MUE.DES-SOP"] <- round(desem_mues_sop["P.MUE.DES-SOP"])
    desem_mues_sop["POR.DIF"] <- (desem_mues_sop["P.MUE.DES-SOP"] * 100) / desem_mues_sop["P.MUE.DES"]
    desem_mues_sop["POR.DIF"] <- round(desem_mues_sop["POR.DIF"])
    ERRORS$desem_mues_sop<-desem_mues_sop

# ---- errors sop = 0
    ERRORS[["sop_zero"]]<-catches_in_lengths[catches_in_lengths[,"S.O.P."] == 0,]

# ---- errors 
    ERRORS[["sampled_weight_zero"]]<-catches_in_lengths[catches_in_lengths[,"P.MUE.DES"] == 0,]    

  
# #### EXPORT ERRORS TO CSV ####################################################
  lapply(names(ERRORS), export_errors_lapply, ERRORS) #The 'ERRORS' argument is an argument to the export_errors_lapply function






