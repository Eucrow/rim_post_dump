#### Review data dump
#### Script to check the data dumps monthly in SIRENO
#### 
#### Return csv files with errors detected
####
#### author: Marco A. Amez Fernandez
#### email: ieo.marco.a.amez@gmail.com
#### date of last modification: 24/6/2016
#### version: 1.3
####
#### files required: esp mezcla.csv, especies_no_mezcla.csv,
#### estratorim_arte.csv, divisiones.csv, especies_no_permitidas.csv


# #### CONFIG ##################################################################

# ---- PACKAGES ----------------------------------------------------------------

library(dplyr) #arrange_()
library(tools) #file_path_sans_ext()


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
FILENAME <- "MUESTREOS_1T_2016.csv"
MONTH <- "1"
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


##select type of file: tallas_x_up from SIRENO or tallas_x_up extracted by sireno's team
##in the future, the tallas_x_up file from SIRENO will be similar than the extracted by sirenos's team
##so this won't be necessary
GLOBAL.TIPO.MUESTREO.ICES <- ""

if (is.null(tallas_x_up$catches$TIPO.MUESTREO.ICES)){
  GLOBAL.TIPO.MUESTREO.ICES="TIP_MUESTREO"
}

#read the mixed species file
cat_spe_mixed<-read.csv("especies_mezcla.csv", header=TRUE)

#read the no mixed species file
sampled_spe_no_mixed<-read.csv("especies_no_mezcla.csv", header=TRUE)

#read the estrato-rim - arte file to obtain the correct estratorim, gear and its relation
CORRECT_ESTRATORIM_ARTE<-read.csv("estratorim_arte.csv", header=TRUE, sep = ";")
CORRECT_ESTRATORIM_ARTE$VALID<-TRUE

##obtain the correct division from divisiones.csv
CORRECT_DIVISION <- read.csv("divisiones.csv", fileEncoding = "UTF-8")
CORRECT_DIVISION <- levels(CORRECT_DIVISION$DIVISION)

###obtain the correct gears
CORRECT_GEARS <- levels(CORRECT_ESTRATORIM_ARTE$ARTE)

###obtain the correct unipescod
CORRECT_UNIPESCOD <- levels(CORRECT_ESTRATORIM_ARTE$ESTRATO_RIM)

###obtain the not allowed species
NOT_ALLOWED_SPECIES <- read.csv("especies_no_permitidas.csv", fileEncoding = "UTF-8")

#isolating dataframes
catches<-tallas_x_up[["catches"]]
catches_in_lengths<-tallas_x_up[["catches_in_lengths"]]


# #### SEARCHING ERRORS ########################################################
# ---- IN HEADER ----

# ---- 'procedencia': must be allways IEO ----
levels(catches$PROCEDENCIA)

##search errors in type sample
ERRORS[["type_sample"]] <- catches[catches["TIPO.MUESTREO"]!="Concurrente en lonja" & catches["TIPO.MUESTREO"]!="Concurrente a bordo" ,]
ERRORS[["type_sample"]] <- unique(ERRORS$type_sample[,c("PUERTO", "FECHA", "BARCO", "UNIPESCOD", "TIPO.MUESTREO")])
ERRORS[["type_sample"]] <- arrange(ERRORS$type_sample, FECHA, BARCO, UNIPESCOD)
  
##search errors in 'gear'
ERRORS[["gears"]] <- catches[!catches$ARTE %in% CORRECT_GEARS, ]
ERRORS[["gears"]] <- ERRORS$gears[,c("FECHA", GLOBAL.TIPO.MUESTREO.ICES, "TIPO.MUESTREO", "UNIPESCOD", "PUERTO", "BARCO", "ORIGEN", "ARTE")]
ERRORS[["gears"]] <- unique(ERRORS$gears)
ERRORS[["gears"]] <- arrange(ERRORS$gears, UNIPESCOD, ARTE, FECHA, BARCO)

##search errors in 'origin'
ERRORS[["division"]] <- catches[!catches$ORIGEN %in% CORRECT_DIVISION, ]
ERRORS[["division"]] <- ERRORS$division[,c("FECHA", GLOBAL.TIPO.MUESTREO.ICES, "TIPO.MUESTREO", "UNIPESCOD", "PUERTO", "BARCO", "ORIGEN", "ARTE")]
ERRORS[["division"]] <- unique(ERRORS$division)

##search errors in 'unipescod'
ERRORS[["unipescod"]] <- catches[!catches$UNIPESCOD %in% CORRECT_UNIPESCOD, ]

##coherence between 'unipescod' and 'gear'
coherence_unipescod_gear<-merge(x=catches, y=CORRECT_ESTRATORIM_ARTE, by.x = c("UNIPESCOD","ARTE"), by.y = c("ESTRATO_RIM", "ARTE"), all.x = TRUE)
incoherent_data<- -which(coherence_unipescod_gear$VALID)
ERRORS[["coherence_unipescod_gear"]]<-coherence_unipescod_gear[incoherent_data,c("PUERTO", "FECHA", "BARCO", "UNIPESCOD", "ARTE")]
ERRORS[["coherence_unipescod_gear"]] <-  unique(ERRORS[["coherence_unipescod_gear"]])

# search errors in number of ships (empty field, =0 or >2)
# only available in tallas_x_up extracted by sireno's team:
if (GLOBAL.TIPO.MUESTREO.ICES == "TIP_MUESTREO"){
  ERRORS[["ships"]] <- subset(catches, NUMBARCOS==0 | NUMBARCOS>2 | is.null(NUMBARCOS))
}

# search errors in number of rejects (only empty fields)
# only available in tallas_x_up extracted by sireno's team:
if (GLOBAL.TIPO.MUESTREO.ICES == "TIP_MUESTREO"){
ERRORS[["rejections"]] <- subset(catches, is.null(NRECHAZOS))
}

# ---- estrato_rim, gear and division coherence ----
# TO DO  


# ---- IN SPECIES ----

    #busca las especies para la categoria que estan en especies del muestreo
    # ---- errors in mixed species of the sample ----
    selected_fields<-catches[,c("PUERTO", GLOBAL.TIPO.MUESTREO.ICES, "UNIPESCOD", "FECHA", "BARCO", "ORIGEN", "ARTE", "ESPECIE.TAX.")]
    #search the errors:
    ERRORS[["mixed_species_sample"]]<-merge(x=selected_fields, y=cat_spe_mixed["ESP_CATEGORIA"], by.x="ESPECIE.TAX.", by.y="ESP_CATEGORIA")
    #change the name of a column in dataframe. ???OMG!!!:
    names(ERRORS$mixed_species_sample)[names(ERRORS$mixed_species_sample) == 'ESPECIE.TAX.'] <- 'ESP_MUESTREO_INCORRECTA'
    #order columns dataframe:
    ERRORS$mixed_species_sample <- ERRORS$mixed_species_sample[, c("PUERTO", GLOBAL.TIPO.MUESTREO.ICES, "UNIPESCOD", "FECHA", "BARCO", "ORIGEN", "ARTE","ESP_MUESTREO_INCORRECTA")]
    #order dataframe:
    temporal_list<- c("PUERTO", GLOBAL.TIPO.MUESTREO.ICES, "FECHA", "BARCO")
    ERRORS[["mixed_species_sample"]]<-arrange_(ERRORS[["mixed_species_sample"]], temporal_list)
    rm(selected_fields, temporal_list)
    
    # ---- errors in not mixed species keyed as mixed species
    selected_fields<-catches[,c("PUERTO", GLOBAL.TIPO.MUESTREO.ICES, "UNIPESCOD", "FECHA", "BARCO", "ORIGEN", "ARTE", "ESPECIE.TAX.")]    
    #search the errors:
    ERRORS[["no_mixed_species_sample"]] <- merge(x=selected_fields, y=sampled_spe_no_mixed["ESPECIE"], by.x="ESPECIE.TAX.", by.y="ESPECIE")    
    #order dataframe:
    temporal_list<- c("PUERTO", GLOBAL.TIPO.MUESTREO.ICES, "FECHA", "BARCO")
    ERRORS[["no_mixed_species_sample"]]<-arrange_(ERRORS[["no_mixed_species_sample"]], temporal_list)
    rm(selected_fields, temporal_list)
    
    # ---- errors in mixed species of the category ----
    selected_fields<-catches_in_lengths[,c("PUERTO", GLOBAL.TIPO.MUESTREO.ICES, "UNIPESCOD", "FECHA", "BARCO", "ORIGEN", "ARTE", "ESPECIE.TAX.", "CATEGORIA", "ESPECIE")]
    #search the errors:
    ERRORS[["mixed_species_category"]]<-merge(x=selected_fields, y=cat_spe_mixed["ESP_MUESTREO"], by.x="ESPECIE", by.y = "ESP_MUESTREO")
    #change the name of a column in dataframe. ???OMG!!!:
    names(ERRORS$mixed_species_category)[names(ERRORS$mixed_species_category) == 'ESPECIE'] <- 'ESP_CATEGORIA_INCORRECTA'
    #order columns dataframe:
    ERRORS$mixed_species_category <- ERRORS$mixed_species_category[, c("PUERTO", GLOBAL.TIPO.MUESTREO.ICES, "UNIPESCOD", "FECHA", "BARCO", "ORIGEN", "ARTE", "ESPECIE.TAX.", "CATEGORIA", "ESP_CATEGORIA_INCORRECTA")]
    #order dataframe:
    temporal_list<- c("PUERTO", GLOBAL.TIPO.MUESTREO.ICES, "FECHA", "BARCO")
    ERRORS[["mixed_species_category"]]<-arrange_(ERRORS[["mixed_species_category"]], temporal_list)
    rm(selected_fields, temporal_list)  
    
    # ---- not allowed species
    selected_fields <- catches[,c("PUERTO", GLOBAL.TIPO.MUESTREO.ICES, "UNIPESCOD", "FECHA", "BARCO", "ESPECIE.TAX.")]    
    ERRORS[["not_allowed_species"]] <- merge(x = selected_fields, y = NOT_ALLOWED_SPECIES, by.x = "ESPECIE.TAX.", by.y = "ESPECIE")
    temporal_list <- c("PUERTO", GLOBAL.TIPO.MUESTREO.ICES, "FECHA", "BARCO", "ESPECIE.TAX.")
    ERRORS[["not_allowed_species"]] <- arrange_(ERRORS[["not_allowed_species"]], temporal_list)
    rm(selected_fields, temporal_list)
    

# ---- IN WEIGHTS
# ---- errors sampled weight greater than landing weight ----
    ERRORS[["sampled_weight_greater_landing_weight"]]<-catches_in_lengths[catches_in_lengths[,"P.MUE.DES"] > catches_in_lengths[,"P.DESEM."],]

# ---- erros in species from the categories: all of them has exactly the same sampled weight
    selected_fields<-c("FECHA","TIPO.MUESTREO","PROCEDENCIA","UNIPESCOD","PUERTO","TIP_MUESTREO","BARCO","DIVISION","ORIGEN","ARTE","NºCAT.","ESPECIE.TAX.","CATEGORIA","P.DESEM.","P.VIVO","ESPECIE","SEXO","P.MUE.DES","P.MUE.VIVO")
    same_sampled_weight<-catches_in_lengths[,selected_fields]
    by <- list(same_sampled_weight$FECHA,same_sampled_weight$TIPO.MUESTREO,same_sampled_weight$UNIPESCOD,same_sampled_weight$PUERTO,same_sampled_weight$BARCO,same_sampled_weight$ESPECIE.TAX.,same_sampled_weight$CATEGORIA,same_sampled_weight$P.DESEM.,same_sampled_weight$P.VIVO,same_sampled_weight$SEXO,same_sampled_weight$P.MUE.DES,same_sampled_weight$P.MUE.VIVO)
    same_sampled_weight<-aggregate(x = same_sampled_weight$P.MUE.DES, by = by, FUN= length)
    colnames(same_sampled_weight) <- c("FECHA","TIPO.MUESTREO","UNIPESCOD","PUERTO","BARCO","ESPECIE.TAX.","CATEGORIA","P.DESEM.","P.VIVO","SEXO","P.MUE.DES","P.MUE.VIVO","NUM_OCU")
    same_sampled_weight<-same_sampled_weight[same_sampled_weight$NUM_OCU>1,]
    same_sampled_weight<-arrange(same_sampled_weight, PUERTO, FECHA, BARCO, ESPECIE.TAX., CATEGORIA)
    ERRORS$same_sampled_weight<-same_sampled_weight 
    rm (selected_fields, by, same_sampled_weight)
    
# ---- errors in the weight sampled similar to the category weight?
    desem_mues_sop <- catches_in_lengths[,c("FECHA", "PUERTO", GLOBAL.TIPO.MUESTREO.ICES, "BARCO", "ESPECIE.TAX.", "CATEGORIA", "P.DESEM.", "ESPECIE", "P.MUE.DES", "S.O.P.")]
    desem_mues_sop <- desem_mues_sop[desem_mues_sop[, "P.DESEM."]==desem_mues_sop[,"P.MUE.DES"],]    
    desem_mues_sop <- arrange_(desem_mues_sop, c("PUERTO", GLOBAL.TIPO.MUESTREO.ICES, "FECHA", "BARCO", "ESPECIE.TAX.", "CATEGORIA"))
    desem_mues_sop["P.MUE.DES-SOP"] <- desem_mues_sop["P.MUE.DES"] - desem_mues_sop["S.O.P."]
    desem_mues_sop["P.MUE.DES-SOP"] <- round(desem_mues_sop["P.MUE.DES-SOP"])
    desem_mues_sop["POR.DIF"] <- (desem_mues_sop["P.MUE.DES-SOP"] * 100) / desem_mues_sop["P.MUE.DES"]
    desem_mues_sop["POR.DIF"] <- round(desem_mues_sop["POR.DIF"])
    ERRORS$desem_mues_sop<-desem_mues_sop
    rm(desem_mues_sop)

# ---- errors sop = 0
    ERRORS[["sop_zero"]]<-catches_in_lengths[catches_in_lengths[,"S.O.P."] == 0, c("PUERTO", "FECHA", "BARCO", "UNIPESCOD", "ARTE", "ORIGEN", "ESPECIE.TAX.", "CATEGORIA", "P.DESEM.", "P.VIVO", "ESPECIE", "P.MUE.DES", "P.MUE.VIVO", "S.O.P.")]

# ---- errors 
    ERRORS[["sampled_weight_zero"]]<-catches_in_lengths[catches_in_lengths[,"P.MUE.DES"] == 0, c("PUERTO", "FECHA", "BARCO", "UNIPESCOD", "ARTE", "ORIGEN", "ESPECIE.TAX.", "CATEGORIA", "P.DESEM.", "P.VIVO", "ESPECIE", "P.MUE.DES", "P.MUE.VIVO", "S.O.P.")]    

  
# #### EXPORT ERRORS TO CSV ####################################################
  lapply(names(ERRORS), export_errors_lapply, ERRORS) #The 'ERRORS' argument is an argument to the export_errors_lapply function


