#### Check monthly data recorded in SIRENO.
#### Return xls files by influence area with errors detected.

# INSTRUCTIONS -----------------------------------------------------------------

# To use this script:
# - Make sure the file "revision_volcado_functions.R" is located in the same
# directory that this file.
# - Change variables in "YOU HAVE ONLY TO CHANGE THIS VARIABLES" section of this
# script.
# - A file "contacts.csv" must be stored in private folder. This data set
# contains the different person roles and its email, used in the distribution of
# error files.
# The possible roles are:
#   - GC, GS, GN and AC: the supervisors of the influence areas.
#   - sender: person responsible for sending the files.
#   - cc: related people to whom the email should also be sent.
# The contacs file must have a comma separated format with two fields: ROLE and
# EMAIL. The first line must contain the name of the variables.
# - A CFPO file must be stored in private folder.
# - Make sure report files tallas_x_up from SIRENO are in PATH_FILES.
# - Run all the script
# - A file by influence area is generated in "errors" directory.

# YOU ONLY HAVE TO CHANGE THIS VARIABLES ---------------------------------------

# Name of the subfolder where will be stored the errors generated in this script.
ERRORS_SUBFOLDER_NAME <- "errors"

# Name of the folder where are stored private files with sensitive information.
PRIVATE_FOLDER_NAME <- "private"

# Name of the files obtained from SIRENO database.
FILENAME_DES_TOT <- "IEOUPMUEDESTOTACANDELARIO.TXT"
FILENAME_DES_TAL <- "IEOUPMUEDESTALACANDELARIO.TXT"
FILENAME_TAL <- "IEOUPMUETALACANDELARIO.TXT"

# MONTH: 1 to 12, or vector with month in numbers
MONTH <- c(10)

# YEAR
YEAR <- 2023

# Suffix to add to path. Use only in case MONTH is a vector of months. This
# suffix will be added to the end of the path with a "_" as separation.
suffix_multiple_months <- "annual_nvdp"

# Suffix to add at the end of the export file name. This suffix will be added to
# the end of the file name with a "_" as separation.
suffix <- ""

# cfpo to use in the script
cfpo_to_use <- "CFPO 2022 DEF Marco.xlsx"


# PACKAGES ---------------------------------------------------------------------

library(plyr)
library(dplyr)
library(blastula) # to send emails
library(devtools)
library(openxlsx) # to read directly CFPO from a excel file

# install sapmuebase from github
# remove.packages("sapmuebase")
# .rs.restartR()
# install_github("eucrow/sapmuebase")

library(sapmuebase)


# FUNCTIONS --------------------------------------------------------------------
# All the functions required in this script are located in
# revision_volcado_functions.R file.
source('rim_post_dump_functions.R')

# function to check the rim files:
source('rim_check.R')

# function to check the annual rim files:
source('rim_check_annual.R')

# function to check the annual rim files:
# source('rim_check_annual_post_cruce_test.R')

# function to check the oab files:
source('oab_check.R')

# GLOBAL VARIABLES -------------------------------------------------------------

# list with all errors found in data frames:
ERRORS <- list()

# Path where the files of the month and year will be stored.
PATH_FILES <- createPathFiles(MONTH, YEAR, suffix_multiple_months)
# PATH_FILES <- "C:/Users/ieoma/Desktop/sap/rim_post_dump/data/2023/2023_08_b"

# Path where private files are stored.
PATH_PRIVATE_FILES <- file.path(getwd(), PRIVATE_FOLDER_NAME)

# list with the common fields used in all tables
BASE_FIELDS <- c("COD_ID", "COD_PUERTO", "PUERTO", "LOCODE", "FECHA_MUE",
                 "COD_BARCO", "BARCO", "ESTRATO_RIM", "COD_TIPO_MUE",
                 "TIPO_MUE")

# path to store errors files
PATH_ERRORS <- file.path(PATH_FILES, "errors")
# path to store files as backup
PATH_BACKUP <- file.path(PATH_FILES, "backup")

# Month as character
MONTH_AS_CHARACTER <- createMonthAsCharacter(MONTH, suffix_multiple_months)

# path to shared folder
PATH_SHARE_ERRORS <- file.path("C:/Users/ieoma/Nextcloud/SAP_RIM/RIM_data_review",
                               YEAR,
                               paste0(YEAR, "_", MONTH_AS_CHARACTER))

# files to backup
FILES_TO_BACKUP <- c("rim_post_dump.R",
                     "rim_post_dump_functions.R",
                     "rim_check.R",
                     "oab_check.R",
                     "especies_sujetas_a_posible_confusion_taxonomica.csv",
                     "especies_no_permitidas.csv")

ERRORS_FILENAME <- createFilename()

EMAIL_TEMPLATE <- "errors_email.Rmd"


# IMPORT DATA ------------------------------------------------------------------

# Get the mixed species data set.
mixed_species <- especies_mezcla

# Get the no mixed species data set.
sampled_spe_no_mixed <- especies_no_mezcla

# Get the not allowed species data set.
NOT_ALLOWED_SPECIES <- read.csv("especies_no_permitidas.csv")

# Get the species susceptible to taxonomic confusion data set.
ESP_TAXONOMIC_CONFUSION <- read.csv(
  "especies_sujetas_a_posible_confusion_taxonomica.csv",
  sep = ";",
  colClasses = c("factor","factor","factor","factor","factor","factor","character","character"))

# Get the CFPO
CFPO <- read.xlsx(paste0(getwd(), "/data-raw/", cfpo_to_use), detectDates=TRUE)
CFPO <- CFPO[, c("CFR", "Matrícula", "Estado.actual")]
colnames(CFPO) <- c("CFR", "MATRICULA", "ESTADO")

# Get the contacts data set.
CONTACTS <- read.csv(file.path(PATH_PRIVATE_FILES, "contacts.csv"))

# IMPORT "muestreos UP" files
muestreos_up <- importRIMFiles(
  catches = FILENAME_DES_TOT,
  catches_in_lengths = FILENAME_DES_TAL,
  lengths = FILENAME_TAL,
  path = PATH_FILES
  # ,  by_month = MONTH
  )


##TODO: create a coherence check rim_stratum-origin??
# SEARCHING ERRORS -------------------------------------------------------------
errors <- rim_check(muestreos_up)
# errors <- rim_check_annual(muestreos_up)
# errors <- rim_check_annual_post_cruce_text(muestreos_up)

errors_complete <- Reduce( function(x, y) { merge(x, y, all=TRUE)}, errors)

# Check oab data dumped in rim:
#   - sampled type 4, MT2B
# errors <- oab_check(muestreos_up)


# EXPORT ERRORS ----------------------------------------------------------------

# Select the way to export errors:

    # one month
    # exportListToCsv(errors, suffix = paste0(YEAR,"_",MONTH_AS_CHARACTER), separation = "_")
    exportErrorsList(errors, ERRORS_FILENAME, separation = "_")
    # a complete year
    # exportErrorsList(errors, suffix = paste0("errors", "_", YEAR), separation = "_")
    # exportCsvSAPMUEBASE(errors_complete, "errors_complete_anual_2021.csv")


# CHECK CODE_ID ----------------------------------------------------------------
# This check is not for send to the sups, so it's out the ERRORS dataframe
# errors_cod_id <- checkCodId(muestreos_up$catches)


# SAVE FILES TO SHARED FOLDER --------------------------------------------------
copyFilesToFolder(PATH_ERRORS, PATH_SHARE_ERRORS)


# BACKUP SCRIPTS AND RELATED FILES ---------------------------------------------
# first save all files opened
rstudioapi::documentSaveAll()
# and the backup the scripts and files:
sapmuebase::backupScripts(FILES_TO_BACKUP, path_backup = PATH_BACKUP)


# SEND EMAILS AUTOMATICALLY ----------------------------------------------------
# The first time the errors will be sent by email, a credential file must be
# generated with create_smtp_creds_file. The credentials file is generated in
# the private folder:
# blastula::create_smtp_creds_file(file = file.path(PRIVATE_FOLDER_NAME, "credentials"),
#                        user = "",
#                        host = "",
#                        port = ,
#                        use_ssl = )

# The internal_links data frame must have two variables:
# - AREA_INF: influence area with the values GC, GS, GN and AC.
# - INTERNAL_LINK: with the link to the error file in its AREA_INF. If there
# aren't any error file of a certain AREA_INF, must be set to "".
# - NOTES: any notes to add to the email. If there aren't, must be set to "".
accesory_email_info <- data.frame(
                          AREA_INF = c("AC",
                                       "GC",
                                       "GN",
                                       "GS"),
                          LINK = c("",
                                   "",
                                   "",
                                   ""),
                          NOTES = c("",
                                    "",
                                    "",
                                    "")
                            )


# getOption("timeout")
# options(timeout = 60)

# sendErrorsByEmail(accesory_email_info = accesory_email_info,
#                   contacts = CONTACTS,
#                   credentials_file = "credentials")

