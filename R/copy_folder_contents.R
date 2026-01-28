#' Copy folder and its contents
#'
#' @description
#' Function that makes a copy of all the files and subdirectories from a source folder to a destination folder.
#' If files already exist in the destination folder, the user is prompted to decide whether to overwrite them or not.
#'
#' @param folder_from Character. Path to the source directory containing the files and folders to be copied.
#' @param folder_to Character. Path to the destination directory where files and folders will be copied
#'
#' @return NULL. The function is called for its side effects of copying files.

copy_folder_contents <- function (folder_from, folder_to){

  tryCatch(
     expr = {
      
      # Take a list with of the files present in the source and destination folders
      files_from <- list.files(folder_from, 
                               recursive = TRUE)
      files_to <- list.files(folder_to, 
                             recursive = TRUE)
      
      
      # Create the overwrite variable with FALSE as default value
      overwrite <- FALSE
      
      # Check if there are files in the destination folder
      if(any(files_from %in% files_to)){

        message("WARNING: There are existing files in the destination folder.")

        answer <- ""

        while(!(answer %in% c("Y", "y", "N", "n"))){
          answer <- readline(prompt="Do you want to overwrite existing files? (Y/N): ")
        }

        if(answer == "Y" | answer == "y"){
          overwrite <- TRUE
        } else {
          message("No files were copied.")
          return(NULL)
        }

      }

      #' Create subdirectories in the destination folder,
      #' if they do not exist already

      folders <- list.dirs(folder_from, 
                          recursive = TRUE, 
                          full.names = FALSE)
      
      lapply(file.path(folder_to, folders), 
             dir.create,
             recursive = TRUE)

      # Copy files from source to destination
      file.copy(from = file.path(folder_from, files_from),
                to = file.path(folder_to, files_from),
                overwrite = overwrite)

      message("Files copied successfully.")

    }, error = function(e){

      stop("An error occurred while copying files: ", e$message)

    }
  )

}

