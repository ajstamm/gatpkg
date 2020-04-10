#' Combine GAT Crosswalks
#' 
#' @description 
#' 
#' This function provides complete crosswalk and final shapefiles in cases 
#' when the user requires GAT to be run twice (for example, a first run that
#' enforces the boundary variable and a second run that does not). 
#' 
#' @param path  The filepath to the files created by GAT. This function 
#'              assumes the same filepath was used in both runs.
#' @param file1 The crosswalk (*in) file created in the first run of GAT. 
#'              For best results, avoid filenames containing a period. It is 
#'              not necessary to add the file extension.
#' @param file2 The crosswalk (*in) file created in the second run of GAT.
#'              For best results, avoid filenames containing a period. It is 
#'              not necessary to add the file extension.
#' @param idvar The ID variable used when running GAT. This function assumes 
#'              the same ID variable was used in both runs.
#' 
#' @details 
#' 
#' Two shapefiles are created and saved in the path provided:
#' 
#' * a revised original crosswalk
#'     * name: file1 ending in "_combined"
#'     * "GATid" renamed to "tempid"
#'     * new "GATid" created with the IDs from the second run
#' * a revised final aggregation 
#'     * name: file2 without "in" ending in "_combined"
#'     * "GATnumIDs" removed
#'     * new "GATnumIDs" created with the number of areas in the original 
#'       shapefile included in each final area
#' 
#' In a future iteration, I may also include maps and a log that 
#' compare the original and final shapefiles.
#' 
#' 
# @example 
# I reall don't have a good exaple right now. 
# Maybe my existing crosswalk plus a crosswalk for a run ignoring county lines.
#' 
#' 
#' 

# for testing
# path <- "P:/Sections/EHS/Abigail/SubcountyData/Rcode/GATtest/testdata/tract_mcd"
# file1 <- "gat_step1_newmcdin.dbf"
# file2 <- "gat_step2_newmcdin.dbf"
# idvar <- "tract10"

combineGATcrosswalks <- function(path, file1, file2, idvar) {
  # clean filepath and filenames ####
  filepath_format <- function(str) {
    folders <- unlist(strsplit(str, split = "/"))
    if (length(folders) > 1) {
      path <- paste(folders[1:(length(folders)-1)], collapse = "/")
    } else {
      path <- ""
    }
    return(path)
  }
  filename_format <- function(str) {
    path <- filepath_format(str)
    if (path != "") {
      str <- gsub(paste0(path, "/"), "", str)
    }
    fileparts <- unlist(strsplit(str, split = "\\."))
    file <- paste(fileparts[1:(length(fileparts)-1)], collapse = ".")
    return(file)
  }
  file1 <- filename_format(file1)
  file2 <- filename_format(file2)
  path <- gsub("\\\\", "/", path)
  path <- gsub("/$", "/", path)
  
  # load crosswalks ####
  old <- rgdal::readOGR(dsn = path, layer = file1,
                        stringsAsFactors = FALSE)
  key1 <- old@data
  key2 <- foreign::read.dbf(paste0(path, "/", file2, ".dbf"), as.is = TRUE)
  
  # combine crosswalks ####
  names(key1)[names(key1) == "GATid"] <- "tempid"
  names(key2)[names(key2) == idvar] <- "tempid"
  key2 <- key2[, c("tempid", "GATid")]
  key <- merge(key1, key2, by = "tempid")
  
  old@data <- key
  rgdal::writeOGR(old, path, paste0(file1, "_combined"),
                  driver = "ESRI Shapefile", verbose = TRUE,
                  overwrite_layer = TRUE)
  
  # add correct area numbers ####
  num_areas <- data.frame(table(key$GATid))
  names(num_areas) <- c(idvar, "GATnumIDs")
  
  new <- rgdal::readOGR(dsn = path, layer = gsub("in$", "", file2),
                        stringsAsFactors = FALSE)
  
  l <- names(new@data)[names(new@data) != "GATnumIDs"]
  n <- new@data[, l]
  n <- merge(n, num_areas, by = idvar)
  new@data <- n
  
  rgdal::writeOGR(new, path, paste0(gsub("in$", "", file2), "_combined"),
                  driver = "ESRI Shapefile", verbose = TRUE,
                  overwrite_layer = TRUE)
}
