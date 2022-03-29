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
# I really don't have a good example right now.
# Maybe my existing crosswalk plus a crosswalk for a run ignoring county lines.
#'
#'
#'
#' @export
#'

# for testing
# path <- "P:/.../tract_mcd"
# file1 <- "gat_step1_newmcdin.dbf"
# file2 <- "gat_step2_newmcdin.dbf"
# idvar <- "tract10" key

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
  old <- sf::st_read(dsn = path, layer = file1)
  # key1 <- old@data
  key2 <- foreign::read.dbf(paste0(path, "/", file2, ".dbf"), as.is = TRUE)

  # combine crosswalks ####
  names(old)[names(old) == "GATid"] <- "tempid"
  names(key2)[names(key2) == idvar] <- "tempid"
  key2 <- key2[, c("tempid", "GATid")]
  old <- merge(old, key2, by = "tempid")

  sf::st_write(old, path, paste0(file1, "_combined"),
                  driver = "ESRI Shapefile", verbose = TRUE,
                  overwrite_layer = TRUE)

  # add correct area numbers ####
  num_areas <- data.frame(table(old$GATid))
  names(num_areas) <- c(idvar, "GATnumIDs")

  new <- sf::st_read(dsn = path, layer = gsub("in$", "", file2))

  l <- names(new)[names(new) != "GATnumIDs"]
  n <- new[, l]
  new <- merge(n, num_areas, by = idvar)

  sf::st_write(new, path, paste0(gsub("in$", "", file2), "_combined"),
                  driver = "ESRI Shapefile", verbose = TRUE,
                  overwrite_layer = TRUE)
}

