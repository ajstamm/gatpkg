#' Write GAT KML File
#'
#' This function writes a KML file of the shapefile.
#'
#' @param myshp A spatial polygons data frame.
#' @param filename The desired name for the KML file.
#' @param filepath The desired location for the KML file.
#' @param myidvar A variable of unique values for the spatial polygons
#'                data frame.
#'
#' @examples
#' # creates the kml file in the project root directory
#'
#' if (interactive()) {
#' writeGATkml(myshp = hftown, filename = "my_kml_example",
#'             filepath = getwd(), myidvar = "ID")
#' }
#' @export

# this function works and is fast, but notes are a mess and kmz may be buggy
# see https://stackoverflow.com/questions/35280417/r-export-to-kml-with-custom-descriptionfield

writeGATkml <- function(myshp, filename, filepath, myidvar = "GEOID10") {
  # temporary sf conversion ----
  area <- sf::st_as_sf(area)
  # reproject for KML
  # output kml (must be in lat/lon)
  crs <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  kml <- sf::st_transform(myshp, crs)
  kml$ID <- 1:nrow(kml)

  # create the description tables for the kml file
  # modified from the description section of Gwen's code
  mycolnames <- names(kml)

  # this is giving warnings, but working. needs debugging later
  desctemp <- character(nrow(kml))
  for (i in 1:nrow(kml)) {
    temp <- c()
    for (j in 1:ncol(kml)) {
      temp <- paste(temp, "<tr>",
                          "<td>", mycolnames[j], "</td>",
                          "<td>", kml[i, j], "</td>",
                          "</tr>")
    }
    desctemp[i] <- paste("<table border = 1>", temp, "</table>")
  }
  kml$desc <- desctemp
  labels <- data.frame(kml)[, myidvar]

  kml_sp <- sf::as_Spatial(kml)

  oldpath <- getwd()
  setwd(filepath)
  plotKML::kml(obj = kml_sp, folder.name = filepath,
               file.name = paste0(filename, ".kml"), kmz = FALSE,
               colour = NULL, labels = labels, alpha = 1, size = 1,
               html.table = data.frame(kml)$desc) # , metadata =,
  setwd(oldpath)
}

