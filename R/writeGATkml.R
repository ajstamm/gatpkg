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
#' writeGATkml(
#'   myshp = hftown,
#'   filename = "my_kml_example",
#'   filepath = getwd(),
#'   myidvar = "ID"
#' )
#' @export

# this function works and is fast, but notes are a mess and kmz may be buggy
# see https://stackoverflow.com/questions/35280417/r-export-to-kml-with-custom-descriptionfield

writeGATkml <- function(myshp, filename, filepath, myidvar = "GEOID10") {
  # reproject for KML
  # output kml (must be in lat/lon)
  crs <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  kmlmap <- sp::spTransform(myshp, sp::CRS(crs))
  kmlmap@data$ID <- 1:nrow(kmlmap@data)
  mytable <- methods::as(kmlmap, "data.frame")
  mycolnames <- names(mytable)

  # create the description tables for the kml file
  # modified from the description section of Gwen's code
  desctemp <- character(nrow(mytable))
  for (i in 1:nrow(mytable)) {
    temp <- c()
    for (j in 1:ncol(mytable)) {
      temp <- paste(temp, "<tr>",
                          "<td>", mycolnames[j], "</td>",
                          "<td>", mytable[i, j], "</td>",
                          "</tr>")
    }
    desctemp[i] <- paste("<table border = 1>", temp, "</table>")
  }
  kmlmap@data$desc <- desctemp
  kmlmap@data$labels <- kmlmap@data[, myidvar]

  oldpath <- getwd()
  setwd(filepath)

  # this section is identical to the kml() command below, but more explicit
  # kml_open(paste0(filename, "_as_new.kml"))
  # kml_layer.SpatialPolygons(kmlmap, # subfolder.name = var,
  #                           labels = labels,
  #                           outline = TRUE, alpha = 1, width = 2,
  #                           colour = NULL, html.table = desc)
  # kml_close(paste0(filename, "_as_new.kml"))

  plotKML::kml(obj = kmlmap, folder.name = filepath,
               file.name = paste0(filename, ".kml"), kmz = FALSE,
               colour = NULL, labels = labels, alpha = 1, size = 1,
               html.table = kmlmap@data$desc) # , metadata =,
  setwd(oldpath)
}

