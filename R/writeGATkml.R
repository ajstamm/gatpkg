#' Write GAT KML File
#'
#' This function writes a KML file of the shapefile.
#'
#' @param myshp    A spatial layer.
#' @param filename The desired name for the KML file.
#' @param filepath The desired location for the KML file.
#' @param myidvar  A variable of unique values for the spatial layer.
#'
#' @examples
#' # creates the kml file in the project root directory
#' if (interactive()) {
#' writeGATkml(myshp = hftown, filename = "my_kml_example",
#'             filepath = getwd(), myidvar = "TOWN")
#' }
#' @export

# this function works and is fast, but notes are a mess and kmz may be buggy
# see https://stackoverflow.com/questions/35280417/r-export-to-kml-with-custom-descriptionfield

writeGATkml <- function(myshp, filename, filepath, myidvar = "GEOID10") {
  # temporary sf conversion ----
  myshp <- sf::st_as_sf(myshp)
  # reproject for KML
  # output kml (must be in lat/lon)
  crs <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  kml <- sf::st_transform(myshp, crs)

  # create the description tables for the kml file
  # modified from the description section of Gwen's code
  mycolnames <- names(kml)[1:(ncol(kml)-1)]
  kml$name <- data.frame(kml)[, myidvar]

  # this is giving warnings, but working. needs debugging later
  desctemp <- character(nrow(kml))
  for (i in 1:nrow(kml)) {
    temp <- c()
    for (j in 1:length(mycolnames)) {
      temp <- paste(temp, "<tr>",
                          "<td>", mycolnames[j], "</td>",
                          "<td>", kml[i, j], "</td>",
                          "</tr>")
    }
    desctemp[i] <- paste("<table border = 1>", temp, "</table>")
  }
  kml$description <- desctemp

  # save raw (kml) and zipped (kmz) files
  filek <- paste0(filepath, "/", filename, ".kml")
  filez <- paste0(filepath, "/", filename, ".kmz")
  sf::st_write(kml, filek, driver = "KML", delete_dsn = TRUE)
  utils::zip(zipfile = filez, files = filek)
}

