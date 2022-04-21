#' Write GAT KML File
#'
#' This function writes a KML file of the spatial layer.
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
#'             filepath = getwd(), myidvar = "ID")
#' }
#' @export

writeGATkml <- function(myshp, filename, filepath, myidvar = "GEOID10") {
  # temporary sf conversion ----
  myshp <- sf::st_as_sf(myshp)
  # reproject for KML
  # output kml (must be in lat/lon)
  crs <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  kml <- sf::st_transform(myshp, crs)
  kml$ID <- 1:nrow(kml)

  # create the description tables for the kml file
  # modified from the description section of Gwen's code
  mycolnames <- names(kml)[1:(ncol(kml)-1)]

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
  labels <- data.frame(kml)[, myidvar]

  file <- paste0(filepath, "/", filename, ".kml")
  sf::st_write(kml, file, driver = "KML", delete_dsn = TRUE)
}

