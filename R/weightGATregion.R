#' Find the Weighted Centriod for a GAT Region
#'
#' This function identifies the population weighted centroid of a new
#' area based on the GAT ID assigned to the old areas and the population
#' shapefile.
#'
#' @param area   The original shapefile.
#' @param pop    The base population shapefile.
#' @param idvar  A variable of unique string values to identify the
#'               observations.
#' @param nrid   A string that will be the ID for the merged observation
#'               (polygon).
#' @param IDlist The list of all GAT IDs and their corresponding area IDs.
#'
#' @examples
#'
#' \donttest{
#' ids <- c("e", "e", "d", "d", "f", "f", "f", "e", "e", "d", "e",
#'          "d", "b", "c", "c", "a", "b", "c", "a", "b", "c")
#' mycentroid <-
#'   weightGATregion(
#'     area = hftown,
#'     pop = hfpop,
#'     IDlist = ids,
#'     idvar = "ID",
#'     nrid = "c"
#'   )
#' }
#'
#'
#' @export
weightGATregion <- function(area, pop, IDlist, idvar, nrid) {
  # isolate polygons of interest
  area@data <- cbind(area@data, IDlist)
  area <- area[area@data$IDlist == nrid, ]

  # convert polygons to sf
  areasf <- sf::st_as_sf(area)

  # assign consistent CRS
  mycrs <- convertlatlong2UTM(area, units = "m")
  areasf <- sf::st_transform(areasf, mycrs)
  sf::st_agr(areasf) <- "constant"
  pop <- sf::st_transform(pop, mycrs)
  sf::st_agr(pop) <- "constant"

  # intersect layers
  popshp <- sf::st_intersection(pop, areasf)
  coords <- data.frame(
    # lat/long not necessary
    # LATITUDE = areasf$LATITUDE[IDlist==nrid][1],
    # LONGITUDE = areasf$LONGITUDE[IDlist==nrid][1],
    GATy = sum(popshp$y * popshp$pop) / sum(popshp$pop),
    GATx = sum(popshp$x * popshp$pop) / sum(popshp$pop)
  )
  return(coords)
}
