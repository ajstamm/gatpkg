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
  # temporary sf conversion ----
  area <- sf::st_as_sf(area)
  # old_crs <- sf::st_crs(area)

  # isolate polygons of interest ----
  area <- cbind(area, IDlist)
  area <- area[area$IDlist == nrid, ]

  # assign consistent CRS ----
  pop <- sf::st_transform(pop, sf::st_crs(area))
  sf::st_agr(pop) <- "constant"

  # intersect layers ----
  popshp <- sf::st_intersection(pop, area)

  coords <- data.frame(
    GATy = sum(popshp$y * popshp$pop) / sum(popshp$pop),
    GATx = sum(popshp$x * popshp$pop) / sum(popshp$pop)
  )
  return(coords)
}
