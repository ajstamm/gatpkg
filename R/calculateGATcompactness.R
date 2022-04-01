#' Calculate GAT Compactness
#'
#' @description
#' This function calculates the compactness ratios for the aggregated areas.
#'
#' It checks whether the projection of a spatial polygons data
#' frame is in lat/long format. If the check passes, the function assigns
#' an alternate projection. The spatial polygons data frame is then used
#' to calculate compactness ratio. The function returns a vector of ratios.
#'
#' @param shp A spatial layer.
#'
#' @examples
#' calculateGATcompactness(shp = hftown)
#'
#' @export

calculateGATcompactness <- function(shp) {
  # sf conversion ----
  area <- sf::st_as_sf(shp)

  # get areas of layer, but first check for planar coordinates
  proj <- sum(grepl("longlat", sf::st_crs(area), fixed = TRUE)) > 0

  if (!proj | is.na(proj)) {
    # if not lat/lon, can use directly to calculate compactness ratio
    map <- shp
  } else if (proj) { # write function to capture pstring?
    # need to find approx longitude of map to pick an appropriate utm zone
    # utms only for use between 80°S and 84°N latitude
    # also there are exceptions for both UTM Zone Exceptions in Norway and Svalbard
    mycrs <- convertlatlong2UTM(area, units = 'm')
    map <- sf::st_transform(area, mycrs)
  } # if lat/long, need to reproject

  # calculate compactness ratio
  myareas <- sf::st_area(map, byid = TRUE)
  hulldists <- lapply(map$geometry, dist)
  diams <- sapply(hulldists, max)
  # to get maximum distance (diameter of circle): max(dist(test1))
  cratio <- myareas / (pi * ((diams / 2) ** 2))
  return(cratio)
}
