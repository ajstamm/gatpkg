#' Latitude/Longitude to UTM Coordinates
#'
#' This function converts scalar to planar coordinates
#'
#' @param area  A SpatialPolygonsDataFrame.
#' @param units The measurement unit. Default is meters.
#'
#' @examples
#' convertlatlong2UTM(area = hftown, units = "m")
#'
#'
#' @export
#'

convertlatlong2UTM <- function(area, units = 'm') {
  # temporary sf conversion
  area <- sf::st_as_sf(area)

  bounds <- sf::st_bbox(area)
  lat = mean(bounds[c(2, 4)]) # latitude
  long = mean(bounds[c(1, 3)]) # longitude

  # find UTM hemisphere (latitude)
  hemisphere <- ifelse(lat > 0, "north", "south")
  # find UTM zone
  zone <- (floor((long + 180) / 6) %% 60) + 1

  crs <- paste0("+proj=utm +zone=", zone, " +ellps=WGS84 +", hemisphere,
                " +units=", units)
  return(crs)
}
