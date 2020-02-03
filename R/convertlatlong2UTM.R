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
  bounds <- sp::bbox(area)
  lat = mean(bounds[2,]) # latitude
  long = mean(bounds[1,]) # longitude

  # find UTM hemisphere (latitude)
  hemisphere <- ifelse(lat > 0, "north", "south")
  # find UTM zone
  zone <- (floor((long + 180) / 6) %% 60) + 1

  crs <- paste0("+proj=utm +zone=", zone, " +ellps=WGS84 +", hemisphere,
                " +units=", units)
  return(crs)
}
