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
#' @param shp A spatial polygons data frame.
#'
#' @examples
#' calculateGATcompactness(
#'   shp = hftown
#' )
#'
#' @export

calculateGATcompactness <- function(shp) {
  # get areas of myshps$aggregated, but first check for planar coordinates
  # gArea ex:
  # "+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
  proj <- grepl("longlat", sp::proj4string(shp), fixed = TRUE)

  if (!proj | is.na(proj)) {
    # if not lat/lon, can use directly to calculate compactness ratio
    map <- shp
  } else if (proj) { # write function to capture pstring?
    # need to find approx longitude of map to pick an appropriate utm zone
    # utms only for use between 80°S and 84°N latitude
    # also there are exceptions for both UTM Zone Exceptions in Norway and Svalbard
    mapcenter <- rgeos::gCentroid(shp, byid = FALSE) # get center of map
    latcenter <- mapcenter@coords[1]
    # myutm <- floor((-75.5+180)/6)+1
    myutm <- floor((latcenter + 180) / 6) + 1
    pstring <- paste0("+proj=utm +zone=", myutm,
                      "+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
    map <- sp::spTransform(shp, sp::CRS(pstring))
  } # if lat/long, need to reproject

  # start calculate compactness ratio
  myareas <- rgeos::gArea(map, byid = TRUE)
  myhulls <- rgeos::gConvexHull(map, byid = TRUE)
  hullcoords <- lapply(myhulls@polygons, function(y) {
    y@Polygons[[1]]@coords
  } )
  hulldists <- lapply(hullcoords, dist)
  diams <- sapply(hulldists, max)

  # calculate compactness ratio
  # to get maximum distance (diameter of circle): max(dist(test1))
  cratio <- myareas / (pi * ((diams / 2) ** 2))
  return(cratio)
}
