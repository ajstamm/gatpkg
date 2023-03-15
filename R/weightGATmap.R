#' Find the Weighted Centriods for the Map
#'
#' This function identifies the population weighted centroids for all areas
#' in your original map.
#'
#' @param area     Spatial layer representing areas to be aggregated.
#' @param pop      Spatial layer containing underlying population values.
#' @param popvar   The base population variable.
#' @param idvar    Variable of unique string values to identify the layer's
#'                 observations.
#' @param crs      User-defined non-lat/long projection, entered as a string.
#'                 For the default, NULL, the function defines the projection.
#'
#' @examples
#'
#' if (interactive()) {
#' cen <- weightGATmap(area = hftown, pop = hfpop, idvar = "ID", popvar = "Pop")
#' }
#'
#' @export

weightGATmap <- function(area, pop, idvar, popvar, crs = NULL) {
  # temporary sf conversion ----
  area <- sf::st_as_sf(area)
  pop <- sf::st_as_sf(pop)
  old_crs <- sf::st_crs(area)

  # load pop file ####
  pop <- pop[, popvar]
  pop$area_old <- sf::st_area(pop$geometry)
  if (is.null(crs)) {
    mycrs <- convertlatlong2UTM(area, units = "m")
  } else {
    mycrs <- crs
  }

  pop <- sf::st_transform(pop, mycrs)

  # to stop warnings; not accurate for population variables,
  # but sufficient here, as we recalculate pop manually anyway
  # and none of the other variables matter
  sf::st_agr(pop) <- "constant"

  # convert area ####
  area <- sf::st_transform(area, mycrs)
  # true for character variables;
  # variables will be unknown and don't matter anyway
  sf::st_agr(area) <- "constant"

  # fix possible issues with the shapefiles ####
  area <- sf::st_set_precision(area, 1000000)
  area <- sf::st_buffer(area, dist = 0)
  area <- sf::st_make_valid(area)
  sf::st_agr(area) <- "constant"

  pop <- sf::st_set_precision(pop, 1000000)
  pop <- sf::st_buffer(pop, dist = 0)
  pop <- sf::st_make_valid(pop)
  sf::st_agr(pop) <- "constant"

  # to plot: plot(sf::st_geometry(pop))
  # or: plot(pop$geometry)
  # plot(area$geometry, border = "blue", add = TRUE)

  # intersect area and pop ####
  popshp <- sf::st_intersection(pop, area)
  popshp <- sf::st_transform(popshp, old_crs)
  sf::st_agr(popshp) <- "constant"

  popshp$area_new <- sf::st_area(popshp$geometry)
  popshp$area_prop <- popshp$area_new / popshp$area_old
  popshp$mypop <- popshp[[popvar]]
  popshp$pop <- round(popshp$mypop * as.numeric(popshp$area_prop))

  # add centroids - not straightforward in sf ####
  # assume all other values constant (they aren't affected anyway)
  sf::st_agr(popshp) <- "constant"
  popshp <- sf::st_centroid(popshp)
  sf::st_geometry(popshp) <- sf::st_centroid(popshp$geometry)
  # popshp <- sf::st_transform(popshp, sf::st_crs(area))
  pts <- do.call(rbind, sf::st_geometry(popshp))
  pts <- data.frame(pts)
  names(pts) <- c("x", "y")
  popshp$x <- pts$x
  popshp$y <- pts$y

  # remove artifacts ####
  mypop <- popshp[, c(popvar, idvar, "pop", "x", "y")]

  myid <- unique(mypop[[idvar]])
  coords <- vector("list", length(myid))
  for (j in 1:length(myid)) {
    temp <- mypop[mypop[[idvar]] == myid[j], ]
    t <- data.frame(GATx = sum(temp$x * temp$pop) / sum(temp$pop),
                    GATy = sum(temp$y * temp$pop) / sum(temp$pop),
                    GATpop = sum(temp$pop))
    if (!is.finite(t$GATx)) {
      t <- data.frame(GATx = mean(temp$x),
                      GATy = mean(temp$y),
                      GATpop = NA)
    }
    coords[[j]] <- t
    rm(temp, t)
  }
  mycoords <- data.frame(do.call(rbind.data.frame, coords))
  names(mycoords) <- c("GATx", "GATy", "GATpop")
  row.names(mycoords) <- myid

  # capture missed areas ####
  missid <- area[!data.frame(area)[, idvar] %in% myid, idvar]
  if (nrow(missid) > 0) {
    temp <- area[area[, idvar] %in% missid, ]
    misscent <- cbind(sf::st_coordinates(temp), GATpop = NA)
    colnames(misscent) <- c("GATx", "GATy", "GATpop")
    mycoords <- rbind(mycoords, misscent)
  }

  mylist <- list(centroids = mycoords, pop = mypop)
  # end function ####
  return(mylist)
}
