#' Find the Weighted Centriods for the Map
#'
#' This function identifies the population weighted centroids for all areas
#' in your original map.
#'
#' @param area     The original shapefile.
#' @param popvar   The base population variable.
#' @param idvar    A variable of unique string values to identify the
#'                 observations.
#' @param filevars A list of strings denoting file names and paths.
#'
#' @examples
#'
#' \donttest{
#' filevars <- list(
#'   poppath = paste0(find.package("gatpkg"), "/extdata"),
#'   popfile = "hfblock"
#' )
#'
#' mycentroids <-
#'   weightGATmap(
#'     area = hftown,
#'     filevars = filevars,
#'     idvar = "ID",
#'     popvar = "Pop_tot"
#'   )
#' }
#'
#'
#' @export
weightGATmap <- function(area, filevars, idvar, popvar) {
  pop <- sf::read_sf(dsn = filevars$poppath, layer = filevars$popfile)
  pop <- pop[, popvar]
  pop$area_old <- sf::st_area(pop$geometry)
  mycrs <- convertlatlong2UTM(area, units = "m")
  pop <- sf::st_transform(pop, mycrs)

  # to stop warnings; not accurate for population variables,
  # but sufficient here, as we recalculate pop manually anyway
  # and none of the other variables matter
  sf::st_agr(pop) <- "constant"

  # convert area
  areasf <- sf::st_as_sf(area)
  areasf <- sf::st_transform(areasf, mycrs)
  # true for character variables;
  # variables will be unknown and don't matter anyway
  sf::st_agr(areasf) <- "constant"

  # intersect area and pop
  popshp <- sf::st_intersection(pop, areasf)
  popshp$area_new <- sf::st_area(popshp$geometry)
  popshp$area_prop <- popshp$area_new / popshp$area_old
  popshp$mypop <- popshp[[popvar]]
  popshp$pop <- round(popshp$mypop * as.numeric(popshp$area_prop))

  # add centroids - not straightforward in sf
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

  # remove artifacts
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
  # capture missed areas
  missid <- area@data[!area@data[, idvar] %in% myid, idvar]
  if (length(missid) > 0) {
    temp <- area[area@data[, idvar] %in% missid, ]
    misscent <- cbind(sp::coordinates(temp), GATpop = NA)
    colnames(misscent) <- c("GATx", "GATy", "GATpop")
    mycoords <- rbind(mycoords, misscent)
  }

  mylist <- list(centroids = mycoords, pop = mypop)
  return(mylist)
}
