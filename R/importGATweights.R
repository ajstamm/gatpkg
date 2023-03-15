#' Create Population Base for Weighting
#'
#' @description
#'
#' This function reads in the layer to be aggregated (large area) and the
#' file details for the shapefile containing small areas, including the file
#' name, file location, and variable containing population values.
#'
#' It subsets the small area layer based on boundaries of the large are layer,
#' then calculates the proportion of each small area that falls in each large
#' area based on the area proportion of the small area that falls in each
#' large area. This creates a base layer from which to create population
#' weighted centroids.
#'
#' @param area   The spatial polygons data frame you want to aggregate.
#' @param filein The filename of the population shapefile.
#' @param pathin The path of the population shapefile.
#' @param popvar The population variable to use in the population
#'               shapefile.
#'
#' @examples
#'
#' if (interactive()) {
#' pathin <- paste0(find.package("gatpkg"), "/extdata")
#' mywtshp <- importGATweights(area = hftown, filein = "hfblockgrp",
#'                             pathin = pathin, popvar = "Pop")
#' }
#'
#' @export

importGATweights <- function(area, filein, pathin, popvar = "Pop_tot") {
  # convert original shapefile
  shp <- sf::st_as_sf(area)
  sf::st_agr(shp) <- "constant"

  # rewrite to use utm function?
  if (!sf::st_is_longlat(area)) {
    proj <- "+proj=longlat +datum=NAD27"
    shp <- sf::st_transform(shp, proj)
  } else {
    proj <- sf::st_crs(area)
  }

  # read in population file
  pop <- sf::read_sf(dsn = pathin, layer = filein)
  pop <- pop[, popvar]
  pop$area_old <- sf::st_area(pop$geometry)
  pop <- sf::st_transform(pop, proj)
  pop <- sf::st_as_sf(pop)
  sf::st_agr(pop) <- "constant"

  # intersect shapefiles
  i <- sf::st_intersection(pop, shp)
  i <- i[, c(popvar, "area_old")]
  i$area_new <- sf::st_area(i$geometry)
  i$area_prop <- i$area_new / i$area_old
  i$mypop <- unlist(c(i[, popvar])[1])
  i$pop <- round(i$mypop * as.numeric(i$area_prop))
  # remove artifacts
  i <- i[, c(popvar, "pop")]
  # add centroids - not straightforward in sf
  sf::st_agr(i) <- "constant"

  j <- sf::st_centroid(i)
  sf::st_geometry(j) <- sf::st_centroid(j$geometry)
  pts <- do.call(rbind, sf::st_geometry(j))
  pts <- data.frame(pts)
  names(pts) <- c("x", "y")
  i$x <- pts$x
  i$y <- pts$y
  return(i)
}

