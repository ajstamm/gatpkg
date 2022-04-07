#' Rank GAT Distance
#'
#' This function orders the distance to neighboring polygons' centroids from
#' nearest to farthest and returns a vector of integers.
#'
#' @param area A spatial polygons data frame.
#' @param nbdata A data frame of (polygon) observations.
#' @param first A data frame with one (polygon) observation.
#' @param gatvars A list of objects created by the GAT tool. It contains the
#'                strings myidvar, aggregator1, aggregator2, and boundary,
#'                which are all variables in the area, and the numbers
#'                minvalue1 and minvalue2. Both aggregator1 and aggregator2
#'                must be numeric. The identifier, myidvar, must contain
#'                unique values.
#' @param mergevars A list of string objects needed to aggregate the areas in
#'                  the GAT tool. It contains mergeopt1, mergeopt2, similar1,
#'                  and similar2. The valid options for mergeopt1 and
#'                  mergeopt2 are "closest", "least", and "similar". If
#'                  "similar" is selected, similar1 and similar2 must be
#'                  numeric variables in the area and similar2 cannot equal
#'                  zero.
#'
#' @examples
#' # add GAT variables
#' centroids <- sf::st_coordinates(sf::st_geometry(sf::st_centroid(hftown)))
#' colnames(centroids) <- c("GATx", "GATy")
#' my_data <- data.frame(hftown, centroids)
#' my_data <- sf::st_as_sf(my_data, coords = c("GATx", "GATy"),
#'                         crs = sf::st_crs(hftown))
#' first <- my_data[which(grepl("37374", my_data$ID)), ] # only one observation
#'
#' # hard coded for simplicity; use spdep::poly2nb() to get these obs
#' nbdata <- my_data[which(grepl("43412|02572|40794|79059", my_data$ID)), ]
#'           # only adjacent neighbors to first
#'
#' gatvars <- list(
#'   myidvar = "ID",             # character variable of unique values
#'   aggregator1 = "TOTAL_POP",  # numeric variable
#'   aggregator2 = "TOTAL_POP",  # numeric variable
#'   minvalue1 = 5000, minvalue2 = 5000,
#'   boundary = "COUNTY"         # character variable of non-unique values
#' )
#'
#' mergevars <- list(
#'   mergeopt1 = "similar",    # can be similar, closest, or least
#'   mergeopt2 = "closest",    # if "similar" fails
#'   similar1 = "B_TOT",       # numeric variable
#'   similar2 = "W_TOT"        # numeric variable without any zeros
#' )
#' # rank the distances
#' my_rank_list <- rankGATdistance(area = hftown, nbdata = nbdata,
#'                                 first = first, gatvars = gatvars,
#'                                 mergevars = mergevars)
#'
#' @export

rankGATdistance <- function(area, nbdata, first, gatvars, mergevars) {
  # temporary sf conversion
  area <- sf::st_as_sf(area)
  nbdata <- sf::st_as_sf(nbdata)
  first <- sf::st_as_sf(first)

  # distance for lat/long, in kilometers
  # if projection is lat/lon, projection = TRUE, otherwise FALSE
  projection <- sum(grepl("longlat", sf::st_crs(area), fixed = TRUE)) > 0
  # default to not lat/long if something goes wrong
  if (is.na(projection)) projection <- FALSE

  # if longlat = FALSE, euclidian distance in metric of points
  mydist <- sf::st_distance(nbdata$geometry, first$geometry)
  nborder <- order(mydist) # order according to distance, this will be default

  # order according to aggregation variable (i.e. cases or population)
  if (mergevars$mergeopt2 == "least" & gatvars$aggregator2 == "NONE"){
    nborder <- order(data.frame(nbdata)[, gatvars$aggregator1])
  } else if (mergevars$mergeopt2 == "least" & gatvars$aggregator2 != "NONE"){
    nborder1 <- order(data.frame(nbdata)[, gatvars$aggregator1])
    nborder2 <- order(data.frame(nbdata)[, gatvars$aggregator2])
    if (data.frame(nbdata)[nborder1[1], gatvars$aggregator1] /
        gatvars$minvalue1 <=
        data.frame(nbdata)[nborder2[1], gatvars$aggregator2] /
        gatvars$minvalue2) {
      nborder <- nborder1
    } else {
      nborder <- nborder2
    }
  } #end code for order according to aggregation variable

  # order according to similarity
  if (mergevars$mergeopt2 == "similar") {
    nborder <- order(abs(data.frame(nbdata)[, mergevars$similar1] /
                           data.frame(nbdata)[, mergevars$similar2] -
                           data.frame(first)[, mergevars$similar1] /
                           data.frame(first)[, mergevars$similar2]))
  }
  return(nborder)
}

