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
#' centroids <- sp::coordinates(hftown)
#' colnames(centroids) <- c("GATx", "GATy")
#' my_data <- data.frame(hftown@data, centroids)
#' first <-
#'   my_data[which(
#'     grepl("37374", my_data$ID)
#'   ), ]                        # only one observation
#'
#' # hard coded for simplicity; use spdep::poly2nb() to get these obs
#' nbdata <-
#'   my_data[which(
#'     grepl("43412|02572|40794|79059", my_data$ID)
#'   ), ]                        # only adjacent neighbors to first
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
#' my_rank_list <-
#'   rankGATdistance(
#'     area = hftown,
#'     nbdata = nbdata,
#'     first = first,
#'     gatvars = gatvars,
#'     mergevars = mergevars
#'   )
#'
#' @export

rankGATdistance <- function(area, nbdata, first, gatvars, mergevars) {
  # distance for lat/long, in kilometers
  # if projection is lat/lon, projection = TRUE, otherwise FALSE
  projection = grepl("longlat", sp::proj4string(area), fixed = TRUE)
  # default to not lat/long if something goes wrong
  if (is.na(projection)) projection <- FALSE

  # if longlat = FALSE, euclidian distance in metric of points
  mydist <- sp::spDistsN1(as.matrix(nbdata[, c("GATx", "GATy")]),
                          as.matrix(first[, c("GATx", "GATy")]),
                          longlat = projection)
  nborder <- order(mydist) # order according to distance, this will be default

  # order according to aggregation variable (i.e. cases or population)
  if (mergevars$mergeopt2 == "least" & gatvars$aggregator2 == "NONE"){
    nborder<-order(nbdata[, gatvars$aggregator1])
  } else if (mergevars$mergeopt2 == "least" & gatvars$aggregator2 != "NONE"){
    nborder1<-order(nbdata[, gatvars$aggregator1])
    nborder2<-order(nbdata[, gatvars$aggregator2])
    if (nbdata[nborder1[1], gatvars$aggregator1] / gatvars$minvalue1 <=
        nbdata[nborder2[1], gatvars$aggregator2] / gatvars$minvalue2) {
      nborder<-nborder1
    } else {
      nborder <- nborder2
    }
  } #end code for order according to aggregation variable

  # order according to similarity
  if (mergevars$mergeopt2 == "similar") {
    nborder <- order(abs(nbdata[, mergevars$similar1] / nbdata[, mergevars$similar2] -
                         first[, mergevars$similar1] / first[, mergevars$similar2]))
  }
  return(nborder)
}

