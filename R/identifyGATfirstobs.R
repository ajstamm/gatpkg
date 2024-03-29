#' Identify GAT First Observation
#'
#' This function calculates the observation with the highest aggregation
#' value, which will be the next observation merged in the full program. The
#' minimum value must be greater than zero, but is only important if there
#' are two aggregation variables.
#'
#' @param tobemerged A data frame, intended to be read from a shapefile DBF.
#' @param aggvar     The first variable by which polygons will be aggregated.
#' @param aggvar2    The second variable by which polygons will be aggregated.
#' @param minval     The desired minimum value at which to aggregate the first
#'                   aggregation variable.
#' @param minval2    The desired minimum value at which to aggregate the second
#'                   aggregation variable.
#'
#' @examples
#' aggvar <- "TOTAL_POP"  # identify the aggregation variable
#' minvalue <- 5000       # identify the minimum value
#'
#' # observation with the highest POP2010
#' my_firstobs <- identifyGATfirstobs(
#'   tobemerged = hftown,
#'   aggvar = aggvar,
#'   aggvar2 = aggvar, # repeat aggvar if only one variable
#'   minval = minvalue,
#'   minval2 = minvalue # repeat minval if only one variable
#' )
#'
#' @export

identifyGATfirstobs <- function(tobemerged, aggvar, aggvar2, minval, minval2) {
  # single line OR gives all results, double line gives only one row
  lowpop <- order(data.frame(tobemerged)[, aggvar], decreasing = TRUE)

  # to sort both high to low, take value that is highest percentage of minval
  if (aggvar != aggvar2) {
    lowpop2 <- order(data.frame(tobemerged)[, aggvar2], decreasing = TRUE)
    if ((as.numeric(data.frame(tobemerged)[lowpop[1], aggvar]) / minval) >=
        (as.numeric(data.frame(tobemerged)[lowpop2[1], aggvar2]) / minval2)) {
      first <- tobemerged[lowpop[1], ]
    } else {
      first <- tobemerged[lowpop2[1], ]
    }
  } else {
    first <- tobemerged[lowpop[1], ]
  }
  return(first)
}
