#' Merge GAT Areas
#'
#' This function merges the areas based on the merge settings created
#' in the previous step. For details on how merges are assigned, see
#' \href{../doc/gat_tech_notes.html}{
#' \code{vignette("gat_tech_notes", package = "gatpkg")}}.
#'
#' @param ratevars A list of rate-related objects.
#' @param aggvars A list of objects produced by the merging process.
#' @param idvar The identifier variable for the original shapefile.
#' @param myshp The original shapefile used in the merging process.
#'
#' @examples
#' gatvars <- list(
#'   myidvar = "ID",             # character variable of unique values
#'   aggregator1 = "TOTAL_POP",  # numeric variable
#'   aggregator2 = "TOTAL_POP",  # numeric variable
#'   minvalue1 = 5000, minvalue2 = 5000,
#'   boundary = "COUNTY",        # character variable of non-unique values
#'   popwt = FALSE,
#'   rigidbound = FALSE,
#'   popvar = "Pop"
#' )
#'
#' mergevars1 <- list( # similar merge
#'   mergeopt1 = "similar",    # can be similar, closest, or least
#'   similar1 = "AREAWATR",    # numeric variable
#'   similar2 = "AREALAND",    # numeric variable without any zeros
#'   centroid = "geographic"
#' )
#'
#' mergevars2 <- list( # closest merge
#'   mergeopt1 = "closest",    # can be similar, closest, or least
#'   similar1 = "NONE",        # numeric variable
#'   similar2 = "NONE",        # numeric variable without any zeros
#'   centroid = "population-weighted"
#' )
#'
#' # merge areas based on your settings
#' # this function identifies areas to merge; it does not merge them
#' my_merge <- defineGATmerge(
#'     area = hftown, pop = hfpop, progressbar = FALSE,
#'     gatvars = gatvars, mergevars = mergevars1)
#'
#' # need all rate options if rate to be calculated
#' # for this example, rate is ignored
#' ratevars <- list(ratename = "no_rate")
#'
#' # aggregate shapefile after identifying areas to merge
#' aggregatedshp <-
#'   mergeGATareas(
#'     ratevars = ratevars,
#'     aggvars = my_merge,      # results from the merge function
#'     idvar = gatvars$myidvar,
#'     myshp = hftown
#'   )
#'
#' @export

mergeGATareas <- function(ratevars, aggvars, idvar = "GATid", myshp) {
  # get the number of polygons per region, and add; has IDlist + Freq
  numpolys <- data.frame(table(IDlist = aggvars$IDlist))
  d <- merge(aggvars$shp, numpolys, by.x = idvar, by.y = "IDlist")
  names(d)[names(d) == "Freq"] <- "GATnumIDs"

  # if should calculate rate
  if (ratevars$ratename != "no_rate") {
    d$myrate <- as.numeric(ratevars$multiplier) *
                data.frame(d)[, ratevars$numerator] /
                data.frame(d)[, ratevars$denominator]
    names(d)[names(d) == "myrate"] <- ratevars$ratename
  }
  return(d)
}
