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
#'   popvar = "Pop_tot"
#' )
#'
#' mergevars1 <- list( # similar merge
#'   mergeopt1 = "similar",    # can be similar, closest, or least
#'   similar1 = "B_TOT",       # numeric variable
#'   similar2 = "W_TOT",       # numeric variable without any zeros
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
#' filevars <- list(
#'   popfile = "hfblock",
#'   poppath = paste0(find.package("gatpkg"), "/extdata")
#' )
#'
#' # merge areas based on your settings
#' # this function identifies areas to merge; it does not merge them
#' my_merge <-
#'   defineGATmerge(
#'     area = hftown,
#'     gatvars = gatvars,
#'     mergevars = mergevars1,
#'     filevars = filevars
#'   )
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

mergeGATareas <- function(ratevars, aggvars, idvar, myshp) {
  # get the number of polygons per region, and add; has IDlist + Freq
  numpolys <- as.data.frame(table(IDlist = aggvars$IDlist))
  d <- aggvars$allpolydata
  d <- merge(d, numpolys, by.x = idvar, by.y = "IDlist")

  # merge doesn't preserve row names like cbind - so restore row names,
  # but ensure they are not more than 10 characters
  row.names(d) <- d[, idvar]
  names(d)[names(d) == "Freq"] <- "GATnumIDs"

  aggregated <- maptools::unionSpatialPolygons(myshp, aggvars$IDlist)

  if (ratevars$ratename != "no_rate") { # if should calculate rate
    m <- as.numeric(ratevars$multiplier)
    myrate <- m * d[, ratevars$numerator] / d[, ratevars$denominator]
    d <- cbind(d, myrate)
    names(d)[names(d) == "myrate"] <- ratevars$ratename
  } # end if should calculate rate

  aggregated <- sp::SpatialPolygonsDataFrame(aggregated, d, match.ID = TRUE)
  # return the completed shapefile
  return(aggregated)
}
