#' Create New GAT Region
#'
#' This function combines multiple observations by summing the numeric
#' variables, assigning one value for the character variables, and replacing
#' the ID value with a unique ID. It returns the resulting single observation.
#'
#' This function can handle more than two observations at a time, but has only
#' been tested with two observations.
#'
#' @param newreg   The rows in the data frame (polygons in the shapefile later)
#'                 that will be joined into a single observation (polygon).
#' @param myidvar  A variable of unique string values to identify the
#'                 observations.
#' @param nrid     A string that will be the ID for the merged observation
#'                 (polygon).
#' @param pop      The base population shapefile (if popwt=TRUE).
#' @param IDlist   The list to match GAT and old IDs (if popwt=TRUE).
#' @param pwrepeat A boolean denoting whether population weighting (if used)
#'                 should be recalculated each time two areas are merged
#'                 (TRUE) or if area centroids should be weighted with area
#'                 populations (FALSE). If population weighting is not used,
#'                 this option is ignored.
#' @param area     The original shapefile.
#' @param popwt    A boolean that denotes whether population weighting is
#'                 being used.
#'
#' @examples
#' # create dataset with polygons to merge
#' ids <- c("e", "e", "d", "d", "f", "f", "f", "e", "e", "d", "e",
#'          "d", "b", "c", "c", "a", "b", "c", "a", "b", "c")
#' my_merged_obs <-
#'   createGATregion(
#'     area = hftown,
#'     newreg = hftown[ids=="a",],
#'     myidvar = "ID",
#'     nrid = "a",
#'     pop = hfpop,
#'     IDlist = ids,
#'     pwrepeat = TRUE,
#'     popwt = TRUE
#'   )
#'
#' @export

# removed mapdata
# a more generic way to create the newregdata
createGATregion <- function(area, newreg, myidvar, nrid, pop = NULL,
                            IDlist = NULL, pwrepeat = FALSE, popwt = FALSE) {
  # sf conversion ----
  area <- sf::st_as_sf(area)
  data <- data.frame(newreg)
  newreg <- sf::st_as_sf(sf::st_union(newreg))

  # get data classes for all columns ----
  types <- sapply(data, class) # get data classes for all columns
  items <- names(data) # to handle non-default use
  ivars <- items[grepl("integer|numeric", types)]

  # find numeric and coordinate variables ----
  # add a function option for user to enter additional lat/long variable names?
  latnames <- c("LATITUDE", "LAT", "Y", "GATY", "OLD_GATY")
  longnames <- c("LONGITUDE", "X", "LON", "LONG", "GATX", "OLD_GATX")
  items <- items[!toupper(items) %in% c(latnames, longnames)]
  latlong <- names(area)[toupper(names(area)) %in% c(latnames, longnames)]
  ivars <- ivars[!toupper(ivars) %in% c(latlong, "GATX", "GATY")]
  for (i in 1:length(latlong)) newreg[, latlong[i]] <- mean(data[, latlong[i]])

  ivars <- ivars[!ivars %in% latlong]
  for (i in 1:length(ivars)) newreg[, ivars[i]] <- sum(data[, ivars[i]])

  # find character variables ----
  chars <- items[!items %in% c(latlong, ivars, "geometry")]
  for (i in 1:length(chars)) {
    x <- unique(unlist(strsplit(paste(data[, chars[i]], collapse = ", "), ", ")))
    x <- paste(x[order(x)], collapse = ", ")
    newreg[, chars[i]] <- x
  }

  # number of regions ----
  # future change? it would be cool if I could provide both pop-weighted and
  # geographic centroids here; these aren't actually retained
  # if calculated above, do not recalculate - geographic centroids
  if (!"GATX" %in% toupper(latlong)) {
    j <- sf::st_centroid(area)
    sf::st_geometry(j) <- sf::st_centroid(j$geometry)
    pts <- do.call(rbind, sf::st_geometry(j))
    pts <- data.frame(pts)
    names(pts) <- c("GATx", "GATy")
    id <- data.frame(area)[, myidvar]
    pts <- cbind(pts, id)
    area <- merge(area, pts, by.x = myidvar, by.y = "id", all.y = FALSE)
  }
  if (pwrepeat) {
      # population weighted centroids (repeat weighting)
      gatmeans <- weightGATregion(area = area, pop = pop, IDlist = IDlist,
                                  idvar = myidvar, nrid = nrid)
      if (!is.na(gatmeans["GATx"])) { # population not missing
        if (is.finite(gatmeans$GATy)) newreg$GATx <- gatmeans$GATx
        if (is.finite(gatmeans$GATx)) newreg$GATy <- gatmeans$GATy
      }
  } else if (popwt) {
    # population weighted area centroids (if population available)
    t <- area[IDlist == nrid,]
    gatmeans <- weightGATregion(area = t, pop = pop,
                                IDlist = rep(nrid, nrow(t)),
                                idvar = myidvar, nrid = nrid)
    if (!is.na(gatmeans["GATx"])) { # population not missing
      if (is.finite(gatmeans$GATy)) newreg$GATx <- gatmeans$GATx
      if (is.finite(gatmeans$GATx)) newreg$GATy <- gatmeans$GATy
    }
  }

  # region numbers data frame ----
  # newreg$Num_Areas = nrow(data) # handled later
  items <- unique(c(names(area), "GATx", "GATy"))
  newreg <- newreg[, items[items %in% names(newreg)]] # keep column order
  newreg[, myidvar] <- nrid # assign the new region the new id
  row.names(newreg) <- nrid # make sure the row name matches
  newreg$geometry <- NULL
  sf::st_geometry(newreg) <- "geometry"
  # return ----
  return(newreg)
}


