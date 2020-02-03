#' Create New GAT Region
#'
#' This function combines multiple observations by summing the numeric
#' variables, assigning one value for the character variables, and replacing
#' the ID value with a unique ID. It returns the resulting single observation.
#'
#' This function can handle more than two observations at a time, but has only
#' been tested with two observations.
#'
#' @param mydata   A data frame from the original shapefile.
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
#' @param area     The area shapefile (if pwrepeat=TRUE or GATx/GATy missing).
#' @param popwt    A boolean that denotes whether population weighting is
#'                 being used.
#'
#' @examples
#' # create dataset with polygons to merge
#' ids <- c("e", "e", "d", "d", "f", "f", "f", "e", "e", "d", "e",
#'          "d", "b", "c", "c", "a", "b", "c", "a", "b", "c")
#' my_merged_obs <-
#'   createGATregion(
#'     mydata = hftown@data,
#'     newreg = hftown@data[ids=="a",],
#'     myidvar = "ID",
#'     nrid = "a",
#'     area = hftown,
#'     pop = hfpop,
#'     IDlist = ids,
#'     pwrepeat = TRUE,
#'     popwt = TRUE
#'   )
#'
#' @export

# a more generic way to create the newregdata
createGATregion <- function(mydata, newreg, myidvar, nrid, area = NULL,
                            pop = NULL, IDlist = NULL, pwrepeat = FALSE,
                            popwt = FALSE) {
  # get data classes for all columns ####
  listtype <- sapply(mydata, class)
  listtypetf <- listtype %in% c("integer", "numeric")
  listitems <- unique(names(mydata)) # to handle non-default use

  # take character variables from the 2nd region ####
  newregchars <- newreg[2, !listtypetf]
  # if only one character variable, must be ID but won't have name
  if (length(newregchars) == 1) {
    newregchars <- data.frame(newregchars)
    names(newregchars) <- myidvar
  }
  # probably should be a more elegant way to take care of this case

  # find possible lat/long/x/y variables ####
  # add a function option for user to enter additional lat/long variable names?
  latnames <- c("LATITUDE", "LAT", "Y", "GATY", "OLD_GATY")
  longnames <- c("LONGITUDE", "X", "LON", "LONG", "GATX", "OLD_GATX")
  locvardex <- which(toupper(listitems) %in% c(latnames, longnames))
  listtypetf[locvardex] <- FALSE

  # number of regions ####
  # future change? it would be cool if I could provide both pop-weighted and
  # geographic centroids here; these aren't actually retained
  if (!popwt) {
    # geographic centroids
    geomeans <- sapply(newreg[, locvardex], mean)
    if (!"GATX" %in% names(geomeans)) {
      centroids <- as.data.frame(sp::coordinates(area))
      colnames(centroids) <- c("GATx", "GATy")
      id <- area@data[, myidvar]
      centroids <- cbind(centroids, id)
      mydata <- merge(mydata, centroids, by.x = myidvar, by.y = "id", all.y = FALSE)
      geomeans <- c(geomeans, GATx = mean(as.numeric(mydata$GATx)),
                    GATy = mean(as.numeric(mydata$GATy)))
      # I want to include population weighted means here, but I will need to include
      # population in the dataset when I do the initial weighting
    }
  } else if (pwrepeat) {
      # population weighted centroids (repeat weighting)
      gatmeans <- weightGATregion(area = area, pop = pop, IDlist = IDlist,
                                  idvar = myidvar, nrid = nrid)
      if (is.na(gatmeans["GATx"])) {
        # this should weight the centroids, but by definition,
        # population is missing or GATx would have been defined above
        geomeans <- sapply(newreg[, locvardex], mean)
        if (!"GATX" %in% names(geomeans)) {
          centroids <- as.data.frame(sp::coordinates(area))
          names(centroids) <- c("GATx", "GATy")
          id <- area@data[, myidvar]
          centroids <- cbind(id, centroids)
          mydata <- merge(mydata, centroids, by.x = myidvar, by.y = "id", all.y = FALSE)
          geomeans <- c(geomeans, GATx = mean(mydata$GATx), GATy = mean(mydata$GATy))
        }
      } else {
        locvars <- which(toupper(listitems) %in% c(latnames, longnames) &
                         !toupper(listitems) %in% c("GATX", "GATY"))
        coordmeans <- sapply(newreg[, locvars], mean)
        geomeans <- c(coordmeans, gatmeans)
      }
  } else if (!pwrepeat) {
    # should be population weighted area centroids (if population available),
    # not geographic
    geomeans <- sapply(newreg[, locvardex], mean)
    if (!"GATx" %in% names(geomeans)) {
      centroids <- as.data.frame(sp::coordinates(area))
      names(centroids) <- c("GATx", "GATy")
      id <- area@data[, myidvar]
      centroids <- cbind(id, centroids)
      mydata <- merge(mydata, centroids, by.x = myidvar, by.y = "id", all.y = FALSE)
      geomeans <- c(geomeans, GATx = mean(mydata$GATx), GATy = mean(mydata$GATy))
    }
    t <- mydata[IDlist == nrid,]
    x <- sum(t$GATx*t$GATpop) / sum(t$GATpop)
    y <- sum(t$GATy*t$GATpop) / sum(t$GATpop)
    if (is.finite(x)) geomeans["GATx"] <- x
    if (is.finite(y)) geomeans["GATy"] <- y
  }

  # region numbers data frame ####
  newregnums <- (sapply(newreg[, listtypetf], sum)) # sum most numeric variables
  # if only one numeric variable, won't get a data frame,
  # so add a line to take care of that
  if (length(names(newregnums)) == 0) {
    newregnums <- data.frame(sum(newregnums))
    names(newregnums) <- "Num_Areas"
  }

  newregdata <- data.frame(c(newregchars, newregnums, geomeans))
  listitems <- unique(c(listitems, "GATx", "GATy"))
  newregdata <- newregdata[, listitems] # make sure to keep same order of columns
  newregdata[, myidvar] <- nrid # assign the new region the new id
  row.names(newregdata) <- nrid # make sure the row name matches
  return(newregdata)
}


