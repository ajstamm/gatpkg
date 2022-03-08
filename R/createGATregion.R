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

  # get data classes for all columns ----
  types <- sapply(data.frame(newreg), class) # get data classes for all columns
  items <- unique(names(data.frame(newreg))) # to handle non-default use
  ivars <- items[grepl("integer|numeric", types)]

  # listtype <- sapply(mydata, class)
  # listtypetf <- listtype %in% c("integer", "numeric")
  # listitems <- unique(names(mydata)) #

  # find possible lat/long/x/y variables ----
  # add a function option for user to enter additional lat/long variable names?
  latnames <- c("LATITUDE", "LAT", "Y", "GATY", "OLD_GATY")
  longnames <- c("LONGITUDE", "X", "LON", "LONG", "GATX", "OLD_GATX")
  items <- items[!toupper(items) %in% c(latnames, longnames)]
  latlong <- names(area)[toupper(names(area)) %in% c(latnames, longnames)]
  ivars <- ivars[!toupper(ivars) %in% c(latlong, "GATX", "GATY")]


  # take character variables from the 2nd region ----
  tempvars <- data.frame(newreg)[, items[!items %in%
                                           c(latlong, ivars, "geometry")]]

  # is there way to keep newregchars in sf?
  # or reconvert geometry at the end?

  newregtemp <- sf::st_union(newreg)
  newregchars <- sf::st_as_sf(newregtemp)
#  sf::st_geometry(newregchars) <- "geometry" # erroring
  for (i in 1:length(names(tempvars))) {
    x <- unique(unlist(strsplit(paste(data.frame(tempvars)[, names(tempvars)[i]],
                                      collapse = ", "), ", ")))
    x <- x[order(x)]
    x <- paste(x, collapse = ", ")
    newregchars[, names(tempvars)[i]] <- x
  }

  # number of regions ----
  # future change? it would be cool if I could provide both pop-weighted and
  # geographic centroids here; these aren't actually retained

  if (!popwt) {
    # geographic centroids
    if (!"GATX" %in% toupper(latlong)) {
      j <- sf::st_centroid(area)
      sf::st_geometry(j) <- sf::st_centroid(j$geometry)
      pts <- do.call(rbind, sf::st_geometry(j))
      pts <- data.frame(pts)
      names(pts) <- c("GATx", "GATy")
      # centroids <- as.data.frame(sp::coordinates(area))
      # colnames(centroids) <- c("GATx", "GATy")
      id <- data.frame(area)[, myidvar]
      pts <- cbind(pts, id)
      area <- merge(area, pts, by.x = myidvar, by.y = "id", all.y = FALSE)
      # I want to include population weighted means here, but I will need to include
      # population in the dataset when I do the initial weighting
    }
    if (length(latlong) > 0) {
      geomeans <- colMeans(data.frame(newreg)[, latlong])
      geomeans <- data.frame(t(geomeans))
      geomeans$GATx <- mean(as.numeric(area$GATx))
      geomeans$GATy <- mean(as.numeric(area$GATy))
    } else {
      geomeans <- data.frame(GATx = mean(as.numeric(area$GATx)),
                             GATy = mean(as.numeric(area$GATy)))
    }
  } else if (pwrepeat) {
      # population weighted centroids (repeat weighting)
      gatmeans <- weightGATregion(area = area, pop = pop, IDlist = IDlist,
                                  idvar = myidvar, nrid = nrid)
      if (is.na(gatmeans["GATx"])) {
        # this should weight the centroids, but by definition,
        # population is missing or GATx would have been defined above
        if (!"GATX" %in% toupper(latlong)) {
          j <- sf::st_centroid(area)
          sf::st_geometry(j) <- sf::st_centroid(j$geometry)
          pts <- do.call(rbind, sf::st_geometry(j))
          pts <- data.frame(pts)
          names(pts) <- c("GATx", "GATy")
          # centroids <- as.data.frame(sp::coordinates(area))
          # colnames(centroids) <- c("GATx", "GATy")
          id <- data.frame(area)[, myidvar]
          pts <- cbind(pts, id)
          area <- merge(area, pts, by.x = myidvar, by.y = "id", all.y = FALSE)
        }
        if (length(latlong) > 0) {
          geomeans <- colMeans(data.frame(newreg)[, latlong])
          geomeans <- data.frame(t(geomeans))
          geomeans$GATx <- mean(as.numeric(area$GATx))
          geomeans$GATy <- mean(as.numeric(area$GATy))
        } else {
          geomeans <- data.frame(GATx = mean(as.numeric(area$GATx)),
                                 GATy = mean(as.numeric(area$GATy)))
        }
      } else {
        lats <- which(!toupper(latlong) %in% c("GATX", "GATY"))
        coordmeans <- data.frame(t(sapply(newreg[, lats], mean)))
        geomeans <- data.frame(coordmeans, gatmeans)
      }
  } else if (!pwrepeat) {
    # should be population weighted area centroids (if population available),
    # not geographic
    if (!"GATx" %in% toupper(latlong)) {
      j <- sf::st_centroid(area)
      sf::st_geometry(j) <- sf::st_centroid(j$geometry)
      pts <- do.call(rbind, sf::st_geometry(j))
      pts <- data.frame(pts)
      names(pts) <- c("GATx", "GATy")
      # centroids <- as.data.frame(sp::coordinates(area))
      # colnames(centroids) <- c("GATx", "GATy")
      id <- data.frame(area)[, myidvar]
      pts <- cbind(pts, id)
      area <- merge(area, pts, by.x = myidvar, by.y = "id", all.y = FALSE)
    }
    if (length(latlong) > 0) {
      geomeans <- colMeans(data.frame(newreg)[, latlong])
      geomeans <- data.frame(t(geomeans))
      geomeans$GATx <- mean(as.numeric(area$GATx))
      geomeans$GATy <- mean(as.numeric(area$GATy))
    } else {
      geomeans <- data.frame(GATx = mean(as.numeric(area$GATx)),
                             GATy = mean(as.numeric(area$GATy)))
    }
    t <- area[IDlist == nrid,]
    gatmeans <- weightGATregion(area = t, pop = pop,
                                IDlist = rep(nrid, nrow(t)),
                                idvar = myidvar, nrid = nrid)
    if (is.finite(gatmeans$GATy)) geomeans$GATy <- gatmeans$GATy
    if (is.finite(gatmeans$GATx)) geomeans$GATx <- gatmeans$GATx
  }

  # region numbers data frame ----
  newregnums <- data.frame(t(colSums(data.frame(newreg)[, ivars])),
                           # sum most numeric variables
                           Num_Areas = ncol(newreg))
  newregdata <- cbind(newregchars, newregnums, geomeans)
  newregdata$geometry <- NULL
  sf::st_geometry(newregdata) <- "geometry"
  items <- unique(c(items, "GATx", "GATy"))
  newregdata <- newregdata[, items[items %in% names(newregdata)]]
                # make sure to keep same order of columns
  newregdata[, myidvar] <- nrid # assign the new region the new id
  row.names(newregdata) <- nrid # make sure the row name matches
  # return ----
  return(newregdata)
}


