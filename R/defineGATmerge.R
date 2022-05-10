#' Define GAT Merge Pattern
#'
#' @description
#' This function reads in the shapefile to be aggregated and the parameters to use.
#' It returns a list of the following elements:
#'
#' \itemize{\bold{IDlist: }
#'   A character vector in which the old IDs are replaced with the merged IDs,
#'   for the crosswalk.
#' }
#' \itemize{\bold{shp: }
#'   A data frame defining the polygons to be merged.
#' }
#' \itemize{\bold{newregno: }
#'   An integer representing the number of merges completed.
#' }
#' \itemize{\bold{logmsg: }
#'   A string that includes any warnings generated during the merging.
#' }
#'
#' @details
#' For details on how merges are assigned, see
#' \href{../doc/gat_tech_notes.html}{
#' \code{vignette("gat_tech_notes", package = "gatpkg")}}.
#'
#' @param area        A spatial layer representing areas to be aggregated.
#' @param pop         A spatial layer containing underlying population values.
#' @param gatvars     A list of objects created by GAT. It contains the
#'                    strings myidvar, aggregator1, aggregator2, and boundary,
#'                    which are all variables in the area, the boolean popwt,
#'                    and the numbers minvalue1 and minvalue2.
#'                    Both aggregator1 and aggregator2 must be numeric and
#'                    myidvar must contain unique values.
#' @param mergevars   A list of string objects needed to aggregate the areas
#'                    in the GAT tool. It contains mergeopt1, similar1, and
#'                    similar2. The valid options for mergeopt1 are "closest",
#'                    "least", and "similar". If "similar" is selected, similar1
#'                    and similar2 must be numeric variables in the area and
#'                    similar2 cannot equal zero.
#' @param pwrepeat    A boolean denoting whether population weighting (if used)
#'                    should be recalculated each time two areas are merged
#'                    (TRUE) or if area centroids should be weighted with area
#'                    populations (FALSE). If population weighting is not used,
#'                    this option is ignored.
#' @param adjacent    A boolean denoting whether to force GAT to merge only
#'                    adjacent areas.
#' @param minfirst    A boolean denoting whether or not to select the most
#'                    desirable neighbor only from among the neighbors that
#'                    have values below the desired minimum. If no neighbors
#'                    are below the desired minimum, the most desirable of all
#'                    eligible neighbors is selected.
#' @param progressbar A boolean denoting whether to display the progress bar.
#' @param exclist     The settings to define areas to be excluded.
#'
#' @examples
#'
#' if (interactive()) {
#' gatvars <- list(
#'   myidvar = "ID",             # character variable of unique values
#'   aggregator1 = "TOTAL_POP",  # numeric variable
#'   aggregator2 = "TOTAL_POP",  # numeric variable
#'   minvalue1 = 5000, minvalue2 = 5000,
#'   maxvalue1 = 15000, maxvalue2 = 15000,
#'   boundary = "COUNTY",        # character variable of non-unique values
#'   rigidbound = TRUE,          # boolean to enforce boundary
#'   popwt = FALSE,              # boolean for population weighting
#'   popvar = "Pop"              # aggregation variable in population laayer
#' )
#'
#' mergevars <- list(
#'   mergeopt1 = "similar",    # string can be similar, closest, or least
#'   similar1 = "AREAWATR",    # numeric variable
#'   similar2 = "AREALAND",    # numeric variable without any zeros
#'   centroid = "geographic"
#' )
#'
#' exclist <- list(
#'   var1 = "TOTAL_POP", math1 = "less than", val1 = 500,
#'   var2 = "NONE", # if not "NONE", define math2 & val2
#'   var3 = "NONE"  # if not "NONE", define math3 & val3
#' )
#' my_merge <- defineGATmerge(
#'     area = hftown, pop = hfpop, gatvars = gatvars, mergevars = mergevars,
#'     exclist = exclist, progressbar = FALSE, pwrepeat = FALSE)
#' }
#'
#' @export

# exclist = NULL; pwrepeat = FALSE; minfirst = FALSE; adjacent = TRUE
# progressbar = TRUE; pop = NULL

defineGATmerge <- function(area, gatvars, mergevars, exclist = NULL,
                           pwrepeat = FALSE, adjacent = TRUE, pop = NULL,
                           minfirst = FALSE, progressbar = TRUE) {
  # sf conversion ----
  area <- sf::st_as_sf(area)
  data <- data.frame(area)

  max1 <- as.numeric(gsub(",", "", gatvars$maxvalue1))
  if (length(max1) == 0) {
    max1 <- max(data.frame(area)[, gatvars$aggregator1])
  }
  min1 <- as.numeric(gsub(",", "", gatvars$minvalue1))
  max2 <- as.numeric(gsub(",", "", gatvars$maxvalue2))
  if (length(max2) == 0) {
    max2 <- max(data.frame(area)[, gatvars$aggregator2])
  }
  min2 <- as.numeric(gsub(",", "", gatvars$minvalue2))
  if (gatvars$aggregator2 == "NONE") gatvars$aggregator2 <- gatvars$aggregator1

  if (!"GATflag" %in% names(data)) {
    data$GATflag <- if (is.null(exclist)) 0 else calculateGATflag(exclist, d = data)
    data$GATflag <- ifelse(data[, gatvars$aggregator1] > max1, 5, data$GATflag)
    if (!gatvars$aggregator2 == gatvars$aggregator1) {
      data$GATflag <- ifelse(data[, gatvars$aggregator2] > max2, 5, data$GATflag)
    }
    area$GATflag <- data$GATflag
  }
  if (!"GATid" %in% names(data)) {
    data$GATid <- data[, gatvars$myidvar]
    area$GATid <- data$GATid
  }
  row.names(area) <- data$GATid

  # draw progress bar ----
  definenv <- new.env()

  if (progressbar) {
    definenv$mb <- list(label = "Preparing merge files. Please wait.",
               title = "NYSDOH GAT: merging")
    definenv$tmb <- tcltk::tkProgressBar(title = definenv$mb$title,
                                         label = definenv$mb$label, min = 0,
                                max = nrow(area), initial = 0, width = 400)
  }

  # if projection is lat/lon, projection = TRUE, otherwise FALSE
  mapvars <- list(projection = sf::st_is_longlat(area))

  # set up centroids ----
  if (gatvars$popwt) {
    if (progressbar) {
      definenv$mb$label = "Loading population file. This step may be slow."
      tcltk::setTkProgressBar(definenv$tmb, value = 0, label = definenv$mb$label)
    }

    temp <- weightGATmap(area = area, pop = pop, popvar = gatvars$popvar,
                         idvar = "GATid")
    mapvars$centroids <- temp$centroids
    mapvars$pop <- temp$pop
    # area$GATpop <- mapvars$centroids$GATpop
  } else {
    j <- sf::st_centroid(area)
    sf::st_geometry(j) <- sf::st_centroid(j$geometry)
    pts <- do.call(rbind, sf::st_geometry(j))
    mapvars$centroids <- data.frame(pts)
    colnames(mapvars$centroids) <- c("GATx", "GATy")
    mapvars$pop <- NULL
  }

  # default to not lat/long if something goes wrong
  if (is.na(mapvars$projection)) mapvars$projection <- FALSE

  # add centroids to polygon data ----
  aggvars <- list(IDlist = data$GATid, newregno = 1, logmsg = "",
                  shp = cbind(area, mapvars$centroids))

  # set up temporary variables ----
  if (gatvars$aggregator2 == "NONE") gatvars$aggregator2 <- gatvars$aggregator1
  # for some reason, numeric sometimes switched to character
  aggvars$shp[, gatvars$aggregator1] <-
    as.numeric(as.character(data.frame(aggvars$shp)[, gatvars$aggregator1]))
  aggvars$shp[, gatvars$aggregator2] <-
    as.numeric(as.character(data.frame(aggvars$shp)[, gatvars$aggregator2]))

  temp <- list(alldata = aggvars$shp[which(aggvars$shp$GATflag == 0), ],
               digits = nchar(nrow(aggvars$shp)),
               index = sapply(data.frame(aggvars$shp), is.integer),
               rownames = data$GATid)

  # test if loop can run ----
  if (nrow(temp$alldata) > 0) {
    # set up more temporary variables ----
    temp$minpop1 = min(data.frame(temp$alldata)[, gatvars$aggregator1])
    temp$minpop2 = min(data.frame(temp$alldata)[, gatvars$aggregator2])

    myids <- as.character(unlist(data$GATid))
    myids <- myids[grepl("GATid", myids)]
    myids <- gsub("GATid_", "",  myids)

    if (length(myids) == 0) {
      maxid <- 0
    } else {
      maxid <- max(as.numeric(myids))
      temp$digits <- nchar(myids[1])
    }

    # set up town variables ----
    # replace with sf
    # get list of neighbors using poly2nb method from spdep package
    townvars <- list(oldtownnb = spdep::poly2nb(area, queen = FALSE,
                                                row.names = temp$rownames),
                     townnb = spdep::poly2nb(area, queen = FALSE,
                                             row.names = temp$rownames))

    # convert integers to double (change to convert when I read in file?)
    # might be redundant now, but check later
    aggvars$shp[, temp$index] <-
      sapply(data.frame(aggvars$shp)[, temp$index], as.numeric)

    # start while loop ----
    while (temp$minpop1 < min1 | temp$minpop2 < min2){
      # identify who is mergeable ----
      # remove flagged areas
      temp$aggdata <-
        aggvars$shp[which(aggvars$shp$GATflag == 0), ]

      # isolate areas that are too small
      temp$tobemerged <- temp$aggdata[which(
        (data.frame(temp$aggdata)[, gatvars$aggregator1] < min1) |
        (data.frame(temp$aggdata)[, gatvars$aggregator2] < min2) ), ]

      # change the merge order high to low
      if (temp$minpop1 < min1) {
        temp$tobemerged <-
          temp$tobemerged[order(-data.frame(temp$tobemerged)[, gatvars$aggregator1]), ]
      } else {
        temp$tobemerged <-
          temp$tobemerged[order(-temp$tobemerged[, gatvars$aggregator2]), ]
      }

      # default merge option is the one selected
      mergevars$mergeopt2 <- mergevars$mergeopt1

      # incremental progress bar ----
      step <- nrow(area) - nrow(temp$tobemerged)
      if (progressbar) {
        definenv$mb$label <- paste0("Merge ", aggvars$newregno + maxid, ": ",
                           nrow(temp$tobemerged), " areas remaining.")
        tryCatch(tcltk::setTkProgressBar(definenv$tmb, value = step,
                                         label = definenv$mb$label),
                 error = function(e) definenv$tmb <- NULL)
        if (is.null(definenv$tmb)) {
          definenv$tmb <- tcltk::tkProgressBar(title = definenv$mb$title,
                                               label = definenv$mb$label,
                                               initial = 0, min = 0,
                                               max = nrow(area), width = 400)
          tcltk::setTkProgressBar(definenv$tmb, value = step)
        }
      }

      # identify the area to merge this loop ----
      temp$first <- identifyGATfirstobs(tobemerged = temp$tobemerged,
                                        aggvar = gatvars$aggregator1,
                                        aggvar2 = gatvars$aggregator2,
                                        minval = min1, minval2 = min2)

      # remove areas that are too large
      temp$aggdata <- temp$aggdata[which(
        (data.frame(temp$aggdata)[, gatvars$aggregator1] +
           data.frame(temp$first)[, gatvars$aggregator1] < max1) |
          (data.frame(temp$aggdata)[, gatvars$aggregator2] +
             data.frame(temp$first)[, gatvars$aggregator2] < max2)), ]


      # set up warnings ----
      temp$logmsg <- paste0("Merge ", aggvars$newregno + maxid, " (",
                            data.frame(temp$first)$GATid, "):")
      temp$warnkey <- "n" # no warnings
      warnings <- c(
        ab = "No physically adjacent neighbors found within the same boundary.",
        amb = paste("No physically adjacent neighbors below the minimum value",
                    "found within the same boundary."),
        mb = paste("Found areas in the same boundary below the minimum value,",
                   "but they are not physically adjacent."),
        b = "Found areas in the same boundary, but they are not physically adjacent.",
        f = "No neighbors found. This area cannot be merged further.",
        nb = "No neighbors found in boundary.",
        am = "No physically adjacent neighbors found below the minimum aggregation value."
      )

      # find neighbors ----
      # temporary flag: neighbors found?
      temp$idfail <- TRUE
      temp$island <- FALSE

      townvars$townnbid <- attr(townvars$townnb, "region.id")
      # as of this point, townvars$nbdata is sf
      # sometimes attr() fails and just gives index numbers
      if (data.frame(temp$first)$GATid %in% townvars$townnbid) {
        townvars$townnbidloc <- which(townvars$townnbid ==
                                      data.frame(temp$first)$GATid)
        townvars$neighbors <- townvars$townnb[[townvars$townnbidloc]]
        townvars$neighborid <- townvars$townnbid[townvars$neighbors]
        townvars$nbdata <-
          temp$aggdata[which(data.frame(temp$aggdata)$GATid %in%
                             townvars$neighborid), ]
      } else {
        townvars$nbdata <- data.frame()
      }

      # get the data about these neighbors ----
      # if boundary variable
      if (gatvars$boundary != "NONE") {
        if (nrow(townvars$nbdata) > 0) {
          temp$firstboundary <- as.character(data.frame(temp$first)[, gatvars$boundary])
          # index of neighbors in same county
          temp$inboundary <- townvars$nbdata[which(
            data.frame(townvars$nbdata)[, gatvars$boundary] == temp$firstboundary), ]
          # find neighbors in boundary, adjacent, below minimum value
          if (minfirst) {
            townvars$nbdata <-
              temp$inboundary[which(temp$inboundary[, gatvars$aggregator1] < min1 |
                                      temp$inboundary[, gatvars$aggregator2] < min2), ]
            temp$inco_dex <- which(townvars$nbdata[, gatvars$boundary] ==
                                     temp$firstboundary)
            temp$inco_nbdata <- townvars$nbdata[temp$inco_dex, ]
            if (nrow(temp$inco_nbdata) > 0) {
              townvars$nbdata <- temp$inco_nbdata
              temp$idfail <- FALSE
            } else {
              temp$warnkey <- "amb" # no adjacent below min within boundary
              temp$idfail <- TRUE
            }
          }
          # if no neighbors, find neighbors in boundary, adjacent
          if (temp$idfail & adjacent) {
            townvars$nbdata <-
              temp$inboundary[which(data.frame(temp$inboundary)$GATid %in%
                                      townvars$neighborid), ]
            # index of neighbors in same county
            temp$inco_dex <- which(data.frame(townvars$nbdata)[, gatvars$boundary] ==
                                     temp$firstboundary)
            temp$inco_nbdata <- townvars$nbdata[temp$inco_dex, ]
            if (nrow(temp$inco_nbdata) > 0) {
              townvars$nbdata <- temp$inco_nbdata
              temp$idfail <- FALSE # found neighbor
            } else {
              temp$warnkey <- "ab" # no adjacent within boundary
              temp$idfail <- TRUE # still failed
            }
          }
        }
        # if no neighbors, find other area in boundary
        if (temp$idfail & !adjacent) {
          # below minimum preferred?
          if (minfirst) {
            temp$inco_nbdata <-
              temp$tobemerged[which(data.frame(temp$tobemerged)[, gatvars$boundary] ==
                                    temp$firstboundary &
                                    data.frame(temp$tobemerged)$GATid !=
                                    data.frame(temp$first)$GATid), ]
            if(nrow(temp$inco_nbdata) > 0) {
              townvars$nbdata <- temp$inco_nbdata
              mergevars$mergeopt2 <- "closest"
              temp$warnkey <- "mb"
              temp$idfail <- FALSE
            } else {
              temp$idfail <- TRUE
            }
          }
          # if still failing
          if (temp$idfail) {
            temp$inco_nbdata <-
              temp$aggdata[which(data.frame(temp$aggdata)[, gatvars$boundary] ==
                                 temp$firstboundary &
                                 data.frame(temp$aggdata)$GATid !=
                                 data.frame(temp$first)$GATid), ]
            if(nrow(temp$inco_nbdata) > 0){
              townvars$nbdata <- temp$inco_nbdata
              mergevars$mergeopt2 <- "closest"
              temp$warnkey <- "b"
              temp$idfail <- FALSE
            } else {
              temp$idfail <- TRUE
            }
          }
        }
        # if no neighbors, quit loop
        if (temp$idfail) {
          temp$warnkey <- "nb"
        }
      }
      # if boundary not enforced
      if (temp$idfail & !gatvars$rigidbound) {
        # if merge below minimum first
        if (minfirst) {
          # reset town list, just in case
          townvars$nbdata <-
            temp$tobemerged[which(data.frame(temp$tobemerged)$GATid %in%
                                    townvars$neighborid), ]
          if (nrow(townvars$nbdata) > 0) {
            temp$idfail <- FALSE # found neighbor
          } else {
            temp$warnkey <- "am" # no adjacent below minimum
            temp$idfail <- TRUE # still failed
          }
        }
        # if must be adjacent
        if (temp$idfail & adjacent) {
          townvars$nbdata <-
            temp$aggdata[which(data.frame(temp$aggdata)$GATid %in%
                                    townvars$neighborid), ]
          if (nrow(townvars$nbdata) > 0) {
            temp$idfail <- FALSE # found neighbor
          } else {
            temp$warnkey <- "ab" # no adjacent within boundary
            temp$idfail <- TRUE # still failed
          }
        }
        # if not adjacent
        if (temp$idfail & !adjacent) {
          # reset town list, just in case
          townvars$nbdata <-
            temp$aggdata[which(data.frame(temp$aggdata)$GATid %in%
                                 townvars$neighborid), ]
          if (nrow(townvars$nbdata) > 0) {
            temp$idfail <- FALSE # found neighbor
          } else {
            if (temp$logmsg == "") {
              temp$logmsg <- paste0(temp$logmsg, "Merge ", aggvars$newregno + maxid,
                                    " (", data.frame(temp$first)$GATid, "):")
            }
            temp$logmsg <- paste(temp$logmsg,
                                 "No physically adjacent neighbors found.")
            temp$idfail <- TRUE # still failed
          }

          if (temp$idfail) {
            # don't want to use least or similar if no adjacent neighbors
            temp$island <- TRUE
            mergevars$mergeopt2 <- "closest"
            townvars$nbdata <- aggvars$shp[which(
              data.frame(aggvars$shp)$GATid !=
                data.frame(temp$first)$GATid), ]
            if (nrow(townvars$nbdata) > 0) {
              temp$idfail <- FALSE # found neighbor
            } else {
              temp$idfail <- TRUE # still failed
            }
          }
        }
      }

      # quit searching for neighbors ----
      if(temp$idfail) {
        # check for and remove flags and oversized at beginning of loop
        aggvars$shp$GATflag[data.frame(aggvars$shp)$GATid ==
                                      data.frame(temp$first)$GATid] <- 10
        temp$warnkey <- "f"
      } else {
        # rank centroid distances ----
        townvars$nborder <- rankGATdistance(area = area, mergevars = mergevars,
                            nbdata = townvars$nbdata, first = temp$first,
                            gatvars = gatvars)

        # data that will be combined to form new region ----
        t <- rbind(temp$first, townvars$nbdata[townvars$nborder[1], ])
        townvars$newreg <- sf::st_make_valid(t) # fix invalid areas

        # add leading zeroes based on maximum number of areas ----
        townvars$nrid <- paste0("GATid_", formatC(aggvars$newregno + maxid,
                                width = temp$digits, format = "d", flag = "0"))

        temp$IDloc <- which(aggvars$IDlist %in%
                              data.frame(townvars$newreg)$GATid)
        aggvars$IDlist[temp$IDloc] <- townvars$nrid

        # calculate new region ----
        aggvars$IDlist[aggvars$IDlist %in%
                         data.frame(townvars$newreg)$ID] <- townvars$nrid

        townvars$newregdata <- createGATregion(pop = mapvars$pop, area = area,
                               newreg = townvars$newreg, myidvar = "GATid",
                               nrid = townvars$nrid, IDlist = aggvars$IDlist,
                               pwrepeat = pwrepeat, popwt = gatvars$popwt)

        # add the new region to the list of data about the regions ----
        # aggvars$shp <- sf::st_as_sf(aggvars$shp)
        aggvars$shp <- rbind(aggvars$shp, townvars$newregdata)

        # need to remove the info about the old regions
        aggvars$shp <- aggvars$shp[which(
                      !data.frame(aggvars$shp)$GATid %in%
                        data.frame(townvars$newreg)$GATid), ]

        # update neighbor listings ----
        townvars$townnb <- aggregateGATnb(nb = townvars$oldtownnb,
                                          ids = aggvars$IDlist)
      }

      # find the minimum population ----
      aggvars$newregno <- aggvars$newregno + 1

      temp$minpop1 <- min(data.frame(aggvars$shp)[which(aggvars$shp$GATflag == 0),
                                              gatvars$aggregator1])
      temp$minpop2 <- min(data.frame(aggvars$shp)[which(aggvars$shp$GATflag == 0),
                                              gatvars$aggregator2])

      if (temp$warnkey != "n") {
        aggvars$logmsg <- paste(aggvars$logmsg, temp$logmsg,
                                warnings[temp$warnkey], "\n")
      }
      # 'garbage collection': free up memory ----
      gc(verbose = FALSE)
    }
  } else {
    if (gatvars$aggregator1 == gatvars$aggregator2) {
      vars <- gatvars$aggregator1
    } else {
      vars <- paste(gatvars$aggregator1, "and", gatvars$aggregator2)
    }

    msg <- paste("All areas have values of", vars,
                 "over your selected minimum value(s). No areas were merged.")
    tcltk::tkmessageBox(title = "Merge failed", message = msg,
                        type = "yesno", icon = "info")
  }
  # close progress bar that monitors the aggregation ----
  if (progressbar) {
    close(definenv$tmb)
  }
  # return ----
  return(aggvars)
}

