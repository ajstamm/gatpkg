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
#' \itemize{\bold{allpolydata: }
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
#' @param area        A spatial polygons data frame.
#' @param gatvars     A list of objects created by the GAT tool. It contains
#'                    the strings myidvar, aggregator1, aggregator2, and
#'                    boundary, which are all variables in the area, the
#'                    boolean popwt, and the numbers minvalue1 and minvalue2.
#'                    Both aggregator1 and aggregator2 must be numeric and
#'                    myidvar must contain unique values.
#' @param mergevars   A list of string objects needed to aggregate the areas
#'                    in the GAT tool. It contains mergeopt1, similar1, and
#'                    similar2. The valid options for mergeopt1 are "closest",
#'                    "least", and "similar". If "similar" is selected, similar1
#'                    and similar2 must be numeric variables in the area and
#'                    similar2 cannot equal zero.
#' @param filevars    A list of string objects that list file names and paths.
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
#'   popvar = "Pop_tot"          # aggregation variable in population laayer
#' )
#'
#' mergevars <- list(
#'   mergeopt1 = "similar",    # string can be similar, closest, or least
#'   similar1 = "AREAWATR",    # numeric variable
#'   similar2 = "AREALAND",    # numeric variable without any zeros
#'   centroid = "geographic"
#' )
#'
#' filevars <- list(
#'   popfile = "hfblock",
#'   poppath = paste0(find.package("gatpkg"), "/extdata")
#' )
#'
#' exclist <- list(
#'   var1 = "TOTAL_POP", math1 = "less than", val1 = 500,
#'   var2 = "NONE", # if not "NONE", define math2 & val2
#'   var3 = "NONE"  # if not "NONE", define math3 & val3
#' )
#' my_merge <-
#'   defineGATmerge(
#'     area = hftown,
#'     gatvars = gatvars,
#'     mergevars = mergevars,
#'     filevars = filevars,
#'     exclist = exclist,
#'     pwrepeat = FALSE # don't need pwrepeat if popwt = FALSE
#'   )
#' }
#'
#' @export

defineGATmerge <- function(area, gatvars, mergevars, filevars, exclist = NULL,
                           pwrepeat = FALSE, adjacent = TRUE,
                           minfirst = FALSE, progressbar = TRUE) {
  # sf conversion ----
  area <- sf::st_as_sf(area)
  row.names(area) <- data.frame(area)[, gatvars$myidvar]
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

  # define GATflag if necessary ----
  if (!"GATflag" %in% names(area)) {
    if (!is.null(exclist)) {
      area$GATflag <- calculateGATflag(exclist, d = area)
    } else {
      area$GATflag <- 0
    }
    area$GATflag <- ifelse(data.frame(area)[, gatvars$aggregator1] > max1, 5,
                             area$GATflag)
    if (!gatvars$aggregator2 == gatvars$aggregator1) {
      area$GATflag <- ifelse(data.frame(area)[, gatvars$aggregator2] > max2, 5,
                               area$GATflag)
    }
  }
  if (gatvars$aggregator2 == "NONE") {
    gatvars$aggregator2 <- gatvars$aggregator1
  }

  # draw progress bar ----
  if (progressbar) {
    mb <- list(label = "Preparing merge files. Please wait.",
               title = "NYSDOH GAT: merging")
    tmb <- tcltk::tkProgressBar(title = mb$title, label = mb$label, min = 0,
                                max = nrow(area), initial = 0, width = 400)
  }

  # if projection is lat/lon, projection = TRUE, otherwise FALSE
  mapvars <- list(projection = sf::st_is_longlat(area))

  # set up centroids ####
  # find a way to not hit memory limit for population weighting
  if (gatvars$popwt) {
    # may need to assign ID as row names
    if (progressbar) {
      mb$label = "Loading population file. This step may be slow."
      tcltk::setTkProgressBar(tmb, value = 0, title = mb$title,
                              label = mb$label)
    }
    temp <- weightGATmap(area = area, popvar = gatvars$popvar,
                         filevars = filevars, idvar = gatvars$myidvar)
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

  # add centroids to polygon data ####
  aggvars <- list(IDlist = as.character(data.frame(area)[, gatvars$myidvar]),
                  newregno = 1,
                  allpolydata = cbind(area, mapvars$centroids),
                  logmsg = "") # record merge warnings in the log)

  # set up temporary variables ####
  if (gatvars$aggregator2 == "NONE") gatvars$aggregator2 <- gatvars$aggregator1
  # for some reason, numeric sometimes switched to character
  aggvars$allpolydata[, gatvars$aggregator1] <-
    as.numeric(as.character(data.frame(aggvars$allpolydata)[, gatvars$aggregator1]))
  aggvars$allpolydata[, gatvars$aggregator2] <-
    as.numeric(as.character(data.frame(aggvars$allpolydata)[, gatvars$aggregator2]))

  # add explicit row.names call because assigning row names in spdep is failing
  # are row names needed?
  row.names(area) <- data.frame(area)[, gatvars$myidvar]
  temp <- list(alldata = aggvars$allpolydata[which(aggvars$allpolydata$GATflag == 0), ],
               digits = nchar(nrow(aggvars$allpolydata)),
               index = sapply(data.frame(aggvars$allpolydata), is.integer),
               rownames = data.frame(area)[, gatvars$myidvar])

  # test if loop can run ----
  if (nrow(temp$alldata) > 0) {
    # set up more temporary variables ####
    temp$minpop1 = min(data.frame(temp$alldata)[, gatvars$aggregator1])
    temp$minpop2 = min(data.frame(temp$alldata)[, gatvars$aggregator2])

    myids <- as.character(unlist(data.frame(area)[, gatvars$myidvar]))
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
    aggvars$allpolydata[, temp$index] <-
      sapply(data.frame(aggvars$allpolydata)[, temp$index], as.numeric)

    # start while loop ----
    while ((temp$minpop1 < gatvars$minvalue1) |
           (temp$minpop2 < gatvars$minvalue2)){
      # identify who is mergeable ----
      # remove flagged areas
      temp$aggdata <-
        aggvars$allpolydata[which(aggvars$allpolydata$GATflag == 0), ]

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
        mb$label <- paste0("Merge ", aggvars$newregno + maxid, ": ",
                           nrow(temp$tobemerged), " areas remaining.")
        close(tmb)
        tmb <- tcltk::tkProgressBar(title = mb$title, label = mb$label, min = 0,
                                    max = nrow(area), initial = 0, width = 400)
        tcltk::setTkProgressBar(tmb, value = step)
      }

      # identify the area to merge this loop ----
      temp$first <- identifyGATfirstobs(tobemerged = temp$tobemerged,
                                        aggvar = gatvars$aggregator1,
                                        aggvar2 = gatvars$aggregator2,
                                        minval = gatvars$minvalue1,
                                        minval2 = gatvars$minvalue2)

      # remove areas that are too large
      temp$aggdata <- temp$aggdata[which(
        (data.frame(temp$aggdata)[, gatvars$aggregator1] +
           data.frame(temp$first)[, gatvars$aggregator1] < max1) |
          (data.frame(temp$aggdata)[, gatvars$aggregator2] +
             data.frame(temp$first)[, gatvars$aggregator2] < max2)), ]


      # set up warnings ----
      temp$logmsg <- paste0("Merge ", aggvars$newregno + maxid, " (",
                            data.frame(temp$first)[, gatvars$myidvar], "):")
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
      if (data.frame(temp$first)[, gatvars$myidvar] %in% townvars$townnbid) {
        townvars$townnbidloc <- which(townvars$townnbid ==
                                      data.frame(temp$first)[, gatvars$myidvar])
        townvars$neighbors <- townvars$townnb[[townvars$townnbidloc]]
        townvars$neighborid <- townvars$townnbid[townvars$neighbors]
        townvars$nbdata <-
          temp$aggdata[which(data.frame(temp$aggdata)[, gatvars$myidvar] %in%
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
              temp$inboundary[which(data.frame(temp$inboundary)[, gatvars$myidvar] %in%
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
                                    data.frame(temp$tobemerged)[, gatvars$myidvar] !=
                                    data.frame(temp$first)[, gatvars$myidvar]), ]
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
                                 data.frame(temp$aggdata)[, gatvars$myidvar] !=
                                 data.frame(temp$first)[, gatvars$myidvar]), ]
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
            temp$tobemerged[which(data.frame(temp$tobemerged)[, gatvars$myidvar] %in%
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
            temp$aggdata[which(data.frame(temp$aggdata)[, gatvars$myidvar] %in%
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
            temp$aggdata[which(data.frame(temp$aggdata)[, gatvars$myidvar] %in%
                                 townvars$neighborid), ]
          if (nrow(townvars$nbdata) > 0) {
            temp$idfail <- FALSE # found neighbor
          } else {
            if (temp$logmsg == "") {
              temp$logmsg <- paste0(temp$logmsg, "Merge ", aggvars$newregno + maxid,
                                    " (", temp$first[, gatvars$myidvar], "):")
            }
            temp$logmsg <- paste(temp$logmsg,
                                 "No physically adjacent neighbors found.")
            temp$idfail <- TRUE # still failed
          }

          if (temp$idfail) {
            # don't want to use least or similar if no adjacent neighbors
            temp$island <- TRUE
            mergevars$mergeopt2 <- "closest"
            townvars$nbdata <- aggvars$allpolydata[which(
              data.frame(aggvars$allpolydata)[, gatvars$myidvar] !=
                data.frame(temp$first)[, gatvars$myidvar]), ]
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
        aggvars$allpolydata$GATflag[data.frame(aggvars$allpolydata)[, gatvars$myidvar] ==
                                      data.frame(temp$first)[, gatvars$myidvar]] <- 10
        temp$warnkey <- "f"
      } else {
        # rank centroid distances ----
        townvars$nborder <- rankGATdistance(area = area,
                                            nbdata = townvars$nbdata,
                                            first = temp$first,
                                            gatvars = gatvars,
                                            mergevars = mergevars)

        # data that will be combined to form new region ----
        t <- rbind(temp$first, townvars$nbdata[townvars$nborder[1], ])

        townvars$newreg <- sf::st_make_valid(t) # fix invalid areas


        # add leading zeroes based on maximum number of areas ----
        temp$zero <- paste(rep("0", temp$digits -
                               nchar(aggvars$newregno + maxid)),
                           collapse = "")
        townvars$nrid <- paste0("GATid_", temp$zero,
                                as.character(aggvars$newregno + maxid))

        temp$IDloc <- which(aggvars$IDlist %in% townvars$newreg[, gatvars$myidvar])
        aggvars$IDlist[temp$IDloc] <- townvars$nrid

        # calculate new region ----
        # erroring due to missing or invalid CRS?
        # but individual functions' code works with these inputs
        # GATx, GATy no longer lat/long?
        # weightGATregion fails because nrid not assigned to IDlist

        aggvars$IDlist[aggvars$IDlist %in%
                         data.frame(townvars$newreg)$ID] <- townvars$nrid

        townvars$newregdata <- createGATregion(pop = mapvars$pop,
                                               area = area,
                                               newreg = townvars$newreg,
                                               myidvar = gatvars$myidvar,
                                               nrid = townvars$nrid,
                                               IDlist = aggvars$IDlist,
                                               pwrepeat = pwrepeat,
                                               popwt = gatvars$popwt)

        # add the new region to the list of data about the regions ----
        # aggvars$allpolydata <- sf::st_as_sf(aggvars$allpolydata)
        aggvars$allpolydata <- rbind(aggvars$allpolydata, townvars$newregdata)

        # need to remove the info about the old regions
        aggvars$allpolydata <- aggvars$allpolydata[which(
                      !data.frame(aggvars$allpolydata)[, gatvars$myidvar] %in%
                        data.frame(townvars$newreg)[, gatvars$myidvar]), ]

        # update neighbor listings ----
        townvars$townnb <- aggregateGATnb(nb = townvars$oldtownnb,
                                          ids = aggvars$IDlist)
      }

      # find the minimum population ----
      aggvars$newregno <- aggvars$newregno + 1

      temp$minpop1 <- min(data.frame(aggvars$allpolydata)[which(aggvars$allpolydata$GATflag == 0),
                                              gatvars$aggregator1])
      temp$minpop2 <- min(data.frame(aggvars$allpolydata)[which(aggvars$allpolydata$GATflag == 0),
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
    close(tmb)
  }
  # return ----
  return(aggvars)
}

