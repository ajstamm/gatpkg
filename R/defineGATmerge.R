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
#' @param area      A spatial polygons data frame.
#' @param gatvars   A list of objects created by the GAT tool. It contains
#'                  the strings myidvar, aggregator1, aggregator2, and
#'                  boundary, which are all variables in the area, the
#'                  boolean popwt, and the numbers minvalue1 and minvalue2.
#'                  Both aggregator1 and aggregator2 must be numeric and
#'                  myidvar must contain unique values.
#' @param mergevars A list of string objects needed to aggregate the areas
#'                  in the GAT tool. It contains mergeopt1, similar1, and
#'                  similar2. The valid options for mergeopt1 are "closest",
#'                  "least", and "similar". If "similar" is selected, similar1
#'                  and similar2 must be numeric variables in the area and
#'                  similar2 cannot equal zero.
#' @param filevars  A list of string objects that list file names and paths.
#' @param pwrepeat  A boolean denoting whether population weighting (if used)
#'                  should be recalculated each time two areas are merged
#'                  (TRUE) or if area centroids should be weighted with area
#'                  populations (FALSE). If population weighting is not used,
#'                  this option is ignored.
#' @param adjacent  A boolean denoting whether to force GAT to merge only
#'                  adjacent areas.
#' @param minfirst  A boolean denoting whether or not to select the most
#'                  desirable neighbor only from among the neighbors that
#'                  have values below the desired minimum. If no neighbors
#'                  are below the desired minimum, the most desirable of all
#'                  elligible neighbors is selected.
#'
#' @examples
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
#' my_merge <-
#'   defineGATmerge(
#'     area = hftown,
#'     gatvars = gatvars,
#'     mergevars = mergevars,
#'     filevars = filevars,
#'     pwrepeat = FALSE # don't need pwrepeat if popwt = FALSE
#'   )
#'
#' @export

defineGATmerge <- function(area, gatvars, mergevars, filevars, pwrepeat = FALSE,
                           adjacent = TRUE, minfirst = FALSE) {
  # temp until it's programmed in ####
  if (!"GATflag" %in% names(area@data)) {
    area@data$GATflag <- 0 # for non-default uses of this function
  }
  if (gatvars$aggregator2 == "NONE") {
    gatvars$aggregator2 <- gatvars$aggregator1
  }
  max1 <- as.numeric(gsub(",", "", gatvars$maxvalue1))
  min1 <- as.numeric(gsub(",", "", gatvars$minvalue1))
  max2 <- as.numeric(gsub(",", "", gatvars$maxvalue2))
  min2 <- as.numeric(gsub(",", "", gatvars$minvalue2))

  # draw progress bar ####
  mb <- list(label = "Preparing merge files. Please wait.",
             title = "NYSDOH GAT: merging")
  tmb <- tcltk::tkProgressBar(title = mb$title, label = mb$label, min = 0,
                              max = nrow(area@data), initial = 0, width = 400)

  # if projection is lat/lon, projection = TRUE, otherwise FALSE
  mapvars <- list(projection = grepl("longlat",
                                     sp::proj4string(area), fixed = TRUE))

  # set up centroids ####
  # find a way to not hit memory limit for population weighting
  if (gatvars$popwt) {
    # may need to assign ID as row names
    mb$label = "Loading population file. This step may be slow."
    tcltk::setTkProgressBar(tmb, value = 0, title = mb$title,
                            label = mb$label)

    temp <- weightGATmap(area = area, popvar = gatvars$popvar,
                         filevars = filevars, idvar = gatvars$myidvar)
    mapvars$centroids <- temp$centroids
    mapvars$pop <- temp$pop
    # area@data$GATpop <- mapvars$centroids$GATpop
  } else {
    mapvars$centroids <- sp::coordinates(area)
    colnames(mapvars$centroids) <- c("GATx", "GATy")
    mapvars$pop <- NULL
  }

  # default to not lat/long if something goes wrong
  if (is.na(mapvars$projection)) mapvars$projection <- FALSE

  # add centroids to polygon data ####
  aggvars <- list(IDlist = as.character(area@data[, gatvars$myidvar]),
                  allpolydata = data.frame(area@data, mapvars$centroids),
                  newregno = 1,
                  logmsg = "") # record merge warnings in the log)

  # set up temporary variables ####
  if (gatvars$aggregator2 == "NONE") gatvars$aggregator2 <- gatvars$aggregator1
  # for some reason, numeric sometimes switched to character
  aggvars$allpolydata[, gatvars$aggregator1] <-
    as.numeric(as.character(aggvars$allpolydata[, gatvars$aggregator1]))
  aggvars$allpolydata[, gatvars$aggregator2] <-
    as.numeric(as.character(aggvars$allpolydata[, gatvars$aggregator2]))

  temp <- list(alldata = aggvars$allpolydata[which(aggvars$allpolydata$GATflag == 0), ],
               digits = nchar(nrow(aggvars$allpolydata)),
               index = sapply(aggvars$allpolydata, is.integer),
               rownames = area@data[, gatvars$myidvar])

  # test if loop can be run ####
  if (nrow(temp$alldata) > 0) {
    # set up more temporary variables ####
    temp$minpop1 = min(temp$alldata[, gatvars$aggregator1])
    temp$minpop2 = min(temp$alldata[, gatvars$aggregator2])

    myids <- as.character(unlist(data.frame(area@data[, gatvars$myidvar])))
    myids <- myids[grepl("GATid", myids)]
    myids <- gsub("GATid_", "",  myids)

    if (length(myids) == 0) {
      maxid <- 0
    } else {
      maxid <- max(as.numeric(myids))
      temp$digits <- nchar(myids[1])
    }

    # set up town variables ####
    # get list of neighbors using poly2nb method from spdep packagetow
    townvars <- list(oldtownnb = spdep::poly2nb(area, queen = FALSE,
                                                row.names = temp$rownames),
                     townnb = spdep::poly2nb(area, queen = FALSE,
                                             row.names = temp$rownames))

    # convert integers to double (change to convert when I read in file?)
    # might be redundant now, but check later
    aggvars$allpolydata[, temp$index] <-
      sapply(aggvars$allpolydata[, temp$index], as.numeric)

    # start while loop ####
    while ((temp$minpop1 < gatvars$minvalue1) |
           (temp$minpop2 < gatvars$minvalue2)){
      # identify who is mergable ####
      # remove flagged areas
      temp$aggdata <-
        aggvars$allpolydata[which(aggvars$allpolydata$GATflag == 0), ]

      # isolate areas that are too small
      temp$tobemerged <- temp$aggdata[which(
        (temp$aggdata[, gatvars$aggregator1] < min1) |
        (temp$aggdata[, gatvars$aggregator2] < min2) ), ]

      # change the merge order high to low
      if (temp$minpop1 < min1) {
        temp$tobemerged <-
          temp$tobemerged[order(-temp$tobemerged[, gatvars$aggregator1]), ]
      } else {
        temp$tobemerged <-
          temp$tobemerged[order(-temp$tobemerged[, gatvars$aggregator2]), ]
      }

      # default merge option is the one selected
      mergevars$mergeopt2 <- mergevars$mergeopt1

      # incremental progress bar ####
      mb$label <- paste0("Merge ", aggvars$newregno + maxid, ": ",
                         nrow(temp$tobemerged), " areas remaining.")
      step <- nrow(area@data) - nrow(temp$tobemerged)
      tcltk::setTkProgressBar(tmb, value = step, title = mb$title,
                              label = mb$label)

      # identify the area to merge this loop ####
      temp$first <- identifyGATfirstobs(tobemerged = temp$tobemerged,
                                        aggvar = gatvars$aggregator1,
                                        aggvar2 = gatvars$aggregator2,
                                        minval = gatvars$minvalue1,
                                        minval2 = gatvars$minvalue2)

      # remove areas that are too large
      temp$aggdata <- temp$aggdata[which(
        (temp$aggdata[, gatvars$aggregator1] +
           temp$first[, gatvars$aggregator1] < max1) |
          (temp$aggdata[, gatvars$aggregator2] +
             temp$first[, gatvars$aggregator2] < max2)), ]


      # set up warnings ####
      temp$logmsg <- paste0("Merge ", aggvars$newregno + maxid, " (",
                            temp$first[, gatvars$myidvar], "):")
      temp$warnkey <- "n" # no warnings
      warnings <- c(
        ab = "No physically adjacent neighbors found within the same boundary.",
        amb = "No physically adjacent neighbors below the minimum value found within the same boundary.",
        mb = "Found areas in the same boundary below the minimum value, but they are not physically adjacent.",
        b = "Found areas in the same boundary, but they are not physically adjacent.",
        f = "No neighbors found. This area cannot be merged further.",
        nb = "No neighbors found in boundary.",
        am = "No physically adjacent neighbors found below the minimum aggregation value."
      )

      # find neighbors ####
      # temporary flag: neighbors found?
      temp$idfail <- TRUE

      townvars$townnbid <- attr(townvars$townnb, "region.id")
      if (temp$first[, gatvars$myidvar] %in% townvars$townnbid) {
        townvars$townnbidloc <- which(townvars$townnbid ==
                                      temp$first[, gatvars$myidvar])
        townvars$neighbors <- townvars$townnb[[townvars$townnbidloc]]
        townvars$neighborid <- townvars$townnbid[townvars$neighbors]
        townvars$nbdata <-
          temp$aggdata[which(temp$aggdata[, gatvars$myidvar] %in%
                             townvars$neighborid), ]
      }

      # get the data about these neighbors ####
      # if boundary variable
      if (gatvars$boundary != "NONE") {
        temp$firstboundary <- as.character(temp$first[, gatvars$boundary])
        # index of neighbors in same county
        temp$inboundary <- townvars$nbdata[which(
            townvars$nbdata[, gatvars$boundary] == temp$firstboundary), ]
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
        if (temp$idfail) {
          townvars$nbdata <-
            temp$inboundary[which(temp$inboundary[, gatvars$myidvar] %in%
                                 townvars$neighborid), ]
          # index of neighbors in same county
          temp$inco_dex <- which(townvars$nbdata[, gatvars$boundary] ==
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

        # if no neighbors, find other area in boundary
        if (!adjacent & temp$idfail) {
          # below minimum preferred?
          if (minfirst) {
            temp$inco_nbdata <-
              temp$tobemerged[which(temp$tobemerged[, gatvars$boundary] ==
                                    temp$firstboundary &
                                    temp$tobemerged[, gatvars$myidvar] !=
                                    temp$first[, gatvars$myidvar]), ]
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
              temp$aggdata[which(temp$aggdata[, gatvars$boundary] ==
                                      temp$firstboundary &
                                      temp$aggdata[, gatvars$myidvar] !=
                                      temp$first[, gatvars$myidvar]), ]
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
        if (temp$idfail) {
          temp$warnkey <- "nb"
        }
      }

      # if no boundary or none in boundary (and boundary not enforced)
      if (minfirst & temp$idfail & !gatvars$rigidbound) {
        # reset town list, just in case
        townvars$nbdata <-
          temp$tobemerged[which(temp$tobemerged[, gatvars$myidvar] %in%
                                townvars$neighborid), ]
        if (nrow(townvars$nbdata) > 0) {
          temp$idfail <- FALSE # found neighbor
        } else {
          temp$warnkey <- "am" # no adjacent below minimum
          temp$idfail <- TRUE # still failed
        }
      }

      # if no boundary or minimum enforcement (or none available)
      if (!adjacent & temp$idfail & !gatvars$rigidbound) {
        # reset town list, just in case
        townvars$nbdata <-
          townvars$nbdata[which(temp$aggdata[, gatvars$myidvar] %in%
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
          mergevars$mergeopt2 <- "closest"
          townvars$townnbidloc <- which(townvars$townnbid ==
                                        temp$first[, gatvars$myidvar])
          townvars$neighbors <- townvars$townnb[[townvars$townnbidloc]]
          townvars$neighborid <- townvars$townnbid[townvars$neighbors]
          townvars$nbdata <-
            temp$aggdata[which(temp$aggdata[, gatvars$myidvar] %in%
                                 townvars$neighborid), ]
          if (nrow(townvars$nbdata) > 0) {
            temp$idfail <- FALSE # found neighbor
          } else {
            temp$idfail <- TRUE # still failed
          }
        }
      }

      # quit searching for neighbors ####
      if(temp$idfail) {
        # check for and remove flags and oversized at beginning of loop
        aggvars$allpolydata$GATflag[aggvars$allpolydata[, gatvars$myidvar] ==
                                    temp$first[, gatvars$myidvar]] <- 10
        temp$warnkey <- "f"
      } else {
        # rank centroid distances ####
        townvars$nborder <- rankGATdistance(area = area,
                                            nbdata = townvars$nbdata,
                                            first = temp$first,
                                            gatvars = gatvars,
                                            mergevars = mergevars)

        # data which will be combined to form new region ####
        townvars$newreg <- rbind(temp$first,
                                 townvars$nbdata[townvars$nborder[1], ])

        # add leading zeroes based on maximum number of areas
        temp$zero <- paste(rep("0", temp$digits -
                               nchar(aggvars$newregno + maxid)),
                           collapse = "")
        townvars$nrid <- paste0("GATid_", temp$zero,
                                as.character(aggvars$newregno + maxid))

        temp$IDloc <- which(aggvars$IDlist %in% townvars$newreg[, gatvars$myidvar])
        aggvars$IDlist[temp$IDloc] <- townvars$nrid

        # calculate new region ####
        townvars$newregdata <- createGATregion(mydata = aggvars$allpolydata,
                                               newreg = townvars$newreg,
                                               myidvar = gatvars$myidvar,
                                               nrid = townvars$nrid,
                                               area = area, pop = mapvars$pop,
                                               IDlist = aggvars$IDlist,
                                               pwrepeat = pwrepeat,
                                               popwt = gatvars$popwt)

        # add the new region to the list of data about the regions ####
        aggvars$allpolydata <- rbind(aggvars$allpolydata, townvars$newregdata)

        # need to remove the info about the old regions
        aggvars$allpolydata <-
          aggvars$allpolydata[which(!aggvars$allpolydata[, gatvars$myidvar] %in%
                                      townvars$newreg[, gatvars$myidvar]), ]

        # use spdep::aggregate.nb to create new object listing neighbors of aggregate
        townvars$townnb <- spdep::aggregate.nb(townvars$oldtownnb, aggvars$IDlist)
        aggvars$newregno <- aggvars$newregno + 1
      }

      # find the minimum population ####
      temp$minpop1 <- min(aggvars$allpolydata[which(aggvars$allpolydata$GATflag == 0),
                                              gatvars$aggregator1])
      temp$minpop2 <- min(aggvars$allpolydata[which(aggvars$allpolydata$GATflag == 0),
                                              gatvars$aggregator2])

      if (temp$warnkey != "n") {
        aggvars$logmsg <- paste(aggvars$logmsg, temp$logmsg,
                                warnings[temp$warnkey], "\n")
      }
      # 'garbage collection': free up memory ####
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
  # if function is isolated, close progress bar that monitors the aggregation
  close(tmb)
  return(aggvars)
}

