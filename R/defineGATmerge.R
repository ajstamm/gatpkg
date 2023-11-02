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
#'   minvalue1 = 5000,
#'   maxvalue1 = 15000,
#'   aggregator2 = "TOTAL_POP",  # numeric variable
#'   minvalue2 = 5000,
#'   maxvalue2 = 15000,
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
#'   var1 = "TOTAL_POP", math1 = "less than", val1 = 200,
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
# progressbar = FALSE; pop = NULL

defineGATmerge <- function(area, gatvars, mergevars, exclist = NULL,
                           pwrepeat = FALSE, adjacent = TRUE, pop = NULL,
                           minfirst = FALSE, progressbar = TRUE) {
  # sf conversion ----
  area <- sf::st_as_sf(area)
  data <- data.frame(area)
  sf::st_agr(area) <- "constant"
  if (!is.null(pop)) sf::st_agr(pop) <- "constant"
  area <- sf::st_make_valid(area, dist=0)

  min1 <- as.numeric(gsub(",", "", gatvars$minvalue1))
  max1 <- as.numeric(gsub(",", "", gatvars$maxvalue1))
  if (length(max1) == 0) max1 <- max(data.frame(area)[, gatvars$aggregator1])
  if (is.na(max1)) max1 <- max(data.frame(area)[, gatvars$aggregator1])
  if (gatvars$aggregator2 == "NONE") {
    gatvars$aggregator2 <- gatvars$aggregator1
    gatvars$minvalue2 <- gatvars$minvalue1
    gatvars$maxvalue2 <- gatvars$maxvalue1
  }
  max2 <- as.numeric(gsub(",", "", gatvars$maxvalue2))
  if (length(max2) == 0) max2 <- max(data.frame(area)[, gatvars$aggregator2])
  if (is.na(max2)) max2 <- max(data.frame(area)[, gatvars$aggregator2])
  min2 <- as.numeric(gsub(",", "", gatvars$minvalue2))
  if (length(min2) == 0) min2 <- min(data.frame(area)[, gatvars$aggregator2])
  if (is.na(min2)) min2 <- min(data.frame(area)[, gatvars$aggregator2])

  if (!"GATflag" %in% names(data)) {
    data$GATflag <- 0
    if (!is.null(exclist)) data$GATflag <- calculateGATflag(exclist, d = data)
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
  area$GATid <- data.frame(area)[, gatvars$myidvar]
  row.names(area) <- area$GATid

  # warnings ----
  # capitol letter = TRUE, lower case = FALSE
  # a = adjacent, b = boundatr, m = min, f = found
  # rethink warnings to what was found
  warnings <- c(
    # rigidbound
    fAMB = "", # desired, never used
    fAmB = "", # acceptable, never used
    faMB = paste("Found areas in the same boundary below the minimum value,",
                 "but they are not physically adjacent."),
    faB = paste("Found areas in the same boundary,",
                "but they are not physically adjacent."),
    fAMb = paste("Found physically adjacent areas below the minimum value,",
                 "but they are not in the same boundary."),
    fAb = paste("Found physically adjacent areas,",
                "but they are not in the same boundary."),
    faMb = paste("Found areas to merge below the minimum value,",
                 "but they are not physically adjacent or in the same boundary."),
    fab = paste("Found areas to merge, but they are not physically adjacent",
                "or in the same boundary."),
    faM = paste("Found areas to merge below the minimum value,",
                "but they are not physically adjacent."),
    fa = paste("Found areas to merge, but they are not physically adjacent",
               "or in the same boundary."),
    na = "No areas found to merge. This area cannot be merged further."
  )
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
    sf::st_agr(area) <- "constant"
    j <- sf::st_centroid(area)
    sf::st_agr(j) <- "constant"
    sf::st_geometry(j) <- sf::st_centroid(j$geometry)
    pts <- do.call(rbind, sf::st_geometry(j))
    mapvars$centroids <- data.frame(pts)
    colnames(mapvars$centroids) <- c("GATx", "GATy")
    mapvars$pop <- NULL
  }

  # default to not lat/long if something goes wrong
  if (is.na(mapvars$projection)) mapvars$projection <- FALSE

  # add centroids to polygon data ----
  aggvars <- list(IDlist = data.frame(area)$GATid, newregno = 1, logmsg = "",
                  shp = cbind(area, mapvars$centroids))
  as <- data.frame(aggvars$shp)

  # set up temporary variables ----
  aggvars$shp[, gatvars$aggregator1] <-
    as.numeric(as.character(data.frame(aggvars$shp)[, gatvars$aggregator1]))
  aggvars$shp[, gatvars$aggregator2] <-
    as.numeric(as.character(data.frame(aggvars$shp)[, gatvars$aggregator2]))

  temp <- list(aggdata = aggvars$shp[which(aggvars$shp$GATflag == 0), ],
               digits = nchar(nrow(aggvars$shp)),
               index = sapply(data.frame(aggvars$shp), is.integer),
               rownames = data.frame(area)$GATid)
  a <- data.frame(temp$aggdata)

  # set up loop ----
  if (nrow(temp$aggdata) > 0) {
    # set up more temporary variables ----
    temp$minpop1 = min(data.frame(temp$aggdata)[, gatvars$aggregator1])
    temp$minpop2 = min(data.frame(temp$aggdata)[, gatvars$aggregator2])

    myids <- as.character(unlist(data$GATid))
    myids <- myids[grepl("GATid", myids)]
    myids <- gsub("GATid_", "",  myids)

    if (length(myids) == 0) {
      maxid <- 0
    } else {
      maxid <- max(as.numeric(myids))
      temp$digits <- nchar(myids[1])
    }

    # start while loop ----
    while (temp['minpop1'] < min1 | temp['minpop2'] < min2){
      # identify who can merge ----
      # remove flagged areas
      as <- data.frame(aggvars$shp)
      temp$aggdata <- aggvars$shp[which(as$GATflag == 0), ]
      temp$aggdata <- temp$aggdata[which(!is.na(temp$aggdata$GATid)), ]
      a <- data.frame(temp$aggdata)

      # isolate areas that are below minimum
      temp$tobemerged <- temp$aggdata[which(
        a[, gatvars$aggregator1] < min1 |
        a[, gatvars$aggregator2] < min2 ), ]
      t <- data.frame(temp$tobemerged)

      # change the merge order high to low
      temp$tobemerged <-
        if (temp$minpop1 < min1) {
          temp$tobemerged[order(-t[, gatvars$aggregator1]), ]
        } else {
          temp$tobemerged[order(-t[, gatvars$aggregator2]), ]
        }
      t <- data.frame(temp$tobemerged)

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
      f <- data.frame(temp$first)
      temp$aggdata <- temp$aggdata[which(!temp$aggdata$GATid == temp$first$GATid), ]

      # remove areas that are too large
      temp$aggdata <- temp$aggdata[which(
        (a[, gatvars$aggregator1] + f[, gatvars$aggregator1] < max1) |
        (a[, gatvars$aggregator2] + f[, gatvars$aggregator2] < max2)), ]
      temp$aggdata <- temp$aggdata[which(!is.na(temp$aggdata$GATid)), ]
      a <- data.frame(temp$aggdata)


      # set up warnings ----
      temp$logmsg <- "" # start with empty log
      temp$warnkey <- "" # no warnings

      # find neighbors ----
      # temporary flag: neighbors found?
      temp$idfail <- TRUE
      temp$island <- FALSE

      # some fails - try tiny buffer
      # temp$first <- sf::st_buffer(temp$first, dist = 0)
      # temp$aggdata <- sf::st_buffer(temp$aggdata, dist = 0)

      # rooks <- lengths(sf::st_relate(temp$aggdata, temp$first,
      #                                pattern = "F***1****")) == 1
      # if (sum(rooks) > 0) {
      #   temp$towns <- temp$aggdata[rooks, ]
      # } else {
        rooks <- lengths(sf::st_relate(temp$aggdata, temp$first,
                                       pattern = "****1****")) == 1
        if (sum(rooks) > 0) {
          temp$towns <- temp$aggdata[rooks, ]
        } else {
          temp$towns <- data.frame()
        }
      # }
      w <- data.frame(temp$towns)

      # get the data about these neighbors ----
      if (gatvars$boundary != "NONE") {
        # if boundary variable
        if (nrow(temp$towns) > 0) {
          temp$townbound <- temp$towns[which(
            w[, gatvars$boundary] == as.character(f[, gatvars$boundary])), ]
          b <- data.frame(temp$townbound)
          if (minfirst) {
            temp$nbdata <-
              temp$townbound[which(b[, gatvars$aggregator1] < min1 |
                                   b[, gatvars$aggregator2] < min2), ]
            temp$idfail <- if (nrow(temp$nbdata) > 0) FALSE else TRUE
          } # in boundary, adjacent, below min
          if (temp$idfail) { # assumes adjacent
            temp$nbdata <- temp$townbound
            temp$idfail <- if (nrow(temp$nbdata) > 0) FALSE else TRUE
          } # in boundary, adjacent
        } # in boundary, adjacent, maybe below min
        if (temp$idfail & !adjacent) {
          # if no neighbors, find other area in boundary
          temp$townbound <- temp$aggdata[which(
            a[, gatvars$boundary] == temp$boundary &
            a$GATid != f$GATid), ]
          b <- data.frame(temp$townbound)
          if (minfirst) { # below minimum preferred?
            temp$nbdata <-
              temp$townbound[which(b[, gatvars$aggregator1] < min1 |
                                   b[, gatvars$aggregator2] < min2), ]
            if (nrow(temp$nbdata) > 0) {
              temp$warnkey <- "faMB"
              temp$idfail <- FALSE
            } else {
              temp$warnkey <- "mB" # no below min within boundary
              temp$idfail <- TRUE
            }
          } # in boundary, below min
          if (temp$idfail) {
            temp$nbdata <-
              temp$townbound
            mergevars$mergeopt2 <- "closest"
            if (nrow(temp$nbdata) > 0) {
              temp$idfail <- FALSE
            } else {
              temp$idfail <- TRUE
              temp$warnkey <- "B"
            }
          } # in boundary
        } # in boundary, not adjacent, maybe below min
      } # in boundary, maybe adjacent, below min
      if (temp$idfail & !gatvars$rigidbound) {
        # if boundary not enforced, adjacent
        if (sum(rooks) > 0) {
          temp$towns <- temp$aggdata[rooks, ]
        } else {
          temp$towns <- data.frame()
        }
        w <- data.frame(temp$towns)
        if (minfirst) { # assume adjacent
          # if merge below minimum first
          temp$nbdata <-
            temp$towns[which(w[, gatvars$aggregator1] < min1 |
                             w[, gatvars$aggregator2] < min2), ]
          temp$idfail <- if (nrow(temp$nbdata) > 0) FALSE else TRUE
          if (gatvars$boundary != "NONE" & !temp$idfail) temp$warnkey <- "fAMb"
        } # adjacent, below min
        if (temp$idfail) { # check among adjacent
          temp$nbdata <- temp$towns
          temp$idfail <- if (nrow(temp$nbdata) > 0) FALSE else TRUE
          if (gatvars$boundary != "NONE" & !temp$idfail) temp$warnkey <- "fAb"
        } # adjacent
      } # adjacent, maybe below min
      if (temp$idfail & !adjacent) { # only if not adjacent allowed
        temp$island <- TRUE
        mergevars$mergeopt2 <- "closest"
        if (minfirst) {
          temp$nbdata <-
            temp$aggdata[which(a[, gatvars$aggregator1] < min1 |
                               a[, gatvars$aggregator2] < min2), ]
          temp$idfail <- if (nrow(temp$nbdata) > 0) FALSE else TRUE
          temp$warnkey <- if (gatvars$boundary != "NONE" & !temp$idfail)
            "faMb" else "faM"
        } # below min
        if (temp$idfail) {
          temp$nbdata <- temp$aggdata
          temp$idfail <- if (nrow(temp$nbdata) > 0) FALSE else TRUE
          temp$warnkey <- if (gatvars$boundary != "NONE" & !temp$idfail)
            "fab" else "fa"
        } # any area
      } # maybe below min
      if (temp$idfail) {
        temp$warnkey <- "na"
        aggvars$shp$GATflag[as$GATid == f$GATid] <- 10
      } # no merge

      # quit searching for neighbors ----
      if (!temp$idfail) {
        # rank centroid distances ----
        temp$nborder <- rankGATdistance(area = area, nbdata = temp$nbdata,
                        first = temp$first, mergevars = mergevars,
                        gatvars = gatvars)

        # data that will be combined to form new region ----
        temp$nbdata <- sf::st_transform(temp$nbdata, sf::st_crs(area))
        t <- rbind(temp$first, temp$nbdata[temp$nborder[1], ])
        temp$newreg <- sf::st_make_valid(t) # fix invalid areas

        n <- data.frame(temp$newreg)

        # add leading zeroes based on maximum number of areas ----
        temp$nrid <- paste0("GATid_", formatC(aggvars$newregno + maxid,
                            width = temp$digits, format = "d", flag = "0"))
        aggvars$IDlist[which(aggvars$IDlist %in% n$GATid)] <- temp$nrid

        # calculate new region ----
        aggvars$IDlist[aggvars$IDlist %in% n$ID] <- temp$nrid
        temp$newregdata <- createGATregion(pop = mapvars$pop, area = area,
                               newreg = temp$newreg, myidvar = "GATid",
                               nrid = temp$nrid, IDlist = aggvars$IDlist,
                               pwrepeat = pwrepeat, popwt = gatvars$popwt)

        # add new region to layer of merged regions ----
        aggvars$shp <- rbind(aggvars$shp, temp$newregdata)
        as <- data.frame(aggvars$shp)

        # need to remove the old regions
        aggvars$shp <- aggvars$shp[which(!as$GATid %in% n$GATid), ]
        as <- data.frame(aggvars$shp)
      }
      # find the minimum population ----
      aggvars$newregno <- aggvars$newregno + 1
      temp$minpop1 <- min(as[which(as$GATflag == 0), gatvars$aggregator1])
      temp$minpop2 <- min(as[which(as$GATflag == 0), gatvars$aggregator2])

      # 'garbage collection': free up memory ----
      # remove flagged areas
      as <- data.frame(aggvars$shp)
      temp$aggdata <- aggvars$shp[which(as$GATflag == 0), ]
      a <- data.frame(temp$aggdata)

      if (!temp$warnkey == "") {
        aggvars$logmsg <- paste(aggvars$logmsg,
                                paste0("Merge ", aggvars$newregno + maxid,
                                       " (", f$GATid, "):"),
                                warnings[temp$warnkey], "\n")
      } # write warning to log
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
                        type = "ok", icon = "info")
  }
  # close progress bar that monitors the aggregation ----
  if (progressbar) close(definenv$tmb)
  # return ----
  return(aggvars)
}

