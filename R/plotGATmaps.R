#' Plot GAT Maps
#'
#' @description
#' This function draws a choropleth map of a numeric variable. It includes
#' options to display summary statistics and subtitle. The legend produced is
#' dependent on the dataset; if there are no exclusions, no areas below the
#' minimum aggregation value, and no areas with missing rates, their respective
#' entries will be excluded from the legend.
#'
#'
#' @details
#' The map produced by this function is designed to be saved to a recordPlot
#' object for later rendering in a PDF rather than displayed directly.
#' If plotting directly, the display may be distorted. For best results, save
#' to a recordPlot object and write to PDF.
#'
#' @param area       A spatial polygons data frame.
#' @param var        A numeric variable within the spatial polygons data frame.
#' @param clr        The color scheme to use for the map, from RColorBrewer.
#' @param title.main The map's main title as a text string.
#' @param class      The cut points for the choropleth categories. Default is
#'                   NULL, in which case the function calculates breaks from
#'                   the data using the breaks value.
#' @param after      Boolean denoting whether the function is mapping
#'                   the aggregated area "after" map.
#' @param title.sub  The map's subtitle (if relevant) as a text string.
#' @param breaks     Number of categories for the choropleth.
#' @param colcode    Color code predefined for before/after maps.
#' @param mapstats   A boolean denoting whether or not to include summary
#'                   statistics: min, median, and max.
#' @param ratemap    Boolean denoting whether the variable that is to
#'                   be plotted contains decimal values that require rounding.
#'                   In GAT, this applies to the GAT-calculated rate.
#' @param closemap   A boolean to denote whether to close the map window after
#'                   the map is drawn and saved.
#'
#' @examples
#'
#' ## plot population
#' # create a flag variable (used to check for and map exceptions)
#' hftown$GATflag <- 0
#'
#' # plot the population
#' plotGATmaps(area = hftown, var = "TOTAL_POP", clr = "YlGn",
#'   title.main = "Population", breaks = 3, mapstats = TRUE)
#'
#' ## to plot a rate
#' # create a randomized flag variable (data will not match labels here)
#' # 0 = no flag, 1-3 = excluded by user, 5 = above maximum aggregation value,
#' # 10 = below minimum aggregation value
#' hftown$GATflag <-
#'   sample(c(0, 1, 5, 10), nrow(hftown), replace = TRUE,
#'   prob = c(.8, .1, .05, .05))
#'
#' # create a rate
#' hftown$pop_dens <- 10000 * hftown$TOTAL_POP / hftown$AREALAND
#' # add a couple random missings to illustrate uncalculated rates
#' hftown$pop_dens[sample(length(hftown), 2)] <- NA
#'
#' # plot the rate
#' plotGATmaps(area = hftown, var = "pop_dens", clr = "YlGn",
#'   title.main = "population density", breaks = 3,
#'   mapstats = TRUE, ratemap = TRUE)
#'
#' @export

plotGATmaps <- function(area, var, clr = "Blues", title.main = "", class = NULL,
                        after = FALSE, title.sub = NULL, breaks = 5,
                        colcode = NULL, mapstats = FALSE, ratemap = FALSE,
                        closemap = FALSE) {
  # temporary sf conversion
  area <- sf::st_as_sf(area)

  # set map size
  dev.new(noRStudioGD = TRUE, res = 1200, width = 20, height = 14)
  # enable display list
  dev.control('enable')

  # plot shapefile ####
  # calling dev.off at the end will reset par()
  graphics::par(mar=c(2.5,0,2,0), mgp = c(0, 0, 0), xpd = TRUE)
    # margins: bottom, left, top, right
    # axes: label, padding, tick (?)
    # xpd: draw outside margins?

  # sp::plot(area, lwd=.5)
  plot(area$geometry, lwd=.5)
  title(main = title.main, sub = title.sub, cex.main = 2)

  if (is.null(colcode)) {
    if (!ratemap) {
      if (is.null(class)) {
        # plotvar <- area@data[, var]
        plotvar <- data.frame(area)[, var]
        plotvargood <- plotvar[which(is.finite(plotvar))]
        if (length(table(plotvargood)) < breaks) {
          breaks = length(table(plotvargood))
          # myclass <- classInt::classIntervals(area@data[, var], breaks,
          #                                     style = "fixed",
          #                                     fixedBreaks = names(table(plotvargood)),
          #                                     warnSmallN = FALSE)
          myclass <- classInt::classIntervals(
                       plotvar, breaks, style = "fixed",
                       fixedBreaks = as.numeric(names(table(plotvargood))),
                       warnSmallN = FALSE)

        } else {
          myclass <- classInt::classIntervals(plotvargood, breaks,
                                              style = "quantile",
                                              warnSmallN = FALSE)
        }
      } else if (after == TRUE) {
        # maxpop <- max(area@data[, var])
        maxpop <- max(data.frame(area)[, var])
        mybreaks <- c(class$brks, maxpop)
        # set upper limits for maps accordingly, so all data is within some range
        # myclass <- classInt::classIntervals(area@data[, var], breaks,
        #                                     style = "fixed",
        #                                     fixedBreaks = mybreaks,
        #                                     warnSmallN = FALSE)
        myclass <- classInt::classIntervals(
                     data.frame(area)[, var], breaks, style = "fixed",
                     fixedBreaks = mybreaks, warnSmallN = FALSE)
      } else {
        myclass <- class
      }
      # assigns a color to each region
      plotclr <- RColorBrewer::brewer.pal(breaks, clr)
      colcode <- classInt::findColours(myclass, plotclr)
    } else if (ratemap) {
      # plotvar <- area@data[, var]
      plotvar <- data.frame(area)[, var]
      plotgood <- plotvar[which(is.finite(plotvar))] # removes missings
      missingtag <- 10*max(plotgood)
      # area@data$rateplot <- ifelse(!is.finite(plotvar) | is.na(plotvar),
      #                              missingtag, plotvar)
      area$rateplot <- ifelse(!is.finite(plotvar) | is.na(plotvar),
                                   missingtag, plotvar)

      myclass <- classInt::classIntervals(plotgood, breaks,
                                          style = "quantile")

      mybreaks <- c(floor(myclass$brks[1]*100)/100,
                    round(myclass$brks[2:breaks], digits = 2),
                    ceiling(myclass$brks[breaks+1]*100)/100)
      plotclr <- RColorBrewer::brewer.pal(breaks, clr)
      if (sum(!is.finite(plotvar) | is.na(plotvar)) > 0) {
        mybreaks <- c(mybreaks, ceiling(missingtag))
        plotclr <- c(plotclr, "Lavender")
      }

      # myclass <- classInt::classIntervals(area@data$rateplot, style = "fixed",
      #                                     fixedBreaks = mybreaks)
      myclass <- classInt::classIntervals(area$rateplot, style = "fixed",
                                          fixedBreaks = mybreaks)
      colcode <- classInt::findColours(myclass, plotclr)

      if (sum(!is.finite(plotvar) | is.na(plotvar)) > 0) {
        names(attr(colcode, "table"))[breaks+1] <- "Rate cannot be calculated"
        names(attr(colcode, "table"))[breaks] <-
          gsub(",.+)", paste0(", ", ceiling(missingtag*10)/100, "]"),
               names(attr(colcode, "table"))[breaks])
      }
    }
  }

  # sp::plot(area, col = colcode, add = TRUE, lwd=.5)
  plot(area$geometry, col = colcode, add = TRUE, lwd=.5)

  labels = names(attr(colcode, "table"))
  fill = attr(colcode, "palette")
  border = rep("black", length(fill))

  # highlight flagged areas if relevant ----
  # myflags <- subset(area, area@data$GATflag %in% 1:3)
  myflags <- area[area$GATflag %in% 1:3, ]
  if (nrow(myflags) > 0) {
    # sp::plot(myflags, border = "CornflowerBlue", add = TRUE, lwd = 3)
    plot(myflags$geometry, border = "CornflowerBlue", col = "transparent",
         add = TRUE, lwd = 2)
    labels = c(labels, "Excluded by user")
    fill = c(fill, "white")
    border = c(border, "CornflowerBlue")
  }

  # myflags <- subset(area, area@data$GATflag == 10)
  myflags <- area[area$GATflag == 10, ]
  if (nrow(myflags) > 0) {
    # sp::plot(myflags, border = "cyan", add = TRUE, lwd = 3)
    plot(myflags$geometry, border = "cyan", col = "transparent",
         add = TRUE, lwd = 2)
    labels = c(labels, "Below minimum aggregation value")
    fill = c(fill, "white")
    border = c(border, "cyan")
  }

  # myflags <- subset(area, area@data$GATflag == 5)
  myflags <- area[area$GATflag == 5, ]
  if (nrow(myflags) > 0) {
    # sp::plot(myflags, border = "magenta", add = TRUE, lwd = 3)
    plot(myflags$geometry, border = "magenta", col = "transparent",
         add = TRUE, lwd = 2)
    labels = c(labels, "Above maximum aggregation value")
    fill = c(fill, "white")
    border = c(border, "magenta")
  }

  legend("topleft", legend = labels, fill = fill, border = border, cex = 1,
         bty = "n", inset = 0, y.intersp = 1.25)

  # calculate summary statistics if mapstats = TRUE ####
  if (mapstats) {
    # statsvar <- area@data[, var]
    statsvar <- data.frame(area)[, var]
    statsgood <- statsvar[which(is.finite(statsvar))]
    if (ratemap) {
      min <- round(min(statsgood), digits = 2)
      max <- round(max(statsgood), digits = 2)
      med <- round(stats::median(statsgood), digits = 2)
    } else {
      min <- min(statsgood)
      max <- max(statsgood)
      med <- round(stats::median(statsgood))
    }
    numformat <- function(num) {
      format(as.numeric(gsub(",", "", num)), big.mark=",", scientific=FALSE)
    }

    stats <- paste("Summary stats for \n", var, ":     \n",
                   "Minimum:", numformat(min), "\n",
                   "Median:", numformat(med), "\n",
                   "Maximum:", numformat(max), "\n")

    # get latitude/longitude limits from shapefile
    # extent <- sp::bbox(area)
    extent <- sf::st_bbox(area)
    # if (extent["x", "max"] - extent["x", "min"] <
    #     (extent["y", "max"] - extent["y", "min"])) {
    #   xbuffer <- extent["x", "max"] + (extent["x", "max"] - extent["x", "min"]) / 2
    # } else if (extent["x", "max"] - extent["x", "min"] <
    #            (extent["y", "max"] - extent["y", "min"]) * 3/2) {
    #   xbuffer <- extent["x", "max"] + (extent["x", "max"] - extent["x", "min"]) / 4
    # } else {
    #   xbuffer <- extent["x", "max"]
    # }
    if (extent$xmax - extent$xmin < (extent$ymax - extent$ymin)) {
      xbuffer <- extent$xmax + (extent$xmax - extent$xmin) / 2
    } else if (extent$xmax - extent$xmin < (extent$ymax - extent$ymin) * 3/2) {
      xbuffer <- extent$xmax + (extent$xmax - extent$xmin) / 4
    } else {
      xbuffer <- extent$xmax
    }

    # graphics::text(xbuffer, extent["y", "max"], labels = stats, adj = c(1, 1))
    graphics::text(xbuffer, extent$ymax, labels = stats, adj = c(1, 1))
    # play with "pos" a bit; there has to be a more efficient method
  }

  # add scale and arrow ####
  # only include this last part if prettymapr is installed
  if (requireNamespace("prettymapr", quietly = TRUE)) {
    suppressMessages(
      prettymapr::addnortharrow(pos = "bottomleft", padin = c(0.2, 0.05),
                                scale = .5, lwd = 1, border = "black",
                                cols = c("white", "black"), text.col = "black")
    )
    suppressMessages(
      prettymapr::addscalebar(plotunit = "mi", plotepsg = 4269, widthhint = 0.25,
                              unitcategory = "imperial", htin = 0.1, lwd = 1,
                              padin = c(0.7, 0.05), style = "ticks",
                              linecol = "black", tick.cex = 0.7,
                              labelpadin = 0.08, label.cex = 0.8,
                              label.col = "black", pos = "bottomleft")
    )
  }

  # save map ####
  map <- recordPlot()

  graphics::par(mar=c(5,4,4,2)+.1, mgp = c(3, 1, 0))
    # default bottom, left, top, right

  # reset
  if (closemap) {
    dev.off()
  }
  return(map)
}
