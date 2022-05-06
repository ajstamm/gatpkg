#' Plot GAT Comparison Map
#'
#' @description
#' This function draws two maps on top of each other with only the polygon
#' boundaries visible, so that polygon sizes and compositions can be compared.
#' It is designed to be saved to a recordPlot object for later rendering in a
#' PDF rather than displayed directly.
#'
#' @details
#' If plotting directly, the display may be distorted. For best results, save
#' to a recordPlot object and write to PDF.
#'
#' @param areaold   A spatial polygons data frame.
#' @param areanew   A second spatial polygons data frame that ahould have the
#'                  same outer boundary as the first one.
#' @param mergevars A list of settings for the aggregation, including type of
#'                  aggregation (mergeopt1) and, if relevant, the variables
#'                  to compare, similar1 and similar2.
#' @param gatvars   A list of objects created by the GAT tool. It contains the
#'                  strings aggregator1 and aggregator2, which are numeric
#'                  variables in the area, and the numbers minvalue1 and
#'                  minvalue2.
#' @param closemap  A boolean to denote whether to close the map window after
#'                  the map is drawn and saved.
#'
#' @examples
#' # define mapping variables
#' gatvars <- list(
#'   myidvar = "ID",             # character variable of unique values
#'   aggregator1 = "TOTAL_POP",  # numeric variable
#'   aggregator2 = "TOTAL_POP",  # numeric variable
#'   minvalue1 = 5000, minvalue2 = 5000,
#'   maxvalue1 = 15000, maxvalue2 = 15000,
#'   boundary = "COUNTY"         # character variable of non-unique values
#' )
#'
#' # define merge type
#' mergevars <- list(
#'   mergeopt1 = "similar",    # can be similar, closest, or least
#'   similar1 = "AREAWATR",       # numeric variable
#'   similar2 = "AREALAND",       # numeric variable without any zeros
#'   centroid = "geographic"
#' )
#'
#' # draw the map
#' plotGATcompare(areaold = hftown, areanew = hfagg610k,
#'                mergevars = mergevars, gatvars = gatvars)
#'
#' @export

# for arrow and scale bar, see:
# https://cran.r-project.org/web/packages/prettymapr/prettymapr.pdf
# NAD83 = epsg:4269; WGS84, GRS80 = epsg:42310; NAD83, GRS80 = epsg:7019,
# but function doesn't recognize them
plotGATcompare <- function(areaold, areanew, mergevars, gatvars,
                           closemap = FALSE) {
  # temporary sf conversion
  areaold <- sf::st_as_sf(areaold)
  areanew <- sf::st_as_sf(areanew)

  # function to handle numbers
  numformat <- function(num) {
    format(as.numeric(gsub(",", "", num)), big.mark=",", scientific=FALSE)
  }

  # set map size
  grDevices::dev.new(noRStudioGD = TRUE, res = 1200, width = 20, height = 14)
  # enable display list
  grDevices::dev.control('enable')
  # plot shapefiles ####
  graphics::par(mar = c(3.5,0,2,0), mgp = c(0,0,0)) # bottom, left, top, right

  plot(areaold$geometry, border = "red", col = "transparent",
           lty = "solid", lwd = 1)
  plot(areanew$geometry, border = "black", col = "transparent",
           lty = "solid", lwd = 2, add = TRUE)

  legend("topleft", legend = c("Original areas", "Aggregated areas"),
         fill = "White", border = c("red", "black"), cex = 1,
         bty = "n", inset = 0, y.intersp = 1.25)

  # add labels ####
  mytitle <- "Map comparing original and aggregated areas"
  mysub <- paste0("Merge type: ", mergevars$mergeopt1)
  if (mergevars$mergeopt1 == "similar") {
    mysub <- paste0(mysub, " ratio between ", mergevars$similar1, " and ",
                    mergevars$similar2)
  } else if (mergevars$mergeopt1 == "closest") {
    mysub <- paste(mysub, mergevars$centroid, "centroid")
  }
  mysub <- paste0(mysub, "\nMerged variable")
  if (gatvars$aggregator1 == gatvars$aggregator2) {
    mysub <- paste0(mysub, ": ", numformat(gatvars$minvalue1), " to ",
                    numformat(gatvars$maxvalue1), " ", gatvars$aggregator1)
  } else {
    mysub <- paste0(mysub, "s: ",
                    numformat(gatvars$minvalue1), " to ",
                    numformat(gatvars$maxvalue1), " ", gatvars$aggregator1, "; ",
                    numformat(gatvars$minvalue2), " to ",
                    numformat(gatvars$maxvalue2), " ", gatvars$aggregator2)
  }
  graphics::title(mytitle, sub = mysub, cex.main = 2)
  raster::scalebar(20, type='bar', divs=5)

  # draw arrow and scale bar ####
  if (requireNamespace("prettymapr", quietly = TRUE)) {
    prettymapr::addnortharrow(pos = "bottomleft", padin = c(0.2, 0.05),
                              scale = .5, lwd = 1, border = "black",
                              cols = c("white", "black"), text.col = "black")
  }
  # save map ####
  map <- grDevices::recordPlot()

  graphics::par(mar=c(5,4,4,2)+.1, mgp = c(3, 1, 0)) # default bottom, left, top, right
  if (closemap) {
    grDevices::dev.off()
  }
  return(map)
}
