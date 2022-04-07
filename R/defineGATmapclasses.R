#' Plot GAT Maps
#'
#' @description
#' This function prepares the classes for the original and aggregated maps
#' so that the same scale can be used for both.
#'
#' @param areaold A spatial polygons data frame.
#' @param areanew A second spatial polygons data frame that ahould have the
#'                same outer boundary as the first one.
#' @param aggvar  The variable to be mapped.
#' @param breaks  The number of desired categories in the maps.
#'
#' @examples
#' # prepare the classes for the choropleth maps
#' defineGATmapclasses(
#'   areaold = hftown,
#'   areanew = hfagg5k,
#'   aggvar = "TOTAL_POP",
#'   breaks = 4
#' )
#'
#' @export

defineGATmapclasses <- function(areaold, areanew, aggvar, breaks = 7) {
  olddata <- as.numeric(data.frame(areaold)[, aggvar])
  newdata <- as.numeric(data.frame(areanew)[, aggvar])
  combinedata <- c(olddata, newdata)

  if (length(table(combinedata)) < breaks) {
    breaks <- length(table(combinedata))
  }

  if (breaks > 2) {
    plotclr1 <- RColorBrewer::brewer.pal(breaks, "OrRd")
  } else if (breaks == 2) {
    plotclr1 <- RColorBrewer::brewer.pal(3, "OrRd")[c(1, 3)]
  } else if (breaks == 1) {
    plotclr1 <- RColorBrewer::brewer.pal(3, "OrRd")[1]
  }

  class1 <- classInt::classIntervals(combinedata, breaks, style = "quantile")
  if (breaks < 4) {
    class1$brks <- c(floor(class1$brks[1]*10)/10,
                     round(class1$brks[2:breaks], digits = 1),
                     ceiling(class1$brks[(breaks + 1)]*10)/10)
  } else {
    class1$brks <- c(floor(class1$brks[1]), round(class1$brks[2:breaks]),
                     ceiling(class1$brks[(breaks + 1)]))
  }

  class1before <- classInt::classIntervals(olddata, n = breaks,
                                           style = "fixed", fixedBreaks = class1$brks)
  colcode1before <- classInt::findColours(class1before, plotclr1)

  tempbrks <- c(floor(min(newdata)), class1$brks[class1$brks > min(newdata)])

  class1after <- classInt::classIntervals(newdata, n = breaks,
                                          style = "fixed", fixedBreaks = tempbrks)
  class1after$brks <- class1$brks
  colcode1after <- classInt::findColours(class1after, plotclr1)

  myvars <- list(colcode1before = colcode1before,
                 colcode1after = colcode1after)
  return(myvars)
}

