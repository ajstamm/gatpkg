# packages not listed as required
# @importFrom methods as :: # needed, but importing throws an error,
# so import full library

#' @import     methods
#' @importFrom grDevices dev.control dev.new dev.off recordPlot replayPlot
#'                       pdf graphics.off
#' @importFrom graphics  legend title text par
#' @importFrom stats     dist quantile median
#' @importFrom utils     browseURL choose.files install.packages memory.limit
#'                       write.table packageDescription
#'

# packages listed as required
# from tcltk, this package uses the following:
#   tktoplevel tktitle tkgrid tkconfigure tkgrid.configure tkframe
#   tkradiobutton ttkcombobox tkcheckbutton tkmessageBox
#   tclvalue tclVar tkcurselection tkicursor tkentry tkProgressBar
#   tkwait.window tkpack.propagate tkpack tkwm.title tkfocus
#   tkselection.set tkselection.from tkselection.to

#' @import     tcltk
#' @importFrom foreign      read.dbf
#' @importFrom rgeos        gCentroid gArea gConvexHull
#' @importFrom tcltk2       tk2label tk2button tk2listbox
#' @importFrom RColorBrewer brewer.pal
#' @importFrom classInt     classIntervals findColours
#' @importFrom spdep        poly2nb aggregate.nb
#' @importFrom sp           proj4string spTransform CRS coordinates plot
#'                          spDistsN1 bbox
#' @importFrom maptools     spCbind
#' @importFrom rgdal        writeOGR readOGR
#' @importFrom sf           st_as_sf st_area as_Spatial read_sf st_transform
#'                          st_intersection
#' @importFrom raster       intersect


NULL
