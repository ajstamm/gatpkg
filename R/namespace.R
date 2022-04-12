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



# package suggested, not required, so don't call import
# @importFrom prettymapr addnortharrow addscalebar

# packages no longer used due to weird behavior in RStudio
# @importFrom svDialogs dlgMessage

NULL
