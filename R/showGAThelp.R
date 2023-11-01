#' Show GAT Help Pages
#'
#' This function creates a dialog box to provide the user with additional
#' assistance in using GAT, including instructions and links to help pages.
#'
#' @param help      A text string containing the help message.
#' @param helppage  A text string that contains the function name for the
#'                  relevant function (if any) in the help dialog.
#' @param helptitle The step name to display in the title bar.
#' @param step      Integer step in the GAT program, for help reference.
#' @param helpimg   A text string denoting the file name of the GAT PNG image to
#'                  be shown, or path and filename of other image to be shown,
#'                  (PNF, PFM, PPM, GIF) relative to the current working
#'                  directory
#' @param tool      A text string denoting the name of the tool
#' @param bgcol      Text string containing UI background color.
#' @param buttoncol  Text string containing UI button color.
#' @param manual    Text String containing the relative path of the tool
#'                  instruction manual.  For GAT, it is relative to the gatpkg
#'                  directory, otherwise it is relative to the working directory.
#'
#' @examples
#'
#' if (interactive()) {
#' showGAThelp(helptitle = "GAT help dialog")
#' }
#'
#' @export

showGAThelp <- function(help = "Find help here.", helppage = "showGAThelp",
                        step = 0, helptitle = "this step",
                        helpimg = "",
                        tool="GAT",
                        bgcol = "lightskyblue3",
                        buttoncol = "cornflowerblue",
                        manual = "/docs/dev/articles/gat_tutorial.html") {

  # define objects ####
  help <- paste(help, "\n\n For further guidance, check the ", tool, " manual")
  if (!is.null(helppage)) {
    help <- paste(help, "\n or the function help for", helppage)
  }
  help <- paste0(help, ".")
  gatpath <- find.package("gatpkg")
  if(tool == 'GAT'){path <- find.package("gatpkg")}
  else
  {path <- getwd()}
  #bgcol <- "lightskyblue3"
  #buttoncol <- "cornflowerblue"

  # create window ####
  hlp <- tcltk::tktoplevel(background = bgcol)
  tcltk::tktitle(hlp) <- paste0("Step ", step, ": Help for ", helptitle)
  hlp$note <- tcltk::tklabel(hlp, text = help, justify = "left",
                             background = bgcol)
  tcltk::tkgrid(hlp$note, sticky = "w", columnspan = 3, padx = 5)

  # add image and text ####
  #if (helpimg == "showGAThelp" & helppage != helpimg & tool == "GAT"){helpimg <- helppage}
  if(tool == "GAT" & helpimg == ""){helpimg <- "showGAThelp"}
  if (!is.null(helpimg)) {
    if (tool == "GAT"){imgpath <- paste0(gatpath, "/man/figures/", helpimg, ".png")}
    else if (tool != "GAT" & helpimg != "" & !is.null(helpimg)){imgpath<-paste0(path,helpimg)}
    else{imgpath<-""}
    imgold <- tcltk::tkimage.create("photo", "imgold", file = imgpath)
    # note: zoom increases size by integer only; subsample reduces size
    # imgnew <- tcltk::tkimage.create("photo", "imgnew")
    # tcltk::tcl(imgnew, "copy", imgold, subsample = 2)
    # source: https://stackoverflow.com/questions/7191662/fit-image-size-to-a-small-button
    hlp$img <- tcltk::ttklabel(hlp, image = imgold, compound = "image")
    tcltk::tkgrid(hlp$img, columnspan = 3, padx = 5)
  }

  # add buttons ----
  onDone <- function() tcltk::tkdestroy(hlp)
  # vignette("gat_step_by_step", package = "gatpkg")
  onManual <- function() utils::browseURL(paste0(path, manual))

  hlp$env$button <- tcltk::tkframe(hlp, width = 200, height = 40,
                                   background = bgcol)
  hlp$env$button$Manual <-
    tcltk::tkbutton(hlp$env$button, command = onManual, width = 15,
                    text = paste(tool, " manual: \n   Step", step),
                    background = buttoncol)
  tcltk::tkgrid(hlp$env$button$Manual, column = 2, row = 1, pady = 5, padx = 5)

  if (!is.null(helppage)) {
    # help(helppage, package = "gatpkg")
    onHelppage <- function() utils::browseURL(paste0(gatpath, "/docs/dev/reference/", helppage, ".html"))
    hlp$env$button$Helppage <-
      tcltk::tkbutton(hlp$env$button, command = onHelppage, width = 25,
                      text = paste("Function help: \n   ", helppage),
                      background = buttoncol)
    tcltk::tkgrid(hlp$env$button$Helppage, column = 3, row = 1, pady = 5,
                  padx = 5)
  }

  hlp$env$button$Done <- tcltk::tkbutton(hlp$env$button, text = "Ok \n",
                         command = onDone, width = 7, default = "active",
                         background = buttoncol)
  tcltk::tkgrid(hlp$env$button$Done, column = 1, row = 1, pady = 5, padx = 5)
  tcltk::tkgrid(hlp$env$button)

  # wait for user ####
  tcltk::tkfocus(hlp)
  tcltk::tkwait.window(hlp) # pauses code to accept user input
  tcltk::tcl("tk_setPalette", "grey93") # set background color
}
