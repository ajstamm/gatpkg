#' Show GAT Help Pages
#'
#' This function creates a dialog box to provide the user with additional
#' assistance in using GAT, including instructions and links to help pages.
#'
#' @param help      A text string containing the help message.
#' @param helppage  A text string that contains the funcion name for the
#'                  relevant function (if any) in the help dialog.
#' @param helptitle The step name to display in the title bar.
#' @param step      Integer step in the GAT program, for help reference.
#' @param helpimg   A text string denoting the file name of the image to be
#'                  shown.
#'
#' @examples
#'
#' if (interactive()) {
#' # create the help message
# "\u2022" creates a bullet for lists
#' hlp <- paste0("Instructions: \n",
#'               "  \u2022  To continue,  click 'Next >'. \n",
#'               "  \u2022  To return to the last option, click '< Back'. \n",
#'               "  \u2022  To quit GAT, click 'Cancel'.")
#'
#' # create the dialog box
#' showGAThelp(
#'   help = hlp,
#'   helppage = "showGAThelp",
#'   step = 0,
#'   helptitle = "GAT help dialog"
#' )
#' }
#'
#' @export

showGAThelp <- function(help = "Find help here.",
                        helppage = "showGAThelp",
                        step = 0, helptitle = "this step",
                        helpimg = "showGAThelp") {
  # tcltk::tkmessageBox(title = "Help Panel", message = help,
  #                     type = "ok", icon = "info")

  # define objects ####
  help <- paste(help, "\n\n", "For further guidance,",
                "check out the GAT manual")
  if (!is.null(helppage)) {
    help <- paste(help, "\n", "or the function help for", helppage)
  }
  help <- paste0(help, ".")
  path <- find.package("gatpkg")

  # create window ####
  hlp <- tcltk::tktoplevel()
  tcltk::tcl("tk_setPalette", "white") # set background color
  tcltk::tktitle(hlp) <- paste0("Step ", step, ": Help for ", helptitle)
  hlp$note <- tcltk2::tk2label(hlp, text = help)
  tcltk::tkconfigure(hlp$note, background = "white")
  tcltk::tkgrid(hlp$note, sticky = "w", columnspan = 3, padx = 5)

  # add image and text ####
  if (helpimg == "showGAThelp" & helppage != helpimg) {
    helpimg <- helppage
  }
  if (!is.null(helpimg)) {
    imgpath <- paste0(path, "/help/figures/", helpimg, ".png")
    imgold <- tcltk::tkimage.create("photo", "imgold", file = imgpath)
    # note: zoom increases size by integer only; subsample reduces size
    # imgnew <- tcltk::tkimage.create("photo", "imgnew")
    # tcltk::tcl(imgnew, "copy", imgold, subsample = 2)
    # source: https://stackoverflow.com/questions/7191662/fit-image-size-to-a-small-button
    hlp$img <- tcltk::ttklabel(hlp, image = imgold, compound = "image")
    tcltk::tkgrid(hlp$img, columnspan = 3, padx = 5)
  }

  # add buttons ####
  onDone <- function() {
    tcltk::tkdestroy(hlp)
  }
  onManual <- function() {
    # vignette("gat_step_by_step", package = "gatpkg")
    browseURL(paste0(path, "/doc/gat_step_by_step.html"))
  }

  hlp$env$button <- tcltk::tkframe(hlp, width = 200, height = 40)
  hlp$env$button$Manual <-
    tcltk2::tk2button(hlp$env$button,
                      text = paste("GAT manual: \n   Step", step),
                      command = onManual, width = 15)
  tcltk::tkgrid(hlp$env$button$Manual, column = 2, row = 1, pady = 5, padx = 5)

  if (!is.null(helppage)) {
    onHelppage <- function() {
      # help(helppage, package = "gatpkg")
      browseURL(paste0(path, "/html/", helppage, ".html"))
    }
    hlp$env$button$Helppage <-
      tcltk2::tk2button(hlp$env$button,
                        text = paste("Function help: \n   ", helppage),
                        command = onHelppage, width = 25)
    tcltk::tkgrid(hlp$env$button$Helppage, column = 3, row = 1, pady = 5,
                  padx = 5)
  }

  hlp$env$button$Done <- tcltk2::tk2button(hlp$env$button, text = "Ok \n",
                                           command = onDone, width = 7,
                                           default = "active")
  tcltk::tkgrid(hlp$env$button$Done, column = 1, row = 1, pady = 5, padx = 5)
  tcltk::tkgrid(hlp$env$button)

  # wait for user ####
  tcltk::tkfocus(hlp)
  tcltk::tkwait.window(hlp) # pauses code to accept user input
  tcltk::tcl("tk_setPalette", "grey93") # set background color
}
