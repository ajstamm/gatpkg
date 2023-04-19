#' Save GAT KML File
#'
#' @description
#' This function opens a dialog window for the user to select whether or not
#' to save a KML file. The dialog window looks like this.
#'
#' \figure{saveGATkml.png}
#'
#' *Figure: Screenshot of dialog to select KML save.png*
#'
#' Select "Yes" if you want to save a KML file. The default selection is "No".
#'
#' Click on one of the following buttons.
#'
#' * Click \code{Yes} if you would like to save a KML file.
#' * Click \code{No} if you do not want to save a KML file.
#' * Click \code{Back} to return to the previous step.
#' * Click \code{Help} to get further guidance and open the manual.
#'
#' @details
#' The resulting KML fie will contain all data, which can be accessed through
#' Google Earth. GE 5.0 or higher is recommended. KML files were tested on
#' GE in Chrome v9.2.90.1.
#'
#' @param step       Integer step in the GAT program, for help reference.
#' @param backopt    Boolean denoting whether to include the back button.
#' @param quitopt    Text string for the cancel button.
#' @param bgcol      Text string containing UI background color.
#' @param buttoncol  Text string containing UI button color.
#'
#' @examples
#'
#' if (interactive()) {
#' saveGATkml()
#' }
#'
#' @export

saveGATkml <- function(step = 0, backopt = TRUE, bgcol = "lightskyblue3",
                       buttoncol = "cornflowerblue", quitopt = "Quit") {
  # set up ####
  msg <- paste("Would you like to save a KML file as well as a shapefile?",
               "\n(The KML file may take a while to write.)")
  help <- paste0("To continue, select 'Yes' or 'No',",
                 "\nand to return to rate selection, click '< Back',")
  helppage <- "saveGATkml_radio"
  title <- "Save KML file?"
  rbValue <- tcltk::tclVar("no")

  # draw window ####
  tt <- tcltk::tktoplevel(background = bgcol)
  tcltk::tktitle(tt) <- paste0("Step ", step, ": ", title)

  # create frames ####
  # for some reason, within functions frames must all be created at the start
  tt$frm <- tcltk::tkframe(tt, width = 300, height = 5, background = bgcol)
  tt$tfbuts <- tcltk::tkframe(tt$frm, width = 300, height = 40, background = bgcol)
  tt$radio <- tcltk::tkframe(tt$frm, width = 150, height = 110, background = bgcol)

  # radiobuttons ####

  tt$radio$instruct <- tcltk::tklabel(tt$radio, text = msg, justify = "left",
                                      background = bgcol)
  tcltk::tkgrid(tt$radio$instruct, sticky = "w", padx = 5, pady = 5,
                columnspan = 4)

  tt$radio$rb1 <- tcltk::tkradiobutton(tt$radio, background = bgcol)
  tt$radio$lab1 <- tcltk::tklabel(tt$radio, text = "Yes", background = bgcol)
  tcltk::tkconfigure(tt$radio$rb1, variable = rbValue, value = "Yes")
  tcltk::tkgrid(tt$radio$rb1, tt$radio$lab1)
  tcltk::tkgrid.configure(tt$radio$rb1, sticky = "e")
  tcltk::tkgrid.configure(tt$radio$lab1, sticky = "w")

  tt$radio$rb2 <- tcltk::tkradiobutton(tt$radio, background = bgcol)
  tt$radio$lab2 <- tcltk::tklabel(tt$radio, text = "No", background = bgcol)
  tcltk::tkconfigure(tt$radio$rb2, variable = rbValue, value = "No")
  tcltk::tkgrid(tt$radio$rb2, tt$radio$lab2)
  tcltk::tkgrid.configure(tt$radio$rb2, sticky = "e")
  tcltk::tkgrid.configure(tt$radio$lab2, sticky = "w")

  # bottom button functions ----
  myenv <- new.env()
  onOk <- function() {
    rbval <- tcltk::tclvalue(rbValue)
    tcltk::tkdestroy(tt)
    assign("myvalue", rbval, envir=myenv)
  }
  onCancel <- function() {
    tcltk::tkdestroy(tt)
    assign("myvalue", "cancel", envir=myenv)
  }
  onHelp <- function() {
    showGAThelp(help = help, helptitle = helppage, helppage = helppage,
                step = step)
  }
  onBack <- function() {
    tcltk::tkdestroy(tt)
    assign("myvalue", "back", envir=myenv)
  }

  if (backopt) {
    tt$tfbuts$BackBut <- tcltk::tkbutton(tt$tfbuts, text = "< Back", width = 12,
                                         command = onBack, background = buttoncol)
    tt$tfbuts$OkBut <- tcltk::tkbutton(tt$tfbuts, text = "Next >", width = 12,
                                       command = onOk, default = "active",
                                       background = buttoncol)
  } else {
    tt$tfbuts$OkBut <- tcltk::tkbutton(tt$tfbuts, text = "Confirm", width = 12,
                                       command = onOk, default = "active",
                                       background = buttoncol)
  }
  tt$tfbuts$HelpBut <- tcltk::tkbutton(tt$tfbuts, text="Help", width = 12,
                                       command = onHelp, background = buttoncol)
  tt$tfbuts$CancelBut <- tcltk::tkbutton(tt$tfbuts, text = quitopt, width = 12,
                                         command = onCancel, background = buttoncol)

  if (backopt) {
    tcltk::tkgrid(tt$tfbuts$BackBut, column = 1, row = 1, pady = 5, padx = c(5, 0))
  }
  tcltk::tkgrid(tt$tfbuts$OkBut, column = 2, row = 1, pady = 5)
  tcltk::tkgrid(tt$tfbuts$CancelBut, column = 3, row = 1, pady = 5)
  tcltk::tkgrid(tt$tfbuts$HelpBut, column = 4, row = 1, pady = 5, padx = c(0, 5))

  # add frames to dialog ####
  tcltk::tkpack(tt$tfbuts, tt$radio, side = "bottom")
  tcltk::tkpack(tt$frm)
  tcltk::tkwait.window(tt)

  # retur value ####
  return(myenv$myvalue)
}
