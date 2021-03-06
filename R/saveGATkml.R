#' Save GAT KML File
#'
#' @description
#' This function opens a dialog window for the user to select whether or not
#' to save a KML file. The dialog window looks like this.
#'
#' \if{html}{\figure{saveGATkml.png}{options: width="405"
#'                   alt="Figure: Screenshot of dialog to select KML save.png"}}
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
#' @param step    Integer step in the GAT program, for help reference.
#' @param backopt Boolean denoting whether to include the back button.
#'
#' @examples
#'
#' \donttest{
#' # choose yes or no
#' saveGATkml()
#' }
#'
#' @export

saveGATkml <- function(step = 0, backopt = TRUE) {
  # set up ####
  msg <- paste("Would you like to save a KML file as well as a shapefile?",
               "\n(The KML file may take a while to write.)")
  help <- paste0("To continue, select 'Yes' or 'No',",
                 "\nand to return to rate selection, click '< Back',")
  helppage <- "saveGATkml_radio"
  title <- "Save KML file?"
  rbValue <- tcltk::tclVar("no")

  # draw window ####
  tt <- tcltk::tktoplevel()
  tcltk::tktitle(tt) <- paste0("Step ", step, ": ", title)

  # create frames ####
  # for some reason, within functions frames must all be created at the start
  tt$frm <- tcltk::tkframe(tt, width = 300, height = 5)
  tt$tfbuts <- tcltk::tkframe(tt$frm, width = 300, height = 40)
  tt$radio <- tcltk::tkframe(tt$frm, width = 150, height = 110)

  # radiobuttons ####

  tt$radio$instruct <- tcltk2::tk2label(tt$radio, text = msg)
  tcltk::tkgrid(tt$radio$instruct, sticky = "w", padx = 5, pady = 5,
                columnspan = 4)

  tt$radio$rb1 <- tcltk::tkradiobutton(tt$radio)
  tt$radio$lab1 <- tcltk2::tk2label(tt$radio, text = "Yes")
  tcltk::tkconfigure(tt$radio$rb1, variable = rbValue, value = "Yes")
  tcltk::tkgrid(tt$radio$rb1, tt$radio$lab1)
  tcltk::tkgrid.configure(tt$radio$rb1, sticky = "e")
  tcltk::tkgrid.configure(tt$radio$lab1, sticky = "w")

  tt$radio$rb2 <- tcltk::tkradiobutton(tt$radio)
  tt$radio$lab2 <- tcltk2::tk2label(tt$radio, text = "No")
  tcltk::tkconfigure(tt$radio$rb2, variable = rbValue, value = "No")
  tcltk::tkgrid(tt$radio$rb2, tt$radio$lab2)
  tcltk::tkgrid.configure(tt$radio$rb2, sticky = "e")
  tcltk::tkgrid.configure(tt$radio$lab2, sticky = "w")

  # bottom button functions ####
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
    showGAThelp(help = help, helptitle = helppage,
                helppage = helppage, step = step)
  }
  onBack <- function() {
    tcltk::tkdestroy(tt)
    assign("myvalue", "back", envir=myenv)
  }

  if (backopt) {
    tt$tfbuts$BackBut <- tcltk2::tk2button(tt$tfbuts, text = "< Back",
                                           command = onBack, width = 12)
    tt$tfbuts$OkBut <- tcltk2::tk2button(tt$tfbuts, text = "Next >",
                                         command = onOk, width = 12,
                                         default = "active")
  } else {
    tt$tfbuts$OkBut <- tcltk2::tk2button(tt$tfbuts, text = "Confirm",
                                         command = onOk, width = 12,
                                         default = "active")
  }
  tt$tfbuts$HelpBut <- tcltk2::tk2button(tt$tfbuts, text="Help", width = 12,
                                         command = onHelp)
  tt$tfbuts$CancelBut <- tcltk2::tk2button(tt$tfbuts, text = "Cancel",
                                           width = 12, command = onCancel)

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
