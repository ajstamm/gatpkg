#' Input GAT Value
#'
#' @description
#'
#' This function creates a dialog box to ask the user for a specific value
#' and provides a link to the relevant help file in the GAT program.
#'
#' \figure{inputGATvalue.png}
#'
#' *Figure: Dialog to enter an alphanumeric value*
#'
#' @param title       A text string that denotes the dialog title.
#' @param help        A text string containing the help message.
#' @param message     A text string stating the request or message for the user.
#' @param defaulttext A text string that is pre-entered in the text box.
#' @param helppage    A text string that contains the funcion name for the
#'                    relevant function (if any) in the help dialog.
#' @param helptitle   The step name to display in the title bar.
#' @param step        Integer step in the GAT program, for help reference.
#' @param backopt     Boolean denoting whether to include the back button.
#' @param quitopt     Text string for the cancel button.
#' @param bgcol       Text string containing UI background color.
#' @param buttoncol   Text string containing UI button color.
#' @param helpopt     Boolean denoting whether to include the help button.
#'
#' @examples
#'
#' if (interactive()) {
#' inputGATvalue(title = "Learning your name",
#'               message = "Please enter your name.",
#'               defaulttext = "Charlie Brown")
#' }
#'
#' @export

# Gwen's original notes, for reference
############## gatInput function #############################################
# Begin second custom dialog function: gatInput
#   allows free typed input into a box
#   R tclTk code to create listbox with "back" next" "help" and "back" buttons
#   list of function arguments:
#     title, defaulttext, helpfile, message
#     returns "go back" or the input from the text box
############## start text input function #####################################

#use this function for free text input, like the minimum values
inputGATvalue <- function(title = "GAT input window", helppage = NULL, step = 0,
                          help = "Enter the desired value and click 'Next >'.",
                          message = "Please enter something in the box.",
                          defaulttext = "default text", helptitle = "this step",
                          backopt = TRUE, bgcol = "lightskyblue3",
                          quitopt = "Quit", buttoncol = "cornflowerblue",
                          helpopt = TRUE) {

  tt <- tcltk::tktoplevel(background = bgcol)
  tcltk::tktitle(tt) <- title
  tt$env$tm <- tcltk::tklabel(tt, text = message, justify = "left",
                              background = bgcol)
  tcltk::tkgrid(tt$env$tm, sticky = "w", padx = 5, pady = 5)

  varText <- tcltk::tclVar(defaulttext)
  tt$env$txt <- tcltk::tkentry(tt, width = "50", textvariable = varText)
  tcltk::tkgrid(tt$env$txt, padx = 5, pady = 5)

  myenv <- new.env()

  onOk <- function() {
    value <- tcltk::tclvalue(varText)
    tcltk::tkdestroy(tt)
    assign("myvalue", value, envir=myenv)
  }
  onCancel <- function() {
    tcltk::tkdestroy(tt)
    assign("myvalue", "cancel", envir=myenv)
  }
  onHelp <- function() {
    gatpkg::showGAThelp(help = help, helptitle = helptitle,
                        helppage = helppage, step = step, bgcol=bgcol,
                        buttoncol=buttoncol)
  }
  onBack <- function() {
    tcltk::tkdestroy(tt)
    assign("myvalue", "back", envir=myenv)
  }

  tt$env$tf <- tcltk::tkframe(tt, background = bgcol)
  if (backopt) {
    tt$env$tf$BackBut <- tcltk::tkbutton(tt$env$tf, text = "< Back", width = 12,
                                         command = onBack, background = buttoncol)
    tt$env$tf$OkBut <- tcltk::tkbutton(tt$env$tf, text = "Next >", width = 12,
                                       command = onOk, default = "active",
                                       background = buttoncol)
  } else {
    tt$env$tf$OkBut <- tcltk::tkbutton(tt$env$tf, text = "Confirm", width = 12,
                                       command = onOk, default = "active",
                                       background = buttoncol)
  }
  if (helpopt) {
    tt$env$tf$HelpBut <- tcltk::tkbutton(tt$env$tf, text="Help", width = 12,
                                         command = onHelp, background = buttoncol)
  }

  tt$env$tf$CancelBut <- tcltk::tkbutton(tt$env$tf, text = quitopt,
                                         width = 12, command = onCancel,
                                         background = buttoncol)

  tcltk::tkgrid(tt$env$tf, pady = 5)
  if (backopt) {
    tcltk::tkgrid(tt$env$tf$BackBut, column = 1, row = 1, pady = 5,
                  padx = c(5, 0))
  }
  tcltk::tkgrid(tt$env$tf$OkBut, column = 2, row = 1, pady = 5)
  tcltk::tkgrid(tt$env$tf$CancelBut, column = 3, row = 1, pady = 5)
  if (helpopt) {
    tcltk::tkgrid(tt$env$tf$HelpBut, column = 4, row = 1, pady = 5,
                  padx = c(0, 5))
  }

  # tkwm.resizable(tt, 0, 0)
  tcltk::tkfocus(tt$env$txt)
  tcltk::tkselection.from(tt$env$txt, "0")
  tcltk::tkselection.to(tt$env$txt, as.character(nchar(defaulttext)))
  tcltk::tkicursor(tt$env$txt, as.character(nchar(defaulttext)))
  # tkbind(tt$env$tf, "<Return>", onOk)
  tcltk::tkwait.window(tt)

  return(myenv$myvalue)
}

