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
#'
#' @examples
#'
#' if (interactive()) {
#' hlp <-
#'   paste0(
#'     "To continue, select an option and click 'Next >',",
#'     "\nto return to the previous step, click '< Back',",
#'     "\nand to quit the program, click 'Cancel'."
#'   )
#'
#' inputGATvalue(
#'   title = "Learning your name",
#'   help = hlp,
#'   message = "Please enter your name.",
#'   defaulttext = "Charlie Brown"
#' )
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
inputGATvalue <- function(title = "GAT input window",
                          help = "Enter the desired value and click 'Next >'.",
                          message = "Please enter something in the box.",
                          defaulttext = "default text",
                          helppage = NULL, step = 0,
                          helptitle = "this step",
                          backopt = TRUE) {
  tt <- tcltk::tktoplevel()
  tcltk::tktitle(tt) <- title
  tt$env$tm <- tcltk::tklabel(tt, text = message)
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
    showGAThelp(help = help, helptitle = helptitle,
                helppage = helppage, step = step)
  }
  onBack <- function() {
    tcltk::tkdestroy(tt)
    assign("myvalue", "back", envir=myenv)
  }
  tt$env$tf <- tcltk::tkframe(tt)
  if (backopt) {
    tt$env$tf$BackBut <- tcltk::tkbutton(tt$env$tf, text = "< Back",
                                           command = onBack, width = 12)
    tt$env$tf$OkBut <- tcltk::tkbutton(tt$env$tf, text = "Next >",
                                         command = onOk, width = 12,
                                         default = "active")
  } else {
    tt$env$tf$OkBut <- tcltk::tkbutton(tt$env$tf, text = "Confirm",
                                         command = onOk, width = 12,
                                         default = "active")
  }

  tt$env$tf$HelpBut <- tcltk::tkbutton(tt$env$tf, text="Help", width = 12,
                                         command = onHelp)
  tt$env$tf$CancelBut <- tcltk::tkbutton(tt$env$tf, text = "Cancel GAT",
                                           width = 12, command = onCancel)

  # draw the frame, then add buttons or add buttons first; both work.
  # frame creates a bar of buttons on the bottom instead of one button under
  # the query and the rest off to the right.
  tcltk::tkgrid(tt$env$tf, pady = 5)
  if (backopt) {
    tcltk::tkgrid(tt$env$tf$BackBut, column = 1, row = 1, pady = 5,
                  padx = c(5, 0))
  }
  tcltk::tkgrid(tt$env$tf$OkBut, column = 2, row = 1, pady = 5)
  tcltk::tkgrid(tt$env$tf$CancelBut, column = 3, row = 1, pady = 5)
  tcltk::tkgrid(tt$env$tf$HelpBut, column = 4, row = 1, pady = 5,
                padx = c(0, 5))

  # tkwm.resizable(tt, 0, 0)
  tcltk::tkfocus(tt$env$txt)
  tcltk::tkselection.from(tt$env$txt, "0")
  tcltk::tkselection.to(tt$env$txt, as.character(nchar(defaulttext)))
  tcltk::tkicursor(tt$env$txt, as.character(nchar(defaulttext)))
  # tkbind(tt$env$tf, "<Return>", onOk)
  tcltk::tkwait.window(tt)

  return(myenv$myvalue)
} # end gatInput function

############## end text input function ######################
