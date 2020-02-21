#' Input GAT Message
#'
#' @description
#' This function opens a dialog window for the user to confirm the contents
#' of the provided message.
#'
#' \if{html}{\figure{inputGATmessage.png}{options: width="300px"
#'                   alt="Figure: Screenshot of message dialog"}}
#'
#' Select your desired variables and the exclusion criterion for each.
#'
#' * Click \code{Yes} to confirm the message.
#' * Click \code{No} to refuse the message.
#' * Click \code{Back} to return to the previous step.
#' * Click \code{Help} to get further guidance and open the manual.
#'
#'
#' @param title     A text string that denotes the dialog title.
#' @param help      A text string containing the help message.
#' @param helppage  A text string that cntains the function name for the
#'                  relevant function (if any) in the help dialog.
#' @param step      Integer step in the GAT program, for help reference.
#' @param buttonopt A string that denotes the display value for the
#'                  no/cancel button.
#' @param msg       A text string that denotes the message for the user.
#' @param helptitle A text string denoting the title bar for the help window.
#' @param backopt   Boolean denoting whether to include the back button.
#'
#' @examples
#'
#' \donttest{
#' # define your message
#' inputGATmessage(
#'   title = "Random message window"
#' )
#' }
#'
#' @export

# can the help options be moved to ... ?

inputGATmessage <- function(title = "GAT input window",
                            help = "There is no help for you.",
                            helptitle = "inputGATmessage",
                            helppage = "inputGATmessage", step = 0,
                            msg = "Is GAT fun?", buttonopt = "Cancel GAT",
                            backopt = TRUE) {
  tt <- tcltk::tktoplevel()
  tcltk::tktitle(tt) <- paste0("Step ", step, ": ", title)
  tt$env$tm <- tcltk2::tk2label(tt, text = msg)
  tcltk::tkgrid(tt$env$tm, sticky = "w", padx = 5, pady = 5)

  myenv <- new.env()

  onOk <- function() {
    tcltk::tkdestroy(tt)
    assign("myvalue", "Yes", envir=myenv)
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
  tt$env$tf <- tcltk::tkframe(tt)

  if (backopt) {
    tt$env$tf$BackBut <- tcltk2::tk2button(tt$env$tf, text = "< Back",
                                           command = onBack, width = 12)
    tt$env$tf$OkBut <- tcltk2::tk2button(tt$env$tf, text = "Next >",
                                         command = onOk, width = 12,
                                         default = "active")
  } else {
    tt$env$tf$OkBut <- tcltk2::tk2button(tt$env$tf, text = "Confirm",
                                         command = onOk, width = 12,
                                         default = "active")
  }

  tt$env$tf$HelpBut <- tcltk2::tk2button(tt$env$tf, text="Help", width = 12,
                                         command = onHelp)
  tt$env$tf$CancelBut <- tcltk2::tk2button(tt$env$tf, text = buttonopt,
                                           width = 12, command = onCancel)

  # draw the frame, then add buttons or add buttons first; both work.
  # frame creates a bar of buttons on the bottom instead of one button under
  # the query and the rest off to the right.
  tcltk::tkgrid(tt$env$tf, pady = 5)
  if (backopt) {
    tcltk::tkgrid(tt$env$tf$BackBut, column = 1, row = 1, pady = 5, padx = c(5, 0))
  }
  tcltk::tkgrid(tt$env$tf$OkBut, column = 2, row = 1, pady = 5)
  tcltk::tkgrid(tt$env$tf$CancelBut, column = 3, row = 1, pady = 5)
  tcltk::tkgrid(tt$env$tf$HelpBut, column = 4, row = 1, pady = 5, padx = c(0, 5))

  tcltk::tkwait.window(tt)

  return(myenv$myvalue)


}
