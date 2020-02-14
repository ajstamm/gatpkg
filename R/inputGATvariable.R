#' Input GAT Variable
#'
#' @description
#'
#' This function creates a dialog box to ask the user to select an item from
#' a list and provides a link to the relevant help file in the GAT program. It
#' includes options to add a check box or text box if desired.
#'
#' \if{html}{\figure{inputGATvariable.png}{options: width="450px"
#'                   alt="Figure: Screenshot of dialog to enter several values"}}
#'
#'
#' @param title       A text string that denotes the dialog title.
#' @param instruction A text string stating the instructions to the user.
#' @param help        A text string containing the help message.
#' @param mylist      A character vector of variable names for the list.
#' @param checkbox    A boolean that denotes whether or not to include a
#'                    checkbox in the dialog.
#' @param checkopt    The text that should accompany the checkbox. If checkbox
#'                    is FALSE, this is ignored.
#' @param valuebox    A boolean that denotes whether or not to include a
#'                    valuebox in the dialog.
#' @param valueopt    The text that should accompany the valuebox. If valuebox
#'                    is FALSE, this is ignored.
#' @param helppage    A text string that contains the function name for the
#'                    relevant function (if any) in the help dialog.
#' @param step        Integer step in the GAT program, for help reference.
#' @param value       A number or string that denotes the default value for
#'                    the text box.
#' @param helptitle   A text string that denotes the help dialog title.
#' @param myvar       Variable selected, if pre-defined.
#' @param check       Boolean denoting the status of the checkbox. If TRUE,
#'                    the checkbox starts checked.
#' @param backopt     Boolean denoting whether to include the back button.
#'
#' @examples
#'
#' \donttest{
#' hlp <- paste0("To continue, select an option and click 'Next >',",
#'               "\nto return to the previous step, click '< Back',",
#'               "\nand to quit the program, click 'Cancel'.")
#'
#' inputGATvariable(
#'   title = "My favorite letter",
#'   instruction = "Please select your favorite letter.",
#'   help = hlp,
#'   mylist = letters,
#'   checkopt = "Check this box \nif you love all letters.",
#'   valueopt = "Enter the number of letters \nyou love.",
#'   checkbox = TRUE,
#'   valuebox = TRUE,
#'   helppage = "inputGATvariable",
#'   value = "5,000"
#' )
#' }
#'
#' @export

# Gwen's original notes (mostly) below, for reference
# this function has changed a lot since she originally wrote it
############## gatgui function ################################################
# Begin custom function for dialogs: gatgui
#   allows selection from a list, with scroll bar
#   R tclTk code to create listbox with "back" next" "help" and "back" buttons
#   list of function arguments:
#     title, message, list, helpfile
#     function returns text: either "go back" or the selection from the list
#     requires package tcltk
#     returns "go back" or the selected item from the list, as text
############## start gui function #############################################

inputGATvariable <- function(title = "GAT window",
                             instruction = "Please select one.",
                             checkopt = "Select this checkbox.",
                             valueopt = "Enter a number:",
                             help = "There is no help for you.",
                             mylist = letters, checkbox = FALSE,
                             helppage = NULL, step = 0,
                             valuebox = FALSE, helptitle = NULL,
                             value = 0, myvar = "NONE", check = FALSE,
                             backopt = TRUE) {
  tt <- tcltk::tktoplevel()
  tcltk::tktitle(tt) <- paste0("Step ", step, ": ", title)

  # create frames ####
  # for some reason, within functions frames must all be created at the start
  tt$frm <- tcltk::tkframe(tt, width = 300, height = 5)
  tt$bound <- tcltk::tkframe(tt$frm, width = 150, height = 110)
  tt$tfbuts <- tcltk::tkframe(tt$frm, width = 300, height = 40)
  # list of options ####
  tt$bound$note <- tcltk2::tk2label(tt$bound, text = instruction)
  tt$bound$tl <- tcltk2::tk2listbox(tt$bound, height = 5,
                                    values = mylist, selectmode = "single",
                                    background = "white", value = myvar)
  tcltk::tkgrid(tt$bound$note, sticky = "w", columnspan = 4, padx = 5)
  tcltk::tkgrid(tt$bound$tl, padx = 10, pady = c(5, 10), sticky = "w",
                row = 2, column = 1)

  # checkbox and valuebox ####
  if (checkbox | valuebox) {
    tt$opts <- tcltk::tkframe(tt$frm, width = 150, height = 110)
    if (checkbox) {
      statebut <- if (check) "active" else "normal"
      tt$bound$cb <- tcltk::tkcheckbutton(tt$opts)
      tt$bound$cblabel <- tcltk2::tk2label(tt$opts, text = checkopt)
      tt$bound$cbvalue <- tcltk::tclVar("0")
      tcltk::tkconfigure(tt$bound$cb, variable = tt$bound$cbvalue, state = statebut)
      tcltk::tkconfigure(tt$bound$cblabel, width = 20)
      tcltk::tkgrid(tt$bound$cb, column = 1, row = 2, sticky = "n")
      tcltk::tkgrid(tt$bound$cblabel, column = 2, row = 2, sticky = "nw")
    }
    if (valuebox) {
      vbvalue <- tcltk::tclVar(value)
      tt$bound$vb <- tcltk::tkentry(tt$opts, textvariable = vbvalue)
      tt$bound$vblabel <- tcltk2::tk2label(tt$opts, text = valueopt)
      tcltk::tkconfigure(tt$bound$vblabel, width = 25)
      tcltk::tkgrid(tt$bound$vblabel, column = 1, columnspan = 2, sticky = "nw")
      tcltk::tkgrid(tt$bound$vb, column = 1, columnspan = 2, sticky = "n")
    }
  }

  # bottom button functions ####
  myenv <- new.env()
  if (checkbox & valuebox) {
    onOk <- function() {
      ind <- as.numeric(tcltk::tkcurselection(tt$bound$tl))
      myvar <- mylist[ind + 1] # list 1
      cbVal <- as.character(tcltk::tclvalue(tt$bound$cbvalue))
      threshold <- as.character(tcltk::tclvalue(vbvalue))
      tcltk::tkdestroy(tt)

      if (cbVal == "1") check <- TRUE else check = FALSE

      assign("myoptions", list(myvar = myvar,
                               check = check,
                               threshold = threshold), envir=myenv)
    }
  } else if (checkbox) {
    onOk <- function() {
      ind <- as.numeric(tcltk::tkcurselection(tt$bound$tl))
      myvar <- mylist[ind + 1] # list 1
      cbVal <- as.character(tcltk::tclvalue(tt$bound$cbvalue))
      tcltk::tkdestroy(tt)

      if (cbVal == "1") {
        check <- TRUE
      } else {
        check = FALSE
      }

      assign("myoptions", list(myvar = myvar,
                               check = check,
                               threshold = 0), envir=myenv)
    }
  } else if (valuebox) {
    onOk <- function() {
      ind <- as.numeric(tcltk::tkcurselection(tt$bound$tl))
      myvar <- mylist[ind + 1] # list 1
      threshold <- as.character(tcltk::tclvalue(vbvalue))
      tcltk::tkdestroy(tt)


      assign("myoptions", list(myvar = myvar,
                               check = FALSE,
                               threshold = threshold), envir=myenv)
    }
  } else {
    onOk <- function() {
      ind <- as.numeric(tcltk::tkcurselection(tt$bound$tl))
      myvar <- mylist[ind + 1] # list 1
      tcltk::tkdestroy(tt)

      assign("myoptions", list(myvar = myvar,
                               check = FALSE,
                               threshold = 0), envir=myenv)
    }
  } # OnOk function versions
  onCancel <- function() {
    tcltk::tkdestroy(tt)
    assign("myoptions", list(myvar = "cancel",
                            check = FALSE,
                            threshold = 0), envir=myenv)
  }
  onHelp <- function() {
    showGAThelp(help = help, helptitle = helppage,
                helppage = helppage, helpimg = helppage, step = step)
  }
  onBack <- function() {
    tcltk::tkdestroy(tt)
    assign("myoptions", list(myvar = "back",
                            check = FALSE,
                            threshold = 0), envir=myenv)
  }

  # bottom button placements ####
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

  tt$tfbuts$CancelBut <- tcltk2::tk2button(tt$tfbuts, text = "Cancel GAT",
                                           command = onCancel, width = 12)
  tt$tfbuts$HelpBut <- tcltk2::tk2button(tt$tfbuts, text = "Help",
                                         command = onHelp, width = 12)

  # add elements to the window
  if (backopt) {
    tcltk::tkgrid(tt$tfbuts$BackBut, column = 1, row = 1, pady = 5, padx = c(5, 0))
  }
  tcltk::tkgrid(tt$tfbuts$OkBut, column = 2, row = 1, pady = 5)
  tcltk::tkgrid(tt$tfbuts$CancelBut, column = 3, row = 1, pady = 5)
  tcltk::tkgrid(tt$tfbuts$HelpBut, column = 4, row = 1, pady = 5, padx = c(0, 5))

  # configure elements ####
  if (backopt) {
    tcltk::tkgrid.configure(tt$tfbuts$BackBut, sticky = "e")
  }
  tcltk::tkgrid.configure(tt$tfbuts$OkBut, sticky = "w")

  tcltk::tkpack(tt$tfbuts, tt$bound, side = "bottom")
  if (checkbox | valuebox) {
    tcltk::tkpack(tt$bound, tt$opts, side = "left", fill = "y")
  }
  tcltk::tkpack(tt$frm)

  # wait for user ####
  tcltk::tkfocus(tt)
  tcltk::tkwait.window(tt) # pauses code to accept user input

  return(myenv$myoptions)
}

# inputGATvariable(mylist = letters, helpfile = "hlp")
# inputGATvariable(mylist = letters, helpfile = "hlp", checkbox = TRUE)

############## end gui function #############################
