#' Input GAT Variable
#'
#' @description
#'
#' This function creates a dialog box to ask the user to select an item from
#' a list and provides a link to the relevant help file in the GAT program. It
#' includes options to add a check box or text box if desired.
#'
#' \figure{inputGATvariable.png}
#'
#' *Figure: Dialog to enter several values*
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
#' @param helpimg     A text string denoting the file name of the GAT image to be
#'                    shown, or path and filename of other image to be shown
#' @param myvar       Variable selected, if pre-defined.
#' @param check       Boolean denoting the status of the checkbox. If TRUE,
#'                    the checkbox starts checked.
#' @param backopt     Boolean denoting whether to include the back button.
#' @param quitopt    Text string for the cancel button.
#' @param bgcol      Text string containing UI background color.
#' @param buttoncol  Text string containing UI button color.
#' @param tool       Text string containing the name of the tool
#'
#' @examples
#'
#' if (interactive()) {
#' inputGATvariable(
#'   title = "My favorite letter", checkbox = TRUE, valuebox = TRUE,
#'   instruction = "Please select your favorite letter.",
#'   checkopt = "Check this box \nif you love all letters.",
#'   valueopt = "Enter the number of letters \nyou love."
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

inputGATvariable <- function(title = "GAT window", instruction = "Select one.",
                             help = "There is no help.", helppage = NULL,
                             helptitle = NULL, step = 0, helpimg= NULL,
                             backopt = TRUE,
                             checkopt = "Check this box.", checkbox = FALSE,
                             valuebox = FALSE, value = 0, check = FALSE,
                             valueopt = "Enter a number:", mylist = letters,
                             myvar = NULL, bgcol = "lightskyblue3",
                             buttoncol = "cornflowerblue", quitopt = "Quit",
                             tool="GAT") {


  # create frames ----
  tt <- tcltk::tktoplevel(background = bgcol)
  tcltk::tktitle(tt) <- paste0("Step ", step, ": ", title)

  # for some reason, within functions frames must all be created at the start?
  tt$frm <- tcltk::tkframe(tt, width = 300, height = 5,
                           background = bgcol)
  tt$bound <- tcltk::tkframe(tt$frm, width = 150, height = 110,
                             background = bgcol)
  tt$tfbuts <- tcltk::tkframe(tt$frm, width = 300, height = 40,
                              background = bgcol)

  # list of options ----
  myvar <- if (is.null(myvar)) tcltk::tclVar("") else tcltk::tclVar(myvar)

  tt$bound$note <- tcltk::tklabel(tt$bound, text = instruction, justify = "left",
                                  background = bgcol)
  tt$bound$tl <- tcltk::ttkcombobox(tt$bound, values = mylist,
                                    textvariable = myvar, state = "readonly")
  tcltk::tkgrid(tt$bound$note, sticky = "w", columnspan = 4, padx = 5)
  tcltk::tkgrid(tt$bound$tl, padx = 10, pady = c(5, 10), sticky = "w",
                row = 2, column = 1)

  # checkbox and valuebox ----
  if (checkbox | valuebox) {
    tt$opts <- tcltk::tkframe(tt$frm, width = 150, height = 110,
                              background = bgcol)
    if (checkbox) {
      statebut <- if (check) "active" else "normal"
      tt$bound$cb <- tcltk::tkcheckbutton(tt$opts, background = bgcol)
      tt$bound$cblabel <- tcltk::tklabel(tt$opts, text = checkopt,
                                         justify = "left", background = bgcol)
      tt$bound$cbvalue <- tcltk::tclVar("0")
      tcltk::tkconfigure(tt$bound$cb, variable = tt$bound$cbvalue, state = statebut)
      tcltk::tkconfigure(tt$bound$cblabel, width = 20)
      tcltk::tkgrid(tt$bound$cb, column = 1, row = 2, sticky = "n")
      tcltk::tkgrid(tt$bound$cblabel, column = 2, row = 2, sticky = "nw")
    }
    if (valuebox) {
      vbvalue <- tcltk::tclVar(value)
      tt$bound$vb <- tcltk::tkentry(tt$opts, textvariable = vbvalue)
      tt$bound$vblabel <- tcltk::tklabel(tt$opts, text = valueopt,
                                         justify = "left", background = bgcol)
      tcltk::tkconfigure(tt$bound$vblabel, width = 25)
      tcltk::tkgrid(tt$bound$vblabel, column = 1, columnspan = 2, sticky = "nw")
      tcltk::tkgrid(tt$bound$vb, column = 1, columnspan = 2, sticky = "n")
    }
  }

  # bottom button functions ----
  myenv <- new.env()
  if (checkbox & valuebox) {
    onOk <- function() {
      # ind <- as.numeric(tcltk::tkcurselection(tt$bound$tl))
      # myvar <- mylist[ind + 1] # list 1
      myvar <- tcltk::tclvalue(myvar)
      cbVal <- as.character(tcltk::tclvalue(tt$bound$cbvalue))
      threshold <- as.character(tcltk::tclvalue(vbvalue))
      tcltk::tkdestroy(tt)

      check <- if (cbVal == "1") TRUE else FALSE
      assign("myoptions", list(myvar = myvar, check = check,
                               threshold = threshold), envir=myenv)
    }
  } else if (checkbox) {
    onOk <- function() {
      # ind <- as.numeric(tcltk::tkcurselection(tt$bound$tl))
      # myvar <- mylist[ind + 1] # list 1
      myvar <- tcltk::tclvalue(myvar)
      cbVal <- as.character(tcltk::tclvalue(tt$bound$cbvalue))
      tcltk::tkdestroy(tt)

      check <- if (cbVal == "1") TRUE else FALSE
      assign("myoptions", list(myvar = myvar, check = check,
                               threshold = 0), envir=myenv)
    }
  } else if (valuebox) {
    onOk <- function() {
      # ind <- as.numeric(tcltk::tkcurselection(tt$bound$tl))
      # myvar <- mylist[ind + 1] # list 1
      myvar <- tcltk::tclvalue(myvar)
      threshold <- as.character(tcltk::tclvalue(vbvalue))
      tcltk::tkdestroy(tt)

      assign("myoptions", list(myvar = myvar, check = FALSE,
                               threshold = threshold), envir=myenv)
    }
  } else {
    onOk <- function() {
      # ind <- as.numeric(tcltk::tkcurselection(tt$bound$tl))
      # myvar <- mylist[ind + 1] # list 1
      myvar <- tcltk::tclvalue(myvar)
      tcltk::tkdestroy(tt)

      assign("myoptions", list(myvar = myvar, check = FALSE,
                               threshold = 0), envir=myenv)
    }
  } # OnOk function versions
  onCancel <- function() {
    tcltk::tkdestroy(tt)
    assign("myoptions", list(myvar = "cancel", check = FALSE,
                            threshold = 0), envir=myenv)
  }
  onHelp <- function() {
    gatpkg::showGAThelp(help = help, helptitle = helptitle,
                helppage = helppage, helpimg = helpimg, step = step, tool=tool,
                bgcol=bgcol,buttoncol=buttoncol, bgcol=bgcol,
                buttoncol=buttoncol)
  }
  onBack <- function() {
    tcltk::tkdestroy(tt)
    assign("myoptions", list(myvar = "back", check = FALSE,
                            threshold = 0), envir=myenv)
  }

  # bottom button placements ----
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

  tt$tfbuts$CancelBut <- tcltk::tkbutton(tt$tfbuts, text = quitopt,
                                         command = onCancel, width = 12,
                                         background = buttoncol)
  tt$tfbuts$HelpBut <- tcltk::tkbutton(tt$tfbuts, text = "Help", width = 12,
                                       command = onHelp, background = buttoncol)

  # configure elements ----
  if (backopt) {
    tcltk::tkgrid(tt$tfbuts$BackBut, column = 1, row = 1, pady = 5, padx = c(5, 0))
  }
  tcltk::tkgrid(tt$tfbuts$OkBut, column = 2, row = 1, pady = 5)
  tcltk::tkgrid(tt$tfbuts$CancelBut, column = 3, row = 1, pady = 5)
  tcltk::tkgrid(tt$tfbuts$HelpBut, column = 4, row = 1, pady = 5, padx = c(0, 5))

  if (backopt) {
    tcltk::tkgrid.configure(tt$tfbuts$BackBut, sticky = "e")
  }
  tcltk::tkgrid.configure(tt$tfbuts$OkBut, sticky = "w")
  tcltk::tkpack(tt$tfbuts, tt$bound, side = "bottom")
  if (checkbox | valuebox) {
    tcltk::tkpack(tt$bound, tt$opts, side = "left", fill = "y")
  }
  tcltk::tkpack(tt$frm)

  # wait for user ----
  tcltk::tkfocus(tt)
  tcltk::tkwait.window(tt) # pauses code to accept user input

  return(myenv$myoptions)
}

