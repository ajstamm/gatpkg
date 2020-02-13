#' Input Multiple GAT Aggregation Variables
#'
#' @description
#' This function opens a dialog window for the user to select which variables
#' in the shapefile should be aggregated and to what minimum and maximum values.
#' The dialog window looks like this.
#'
#' \if{html}{\figure{inputGATaggregators.png}{options: width="340px"
#'                   alt="Figure: Screenshot of dialog to select your aggregators"}}
#'
#' Select your desired aggregation variables. If you do not want to include a
#' second aggregation variable, select "NONE". Enter your desired minimum and
#' maximum values in the relevant boxes.
#' Then click on one of the following buttons.
#'
#' * Click \code{Next} to continue to the next step.
#' * Click \code{Cancel} to end GAT.
#' * Click \code{Back} to return to the previous step.
#' * Click \code{Help} to get further guidance and open the manual.
#'
#'
#' @details
#' For the minimum and maximum values, only positive or negative numbers,
#' commas, and decimals are allowed. If you enter any other characters, the
#' function will allow it, but GAT will run a check that triggers the
#' function inputGATvalue() to force you to enter a number.
#'
#' @param mapdata A data frame.
#' @param step    An integer step in the GAT program, for help reference.
#' @param min1    The minimum value for the first aggregation variable.
#' @param min2    The minimum value for the second aggregation variable.
#' @param max1    The maximum value for the first aggregation variable.
#' @param max2    The maximum value for the second aggregation variable.
#' @param var1    The name of the first aggregation variable.
#' @param var2    The name of the second aggregation variable.
#'
#' @examples
#'
#' \donttest{
#' # identify variable to aggregate
#' inputGATaggregators(
#'   mapdata = hftown@data
#' )
#' }
#'
#' @export

inputGATaggregators <- function(mapdata, step = 4, min1 = "5,000",
                                min2 = "none", max1 = "none", max2 = "none",
                                var1 = "", var2 = "NONE") {
  ## define objects ####
  helppage <- "inputGATaggregators"
  hlp <- paste0("Select your aggregation variables. In the text boxes, \n",
                "enter your desired minimum and maximum values. \n",
                "  \u2022  To continue,  click 'Next >'. \n",
                "  \u2022  To return to aggregation variable selection,",
                "click '< Back'. \n", "  \u2022  To quit GAT, click 'Cancel'.")

  instruct <- paste(" 1. Select each variable you would like to aggregate. \n",
                    "     To ignore the second aggregation variable, select",
                    "'NONE' \n      for its variable name. \n",
                    "2. Enter a number for the first minimum value, which is",
                    "required. \n      If you do not require a second minimum",
                    "value, enter 'none'. \n",
                    "3. Enter a number for each maximum value or 'none' if it",
                    "is not \n     required. \n \n",
                    "Note: The first variable and its minimum aggregation value",
                    "are required. \n")

  fonthead <- tcltk2::tk2font.set(font = "fonthead",
                                  settings = list(family = "Segoe UI", size = 10,
                                                  bold = TRUE, italic = FALSE))

  mylist1 <- checkGATvariabletypes(mapdata, type = "number")
  mylist2 <- c("NONE", mylist1)
  if (var1 %in% mylist1) {
    myvar1 <- tcltk::tclVar(var1)
  } else {
    myvar1 <- tcltk::tclVar(mylist1[1])
  }
  if (var2 %in% mylist2) {
    myvar2 <- tcltk::tclVar(var2)
  } else {
    myvar2 <- tcltk::tclVar("NONE")
  }
  minval1 <- tcltk::tclVar(min1)
  minval2 <- tcltk::tclVar(min2)
  maxval1 <- tcltk::tclVar(max1)
  maxval2 <- tcltk::tclVar(max2)

  ## draw window ####

  tt <- tcltk::tktoplevel()
  tcltk::tkwm.title(tt, paste0("Step ", step, ": Aggregation Variables"))
  tt$insttl <- tcltk2::tk2label(tt, text = "Instructions", font = "fonthead")
  tcltk::tkgrid(tt$insttl, sticky = "w", padx = 5, pady = 5)
  tcltk::tkgrid(tcltk2::tk2label(tt, text = instruct), columnspan = 4,
                sticky = "w")

  tt$tr <- tcltk::tkframe(tt)

  ## table headings ####

  tt$tr$varlabel = tcltk2::tk2label(tt$tr, text = " Variable", font = "fonthead")
  tt$tr$minlabel = tcltk2::tk2label(tt$tr, text = " Minimum value", font = "fonthead")
  tt$tr$maxlabel = tcltk2::tk2label(tt$tr, text = " Maximum value", font = "fonthead")
  tcltk::tkgrid(tt$tr$varlabel, row = 1, column = 1, sticky = "w")
  tcltk::tkgrid(tt$tr$minlabel, row = 1, column = 2, sticky = "w")
  tcltk::tkgrid(tt$tr$maxlabel, row = 1, column = 3, sticky = "w")


  ## variable lists ####

  tt$tr$Varlist1 <- tcltk::ttkcombobox(tt$tr,
                                       values = mylist1,
                                       textvariable = myvar1,
                                       state = "readonly")
  tt$tr$Varlist2 <- tcltk::ttkcombobox(tt$tr,
                                       values = mylist2,
                                       textvariable = myvar2,
                                       state = "readonly")

  tcltk::tkgrid(tt$tr$Varlist1, row = 2, column = 1, sticky = "w",
                padx = 5, pady = 5)
  tcltk::tkgrid(tt$tr$Varlist2, row = 3, column = 1, sticky = "w",
                padx = 5, pady = 5)

  ## minimum values ####

  tt$tr$minval1 <- tcltk::tkentry(tt$tr, width = "20",
                                     textvariable = minval1)
  tt$tr$minval2 <- tcltk::tkentry(tt$tr, width = "20",
                                     textvariable = minval2)

  tcltk::tkgrid(tt$tr$minval1, row = 2, column = 2, sticky = "w",
                padx = 5, pady = 5)
  tcltk::tkgrid(tt$tr$minval2, row = 3, column = 2, sticky = "w",
                padx = 5, pady = 5)

  ## maximum values ####

  tt$tr$maxval1 <- tcltk::tkentry(tt$tr, width = "20",
                                     textvariable = maxval1)
  tt$tr$maxval2 <- tcltk::tkentry(tt$tr, width = "20",
                                     textvariable = maxval2)

  tcltk::tkgrid(tt$tr$maxval1, row = 2, column = 3, sticky = "w",
                padx = 5, pady = 5)
  tcltk::tkgrid(tt$tr$maxval2, row = 3, column = 3, sticky = "w",
                padx = 5, pady = 5)

  ## end variable selection ####

  tcltk::tkgrid(tt$tr, columnspan = 3)
  myenv <- new.env()


  ## define buttons ####


  # on ok, create the following list:
  # var1, var2, var3, math1, math2, math3, val1, val2, val3

  onOk <- function() {
    var1 <- tcltk::tclvalue(myvar1)
    var2 <- tcltk::tclvalue(myvar2)

    min1 <- tcltk::tclvalue(minval1)
    min2 <- tcltk::tclvalue(minval2)

    max1 <- tcltk::tclvalue(maxval1)
    max2 <- tcltk::tclvalue(maxval2)

    tcltk::tkdestroy(tt)


    assign("agglist", list(var1 = var1, minval1 = min1, maxval1 = max1,
                           var2 = var2, minval2 = min2, maxval2 = max2),
           envir=myenv)
  }
  onCancel <- function() {
    tcltk::tkdestroy(tt)
    assign("agglist", list(var1 = "cancel", minval1 = 0, maxval1 = "none",
                           var2 = "NONE", minval2 = "none", maxval2 = "none"),
           envir=myenv)
  }
  onBack <- function() {
    tcltk::tkdestroy(tt)
    assign("agglist", list(var1 = "back", minval1 = 0, maxval1 = "none",
                           var2 = "NONE", minval2 = "none", maxval2 = "none"),
           envir=myenv)
  }
  onHelp <- function() {
    showGAThelp(help = hlp, helptitle = "aggregation variable settings",
                helppage = helppage, step = step)
  }

  ## draw buttons ####

  tt$tf <- tcltk::tkframe(tt)
  tt$tf$BackBut <- tcltk2::tk2button(tt$tf, text="< Back",
                                         width = 12, command = onBack)
  tt$tf$HelpBut <- tcltk2::tk2button(tt$tf, text="Help",
                                         width = 12, command = onHelp)
  tt$tf$OkBut <- tcltk2::tk2button(tt$tf, text = "Next >",
                                       width = 12, command = onOk,
                                       default = "active")
  tt$tf$CancelBut <- tcltk2::tk2button(tt$tf, text = "Cancel",
                                           width = 12, command = onCancel)

  tcltk::tkgrid(tt$tf$BackBut, column = 1, row = 1, padx = 10)
  tcltk::tkgrid(tt$tf$OkBut, column = 2, row = 1, padx = 10)
  tcltk::tkgrid(tt$tf$CancelBut, column = 3, row = 1, padx = 10)
  tcltk::tkgrid(tt$tf$HelpBut, column = 4, row = 1, padx = 10)
  tcltk::tkgrid(tt$tf, pady = 5)

  # wait ####
  tcltk::tkwait.window(tt)
  return(myenv$agglist)
}
