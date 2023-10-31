#' Input Multiple GAT Aggregation Variables
#'
#' @description
#' This function opens a dialog window for the user to select which variables
#' in the shapefile should be aggregated and to what minimum and maximum values.
#' The dialog window looks like this.
#'
#' \figure{inputGATaggregators.png}
#'
#' *Figure: Dialog to select your aggregators*
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
#' @param shp        Spatial layer.
#' @param step       Integer step in GAT, for help reference.
#' @param min1       Minimum value for the first aggregation variable.
#' @param min2       Minimum value for the second aggregation variable.
#' @param max1       Maximum value for the first aggregation variable.
#' @param max2       Maximum value for the second aggregation variable.
#' @param var1       Name of the first aggregation variable.
#' @param var2       Name of the second aggregation variable.
#' @param backopt    Boolean denoting whether to include the back button.
#' @param quitopt    Text string for the cancel button.
#' @param bgcol      Text string containing UI background color.
#' @param buttoncol  Text string containing UI button color.
#'
#' @examples
#'
#' if (interactive()) {
#' inputGATaggregators(shp = hftown)
#' }
#'
#' @export

inputGATaggregators <- function(shp, step = 4, min1 = "5,000", min2 = "none",
                                max1 = "none", max2 = "none", var1 = "",
                                var2 = "NONE", backopt = TRUE,
                                bgcol = "lightskyblue3", quitopt = "Quit",
                                buttoncol = "cornflowerblue") {
  ## define objects ----
  helppage <- "inputGATaggregators"
  hlp <- paste0("Select your aggregation variables. In the text boxes, \n",
                "enter your desired minimum and maximum values. \n",
                "  \u2022  To continue,  click 'Next >'. \n",
                "  \u2022  To return to aggregation variable selection,",
                "click '< Back'. \n", "  \u2022  To quit GAT, click '", quitopt, "'.")
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
  bgcol <- "lightskyblue3"
  buttoncol <- "cornflowerblue"

  fonthead <- tcltk::tkfont.create(family = "Segoe UI", size = 10, weight = "bold")
  mylist1 <- checkGATvariabletypes(shp, type = "number")
  mylist2 <- c("NONE", mylist1)
  myvar1 <- if (var1 %in% mylist1) tcltk::tclVar(var1) else tcltk::tclVar(mylist1[1])
  myvar2 <- if (var2 %in% mylist2) tcltk::tclVar(var2) else tcltk::tclVar("NONE")

  minval1 <- tcltk::tclVar(min1)
  minval2 <- tcltk::tclVar(min2)
  maxval1 <- tcltk::tclVar(max1)
  maxval2 <- tcltk::tclVar(max2)

  ## draw window ----
  tt <- tcltk::tktoplevel(background = bgcol)
  tcltk::tkwm.title(tt, paste0("Step ", step, ": Aggregation Variables"))
  tt$insttl <- tcltk::tklabel(tt, text = "Instructions", font = fonthead,
                              background = bgcol)
  tcltk::tkgrid(tt$insttl, sticky = "w", padx = 5, pady = 5)
  tcltk::tkgrid(tcltk::tklabel(tt, text = instruct, justify = "left",
                               background = bgcol),
                columnspan = 4, sticky = "w")

  tt$tr <- tcltk::tkframe(tt, background = bgcol)

  ## table headings ----
  tt$tr$varlabel = tcltk::tklabel(tt$tr, text = " Variable", font = fonthead,
                                  background = bgcol)
  tt$tr$minlabel = tcltk::tklabel(tt$tr, text = " Minimum value", font = fonthead,
                                  background = bgcol)
  tt$tr$maxlabel = tcltk::tklabel(tt$tr, text = " Maximum value", font = fonthead,
                                  background = bgcol)
  tcltk::tkgrid(tt$tr$varlabel, row = 1, column = 1, sticky = "w")
  tcltk::tkgrid(tt$tr$minlabel, row = 1, column = 2, sticky = "w")
  tcltk::tkgrid(tt$tr$maxlabel, row = 1, column = 3, sticky = "w")


  ## variable lists ----
  tt$tr$Varlist1 <- tcltk::ttkcombobox(tt$tr,  values = mylist1,
                    textvariable = myvar1, state = "readonly")
  tt$tr$Varlist2 <- tcltk::ttkcombobox(tt$tr, values = mylist2,
                    textvariable = myvar2, state = "readonly")

  tcltk::tkgrid(tt$tr$Varlist1, row = 2, column = 1, sticky = "w",
                padx = 5, pady = 5)
  tcltk::tkgrid(tt$tr$Varlist2, row = 3, column = 1, sticky = "w",
                padx = 5, pady = 5)

  ## minimum values ----
  tt$tr$minval1 <- tcltk::tkentry(tt$tr, width = "20", textvariable = minval1,
                                  background = "white")
  tt$tr$minval2 <- tcltk::tkentry(tt$tr, width = "20", textvariable = minval2,
                                  background = "white")

  tcltk::tkgrid(tt$tr$minval1, row = 2, column = 2, sticky = "w",
                padx = 5, pady = 5)
  tcltk::tkgrid(tt$tr$minval2, row = 3, column = 2, sticky = "w",
                padx = 5, pady = 5)

  ## maximum values ----
  tt$tr$maxval1 <- tcltk::tkentry(tt$tr, width = "20", textvariable = maxval1,
                                  background = "white")
  tt$tr$maxval2 <- tcltk::tkentry(tt$tr, width = "20", textvariable = maxval2,
                                  background = "white")

  tcltk::tkgrid(tt$tr$maxval1, row = 2, column = 3, sticky = "w", padx = 5,
                pady = 5)
  tcltk::tkgrid(tt$tr$maxval2, row = 3, column = 3, sticky = "w", padx = 5,
                pady = 5)

  ## end variable selection ----

  tcltk::tkgrid(tt$tr, columnspan = 3)
  myenv <- new.env()


  ## define buttons ----

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
    gatpkg::showGAThelp(help = hlp, helptitle = "aggregation variable settings",
                helppage = helppage, step = step, bgcol=bgcol,
                buttoncol=buttoncol)
  }

  ## draw buttons ----

  tt$tf <- tcltk::tkframe(tt, background = bgcol)
  if (backopt) {
    tt$tf$BackBut <- tcltk::tkbutton(tt$tf, text = "< Back",
                                     command = onBack, width = 12,
                                     background = buttoncol)
    tt$tf$OkBut <- tcltk::tkbutton(tt$tf, text = "Next >", command = onOk,
                                   width = 12, default = "active",
                                   background = buttoncol)
  } else {
    tt$tf$OkBut <- tcltk::tkbutton(tt$tf, text = "Confirm", command = onOk,
                                   width = 12, default = "active",
                                   background = buttoncol)
  }

  tt$tf$HelpBut <- tcltk::tkbutton(tt$tf, text="Help", width = 12,
                                   command = onHelp,
                                   background = buttoncol)
  tt$tf$CancelBut <- tcltk::tkbutton(tt$tf, text = quitopt,
                                     width = 12, command = onCancel,
                                     background = buttoncol)

  if (backopt) tcltk::tkgrid(tt$tf$BackBut, column = 1, row = 1, padx = 10)
  tcltk::tkgrid(tt$tf$OkBut, column = 2, row = 1, padx = 10)
  tcltk::tkgrid(tt$tf$CancelBut, column = 3, row = 1, padx = 10)
  tcltk::tkgrid(tt$tf$HelpBut, column = 4, row = 1, padx = 10)
  tcltk::tkgrid(tt$tf, pady = 5)

  # wait ####
  tcltk::tkwait.window(tt)
  return(myenv$agglist)
}
