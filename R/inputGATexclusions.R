#' Input GAT Exclusions
#'
#' @description
#' This function opens a dialog window for the user to select the variables
#' to be used when identifying exclusions.
#'
#' \if{html}{\figure{inputGATexclusions.png}{options: width="450px"
#'                   alt="Figure: Screenshot of dialog to select your rate settings"}}
#'
#' Select your desired variables and the exclusion criterion for each.
#'
#' * Click \code{Next} to continue to the next step.
#' * Click \code{Cancel} to end GAT.
#' * Click \code{Back} to return to the previous step.
#' * Click \code{Help} to get further guidance and open this manual.
#'
#'
#' @details
#' Change the limitdenom option from FALSE to TRUE in step 9 (requesting rate
#' calculation information) in \code{\link{runGATprogram}} if you want to
#' allow only denominators with finite, non-zero values.
#'
#' This function is very basic and can only handle numeric variables, so if you
#' need to exclude based on a character variable, such as county name, you will
#' need to create a numeric flag variable to use in this step.
#'
#' If you have more than three exclusion criteria, you will need to create a
#' composite flag variable that you can use in this step.
#'
#' This function returns a list with the following nine elements:
#'
#' \itemize{\bold{var1, var2, var3: }
#'   The names of the three variables to use when determining exclusions.
#' }
#' \itemize{\bold{math1, math2, math3: }
#'   The mathematical operators to use when calculating exclusions. Options
#'   are "equals", "less than", and "greater than".
#' }
#' \itemize{\bold{val1, val2, val3: }
#'   The values to use in the exclusion calculations.
#' }
#'
#' @param mapdata The data frame from which to select variables.
#' @param step    Integer step in the GAT program, for help reference.
#' @param exclist The list of exclusion criteria, if pre-defined.
#' @param backopt Boolean denoting whether to include the back button.
#'
#' @examples
#'
#' \donttest{
#' # define exclusion criteria
#' inputGATexclusions(
#'   mapdata = hftown@data,
#'   step = 10
#' )
#' }
#'
#' @export

inputGATexclusions <- function(mapdata, step = 0, exclist = NULL,
                               backopt = TRUE) {

  ## define objects ####

  helppage <- "inputGATexclusions"
  hlp <- paste0("Select your first aggregation variable. In the text box, \n",
                "enter your desired minimum value. \n",
                "  \u2022  To continue,  click 'Next >'. \n",
                "  \u2022  To return to aggregation variable selection,",
                "click '< Back'. \n", "  \u2022  To quit GAT, click 'Cancel'.")

  mylist <- checkGATvariabletypes(mapdata, type = "number")
  mylist <- c("NONE", mylist)
  mathlist <- c("equals", "less than", "greater than")

  instruct <- paste("   1. Select each variable on which you would like to exclude. \n",
                    "   2. Select the direction of each exclusion. \n",
                    "   3. Enter a numeric value for each exclusion. \n",
                    "\nTo ignore an option, select 'NONE' for the variable name. \n")
  fonthead <- tcltk2::tk2font.set(font = "fonthead",
                                  settings = list(family = "Segoe UI", size = 10,
                                                  bold = TRUE, italic = FALSE))

  if (is.null(exclist)) {
    myvar1 <- tcltk::tclVar("NONE")
    myvar2 <- tcltk::tclVar("NONE")
    myvar3 <- tcltk::tclVar("NONE")

    mymath1 <- tcltk::tclVar("equals")
    mymath2 <- tcltk::tclVar("equals")
    mymath3 <- tcltk::tclVar("equals")

    myvalue1 <- tcltk::tclVar(0)
    myvalue2 <- tcltk::tclVar(0)
    myvalue3 <- tcltk::tclVar(0)
  } else {
    if (exclist$var1 %in% c("back", "cancel", "repeat")) {
      exclist$var1 <- "NONE"
    }
    if (exclist$var2 %in% c("back", "cancel", "repeat")) {
      exclist$var2 <- "NONE"
    }
    if (exclist$var3 %in% c("back", "cancel", "repeat")) {
      exclist$var3 <- "NONE"
    }
    myvar1 <- tcltk::tclVar(exclist$var1)
    myvar2 <- tcltk::tclVar(exclist$var2)
    myvar3 <- tcltk::tclVar(exclist$var3)

    mymath1 <- tcltk::tclVar(exclist$math1)
    mymath2 <- tcltk::tclVar(exclist$math2)
    mymath3 <- tcltk::tclVar(exclist$math3)

    myvalue1 <- tcltk::tclVar(exclist$val1)
    myvalue2 <- tcltk::tclVar(exclist$val2)
    myvalue3 <- tcltk::tclVar(exclist$val3)
  }

  ## draw window ####

  tt <- tcltk::tktoplevel()
  tcltk::tkwm.title(tt, paste0("Step ", step, ": Exclusions"))

  tt$insttl <- tcltk2::tk2label(tt, text = "Instructions", font = "fonthead")
  tcltk::tkgrid(tt$insttl, sticky = "w", padx = 5, pady = 5)
  tcltk::tkgrid(tcltk2::tk2label(tt, text = instruct), columnspan = 4,
                sticky = "w")

  tt$env$tr <- tcltk::tkframe(tt)

  ## variable lists ####

  tt$env$tr$Varlist1 <- tcltk::ttkcombobox(tt$env$tr,
                                           values = mylist,
                                           textvariable = myvar1,
                                           state = "readonly")
  tt$env$tr$Varlist2 <- tcltk::ttkcombobox(tt$env$tr,
                                           values = mylist,
                                           textvariable = myvar2,
                                           state = "readonly")
  tt$env$tr$Varlist3 <- tcltk::ttkcombobox(tt$env$tr,
                                           values = mylist,
                                           textvariable = myvar3,
                                           state = "readonly")

  tcltk::tkgrid(tt$env$tr$Varlist1, row = 1, column = 1, sticky = "w",
                padx = 5, pady = 5)
  tcltk::tkgrid(tt$env$tr$Varlist2, row = 2, column = 1, sticky = "w",
                padx = 5, pady = 5)
  tcltk::tkgrid(tt$env$tr$Varlist3, row = 3, column = 1, sticky = "w",
                padx = 5, pady = 5)

  ## math lists ####

  tt$env$tr$Mathlist1 <- tcltk::ttkcombobox(tt$env$tr,
                                            values = mathlist,
                                            textvariable = mymath1,
                                            state = "readonly")
  tt$env$tr$Mathlist2 <- tcltk::ttkcombobox(tt$env$tr,
                                            values = mathlist,
                                            textvariable = mymath2,
                                            state = "readonly")
  tt$env$tr$Mathlist3 <- tcltk::ttkcombobox(tt$env$tr,
                                            values = mathlist,
                                            textvariable = mymath3,
                                            state = "readonly")

  tcltk::tkgrid(tt$env$tr$Mathlist1, row = 1, column = 2, sticky = "w",
                padx = 5, pady = 5)
  tcltk::tkgrid(tt$env$tr$Mathlist2, row = 2, column = 2, sticky = "w",
                padx = 5, pady = 5)
  tcltk::tkgrid(tt$env$tr$Mathlist3, row = 3, column = 2, sticky = "w",
                padx = 5, pady = 5)

  ## value boxes ####

  tt$env$tr$Value1 <- tcltk::tkentry(tt$env$tr, width = "20",
                                     textvariable = myvalue1)
  tt$env$tr$Value2 <- tcltk::tkentry(tt$env$tr, width = "20",
                                     textvariable = myvalue2)
  tt$env$tr$Value3 <- tcltk::tkentry(tt$env$tr, width = "20",
                                     textvariable = myvalue3)

  tcltk::tkgrid(tt$env$tr$Value1, row = 1, column = 3, sticky = "w",
                padx = 5, pady = 5)
  tcltk::tkgrid(tt$env$tr$Value2, row = 2, column = 3, sticky = "w",
                padx = 5, pady = 5)
  tcltk::tkgrid(tt$env$tr$Value3, row = 3, column = 3, sticky = "w",
                padx = 5, pady = 5)

  ## define buttons ####


  tcltk::tkgrid(tt$env$tr, columnspan = 3)
  myenv <- new.env()

  # on ok, create the following list:
  # var1, var2, var3, math1, math2, math3, val1, val2, val3

  onOk <- function() {
    var1 <- tcltk::tclvalue(myvar1)
    var2 <- tcltk::tclvalue(myvar2)
    var3 <- tcltk::tclvalue(myvar3)

    math1 <- tcltk::tclvalue(mymath1)
    math2 <- tcltk::tclvalue(mymath2)
    math3 <- tcltk::tclvalue(mymath3)

    val1 <- tcltk::tclvalue(myvalue1)
    val2 <- tcltk::tclvalue(myvalue2)
    val3 <- tcltk::tclvalue(myvalue3)

    tcltk::tkdestroy(tt)

    if (var1 == "NONE") {
      math1 <- "equals"
      val1 <- 0
    }
    if (var2 == "NONE") {
      math2 <- "equals"
      val2 <- 0
    }
    if (var3 == "NONE") {
      math3 <- "equals"
      val3 <- 0
    }

    assign("exclist", list(var1 = var1, math1 = math1, val1 = val1,
                           var2 = var2, math2 = math2, val2 = val2,
                           var3 = var3, math3 = math3, val3 = val3),
           envir=myenv)
  }
  onCancel <- function() {
    tcltk::tkdestroy(tt)
    assign("exclist", list(var1 = "cancel", math1 = "equals", val1 = 0,
                           var2 = "cancel", math2 = "equals", val2 = 0,
                           var3 = "cancel", math3 = "equals", val3 = 0),
           envir=myenv)
  }
  onBack <- function() {
    tcltk::tkdestroy(tt)
    assign("exclist", list(var1 = "back", math1 = "equals", val1 = 0,
                           var2 = "back", math2 = "equals", val2 = 0,
                           var3 = "back", math3 = "equals", val3 = 0),
           envir=myenv)
  }
  onHelp <- function() {
    showGAThelp(help = hlp, helptitle = "exclusion settings",
                helppage = helppage, step = step)
  }

  ## draw buttons ####

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

  tt$env$tf$HelpBut <- tcltk2::tk2button(tt$env$tf, text="Help",
                                         width = 12, command = onHelp)
  tt$env$tf$CancelBut <- tcltk2::tk2button(tt$env$tf, text = "Cancel GAT",
                                           width = 12, command = onCancel)

  if (backopt) {
    tcltk::tkgrid(tt$env$tf$BackBut, column = 1, row = 1, padx = 10)
  }
  tcltk::tkgrid(tt$env$tf$OkBut, column = 2, row = 1, padx = 10)
  tcltk::tkgrid(tt$env$tf$CancelBut, column = 3, row = 1, padx = 10)
  tcltk::tkgrid(tt$env$tf$HelpBut, column = 4, row = 1, padx = 10)
  tcltk::tkgrid(tt$env$tf, pady = 5)

  ## return selections ####

  tcltk::tkwait.window(tt)

  return(myenv$exclist)

}
