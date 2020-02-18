#' Input GAT Merge
#'
#' @description
#' This function opens a dialog window for the user to select the desired
#' merge method. The dialog window looks like this.
#'
#' \if{html}{\figure{inputGATmerge.png}{options: width="340px"
#'                   alt="Figure: Screenshot of dialog to select your merge type"}}
#'
#' Select your desired merge type. If you select "similar ratio", you will also
#' need to supply two variables. Then click on one of the following buttons.
#'
#' * Click \code{Next} to continue to the next step.
#' * Click \code{Cancel} to end GAT.
#' * Click \code{Back} to return to the previous step.
#' * Click \code{Help} to get further guidance and open this manual.
#'
#' @details
#' For the "similar" method, in which two variables are compared, the function
#' reads in a list of numeric variables from the dataset. For the "least"
#' method, the function reads in the names of the aggregation variables.
#'
#' This function returns a list with the following elements:
#'
#' \itemize{\bold{mergeopt1: }
#'   The merge option chosen. Options include "least", "closest", and
#'   "similar".
#' }
#' \itemize{\bold{similar1: }
#'   The numerator in the ratio that compares variables if "similar" is the
#'   method chosen. This variable will also hold the value for "back" or
#'   "cancel", if the user selects one of those.
#' }
#' \itemize{\bold{similar2: }
#'   The denominator in the ratio that compares variables if "similar" is the
#'   method chosen. The list of options removes variables with values that
#'   equal 0 or non-finite values. If there are no suitable variables, the
#'   list will be blank.
#' }
#'
#' @param mapdata    The data frame.
#' @param aggvar     A string denoting the name of the first aggregation
#'                   variable in the data frame.
#' @param aggvar2    A string denoting the name of the second aggregation
#'                   variable in the data frame.
#' @param step       Integer step in the GAT program, for help reference.
#' @param limitdenom Boolean denoting whether to force denominators in rates
#'                   and ratios to contain only non-zero values.
#' @param mergevars  List of variables created by the function if pre-defined.
#' @param backopt    Boolean denoting whether to include the back button.
#'
#' @examples
#'
#' \donttest{
#' # make your selections and click "OK"
#' inputGATmerge(
#'   mapdata = hftown@data,
#'   aggvar = "TOTAL_POP",
#'   aggvar2 = "W_TOT"
#' )
#' }
#'
#' @export

inputGATmerge <- function(mapdata, aggvar, aggvar2, step = 8,
                          limitdenom = TRUE, mergevars = NULL,
                          backopt = TRUE) {

  # create variable lists ####
  numlistitems <- checkGATvariabletypes(mapdata, type = "number")
  helppage = "inputGATmerge"

  idlist <- c()
  for (i in 1:length(numlistitems)) {
    x <- mapdata[, numlistitems[i]]
    t <- table(x==0 | !is.finite(x))
    idlist[i] <- grepl("TRUE", paste(names(t), collapse = " "))
  }
  if (limitdenom) {
    numlistitems2 <- numlistitems[idlist == FALSE] # denominator
  } else {
    numlistitems2 <- numlistitems
  }

  if (is.null(mergevars) | !exists("mergevars")) {
    mergevars <- list(similar1 = "NONE", similar2 = "NONE",
                      mergeopt1 = 0, centroid = "geographic")
  }


  # draw the window ####
  tt <- tcltk::tktoplevel()
  tcltk::tkwm.title(tt, paste0("Step ", step, ": Merging method"))
  tcltk::tkgrid(tcltk2::tk2label(tt, text = "Merge each area with the"),
                columnspan = 2, sticky = "w")
  rbValue <- tcltk::tclVar(mergevars$mergeopt1)

  # option 1: "closest" ####
  tt$env$closebut <- tcltk::tkradiobutton(tt)
  tcltk::tkconfigure(tt$env$closebut, variable = rbValue, value = "closest")
  centroidlist <- c("geographic", "population-weighted")
  centroidval <- tcltk::tclVar(mergevars$centroid)
  tt$env$ts <- tcltk::tkframe(tt)

  tt$env$ts$closelab <- tcltk2::tk2label(tt$env$ts, text = "closest area by")
  tt$env$ts$closelist <- tcltk::ttkcombobox(tt$env$ts, values = centroidlist,
                                            textvariable = centroidval,
                                            state = "readonly")
  tcltk::tkgrid(tt$env$ts$closelab, tt$env$ts$closelist,
                tcltk2::tk2label(tt$env$ts, text = "centroid"), sticky = "w")

  tcltk::tkgrid(tt$env$closebut, tt$env$ts)
  tcltk::tkgrid.configure(tt$env$closebut, sticky = "e")
  tcltk::tkgrid.configure(tt$env$ts, sticky = "w")

  note <- paste("(note: selecting population weighting will open a dialog",
                "to select a population shapefile)")
  tcltk::tkgrid(tcltk2::tk2label(tt, text = note), column = 2-1, columnspan = 4)


  # option 2: "least" ####
  tt$env$rb2 <- tcltk::tkradiobutton(tt)
  tcltk::tkconfigure(tt$env$rb2, variable = rbValue, value = "least")
  if (!aggvar2 %in% c(aggvar, "NONE")) {
    msg <- paste("area with least", aggvar, "and/or", aggvar2)
  } else {
    msg <- paste("area with least", aggvar)
  } # tt$env$lab2
  tt$env$lab2 <- tcltk2::tk2label(tt, text = msg)

  tcltk::tkgrid(tt$env$rb2, tt$env$lab2)
  tcltk::tkgrid.configure(tt$env$rb2, sticky = "e")
  tcltk::tkgrid.configure(tt$env$lab2, sticky = "w")

  # option 3: "similar" ####
  # set default similar variables to none
  tt$env$rb3 <- tcltk::tkradiobutton(tt)
  tcltk::tkconfigure(tt$env$rb3, variable = rbValue, value = "similar")

  if (mergevars$similar1 %in% names(mapdata)) {
    simvar1 <- tcltk::tclVar(mergevars$similar1)
  } else {
    simvar1 <- tcltk::tclVar(numlistitems[1])
  }
  if (mergevars$similar2 %in% names(mapdata)) {
    simvar2 <- tcltk::tclVar(mergevars$similar2)
  } else {
    simvar2 <- tcltk::tclVar(numlistitems2[1])
  }
  tt$env$tr <- tcltk::tkframe(tt)

  tt$env$tr$varnum <- tcltk::ttkcombobox(tt$env$tr, values = numlistitems,
                                         textvariable = simvar1,
                                         state = "readonly")
  tt$env$tr$varden <- tcltk::ttkcombobox(tt$env$tr, values = numlistitems2,
                                         textvariable = simvar2,
                                         state = "readonly")

  tt$env$tr$msg <- tcltk2::tk2label(tt$env$tr, text = "area with most similar ratio of ")
  tcltk::tkgrid(tt$env$tr$msg, tt$env$tr$varnum,
                tcltk2::tk2label(tt$env$tr, text = "to"), tt$env$tr$varden,
                tcltk2::tk2label(tt$env$tr, text = "  "))

  note <- "(note: variables with 0 or missings cannot be in the denominator)"
  tcltk::tkgrid(tcltk2::tk2label(tt$env$tr, text = note), columnspan = 4,
                sticky = "w")
  tcltk::tkgrid(tt$env$rb3, tt$env$tr)
  tcltk::tkgrid.configure(tt$env$rb3, sticky = "ne")
  tcltk::tkgrid.configure(tt$env$tr, sticky = "w")

  # create variable environment ####
  myenv <- new.env()
  hlp <- paste0("Select your desired option. If you select 'Similar', \n",
                "please also select the ratio variables. \n",
                "  \u2022  To continue,  click 'Next >'. \n",
                "  \u2022  To return to exclusions selection, click '< Back'. \n",
                "  \u2022  To quit GAT, click 'Cancel'.")

  # button functions and layout ####
  onBack <- function() {
    Rbval <- tcltk::tclvalue(rbValue)
    tcltk::tkdestroy(tt)
    assign("vars", list(similar1 = "back", similar2 = "NONE",
                        mergeopt1 = Rbval, centroid = "geographic"), envir=myenv)
  }
  onOk <- function() {
    Rbval <- tcltk::tclvalue(rbValue)
    combo1 <- tcltk::tclvalue(simvar1)
    combo2 <- tcltk::tclvalue(simvar2)
    centype <- tcltk::tclvalue(centroidval)
    tcltk::tkdestroy(tt)

    if (Rbval != "similar") {
      combo1 <- combo2 <- "NONE"
    }
    if (Rbval != "closest") {
      centype <- "geographic"
    }

    assign("vars", list(similar1 = combo1, similar2 = combo2,
                        mergeopt1 = Rbval, centroid = centype), envir=myenv)
  }
  onHelp <- function() {
    showGAThelp(help = hlp, helptitle = helppage, helppage = helppage, step = step)
  }
  onCancel <- function() {
    Rbval <- tcltk::tclvalue(rbValue)
    tcltk::tkdestroy(tt)
    assign("vars", list(similar1 = "cancel", similar2 = "NONE",
                        mergeopt1 = Rbval, centroid = "geographic"), envir=myenv)
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
  tcltk::tkgrid(tt$env$tf, columnspan = 2, pady = 5)

  # wait to continue ####
  tcltk::tkwait.window(tt)

  return(myenv$vars)
}
