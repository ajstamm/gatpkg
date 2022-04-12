#' Input GAT Merge
#'
#' @description
#' This function opens a dialog window for the user to select the desired
#' merge method. The dialog window looks like this.
#'
#' \figure{inputGATmerge.png}
#'
#' *Figure: Dialog to select your merge type*
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
#' if (interactive()) {
#' # make your selections and click "OK"
#' inputGATmerge(mapdata = hftown, aggvar = "TOTAL_POP", aggvar2 = "W_TOT")
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
    x <- data.frame(mapdata)[, numlistitems[i]]
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

  instruct <- paste(
    "    1. Select your merging method. \n",
    "   2. If you select the first or third option, also select your choice(s) \n",
    "       from the drop-down menu(s).  \n")
  rbValue <- tcltk::tclVar(mergevars$mergeopt1)
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


  # draw the window ####
  tt <- tcltk::tktoplevel()
  tcltk::tkwm.title(tt, paste0("Step ", step, ": Merging method"))

  tt$inst <- tcltk::tkframe(tt, width = 300, height = 5)
  tt$inst$inst <- tcltk2::tk2label(tt$inst, text = "Instructions", font = "fonthead")
  tt$inst$title <- tcltk2::tk2label(tt$inst, text = "Merge options", font = "fonthead")
    # instructions layout ####
  tcltk::tkgrid(tt$inst$inst, sticky = "w", padx = 1, pady = 5)
  tcltk::tkgrid(tcltk2::tk2label(tt$inst, text = instruct), columnspan = 4,
                sticky = "w")
  tcltk::tkgrid(tt$inst$title, sticky = "w", padx = 1, pady = 5)
  tcltk::tkgrid(tt$inst, columnspan = 2, pady = 5)

  tt$opts <- tcltk::tkframe(tt, width = 300, height = 5)

  # option 1: "closest" ####
  tt$opts$ts <- tcltk::tkframe(tt$opts)
  tt$opts$ts$closebut <- tcltk::tkradiobutton(tt$opts$ts)
  tcltk::tkconfigure(tt$opts$ts$closebut, variable = rbValue, value = "closest")
  centroidlist <- c("geographic", "population-weighted")
  centroidval <- tcltk::tclVar(mergevars$centroid)
  tt$opts$ts$closelab <- tcltk2::tk2label(tt$opts$ts, text = "closest area by")
  tt$opts$ts$closelist <- tcltk::ttkcombobox(tt$opts$ts, values = centroidlist,
                                            textvariable = centroidval,
                                            state = "readonly")
    # option 1 layout ####
  tcltk::tkgrid(tt$opts$ts$closebut, tt$opts$ts$closelab,
                tt$opts$ts$closelist,
                tcltk2::tk2label(tt$opts$ts, text = "centroid"),
                sticky = "w")
  tcltk::tkgrid.configure(tt$opts$ts$closebut, sticky = "w")
  tcltk::tkgrid.configure(tt$opts$ts, sticky = "w", padx = 20)

    # option 1 note ####
  note <- paste("         (note: selecting population weighting will open a",
                "dialog to \n",
                "         select a population shapefile)")
  tcltk::tkgrid(tcltk2::tk2label(tt$opts, text = note), columnspan = 2,
                sticky = "w", padx = 20)


  # option 2: "least" ####
  tt$opts$tl <- tcltk::tkframe(tt$opts)
  tt$opts$tl$rb2 <- tcltk::tkradiobutton(tt$opts$tl)
  tcltk::tkconfigure(tt$opts$tl$rb2, variable = rbValue, value = "least")
  if (!aggvar2 %in% c(aggvar, "NONE")) {
    msg <- paste("area with least", aggvar, "and/or", aggvar2)
  } else {
    msg <- paste("area with least", aggvar)
  } # tt$env$lab2
  tt$opts$tl$lab2 <- tcltk2::tk2label(tt$opts$tl, text = msg)
    # option 2 layout ####
  tcltk::tkgrid(tt$opts$tl$rb2, tt$opts$tl$lab2)
  tcltk::tkgrid.configure(tt$opts$tl$rb2, sticky = "w")
  tcltk::tkgrid.configure(tt$opts$tl$lab2, sticky = "w")
  tcltk::tkgrid.configure(tt$opts$tl, sticky = "w", padx = 20)

  # option 3: "similar" ####
  tt$opts$tr1 <- tcltk::tkframe(tt$opts)
  tt$opts$tr2 <- tcltk::tkframe(tt$opts)
  tt$opts$tr1$rb3 <- tcltk::tkradiobutton(tt$opts$tr1)
  tcltk::tkconfigure(tt$opts$tr1$rb3, variable = rbValue, value = "similar")

  tt$opts$tr1$varnum <- tcltk::ttkcombobox(tt$opts$tr1, values = numlistitems,
                                         textvariable = simvar1,
                                         state = "readonly")
  tt$opts$tr2$varden <- tcltk::ttkcombobox(tt$opts$tr2, values = numlistitems2,
                                         textvariable = simvar2,
                                         state = "readonly")
  tt$opts$tr1$msg <- tcltk2::tk2label(tt$opts$tr1, text = "area with most similar ratio of ")
    # option 3 layout ####
  tcltk::tkgrid(tt$opts$tr1$rb3, tt$opts$tr1$msg, tt$opts$tr1$varnum,
                sticky = "w")
  tcltk::tkgrid.configure(tt$opts$tr1$rb3, sticky = "w")

  tcltk::tkgrid(tcltk2::tk2label(tt$opts$tr2, text = "        to"),
                tt$opts$tr2$varden, sticky = "w")
  tcltk::tkgrid(tt$opts$tr1, sticky = "w", padx = 20)
  tcltk::tkgrid(tt$opts$tr2, sticky = "w", padx = 20)
    # option 3 note ####
  note <- paste("       (note: the numerator and denominator must be different;",
                "\n       variables with 0 or missings cannot be in the denominator)")
  tcltk::tkgrid(tcltk2::tk2label(tt$opts, text = note), columnspan = 4,
                sticky = "w", padx = 20)

  tcltk::tkgrid(tt$opts, columnspan = 2, pady = 5)

  # create variable environment ####
  myenv <- new.env()
  hlp <- paste0("Select your desired option. If you select 'Similar', \n",
                "please also select the ratio variables. \n",
                "  \u2022  To continue,  click 'Next >'. \n",
                "  \u2022  To return to exclusions selection, click '< Back'. \n",
                "  \u2022  To quit GAT, click 'Cancel'.")

  # button functions ####
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
    # buttons ####
  tt$tfbuts <- tcltk::tkframe(tt)
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

  tt$tfbuts$HelpBut <- tcltk2::tk2button(tt$tfbuts, text="Help",
                                         width = 12, command = onHelp)
  tt$tfbuts$CancelBut <- tcltk2::tk2button(tt$tfbuts, text = "Cancel GAT",
                                           width = 12, command = onCancel)
    # button layout ####
  if (backopt) {
    tcltk::tkgrid(tt$tfbuts$BackBut, column = 1, row = 1, padx = 10)
  }
  tcltk::tkgrid(tt$tfbuts$OkBut, column = 2, row = 1, padx = 2)
  tcltk::tkgrid(tt$tfbuts$CancelBut, column = 3, row = 1, padx = 2)
  tcltk::tkgrid(tt$tfbuts$HelpBut, column = 4, row = 1, padx = 2)
  tcltk::tkgrid(tt$tfbuts, columnspan = 2, pady = 5)

  # wait to continue ####
  tcltk::tkwait.window(tt)

  return(myenv$vars)
}
