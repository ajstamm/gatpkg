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
#' @param shp        Spatial layer.
#' @param aggvar     String denoting the name of the first aggregation
#'                   variable in the data frame.
#' @param aggvar2    String denoting the name of the second aggregation
#'                   variable in the data frame.
#' @param step       Integer step in the GAT program, for help reference.
#' @param limitdenom Boolean denoting whether to force denominators in rates
#'                   and ratios to contain only non-zero values.
#' @param mergevars  List of variables created by the function if pre-defined.
#' @param backopt    Boolean denoting whether to include the back button.
#' @param quitopt    Text string for the cancel button.
#' @param bgcol      Text string containing UI background color.
#' @param buttoncol  Text string containing UI button color.
#'
#' @examples
#'
#' if (interactive()) {
#' # make your selections and click "OK"
#' inputGATmerge(shp = hftown, aggvar = "TOTAL_POP", aggvar2 = "W_TOT")
#' }
#'
#' @export

inputGATmerge <- function(shp, aggvar, aggvar2, step = 8, limitdenom = TRUE,
                          mergevars = NULL, backopt = TRUE, quitopt = "Quit",
                          bgcol = "lightskyblue3", buttoncol = "cornflowerblue") {
  # create variable lists ----
  nums <- checkGATvariabletypes(shp, type = "number")
  helppage <- "inputGATmerge"

  idlist <- c()
  for (i in 1:length(nums)) {
    x <- data.frame(shp)[, nums[i]]
    t <- table(x==0 | !is.finite(x))
    idlist[i] <- grepl("TRUE", paste(names(t), collapse = " "))
  }
  nums2 <- if (limitdenom) nums[idlist == FALSE] else nums
  if (is.null(mergevars) | !exists("mergevars")) {
    mergevars <- list(similar1 = "NONE", similar2 = "NONE",
                      mergeopt1 = 0, centroid = "geographic")
  }

  fonthead <- tcltk::tkfont.create(family = "Segoe UI", size = 10, weight = "bold")
  instruct <- paste(
    "    1. Select your merging method. \n",
    "   2. If you select the first or third option, also select your choice(s) \n",
    "       from the drop-down menu(s).  \n")
  rbValue <- tcltk::tclVar(mergevars$mergeopt1)
  if (mergevars$similar1 %in% names(shp)) {
    simvar1 <- tcltk::tclVar(mergevars$similar1)
  } else simvar1 <- tcltk::tclVar(nums[1])
  if (mergevars$similar2 %in% names(shp)) {
    simvar2 <- tcltk::tclVar(mergevars$similar2)
  } else simvar2 <- tcltk::tclVar(nums2[1])

  # draw the window ----
  tt <- tcltk::tktoplevel(background = bgcol)
  tcltk::tkwm.title(tt, paste0("Step ", step, ": Merging method"))

  tt$inst <- tcltk::tkframe(tt, width = 300, height = 5, background = bgcol)
  tt$inst$inst <- tcltk::tklabel(tt$inst, text = "Instructions", font = fonthead,
                                 background = bgcol)
  tt$inst$title <- tcltk::tklabel(tt$inst, text = "Merge options", font = fonthead,
                                  background = bgcol)
    # instructions layout ----
  tcltk::tkgrid(tt$inst$inst, sticky = "w", padx = 1, pady = 5)
  tcltk::tkgrid(tcltk::tklabel(tt$inst, text = instruct, justify = "left",
                               background = bgcol),
                columnspan = 4, sticky = "w")
  tcltk::tkgrid(tt$inst$title, sticky = "w", padx = 1, pady = 5)
  tcltk::tkgrid(tt$inst, columnspan = 2, pady = 5)

  tt$opts <- tcltk::tkframe(tt, width = 300, height = 5, background = bgcol)

  # option 1: "closest" ----
  tt$opts$ts <- tcltk::tkframe(tt$opts, background = bgcol)
  tt$opts$ts$closebut <- tcltk::tkradiobutton(tt$opts$ts, background = bgcol)
  tcltk::tkconfigure(tt$opts$ts$closebut, variable = rbValue, value = "closest")
  centroidlist <- c("geographic", "population-weighted")
  centroidval <- tcltk::tclVar(mergevars$centroid)
  tt$opts$ts$closelab <- tcltk::tklabel(tt$opts$ts, text = "closest area by",
                                        background = bgcol)
  tt$opts$ts$closelist <- tcltk::ttkcombobox(tt$opts$ts, values = centroidlist,
                                            textvariable = centroidval,
                                            state = "readonly")
    # option 1 layout ----
  tcltk::tkgrid(tt$opts$ts$closebut, tt$opts$ts$closelab,
                tt$opts$ts$closelist,
                tcltk::tklabel(tt$opts$ts, text = "centroid",
                               background = bgcol), sticky = "w")
  tcltk::tkgrid.configure(tt$opts$ts$closebut, sticky = "w")
  tcltk::tkgrid.configure(tt$opts$ts, sticky = "w", padx = 20)

    # option 1 note ----
  note <- paste("         (note: selecting population weighting will open a",
                "dialog to \n", "         select a population shapefile)")
  tcltk::tkgrid(tcltk::tklabel(tt$opts, text = note, justify = "left",
                               background = bgcol),
                columnspan = 2, sticky = "w", padx = 20)

  # option 2: "least" ----
  tt$opts$tl <- tcltk::tkframe(tt$opts, background = bgcol)
  tt$opts$tl$rb2 <- tcltk::tkradiobutton(tt$opts$tl, background = bgcol)
  tcltk::tkconfigure(tt$opts$tl$rb2, variable = rbValue, value = "least")
  if (!aggvar2 %in% c(aggvar, "NONE")) {
    msg <- paste("area with least", aggvar, "and/or", aggvar2)
  } else {
    msg <- paste("area with least", aggvar)
  } # tt$env$lab2
  tt$opts$tl$lab2 <- tcltk::tklabel(tt$opts$tl, text = msg, background = bgcol)
    # option 2 layout ----
  tcltk::tkgrid(tt$opts$tl$rb2, tt$opts$tl$lab2)
  tcltk::tkgrid.configure(tt$opts$tl$rb2, sticky = "w")
  tcltk::tkgrid.configure(tt$opts$tl$lab2, sticky = "w")
  tcltk::tkgrid.configure(tt$opts$tl, sticky = "w", padx = 20)

  # option 3: "similar" ----
  tt$opts$tr1 <- tcltk::tkframe(tt$opts, background = bgcol)
  tt$opts$tr2 <- tcltk::tkframe(tt$opts, background = bgcol)
  tt$opts$tr1$rb3 <- tcltk::tkradiobutton(tt$opts$tr1, background = bgcol)
  tcltk::tkconfigure(tt$opts$tr1$rb3, variable = rbValue, value = "similar")

  tt$opts$tr1$varnum <- tcltk::ttkcombobox(tt$opts$tr1, values = nums,
                                           textvariable = simvar1,
                                           state = "readonly")
  tt$opts$tr2$varden <- tcltk::ttkcombobox(tt$opts$tr2, values = nums2,
                                           textvariable = simvar2,
                                           state = "readonly")
  tt$opts$tr1$msg <- tcltk::tklabel(tt$opts$tr1, text = "area with most similar ratio of ",
                                    background = bgcol)
    # option 3 layout ----
  tcltk::tkgrid(tt$opts$tr1$rb3, tt$opts$tr1$msg, tt$opts$tr1$varnum,
                sticky = "w")
  tcltk::tkgrid.configure(tt$opts$tr1$rb3, sticky = "w")

  tcltk::tkgrid(tcltk::tklabel(tt$opts$tr2, text = "        to", justify = "left",
                               background = bgcol),
                tt$opts$tr2$varden, sticky = "w")
  tcltk::tkgrid(tt$opts$tr1, sticky = "w", padx = 20)
  tcltk::tkgrid(tt$opts$tr2, sticky = "w", padx = 20)
    # option 3 note ----
  note <- paste("       (note: the numerator and denominator must be different;",
                "\n       variables with 0 or missings cannot be in the denominator)")
  tcltk::tkgrid(tcltk::tklabel(tt$opts, text = note, justify = "left",
                               background = bgcol),
                columnspan = 4, sticky = "w", padx = 20)

  tcltk::tkgrid(tt$opts, columnspan = 2, pady = 5)

  # create variable environment ----
  myenv <- new.env()
  hlp <- paste0("Select your desired option. If you select 'Similar', \n",
                "please also select the ratio variables. \n",
                "  \u2022  To continue,  click 'Next >'. \n",
                "  \u2022  To return to exclusions selection, click '< Back'. \n",
                "  \u2022  To quit GAT, click '", quitopt, "'.")

  # button functions ----
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

    if (Rbval != "similar") combo1 <- combo2 <- "NONE"
    if (Rbval != "closest") centype <- "geographic"

    assign("vars", list(similar1 = combo1, similar2 = combo2,
                        mergeopt1 = Rbval, centroid = centype), envir=myenv)
  }
  onHelp <- function() {
    gatpkg::showGAThelp(help = hlp, helptitle = helppage, helppage = helppage,
                        step = step, bgcol=bgcol, buttoncol=buttoncol)
  }
  onCancel <- function() {
    Rbval <- tcltk::tclvalue(rbValue)
    tcltk::tkdestroy(tt)
    assign("vars", list(similar1 = "cancel", similar2 = "NONE",
                        mergeopt1 = Rbval, centroid = "geographic"), envir=myenv)
  }
    # buttons ----
  tt$tfbuts <- tcltk::tkframe(tt, background = bgcol)
  if (backopt) {
    tt$tfbuts$BackBut <- tcltk::tkbutton(tt$tfbuts, text = "< Back",
                                         command = onBack, width = 12,
                                         background = buttoncol)
    tt$tfbuts$OkBut <- tcltk::tkbutton(tt$tfbuts, text = "Next >", width = 12,
                                       command = onOk, default = "active",
                                       background = buttoncol)
  } else {
    tt$tfbuts$OkBut <- tcltk::tkbutton(tt$tfbuts, text = "Confirm", width = 12,
                                       command = onOk, default = "active",
                                       background = buttoncol)
  }

  tt$tfbuts$HelpBut <- tcltk::tkbutton(tt$tfbuts, text="Help",
                                       width = 12, command = onHelp,
                                       background = buttoncol)
  tt$tfbuts$CancelBut <- tcltk::tkbutton(tt$tfbuts, text = quitopt,
                                         width = 12, command = onCancel,
                                         background = buttoncol)
    # button layout ----
  if (backopt) tcltk::tkgrid(tt$tfbuts$BackBut, column = 1, row = 1, padx = 10)
  tcltk::tkgrid(tt$tfbuts$OkBut, column = 2, row = 1, padx = 2)
  tcltk::tkgrid(tt$tfbuts$CancelBut, column = 3, row = 1, padx = 2)
  tcltk::tkgrid(tt$tfbuts$HelpBut, column = 4, row = 1, padx = 2)
  tcltk::tkgrid(tt$tfbuts, columnspan = 2, pady = 5)

  # wait to continue ----
  tcltk::tkwait.window(tt)

  return(myenv$vars)
}
