#' Input GAT Rate
#'
#' @description
#' This function opens a dialog window for the user to select the settings to
#' calculate the rate for a map, if so desired. The dialog window looks like
#' this.
#'
#' \figure{inputGATrate.png}
#'
#' *Figure: Dialog to select your rate settings*
#'
#' Select your desired rate settings. If you do not want to calculate a rate,
#' check the box at the top. If you do want to calculate a rate, sselect the
#' desired numerator, denominator, and color scheme from the drop-down lists.
#' Name your rate something other than "no_rate", the flag that tells GAT a
#' rate should not be calculated. Then click on one of the following buttons.
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
#' This function returns a list with the following elements:
#'
#' \itemize{\bold{ratename: }
#'   What to call the rate. The default is "gat_rate". If the user chooses not
#'   to calculate a rate, the value is set to "no_rate" to indicate that rate
#'   calculations should not be done later. This variable also holds the value
#'   for "back" or "cancel", if the user selects one of those.
#' }
#' \itemize{\bold{numerator: }
#'   The name of the numerator variable for the rate.
#' }
#' \itemize{\bold{denominator: }
#'   The name of the denominator variable for the rate.
#' }
#' \itemize{\bold{multiplier: }
#'   The number by which to multiply the rate. If the user includes commas,
#'   they will be removed.
#' }
#' \itemize{\bold{colorscheme: }
#'   The ColorBrewer color scheme to use when mapping the rate later.
#' }
#' \itemize{\bold{colorname: }
#'   The formal name of the selected ColorBrewer color scheme.
#' }
#'
#' @param help       A text string containing the help message.
#' @param defaultopt An integer that notes which initial list item should be
#'                   highlighted.
#' @param shp        The layer from which to select variables.
#' @param limitdenom A boolean denoting whether to limit the denominator to
#'                   only variables without zeroes or missings before
#'                   aggregation.
#' @param step       Integer step in the GAT program, for help reference.
#' @param ratevars   Rate settings, if pre-defined.
#' @param backopt    Boolean denoting whether to include the back button.
#'
#' @examples
#'
#' if (interactive()) {
#' inputGATrate(shp = hftown, help = "Select your options.", limitdenom = FALSE)
#' }
#'
#' @export

# Gwen's original notes (mostly) below, for reference
############## gatrateInput function #########################################
# Begin third custom dialog function: gatrateInput
#   allows choice of two variables, the name of the rate, and the multiplier
#   has option not to calculate rate
#   R tclTk code to create the dialog box with two listboxes, two free-text
#	  entry boxes, and "back" "next" "cancel" and "help" buttons
#   list of function arguments:
#     helpfile, defaultoption, gatlist1, and gatlist2
#     function returns text vector with four items
#     requires package tcltk
#     returns [1] "go back" "no rate" or the rate name
#		          [2] the multiplier
#		          [3] numerator
#		          [4] denominator
# file://P:/Sections/EHS/Aggregation/GAT/GAT vR4 manual.html
############## start text input function 2 ###################################

# use this function for free text input, like the minimum values
inputGATrate <- function(shp, defaultopt = 0,
                         help = "There is no help for you.",
                         limitdenom = TRUE, step = 9,
                         ratevars = NULL, backopt = TRUE) {
  # define variable lists ----
  gatlist1 <- checkGATvariabletypes(shp, type = "number")
  idlist <- c()
  for (i in 1:length(gatlist1)) {
    x <- data.frame(shp)[, gatlist1[i]]
    t <- table(x==0 | !is.finite(x))
    idlist[i] <- grepl("TRUE", paste(names(t), collapse = " "))
  }
  gatlist2 <- if (limitdenom) gatlist1[idlist == FALSE] else gatlist1
  if (is.null(ratevars)) {
    ratevars <- list(ratename = "gat_rate",
                     numerator = gatlist1[1],
                     denominator = gatlist2[1],
                     multiplier = "10,000",
                     colorscheme = "Blues",
                     colorname = "Blues")
  } else if (ratevars$ratename %in% c("no_rate", "back", "cancel")) {
    ratevars$ratename <- "gat_rate"
    ratevars$multiplier <- "10,000"
  }

  helppage = "inputGATrate"
  hlp <- paste0("Select your rate settings. To calculate a rate, select the desired numerator, \n",
                "denominator, and color scheme from the drop-down lists. \n",
                "If you do not want to calculate a rate, check the box at the top. \n",
                "  \u2022  To continue,  click 'Next >'. \n",
                "  \u2022  To return to boundary selection, click '< Back'. \n",
                "  \u2022  To quit GAT, click 'Cancel'.")
  bgcol <- "lightskyblue3"
  buttoncol <- "cornflowerblue"


  # color vectors ----
  # sequential palettes are: Blues BuGn BuPu GnBu Greens Greys Oranges OrRd
  # PuBu PuBuGn PuRd Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd
  colorlist <- c("Blues", "Blue-Green", "Blue-Purple", "Green-Blue", "Greens",
                 "Greys", "Oranges", "Orange-Red", "Purple-Blue",
                 "Purple-Blue-Green", "Purple-Red", "Purples", "Red-Purple",
                 "Reds", "Yellow-Green", "Yellow-Green-Blue",
                 "Yellow-Orange-Brown", "Yellow-Orange-Red")
  colors <- c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges",
              "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples",
              "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd")
  fonthead <- tcltk::tkfont.create(family = "Segoe UI", size = 10, weight = "bold")

  # create the window ----
  tt <- tcltk::tktoplevel(width=450, height=250, background = bgcol)
  title <- "Enter rate settings"
  tcltk::tktitle(tt) <- paste0("Step ", step, ": ", title)
  tcltk::tkpack.propagate(tt, FALSE) ## Window won't resize

  # instructions ----
  instruct <- paste(
    " To calculate a rate, select your choices from the drop-down menus \n",
    "and enter your desired rate name and multiplier.")
  tt$inst <- tcltk::tkframe(tt, width = 300, height = 5, background = bgcol)
  tt$inst$inst <- tcltk::tklabel(tt$inst, text = "Instructions", font = fonthead,
                                 background = bgcol)
  tcltk::tkgrid(tt$inst$inst, sticky = "w", padx = 3)
  tcltk::tkgrid(tcltk::tklabel(tt$inst, text = instruct, justify = "left",
                               background = bgcol),
                sticky = "w", padx = 5)
  tcltk::tkgrid(tt$inst, columnspan = 2, pady = 2, sticky = "w")

  # checkbox ----
  tt$check <- tcltk::tkframe(tt, width = 400, height = 30, background = bgcol)
  tt$check$cb <- tcltk::tkcheckbutton(tt$check, background = bgcol)
  msg <- "Click here if you do NOT want to calculate a rate."
  tt$check$cblabel <- tcltk::tklabel(tt$check, text = msg, background = bgcol)
  tt$check$cbvalue <- tcltk::tclVar("0")
  tcltk::tkconfigure(tt$check$cb, variable = tt$check$cbvalue)
  tcltk::tkgrid(tt$check$cb, tt$check$cblabel, sticky = "w", pady = 2, padx = 5)

  tt$check$title <- tcltk::tklabel(tt$check, text = "Rate settings",
                                   font = fonthead, background = bgcol)
  tcltk::tkgrid(tt$check$title, padx = 3, pady = 5, columnspan = 3, sticky = "w")
  tcltk::tkgrid(tt$check, sticky = "w")

  # lists ----
  tt$list <- tcltk::tkframe(tt, width = 420, height = 110, background = bgcol)
  tt$list$numvar <- tcltk::tclVar(ratevars$numerator)
  tt$list$denvar <- tcltk::tclVar(ratevars$denominator)
  tt$list$colvar <- tcltk::tclVar(ratevars$colorname)

  tt$list$numlbl = tcltk::tklabel(tt$list, text = "Select the numerator:",
                                  background = bgcol)
  tt$list$tnum <- tcltk::ttkcombobox(tt$list, values = gatlist1,
                                     state = "readonly",
                                     textvariable = tt$list$numvar)
  tcltk::tkgrid(tt$list$numlbl, tt$list$tnum, sticky = "w", pady = 2)
  tt$list$denlbl = tcltk::tklabel(tt$list, text = "Select the denominator:",
                                  background = bgcol)
  tt$list$tden <- tcltk::ttkcombobox(tt$list, values = gatlist2,
                                     state = "readonly",
                                     textvariable = tt$list$denvar)
  tcltk::tkgrid(tt$list$denlbl, tt$list$tden, sticky = "w", pady = 2)

  note <- "Note: The numerator and denominator must be different."
  if (limitdenom) {
    note <- paste(note, "\n          ",
                  "Variables with 0 or missings cannot be in the denominator.")
  }
  tcltk::tkgrid(tcltk::tklabel(tt$list, text = note, justify = "left",
                               background = bgcol),
                columnspan = 2, sticky = "w", padx = 5)

  tt$list$collbl = tcltk::tklabel(tt$list, text = "Select the map colors:",
                                  background = bgcol)
  tt$list$tcol <- tcltk::ttkcombobox(tt$list, values = colorlist,
                                     state = "readonly",
                                     textvariable = tt$list$colvar)
  tcltk::tkgrid(tt$list$collbl, tt$list$tcol, sticky = "w", pady = 2)

  # text boxes ----
  txt <- "Enter the rate name: \n (ex. cancer_incidence)"
  tt$list$namelbl <- tcltk::tklabel(tt$list, text = txt, justify = "left",
                                    background = bgcol)
  tt$list$namevar <- tcltk::tclVar(ratevars$ratename)
  tt$list$nametxt <- tcltk::tkentry(tt$list, width = "20", background = "white",
                                    textvariable = tt$list$namevar)
  tcltk::tkgrid(tt$list$namelbl, tt$list$nametxt, sticky = "w",
                rowspan = 2, pady = 2)
  txt <- "Enter the rate multiplier: \n (ex. per 10,000 people)"
  tt$list$multlbl <- tcltk::tklabel(tt$list, text = txt, justify = "left",
                                    background = bgcol)
  tt$list$multvar <- tcltk::tclVar(ratevars$multiplier)
  tt$list$multtxt <- tcltk::tkentry(tt$list, width = "20",
                                    textvariable = tt$list$multvar,
                                    background = "white")
  tcltk::tkgrid(tt$list$multlbl, tt$list$multtxt, sticky = "w",
                rowspan = 2, pady = 2)

  tcltk::tkgrid(tt$list, sticky = "w", padx = 10)

  # button functions ----
  myenv <- new.env()
  onOk <- function() {
    cbVal <- as.character(tcltk::tclvalue(tt$check$cbvalue))
    numerator <- tcltk::tclvalue(tt$list$numvar)
    denominator <- tcltk::tclvalue(tt$list$denvar)
    colorname <- tcltk::tclvalue(tt$list$colvar)
    ratename <- tcltk::tclvalue(tt$list$namevar)
    multiplier <- tcltk::tclvalue(tt$list$multvar)
    tcltk::tkdestroy(tt)

    if (cbVal == "1") ratename <- "no_rate"

    # keep all alphabetical characters
    ratename <- gsub("([^a-z|A-Z|_])", "", ratename)

    # set default if user gives invalid name
    if (ratename == "" | is.na(ratename)) ratename <- "gat_rate"

    assign("reslist", list(ratename = ratename,
                           numerator = numerator,
                           denominator = denominator,
                           multiplier = multiplier,
                           colorname = colorname), envir=myenv)
  }
  onCancel <- function() {
    tcltk::tkdestroy(tt)
    assign("reslist", list(ratename = "cancel",
                           numerator = "NONE",
                           denominator = "NONE",
                           multiplier = "0",
                           colorname = "NONE"), envir=myenv)
  }
  onBack <- function() {
    tcltk::tkdestroy(tt)
    assign("reslist", list(ratename = "back",
                           numerator = "NONE",
                           denominator = "NONE",
                           multiplier = "0",
                           colorname = "NONE"), envir=myenv)
  }
  onHelp <- function() {
    showGAThelp(help = hlp, helptitle = "rate settings",
                helppage = helppage, step = step)
  }
  # button layout ----
  tt$tfbuts <- tcltk::tkframe(tt, width = 400, height = 40, background = bgcol)

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
  tt$tfbuts$CancelBut <- tcltk::tkbutton(tt$tfbuts, text = "Cancel GAT",
                                         command = onCancel, width = 12,
                                         background = buttoncol)
  tt$tfbuts$HelpBut <- tcltk::tkbutton(tt$tfbuts, text = "Help", width = 12,
                                       command = onHelp, background = buttoncol)
  if (backopt) {
    tcltk::tkgrid(tt$tfbuts$BackBut, column = 1, row = 11, padx = 5,
                  pady = 5, padx = c(5, 0), sticky = "e")
  }
  tcltk::tkgrid(tt$tfbuts$OkBut, column = 2, row = 11, padx = 5, pady = 5, sticky = "w")
  tcltk::tkgrid(tt$tfbuts$CancelBut, column = 3, row = 11, padx = 5, pady = 5)
  tcltk::tkgrid(tt$tfbuts$HelpBut, column = 4, row = 11, padx = 5,
                pady = 5, padx = c(0, 5))
  tcltk::tkgrid(tt$tfbuts, sticky = "w", padx = 5)

  ######### end program ########

  tcltk::tkwait.window(tt)

  if (identical(myenv$reslist$colorname, character(0))) {
    myenv$reslist$colorname <- "Blues"
  }
  myenv$reslist$colorscheme <- colors[colorlist == myenv$reslist$colorname]

  return(myenv$reslist) # ratevars values
} # end gatrateInput function

