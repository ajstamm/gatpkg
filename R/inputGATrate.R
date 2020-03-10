#' Input GAT Rate
#'
#' @description
#' This function opens a dialog window for the user to select the settings to
#' calculate the rate for a map, if so desired. The dialog window looks like
#' this.
#'
#' \if{html}{\figure{inputGATrate.png}{options: width="450px"
#'                   alt="Figure: Screenshot of dialog to select your rate settings"}}
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
#' @param mapdata    The data frame from which to select variables.
#' @param limitdenom A boolean denoting whether to limit the denominator to
#'                   only variables without zeroes or missings before
#'                   aggregation.
#' @param step       Integer step in the GAT program, for help reference.
#' @param ratevars   Rate settings, if pre-defined.
#' @param backopt    Boolean denoting whether to include the back button.
#'
#' @examples
#'
#' \donttest{
#' hlp <- paste0("To continue, select an option and click 'Next >',",
#'               "\nto return to the previous step, click '< Back',",
#'               "\nand to quit the program, click 'Cancel'.")
#'
#' # define rate settings
#' inputGATrate(
#'   mapdata = hftown@data,
#'   help = hlp,
#'   limitdenom = FALSE
#' )
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
inputGATrate <- function(mapdata, defaultopt = 0,
                         help = "There is no help for you.",
                         limitdenom = TRUE, step = 9,
                         ratevars = NULL, backopt = TRUE) {
  # define variable lists ####
  gatlist1 <- checkGATvariabletypes(mapdata, type = "number")
  idlist <- c()
  for (i in 1:length(gatlist1)) {
    x <- mapdata[, gatlist1[i]]
    t <- table(x==0 | !is.finite(x))
    idlist[i] <- grepl("TRUE", paste(names(t), collapse = " "))
  }
  if (limitdenom) {
    gatlist2 <- gatlist1[idlist == FALSE] # denominator
  } else {
    gatlist2 = gatlist1
  }

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

  # color vectors ####
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

  ######### create the window #########
  tt <- tcltk::tktoplevel(width=450, height=250)
  title <- "Enter rate settings"
  tcltk::tktitle(tt) <- paste0("Step ", step, ": ", title)
  tcltk::tkpack.propagate(tt, FALSE) ## Window won't resize

  # create the frames
  tt$tfcheck <- tcltk::tkframe(tt, width = 400, height = 30)
  tt$tflists <- tcltk::tkframe(tt, width = 420, height = 110)
  tt$tftexts <- tcltk::tkframe(tt, width = 400, height = 30)
  tt$tfbuts <- tcltk::tkframe(tt, width = 400, height = 40)
  tt$tfhoriz1 <- tcltk::tkframe(tt, width = 440, height = 2, bg = "black")
  tt$tfhoriz2 <- tcltk::tkframe(tt, width = 440, height = 2, bg = "black")
  tcltk::tkpack(tt$tfcheck, tt$tfhoriz1, side = "top")
  tcltk::tkpack(tt$tfcheck, tt$tflists, side = "top")
  tcltk::tkpack(tt$tfcheck, anchor = "w")
  tcltk::tkpack(tt$tflists, tt$tfhoriz2, side = "top")
  tcltk::tkpack(tt$tflists, tt$tftexts, side = "top")
  tcltk::tkpack(tt$tftexts, anchor = "w")
  tcltk::tkpack(tt$tftexts, tt$tfbuts, side = "top")

  ######### code for checkbox #########
  tt$tfcheck$cb <- tcltk::tkcheckbutton(tt$tfcheck)
  tt$tfcheck$cblabel <- tcltk2::tk2label(tt$tfcheck,
                                         text = "Do NOT calculate a rate")
  tt$tfcheck$cbvalue <- tcltk::tclVar("0")
  tcltk::tkconfigure(tt$tfcheck$cb, variable = tt$tfcheck$cbvalue)
  tcltk::tkgrid(tt$tfcheck$cb, column = 1, sticky = "e")
  tcltk::tkgrid(tt$tfcheck$cblabel, column = 3, row = 0, sticky = "w")

  ######### code for listboxes #########
  tt$tflists$list1label = tcltk2::tk2label(tt$tflists,
                                           text = "Select the numerator")
  tt$tflists$list2label = tcltk2::tk2label(tt$tflists,
                                           text = "Select the denominator")
  tt$tflists$list3label = tcltk2::tk2label(tt$tflists,
                                           text = "Select map color scheme")
  tcltk::tkgrid(tt$tflists$list1label, row = 1, column = 1, sticky = "w")
  tcltk::tkgrid(tt$tflists$list2label, row = 1, column = 2, sticky = "w")
  tcltk::tkgrid(tt$tflists$list3label, row = 1, column = 3, sticky = "w")

  # selectmode = {single, extended}, height is of listbox
  # height controls height of listbox
  tt$tflists$tnum <- tcltk2::tk2listbox(tt$tflists, height = 5,
                                        selectmode = "single",
                                        values = gatlist1,
                                        background = "white",
                                        value = ratevars$numerator)
  tt$tflists$tden <- tcltk2::tk2listbox(tt$tflists, height = 5,
                                        selectmode = "single",
                                        background = "white",
                                        values = gatlist2,
                                        value = ratevars$denominator)
  tt$tflists$tcol <- tcltk2::tk2listbox(tt$tflists, height = 5,
                                        selectmode = "single",
                                        background = "white",
                                        values = colorlist,
                                        value = ratevars$colorname)
  tcltk::tkgrid(tt$tflists$tnum, row = 2, column = 1, sticky = "e")
  tcltk::tkgrid(tt$tflists$tden, row = 2, column = 2, sticky = "e")
  tcltk::tkgrid(tt$tflists$tcol, row = 2, column = 3, sticky = "e")

  # add note about removing variables from denominator list
  if (limitdenom) {
    note <- "Note: Variables with 0 or missings cannot be in the denominator."
    tt$tflists$note = tcltk2::tk2label(tt$tflists, text = note)
    tcltk::tkgrid(tt$tflists$note, columnspan = 4, sticky = "w")
  }

  ######### code for text entry boxes #########
  txt <- "Enter the rate multiplier\n ex. per 10,000 population"
  tt$tftexts$multlabel <- tcltk2::tk2label(tt$tftexts, text = txt)
  txt <- "Enter the rate variable name\n ex. cancer_incidence"
  tt$tftexts$namelabel <- tcltk2::tk2label(tt$tftexts, text = txt)
  tt$tftexts$perlabel <- tcltk2::tk2label(tt$tftexts, text = "  per     ")

  # this should not be necessary, but debugging
  if (is.null(ratevars$multiplier) | is.null(ratevars$ratename)) {
    ratevars$ratename <- "gat_rate"
    ratevars$multiplier <- "10,000"
  }

  tt$tftexts$vartext1 <- tcltk::tclVar(ratevars$multiplier)
  tt$tftexts$vartext2 <- tcltk::tclVar(ratevars$ratename)

  tt$tftexts$multtext <- tcltk::tkentry(tt$tftexts, width = "20",
                                        textvariable = tt$tftexts$vartext1)
  tt$tftexts$nametext <- tcltk::tkentry(tt$tftexts, width = "20",
                                        textvariable = tt$tftexts$vartext2)

  tcltk::tkgrid(tt$tftexts$namelabel, sticky = "w", padx = 5, column = 1,
                row = 8, rowspan = 2, columnspan = 1) # could also use pady = 5
  tcltk::tkgrid(tt$tftexts$multlabel, sticky = "w", padx = 5, column = 3,
                row = 8, rowspan = 2, columnspan = 1)
  tcltk::tkgrid(tt$tftexts$nametext, column = 1, row = 10, padx = 5,
                sticky = "w") # text entry box
  tcltk::tkgrid(tt$tftexts$perlabel, column = 2, row = 10)
  tcltk::tkgrid(tt$tftexts$multtext, column = 3, row = 10, padx = 5, pady = 5,
                columnspan = 2, sticky = "w") # text entry box

  tcltk::tkselection.from(tt$tftexts$multtext, "0")
  tcltk::tkfocus(tt$tftexts$multtext)

  myenv <- new.env()

  ######### code for buttons #########
  onOk <- function() {
    multiplier <- tcltk::tclvalue(tt$tftexts$vartext1)
    ratename <- tcltk::tclvalue(tt$tftexts$vartext2)
    ind <- as.numeric(tcltk::tkcurselection(tt$tflists$tnum))
    numerator <- gatlist1[ind + 1] # list 1
    ind <- as.numeric(tcltk::tkcurselection(tt$tflists$tden))
    denominator <- gatlist2[ind + 1] # list 2
    ind <- as.numeric(tcltk::tkcurselection(tt$tflists$tcol))
    colorscheme <- colors[ind + 1] # list 3
    cbVal <- as.character(tcltk::tclvalue(tt$tfcheck$cbvalue))
    tcltk::tkdestroy(tt)

    if (cbVal == "1") {
      ratename <- "no_rate"
    }

    # keep all alphabetical characters
    ratename <- gsub("([^a-z|A-Z|_])", "", ratename)

    # set default if user gives invalid name
    if (ratename == "" | is.na(ratename)) ratename <- "gat_rate"

    assign("reslist", list(ratename = ratename,
                           numerator = numerator,
                           denominator = denominator,
                           multiplier = multiplier,
                           colorscheme = colorscheme), envir=myenv)
  }
  onCancel <- function() {
    tcltk::tkdestroy(tt)
    assign("reslist", list(ratename = "cancel",
                           numerator = "NONE",
                           denominator = "NONE",
                           multiplier = "0",
                           colorscheme = "NONE"), envir=myenv)
  }
  onBack <- function() {
    tcltk::tkdestroy(tt)
    assign("reslist", list(ratename = "back",
                           numerator = "NONE",
                           denominator = "NONE",
                           multiplier = "0",
                           colorscheme = "NONE"), envir=myenv)
  }
  onHelp <- function() {
    showGAThelp(help = hlp, helptitle = "rate settings",
                helppage = helppage, step = step)
  }

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
    tcltk::tkgrid(tt$tfbuts$BackBut, column = 1, row = 11, padx = 5,
                  pady = 5, padx = c(5, 0))
  }
  tcltk::tkgrid(tt$tfbuts$OkBut, column = 2, row = 11, padx = 5, pady = 5)
  tcltk::tkgrid(tt$tfbuts$CancelBut, column = 3, row = 11, padx = 5, pady = 5)
  tcltk::tkgrid(tt$tfbuts$HelpBut, column = 4, row = 11, padx = 5,
                pady = 5, padx = c(0, 5))

  # configure elements
  if (backopt) {
    tcltk::tkgrid.configure(tt$tfbuts$BackBut, sticky = "e")
  }
  tcltk::tkgrid.configure(tt$tfbuts$OkBut, sticky = "w")

  ######### end program ########

  tcltk::tkwait.window(tt)

  if (identical(myenv$reslist$colorscheme, character(0))) {
    myenv$reslist$colorscheme <- "BuGn"
  }
  myenv$reslist$colorname <- colorlist[colors == myenv$reslist$colorscheme]

  return(myenv$reslist) # ratevars values
} # end gatrateInput function

