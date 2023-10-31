#' Identify GAT Boundary
#'
#' @description
#' This function opens a dialog window for the user to select a character
#' variable to prioritize larger geographic areas such as counties in
#' which to aggregate. The dialog window looks like this.
#'
#' \figure{identifyGATboundary.png}
#'
#' *Figure: Dialog to select your boundary variable*
#'
#' Select your desired boundary variable. If you do not want to include a
#' boundary, select "NONE". If you want to enforce the boundary, check the box
#' on the right as shown. Then click on one of the following buttons.
#'
#' * Click \code{Next} to continue to the next step.
#' * Click \code{Cancel} to end GAT.
#' * Click \code{Back} to return to the previous step.
#' * Click \code{Help} to get further guidance and open this manual.
#'
#' @details
#' This function reads in a dataset and extracts the names of variables that
#' are (1) character and (2) not unique, since only these variables will be
#' suitable for defining larger geographic areas in GAT.
#'
#' @param shp        Spatial layer.
#' @param step       Integer step in GAT, for help reference.
#' @param boundary   Boundary variable, if pre-selected. Defaults to "NONE",
#'                   which means no boundary selected.
#' @param borders    Boolean denoting whether to enforce boundary. Default is
#'                   FALSE.
#' @param myvar      Boundary variable, if pre-selected.
#' @param check      Checkbox setting, if pre-selected. Currently does not do
#'                   anything.
#' @param backopt    Boolean denoting whether to include the back button.
#' @param quitopt    Text string for the cancel button.
#' @param bgcol      Text string containing UI background color.
#' @param buttoncol  Text string containing UI button color.
#'
#' @examples
#'
#' if (interactive()) {
#' # select boundary variable
#' identifyGATboundary(shp = hftown)
#' }
#'
#' @export

identifyGATboundary <- function(shp, step = 3, boundary = "NONE", myvar = "NONE",
                                borders = FALSE, check = FALSE, backopt = TRUE,
                                bgcol = "lightskyblue3", quitopt = "Quit",
                                buttoncol = "cornflowerblue") {
  # should be at least two choices, because we add "NONE"
  chars <- checkGATvariabletypes(shp, type = "character")
  idlist <- c()
  for (i in 1:length(chars)) {
    t <- table(data.frame(shp)[, chars[i]])
    idlist[i] <- length(t) == nrow(shp)
  }

  boundaryitems <- c("NONE", chars[idlist == FALSE])

  noofchoices <- length(boundaryitems)

  if (noofchoices > 1) {
    msg <- paste("Please select the variable that \nidentifies boundaries within",
                 "\nwhich GAT should merge.")
    chk <- paste("Click here to enforce \nboundaries, even if the \narea",
                 "total will be less \nthan the desired \nminimum.")
    hlp <- paste0("Select your boundary variable. If you do not want a \n",
                  "boundary variable, select 'NONE'. \n",
                  "  \u2022  To continue,  click 'Next >'. \n",
                  "  \u2022  To return to identifier selection, click '< Back'. \n",
                  "  \u2022  To quit GAT, click '", quitopt, "'.")

    error <- TRUE

    while (error) {
      boundaryvars <- inputGATvariable(mylist = boundaryitems, instruction = msg,
                      title = "Boundary Variable", checkopt = chk, checkbox = TRUE,
                      help = hlp, step = step, helppage = "identifyGATboundary",
                      myvar = boundary, check = borders, backopt = backopt,
                      bgcol = bgcol, buttoncol=buttoncol)
      error <- FALSE
      if (is.null(boundaryvars)) {
        x <- confirmGATquit()
        if (x == "quit") {
          boundaryvars <- list(myvar = "cancel", check = FALSE)
        } else {
          error < TRUE
        }
      } else if (length(boundaryvars$myvar) == 0) {
        msg <- paste("Please select a variable to identify \nyour boundaries,",
                     "or select 'NONE' \nif you do not want a boundary.")
        error <- TRUE
      }
    }
  } else {
    boundaryvars <- list(myvar = "NONE", check = FALSE)
    msg <- paste("The shapefile does not contain a suitable boundary variable.")
    tcltk::tkmessageBox(title = "No suitable boundary variable", message = msg,
                        type = "ok", icon = "warning")
  }
  return(boundaryvars)
}
