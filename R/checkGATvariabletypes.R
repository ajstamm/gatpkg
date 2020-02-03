#' Check GAT Variable Types
#'
#' This function is used to create a list of either numeric or character
#' variables in the data frame.
#'
#' @param mapdata A data frame, intended to be read from a shapefile DBF.
#' @param type Variable type. Options include "number", which returns a vector
#'             of names for all numeric variables, and "character", which
#'             returns a vector of names for all character variables.
#'
#' @examples
#' # identify numeric variables
#' my_numericvars <-
#'   checkGATvariabletypes(
#'     mapdata = hftown@data,
#'     type = "number"
#'   )
#'
#' @export

checkGATvariabletypes <- function(mapdata, type = "number") {
  # once read in, get variable names and classes
  # note "count" option gives a vector instead of a logical
  listitems <- names(mapdata) # need to limit this to numeric variables
  listtype <- sapply(mapdata, class) # get data classes for all columns
  listtype[listtype %in% c("factor", "character")] <- FALSE
  listtype[listtype %in% c("integer", "numeric")] <- TRUE
  if (type == "number") {
    vars <- listitems[listtype == TRUE]
    msg = "Sorry, Your shapefile has no numeric data and won't work in the GAT."
  } else if (type == "character") {
    vars <- listitems[listtype == FALSE]
    msg = "Sorry, Your shapefile has no character data and won't work in the GAT."
  }
  if (length(vars) < 1) {
    tcltk::tkmessageBox(title = "File data problem", message = msg,
                        type = "ok", icon = "error")
    vars <- c()
  }
  return(vars)
}
