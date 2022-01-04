#' Confirm Quitting GAT
#'
#' @description
#' This function opens a dialog window for the user to select whether to quit
#' or continue GAT.
#'
#' \figure{confirmGATquit.png}
#'
#' *Figure: Dialog to confirm quitting GAT*
#'
#' Click on one of the following buttons.
#'
#' * Click \code{Yes} if you would like to quit GAT.
#' * Click \code{No} if you do not want to quit GAT.
#'
#' @examples
#'
#' if (interactive()) {
#' # choose yes or no
#' confirmGATquit()
#' }
#'
#' @export

confirmGATquit <- function() {
  msg <- "Are you sure you want to quit GAT?"
  x <- tcltk::tkmessageBox(title = "GAT cancelled",
                      message = msg,
                      type = "yesno", icon = "warning")
  x <- tcltk::tclvalue(x)
  if (x == "yes") {
    x <- "quit"
  } else if (x == "no") {
    x <- "continue"
  }
  return(x)
}
