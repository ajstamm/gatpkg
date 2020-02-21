#' Confirm Quitting GAT
#'
#' @description
#' This function opens a dialog window for the user to select whether to quit
#' or continue GAT.
#'
#' \if{html}{\figure{confirmGATquit.png}{options: width="405"
#'                   alt="Figure: Screenshot of dialog to confirm that you want to quit GAT"}}
#'
#' Click on one of the following buttons.
#'
#' * Click \code{Yes} if you would like to quit GAT.
#' * Click \code{No} if you do not want to quit GAT.
#'
#' @examples
#'
#' \donttest{
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
