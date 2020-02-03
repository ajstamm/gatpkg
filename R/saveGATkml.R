#' Save GAT KML File
#'
#' @description
#' This function opens a dialog window for the user to select whether or not
#' to save a KML file. The dialog window looks like this.
#'
#' \if{html}{\figure{saveGATkml.png}{options: width="405"
#'                   alt="Figure: Screenshot of dialog to select KML save.png"}}
#'
#' Click on one of the following buttons.
#'
#' * Click \code{Yes} if you would like to save a KML file.
#' * Click \code{No} if you do not want to save a KML file.
#' * Click \code{Back} to return to the previous step.
#' * Click \code{Help} to get further guidance and open the manual.
#'
#' @details
#' The resulting KML fie will contain all data, which can be accessed through
#' Google Earth. GE 5.0 or higher is recommended. KML files were tested on
#' GE in Chrome v9.2.90.1.
#'
#' @param step Integer step in the GAT program, for help reference.
#'
#' @examples
#'
#' \donttest{
#' # choose yes or no
#' saveGATkml()
#' }
#'
#' @export

saveGATkml <- function(step = 0) {
  msg <- paste("Would you like to save a KML file as well as a shapefile?",
               "\n(The KML file may take a while to write.)")
  help <- paste0("To continue, select 'Yes' or 'No',",
                 "\nand to return to rate selection, click '< Back',")
  helppage <- "saveGATkml"
  title <- "Save KML file?"

  myvalue <- "repeat"

  while (myvalue == "repeat") {
    myvalue <- inputGATmessage(help = help, title = title, step = step,
                               helppage = helppage, msg = msg,
                               buttonopt = "No", helptitle = helppage)

    if (is.null(myvalue)) {
      x <- confirmGATquit()
      if (x == "quit") {
        myvalue <-  "quit"
      } else {
        myvalue <- "repeat"
      }
    }
  }

  if (myvalue == "cancel") {
    myvalue <- "No"
  }

  return(myvalue)
}
