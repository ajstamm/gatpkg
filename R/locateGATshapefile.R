#' Locate GAT Shapefile
#'
#' @description
#' This function opens a window that asks the user to select a shapefile for
#' aggregation. The file selection window looks like this.
#'
#' \figure{locateGATshapefile.png}
#'
#' *Figure: Dialog to select your shapefile*
#'
#' Select your folder and file and click \code{Open}. Clicking \code{Cancel}
#' on this window will trigger a flag to cancel GAT.
#'
#' @details
#' This function returns a list with the following elements.
#'
#' \itemize{\bold{userin: }
#'   The full name and path for the shapefile, without the extension.
#' }
#' \itemize{\bold{filein: }
#'   The name of the shapefile, without the extension.
#' }
#' \itemize{\bold{pathin: }
#'   The full path for the shapefile.
#' }
#'
#' The window may be hidden behind other windows. If so, minimize or close the
#' other windows or click on it to bring it to the front.
#'
#' @param msg        String denoting your message to display in the status bar.
#' @param myfile     String denoting default file name and location to open.
#' @param step       Step number to print in title bar. Default is 1.
#' @param myprogram  String denoting the program name. GAT is the default.
#'
#' @examples
#'
#' if (interactive()) {
#' # navigate to and select a shapefile
#' locateGATshapefile()
#'
#' # provide a default location to start
#' locateGATshapefile(
#'   msg = "Select your shapefile",
#'   myfile = getwd()
#' )
#'
#' }
#'
#' @export

locateGATshapefile <- function(myfile = "", step = 1, msg = "",
                               myprogram = "GAT") {
  fil <- cbind("Shapefiles", "*.shp") # creates 1x2 matrix
  checkfile <- 100
  if (msg != "") {
    mycaption <- paste0("Step ", step, ": ", msg)
  } else {
    mycaption <- paste0("Step ", step, ": Select your shapefile")
  }
  while (checkfile != 0){
    userfile <- utils::choose.files(filters = fil, caption = mycaption,
                                    default = myfile)
    if (length(userfile) > 0) {
      userfile <- gsub("\\\\", "/", userfile) # add "if Windows" tag
      # remove extension if present
      periodloc <- max(unlist(gregexpr(".", userfile, fixed = TRUE)))
                   # will be -1 if no match, otherwise location(s) of matches
      if(periodloc > 0) {
        userfile <- substr(userfile, 1, periodloc[1] - 1)
      }
      checkfile <- file.access(paste0(userfile, ".shp"), mode = 4)
        # -1 for bad, 0 for OK
      if (checkfile != 0) { # file not found
        msg <- paste("Sorry,", myprogram, "could not find your shapefile.",
                     "Please select a new shapefile.")
        tcltk::tkmessageBox(title = "File error", message = msg,
                            type = "ok", icon = "error")
      } else if (length(userfile) > 0) {
        # find location of last slash, divides path and file name
        slashloc = max(unlist(gregexpr("/", userfile, fixed = TRUE)))

        # find input file name and path
        filein = substr(userfile, slashloc + 1, nchar(userfile))
        pathin = substr(userfile, 1, slashloc - 1)

        myfiles <- list(userin = userfile, filein = filein, pathin = pathin)
      }
    } else {
      myfiles <- list(userin = "cancel", filein = "", pathin = "")
      checkfile <- 0
    }
  }
  return(myfiles)
}
