#' Save GAT Files
#'
#' @description
#' This function opens a dialog window for the user to select the folder and
#' filename to use when saving the files to be processed. The dialog window
#' looks like this.
#'
#' \if{html}{\figure{identify_save.png}{options: width="705px"
#'                   alt="Figure: Screenshot of dialog to select your save location"}}
#'
#' Select the folder in which to save your file, then type a file name without
#' the extension (extensions are removed anyway). Then click on one of the
#' following buttons.
#'
#' * Click \code{Save} to record the location and filename. (No file is saved yet.)
#' * Click \code{Cancel} to end GAT.
#'
#' @details
#' This function does not write to the folder you select. Instead, it returns
#' a list of the following elements.
#'
#' \itemize{\bold{userout: }
#'   The full name and path for the shapefile.
#' }
#' \itemize{\bold{fileout: }
#'   The name of the shapefile.
#' }
#' \itemize{\bold{pathout: }
#'   The full path for the shapefile.
#' }
#'
#' @examples
#'
#' \donttest{
#' # navigate to the folder and type a filename
#' saveGATfiles()
#' }
#'
#' @export

# this function identifies the folder and name of the save files
# by expanding the filevars list

saveGATfiles <- function() {
  # request save file location
  filevars <- list(fileout = "") # set as default missing

  while (filevars$fileout == "") {
    # replaced svDialogs::dlgSave() with tcltk to avoid
    # "This file doesn't exist" spam in base R;
    # spam did not occur in RStudio
    temp <- list(savefile = tcltk::tclvalue(tcltk::tkgetSaveFile()))
    if (temp$savefile =="") {
      filevars$fileout = "cancel"
    } else {
      # remove any file extention
      temp$periodloc <- regexpr(".", temp$savefile, fixed = TRUE)
      # will be -1 if no match, otherwise location(s) of matches
      if (temp$periodloc[1] > 0) { # if there is an extension
        filevars$userout <- substr(temp$savefile, 1, temp$periodloc[1] - 1)
      } else {
        #find output file name and path
        filevars$userout <- temp$savefile
      }
      temp$slashloc <- max(unlist(gregexpr("/", filevars$userout,
                                           fixed = TRUE)))
      # find location of last slash, divides path and file name
      filevars$fileout <- substr(filevars$userout, temp$slashloc + 1,
                                 nchar(filevars$userout))
      filevars$pathout <- substr(filevars$userout, 1, temp$slashloc - 1)
      # filename shouldn't contain ;=+<>|"[]/\'<>:*?

      temp$checkfile1 <- regexpr(";|:|\\+|=|<|>|\\||\\[|\\]|/|\"|'|\\*|\\?\n",
                                filevars$fileout, perl=TRUE)
      temp$checkfile2 <- charmatch("\\", filevars$fileout, nomatch = -1)
      if (temp$checkfile1[1] != "-1" | temp$checkfile2[1] != "-1") {
        filevars$fileout <- ""
      }
    }

    if (filevars$fileout == "") {
      msg <- "Your file name may be invalid. Please re-enter your file name."
      tcltk::tkmessageBox(title = "Warning", message = msg,
                          type = "ok", icon = "warning")
    }
  } # end while no good file name
  return(filevars)
}
