#' Get Operating System
#'
#' Utility to detect the operating system of the current `R` session.
#' Based on <http://conjugateprior.org/2015/06/identifying-the-os-from-r/>.
#'
#' @return Return any of the following:
#' \describe{
#'   \item{"linux"}{GNU/Linux}
#'   \item{"osx"}{Macintosh OSx}
#'   \item{"windows"}{Windows}
#'   \item{mystery machine}{Operating systems other than the three above}
#' }
#' @examples
#' get_os()
#' @export
get_os <- function() {
  sysinf <- Sys.info()
  if (!is.null(sysinf)) {
    os <- sysinf["sysname"]
    if (os == "Darwin") {
      os <- "osx"
    }
  } else {
    ## mystery machine
    os <- .Platform$OS.type
    if (grepl(
      "^darwin",
      R.version$os
    )
    ) {
      os <- "osx"
    }
    if (grepl(
      "linux-gnu",
      R.version$os
    )
    ) {
      os <- "linux"
    }
  }
  tolower(os)
}
