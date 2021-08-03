#' Read Creativity Task Data
#'
#' @param path A character string defining the path to the output of the CRT.
#'
#' @return The raw data downloaded from the task. It is a data frame with two
#'   columns, time and data, and 1875 observations.
#'
#' @export
#'
#' @examples
#'
#' #' # Read in the raw data included as part of this package
#' read_crt(path = system.file("extdata/creativity_raw.txt",
#'                             package = "creatr")
#' )
#'
read_crt <- function(path) {
  readr::read_delim(file = path,
                    delim = "\t",
                    col_names = c("time", "data"))
}




