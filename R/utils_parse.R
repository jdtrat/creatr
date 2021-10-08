
#' Retrieve Exit Questions from CRT
#'
#' This function returns the data from the exit questions of the CRT.
#'
#' @inheritParams parse_init
#'
#' @return A data frame with two columns corresponding to the *exit questions*:
#' \itemize{
#' \item{time:} The time since January, 1970
#' \item{data:} Participant data
#' }
#'
#' @keywords internal
#'
#' @examples
#'
#' get_exit_questions(crt_data)
#'
get_exit_questions <- function(.data) {

  exit_question_start <- .data %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    dplyr::filter(stringr::str_detect(data, 'SHOW: QUESTION')) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::pull(row)

  n_rows <- nrow(.data)

  .data %>%
    dplyr::slice(exit_question_start:n_rows)

}


# From jdtools R package
# https://github.com/jdtrat/jdtools/blob/bd7e7baf7522b8c225b1beb41fea65cad85855fd/R/func_strings-extract.R#L72
extract_after <- function(full_string, after_string, trim_spaces = TRUE) {

  reg_pattern <- paste0("(?<=", after_string, ").*")

  out <- regmatches(full_string, gregexpr(pattern = reg_pattern,
                                          text = full_string,
                                          perl = T))[[1]]

  if (trim_spaces) {
    out <- trimws(out)
  }

  return(out)
}
