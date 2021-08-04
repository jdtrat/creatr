
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

