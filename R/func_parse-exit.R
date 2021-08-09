
#' Parse Familia
#'
#'
#' Add description ;aksdj;as
#' @inheritParams get_exit_data
#'
#' @return What do you expect to return? Data frame containing ...
#' @export
#'
#' @examples
parse_familiarity <- function(.data) {

  # Get answer to "how familiar are you with the following objects?"
  get_exit_questions(.data = .data) %>%
    # Filter data to only responses after "FAMILIAR QUESTION ANSWERS:"
    dplyr::filter(stringr::str_detect(data, "FAMILIAR QUESTION ANSWERS")) %>%
    dplyr::mutate(data = stringr::str_remove(data, "FAMILIAR QUESTION ANSWERS: "))  %>%
    # Separate the field column into each object "prompt":
    tidyr::separate(col = data,
                    into =  c("chair", "key",
                              "watch", "brick",
                              "cup", "knife"),
                    sep = "&",
                    extra = "merge",
                    fill = "right") %>%
    dplyr::mutate(dplyr::across(.cols = -time,
                                ~stringr::str_extract(.x, "[:digit:]"))
    ) %>%
    tidyr::pivot_longer(cols = -time,
                        names_to = "object",
                        values_to = "familiarity_score")
}

#' Title
#'
#' @inheritParams get_exit_data
#'
#' @return
#' @export
#'
#' @examples
parse_aut <- function(.data) {

  # Get answer to "have you heard of or completed the AUT?"
  get_exit_questions(.data = .data) %>%
    # Filter data to find only answer to question (e.g. "ANSWER: YES")
    dplyr::filter(stringr::str_detect(data, "Answer submitted ")) %>%
    dplyr::mutate(data = stringr::str_extract(data, "ANSWER:.*"))

}

#' Title
#'
#' @inheritParams get_exit_data
#'
#' @return
#' @export
#'
#' @examples
parse_creativity_def <- function(.data) {

  # Get answer to creativity definition
  get_exit_questions(.data = .data) %>%
    # Find all rows where there was a field someone filled out
    dplyr::filter(stringr::str_detect(data, "FIELD: ")) %>%
    # Separate the data by spaces, into these columns
    tidyr::separate(col = data,
                    into =  c("name_field", "field",
                              "name_kepyress", "keypress",
                              "name_value", "value"),
                    sep = " ",
                    extra = "merge",
                    fill = "right") %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::select(time, creativity_def = value)

}
