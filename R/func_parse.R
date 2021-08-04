
#' Initial Parsing of CRT Data
#'
#' This function accepts the data output from \code{\link{read_crt()}} and
#' parses it for the initial values.
#'
#' @param .data
#'
#' @return It returns a data frame with the following columns:
#' \itemize{
#' \item{time:} The time since January, 1970
#' \item{prompt:} The category that the individual typed in (e.g. month or object)
#' \item{prompt_id:} The specific text box the participant filled in
#' \item{keypress} The key the individual pressed
#' \item{value} The running value in the text box
#' }
#'
#' @export
#'
#' @examples
#'
#' parse_init(crt_data)
#'
parse_init <- function(.data) {
  .data %>%
    # Find all rows where there was a field someone filled out
    dplyr::filter(stringr::str_detect(data, "FIELD: ")) %>%
    # Separate the data by spaces, into these columns
    tidyr::separate(col = data,
                    into = c("name_field", "field",
                             "name_kepyress", "keypress",
                             "name_value", "value"),
                    sep = " ",
                    extra = "drop",
                    fill = "right") %>%
    # Remove the fields where someone pressed tab and is going to the next field
    dplyr::filter(stringr::str_detect(field, "Tab", negate = TRUE)) %>%
    # Separate the field column into "prompt" such as "KNIFE" and prompt ID
    tidyr::separate(col = field,
                    into = c("prompt", "prompt_id"),
                    extra = "drop",
                    fill = "right") %>%
    # Drop all the meta columns
    dplyr::select(-contains("name_")) %>%
    # Remove commaas from the keypress column
    dplyr::mutate(keypress = stringr::str_remove(keypress, "\\,")) %>%
    # Remove rows where they pressed keys that are not letters/numbers
    dplyr::filter(keypress %in% c(as.character(seq(0,9)), letters, LETTERS))
}
