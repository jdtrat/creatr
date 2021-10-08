#' Get Item Responses for CRT Data
#'
#' @param .data CRT Data processed by \code{\link{read_crt}}.
#' @param items The items to query. Default is `c("brick", "chair", "cup",
#'   "key", "knife","watch")`.
#'
#' @return It returns a data frame with the following columns: \itemize{
#'   \item{item:} The item a to which the given response corresponds.
#'   \item{response:} The creative use for the item. }
#'
#' @export
#'
#' @examples
#'
#' # By default, get all six item responses
#' get_item_responses(crt_data)
#'
#' # Only get individual item responses
#' get_item_responses(crt_data, "cup")
#' get_item_responses(crt_data, "chair")
#' get_item_responses(crt_data, "Key")
#'
get_item_responses <- function(.data, items = c("brick", "chair", "cup", "key", "knife", "watch")) {

  do.call(rbind,
          lapply(items, get_item_responses_internal, .data = .data)
  )

}


get_item_responses_internal <- function(.data, item) {

  field <- toupper(item)

  .data %>%
    dplyr::filter(stringr::str_detect(.data$data, paste0("FIELD ", field))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(response = extract_after(.data$data, paste0(field, "-[[:digit:]]{3}:"))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$response != "") %>%
    dplyr::transmute(item = item,
                     response = stringr::str_trim(.data$response, "both"))


}

