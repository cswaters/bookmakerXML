#' Build a file name
#'
#' @param name sport or league name
#'
#' @return character vector
#' @noRd
#'
#' @importFrom stringr str_to_lower str_remove_all str_replace_all
#'
bm_mk_fname <- function(name) {
  f_name <- stringr::str_remove_all(name, '[:punct:]') %>%
    stringr::str_replace_all(., '[:space:]', '_') %>%
    stringr::str_to_lower()
  paste0(f_name, ".csv")
}
