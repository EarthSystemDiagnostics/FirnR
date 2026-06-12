#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom dplyr %>%
#' @importFrom rlang .data
## usethis namespace: end
NULL

#' Global variables
#'
#' Definition of required global variables to pass R CMD CHECK.
#' 
#' @source https://github.com/tidyverse/magrittr/issues/29
#' 
#' @name global_variables
#' @keywords internal
#' @noRd
if (getRversion() >= "2.15.1")  utils::globalVariables(c("."))
