#' souk: trade, groups and institutions in a simulated village
#'
#' The package has four layers. [set_rules()] holds every parameter.
#' [new_village()] creates the state. A day is a schedule of steps
#' ([default_schedule()]), each built from decisions that players or groups
#' make. [run_village()] plays many days and records them.
#'
#' @keywords internal
#' @importFrom dplyr bind_rows case_when coalesce if_else
#' @importFrom purrr accumulate map
#' @importFrom stats ave rnorm runif
#' @importFrom tibble tibble
#' @importFrom utils modifyList
"_PACKAGE"
