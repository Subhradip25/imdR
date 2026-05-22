#' @importFrom utils write.csv combn txtProgressBar setTxtProgressBar
#' @importFrom stats quantile median aggregate
#' @importFrom rlang .data
NULL

utils::globalVariables(c(
     "india_states",
     "india_districts",
     "year",
     "value",
     "trend",
     "smooth"
))
