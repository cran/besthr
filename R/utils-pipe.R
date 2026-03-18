#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @return  result of previous expression
NULL

# Global variable declarations for data.frame columns used in dplyr/ggplot
# These avoid R CMD check NOTEs about undefined global variables
utils::globalVariables(c(
  "count",
  "low",
  "high",
  "rank",
  "mean",
  "quantile"
))
