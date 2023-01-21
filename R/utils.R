# utils.R

#' Values Not In
#'
#' This returns the negation of `%in%`.
#'
#' @param x The values to be matched.
#' @param table The values to matched against.
#'
#' @return A logical vector indicating if a match was not located for each
#'    element of `x`.
#' @export
#'
#' @examples
#' 1 %nin% 1:4
"%nin%" <- function(x, table) {
  match(x, table, nomatch = 0L) == 0L
}
