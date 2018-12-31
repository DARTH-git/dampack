
#' head method for the ceac object
#'
#' @param x object returned from the \code{ceac} function
#' @param n number of lines to display
#' @param ... further arguments (not used)
#' @importFrom utils head
#' @export
head.ceac <- function(x, n = 6L, ...){
  head(x$ceac, n, ...)
}
