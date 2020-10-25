#' Convert a N-category vector to a N-dimension matrix
#' 
#' The function \code{dummies} converts a N-category vector to a N-dimension matrix
#' 
#' @param x A N-category vector 
#'
#' @return A N-dimension matrix with 0/1 values
#'
#' @examples
#' data(iris, package = "datasets")
#' dummies(iris[, 5])

dummies <- function(x) {
  l <- levels(factor(x))
  d <- Reduce(cbind, lapply(l, function(xi) (x == xi) + 0))
  dimnames(d) <- list(seq(length(x)), l)
  return(d)
}
