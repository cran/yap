#' Create a probabilistic neural network
#' 
#' The function \code{pnn.fit} creates a probabilistic neural network (PNN)
#' 
#' @param x     A matrix of predictors
#' @param y     A vector of N-category factors 
#' @param sigma A scalar with the positive value
#' 
#' @return A PNN object
#'
#' @references
#' Donald Specht. (1990). Probabilistic Neural Networks.
#'
#' @examples
#' data(iris, package = "datasets")
#' Y <- iris[, 5]
#' X <- scale(iris[, 1:4])
#' pnet <- pnn.fit(x = X, y = Y)

pnn.fit <- function(x, y, sigma = 1) {
  ### CHECK X MATRIX ###
  if (is.matrix(x) == F) stop("x needs to be a matrix.", call. = F)
  if (anyNA(x) == T) stop("NA found in x.", call. = F)
  ### CHECK Y VECTOR ###
  if (anyNA(y) == T) stop("NA found in y.", call. = F)
  if (length(y) != nrow(x)) stop("x and y need to share the same length.", call. = F)
  ### CHECK SIGMA ###
  if (sigma <= 0) stop("sigma needs to be positive", call. = F)

  pn <- structure(list(), class = "Probabilistic Neural Net")
  pn$x <- x
  pn$y.raw <- y
  pn$y.ind <- dummies(y)
  pn$sigma <- sigma
  return(pn)
}
