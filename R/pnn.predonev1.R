#' Calculate the predicted probability for each category of PNN
#'
#' The function \code{pnn.predone} calculates the predicted probability for each category of PNN
#'
#' @param net A PNN object created by pnn.fit()
#' @param x   A vector of input predictors
#'
#' @return A one-row matrix of predicted probabilities
#'
#' @seealso \code{\link{pnn.fit}}
#'
#' @examples
#' data(iris, package = "datasets")
#' Y <- iris[, 5]
#' X <- scale(iris[, 1:4])
#' pnet <- pnn.fit(x = X, y = Y)
#' for (i in seq(5)) print(pnn.predone(pnet, X[i, ]))

pnn.predone <- function(net, x) {
  if (class(net) != "Probabilistic Neural Net") stop("net needs to be a PNN object.", call. = F)

  cn <- ncol(net$y.ind)
  xl <- split(net$x, seq(nrow(net$x)))
  d <- sum(exp(-Reduce(c, lapply(xl, function(xi) sum((x - xi) ^ 2))) / (2 * (net$sigma ^ 2))))
  n <- lapply(seq(cn), 
         function(i) sum(net$y.ind[, i] * exp(-Reduce(c, lapply(xl, function(xi) sum((x - xi) ^ 2))) / (2 * (net$sigma ^ 2)))))
  p <- matrix(unlist(n) / d, ncol = cn)
  colnames(p) <- colnames(net$y.ind) 
  return(p)
}
