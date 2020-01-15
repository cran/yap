#' Calculate the multiclass cross-entropy
#'
#' The function \code{logl} calculates the multiclass cross entropy
#'
#' @param y_true A matrix of multiclass 0/1 indicators
#' @param y_pred A matrix of predicted probability of each class
#'
#' @return The value of multiclass cross entropy
#'
#' @examples
#' data(iris, package = "datasets")
#' Y <- iris[, 5]
#' X <- scale(iris[, 1:4])
#' pnet <- pnn.fit(x = X, y = Y)
#' logl(y_true = pnet$y.ind, y_pred = pnn.predict(pnet, X))

logl <- function(y_true, y_pred) {
  if (is.matrix(y_true) == F) stop("y_true needs to be a matrix.", call. = F)
  if (is.matrix(y_pred) == F) stop("y_pred needs to be a matrix.", call. = F)
  return(mean(rowSums(-y_true * log(y_pred))))  
}
