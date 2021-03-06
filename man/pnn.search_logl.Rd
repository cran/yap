% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pnn.search_logl.R
\name{pnn.search_logl}
\alias{pnn.search_logl}
\title{Search for the optimal value of PNN smoothing parameter based on the cross entropy}
\usage{
pnn.search_logl(net, sigmas, nfolds = 4, seed = 1)
}
\arguments{
\item{net}{A PNN object generated by pnn.fit()}

\item{sigmas}{A numeric vector to search for the best smoothing parameter}

\item{nfolds}{A scalar for the number of n-fold, 4 by default}

\item{seed}{The seed value for the n-fold cross-validation, 1 by default}
}
\value{
The list of all searching outcomes and the best outcome
}
\description{
The function \code{pnn.search_logl} searches for the optimal value of PNN smoothing parameter by cross-validation.
}
\examples{
data(iris, package = "datasets")
Y <- iris[, 5]
X <- scale(iris[, 1:4])
pnet <- pnn.fit(x = X, y = Y)
pnn.search_logl(pnet, c(0.5, 1), nfolds = 2)
}
