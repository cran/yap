% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pnn.fit.R
\name{pnn.fit}
\alias{pnn.fit}
\title{Create a probabilistic neural network}
\usage{
pnn.fit(x, y, sigma = 1)
}
\arguments{
\item{x}{A matrix of predictors}

\item{y}{A vector of N-category factors}

\item{sigma}{A scalar with the positive value}
}
\value{
A PNN object
}
\description{
The function \code{pnn.fit} creates a probabilistic neural network (PNN)
}
\examples{
data(iris, package = "datasets")
Y <- iris[, 5]
X <- scale(iris[, 1:4])
pnet <- pnn.fit(x = X, y = Y)
}
\references{
Donald Specht. (1990). Probabilistic Neural Networks.
}
