% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluateDataLikelihood.R
\name{evaluateDataLikelihood}
\alias{evaluateDataLikelihood}
\title{Compute the Negative Two-Loglikelihood of some data given a model (either OpenMx or lavaan)}
\usage{
evaluateDataLikelihood(model, data, data_type = "raw")
}
\arguments{
\item{model}{A \code{\link{OpenMx}} model as used in \code{\link{semtree}}
and \code{\link{semforest}}.}

\item{data}{Data set to apply to a fitted model.}

\item{data_type}{Type of data ("raw", "cov", "cor")}
}
\value{
Returns a -2LL model fit for the model
}
\description{
This helper function is used
in the \code{\link{semforest}} \code{\link{varimp}} and
\code{\link{proximity}} aggregate functions.
}
\references{
Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
U. (2013). Structural equation model trees. \emph{Psychological Methods},
18(1), 71-86.
}
\seealso{
\code{\link{semtree}}, \code{\link{semforest}}
}
\author{
Andreas M. Brandmaier, John J. Prindle
}
