% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subforest.R
\name{subforest}
\alias{subforest}
\title{Creates subsets of trees from forests}
\usage{
subforest(forest, num = NULL, type = "nonnull", from = 1)
}
\arguments{
\item{forest}{A SEM Forest object.}

\item{num}{Number of trees to select.}

\item{type}{Either 'random' or 'nonnull' or NULL. First selects a random
subset, second selects all non-null trees, third allows subsetting trees.}

\item{from}{Starting index if type=NULL.}
}
\description{
Creates subsets of a forest. This can be used to subset a number of trees,
e.g. from:(from+num), or to remove all null (type="nonnull") trees that were
due to errors, or to randomly select a sub forest (type=random).
}
\references{
Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
U. (2013). Structural equation model trees. \emph{Psychological Methods},
18(1), 71-86.
}
\author{
Andreas M. Brandmaier, John J. Prindle
}
