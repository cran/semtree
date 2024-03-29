% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getLeafs.R
\name{getLeafs}
\alias{getLeafs}
\title{Get a list of all leafs in a tree}
\usage{
getLeafs(tree, data = NULL)
}
\arguments{
\item{tree}{A \code{\link{semtree}} object}

\item{data}{A \code{data.frame}}
}
\description{
Get a list of all leafs in a tree by recursively searching the tree starting
at the given node (if not \code{data} object is given. If \code{data} is
given, the function returns the leafs that are predicted for each row of the
given data.
}
\references{
Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
U. (2013). Structural equation model trees. \emph{Psychological Methods},
18(1), 71-86.
}
\author{
Andreas M. Brandmaier
}
