% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/semtree.R
\name{semtree}
\alias{semtree}
\alias{plot.semtree}
\alias{print.semtree}
\alias{summary.semtree}
\alias{toLatex.semtree}
\alias{nodeFunSemtree}
\title{SEM Tree: Recursive Partitioning for Structural Equation Models}
\usage{
semtree(
  model,
  data = NULL,
  control = NULL,
  constraints = NULL,
  predictors = NULL,
  ...
)
}
\arguments{
\item{model}{A template model specification from \code{\link{OpenMx}} using
the \code{\link{mxModel}} function (or a \code{\link[lavaan]{lavaan}} model
using the \code{\link[lavaan]{lavaan}} function with option fit=FALSE).
Model must be syntactically correct within the framework chosen, and
converge to a solution.}

\item{data}{Data.frame used in the model creation using
\code{\link{mxModel}} or \code{\link[lavaan]{lavaan}} are input here. Order
of modeled variables and predictors is not important when providing a
dataset to \code{semtree}.}

\item{control}{\code{\link{semtree}} model specifications from
\code{\link{semtree.control}} are input here. Any changes from the default
setting can be specified here.}

\item{constraints}{A \code{\link{semtree.constraints}} object setting model
parameters as constrained from the beginning of the \code{semtree}
computation. This includes options to globally or locally set equality
constraints and to specify focus parameters (i.e., parameter subsets that
exclusively go into the function evaluating splits). Also, options for
measurement invariance testing in trees are included.}

\item{predictors}{A vector of variable names matching variable names in
dataset. If NULL (default) all variables that are in dataset and not part of
the model are potential predictors. Optional function input to select a
subset of the unmodeled variables to use as predictors in the \code{semtree}
function.}

\item{\dots}{Optional arguments passed to the tree growing function.}
}
\value{
A \code{semtree} object is created which can be examined with
\code{summary}, \code{plot}, and \code{print}.
}
\description{
Structural equation model (SEM) trees are a combination of SEM and decision
trees (also known as classification and regression trees or recursive
partitioning). SEM trees hierarchically split empirical data into
homogeneous groups sharing similar data patterns with respect to a SEM by
recursively selecting optimal predictors of these differences from a
potentially large set of predictors.
}
\details{
Calling \code{semtree} with an \code{\link{OpenMx}} or
\code{\link[lavaan]{lavaan}} model creates a tree that recursively
partitions a dataset such that the partitions maximally differ with respect
to the model-predicted distributions. Each resulting subgroup (represented
as a leaf in the tree) is represented by a SEM with a distinct set of
parameter estimates.

Predictors (yet unmodeled variables) can take on any form for the splitting
algorithm to function (categorical, ordered categories, continuous). Care
must be taken in choosing how many predictors to include in analyses because
as the number of categories grows for unordered categorical variables, the
number of multigroup comparisons increases exponentially for unordered
categories.

Currently available evaluation methods for assessing partitions:

1. "naive" selection method compares all possible split values to one
another over all predictors included in the dataset.

2. "fair" selection uses a two step procedure for analyzing split values on
predictors at each node of the tree. The first phase uses half of the sample
to examine the model improvement for each split value on each predictor, and
retains the the value that presents the largest improvement for each
predictor. The second phase then evaluates these best split points for each
predictor on the second half of the sample. The best improvement for the c
splits tested on c predictors is selected for the node and the dataset is
split from this node for further testing.

3. "score" uses score-based test statistics. These statistics are much
faster than the classic SEM tree approach while having favorable
statistical properties.

All other parameters controlling the tree growing process are available
through a separate \code{\link{semtree.control}} object.
}
\references{
Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger, U. (2013). Structural equation model trees. \emph{Psychological Methods}, 18(1), 71-86.

Arnold, M., Voelkle, M. C., & Brandmaier, A. M. (2021). Score-guided structural equation model trees. \emph{Frontiers in Psychology}, 11, Article 564403. https://doi.org/10.3389/fpsyg.2020.564403
}
\seealso{
\code{\link{semtree.control}}, \code{\link{summary.semtree}},
\code{\link{parameters}}, \code{\link{se}}, \code{\link{prune.semtree}},
\code{\link{subtree}}, \code{\link[OpenMx]{OpenMx}},
\code{\link[lavaan]{lavaan}}
}
\author{
Andreas M. Brandmaier, John J. Prindle, Manuel Arnold
}
\keyword{models}
\keyword{multivariate}
\keyword{tree}
