\name{JoinBranchesToStem}
\alias{JoinBranchesToStem}

\title{Joins stem-analyses of large branches to stem-analysis of main stem}

\description{
  The complex architecture of broadleaf trees requires stem-analysis be computed on each large branch. To get an overall picture of tree increments the stem-analyses should be joined together.
}

\usage{
JoinBranchesToStem(RWdata, Tree.object, branches)
}

\arguments{
	\item{RWdata}{a \code{data.frame} holding the set of ring-widths. It can be imported from a csv text file through \code{\link{ImportRW}}.}
	\item{Tree.object}{a \code{data.frame} holding tree data. This object is easily created through \code{\link{BuildTreeObject}}.}
	\item{branches}{a list of \code{dataframe} objects holding the stem analysis of the stem and of each branch. The list can be created with the \code{c} function}
}

\details{

}

\seealso{\code{\link{StemAnalysis}}}

\note{
It should be noted that the current version of tReeglia does not support different tree data objects for the branches of trees. 
As a result, it is not possible to assign different volume reduction coefficients, densities, and carbon contents to individual branches of the tree.}

\author{Marco Bascietto}

\examples{
# Recall and attach RW data
data(Lei301)
attach(Lei301)

SA.1 <- JoinBranchesToStem(RW.1A, Tree.features.1, c(SA.1.a,SA.1.b))

detach(Lei301)
}

\keyword{manip}

