\name{BuildAgeTable}
\alias{BuildAgeTable}

\title{Builds a data.frame holding an age table}

\description{
  Builds a \code{data.frame} holding generic data for each cross-section.
There will be as many rows as the number of cross-sections.
The \code{data.frame} can be saved in a csv file through \code{\link{ExportFile}}.
}

\usage{
BuildAgeTable(RWdata, Tree.object)
}

\arguments{
	\item{RWdata}{a \code{data.frame} holding the set of ring-widths. It can be imported from a csv text file through \code{\link{ImportRW}}. In case of broadleaf trees, the age table must be computed on the ring-width set of the main stem.}
	\item{Tree.object}{a \code{data.frame} holding tree data. This object is easily created through \code{\link{BuildTreeObject}}.}
}

\seealso{\code{\link{ImportRW}}\code{\link{BuildTreeObject}}\code{\link{ExportFile}}}

\value{
Returns a \code{data.frame} holding cross-sections heights as rownames. 
For each cross-section, the first column holds the number of rings counted, the second column holds the its radius (as the sum of the full set of ring-widths), the third column holds its age (the tree age minus number of rings) when its tip got to the cross-section level. 
Tree age is computed as the number of rings on the lowest cross-section.
}

\author{Marco Bascietto}

\examples{
# Recall and attach RW data
data(Lei301)
attach(Lei301)

AgeTable1 <- BuildAgeTable(RW.1A, Tree.features.1)

detach(Lei301)
}

\keyword{attribute}


